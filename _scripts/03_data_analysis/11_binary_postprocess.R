source("_scripts/03_data_analysis/08_binary_config.R")
source("_scripts/03_data_analysis/09_binary_helpers.R")

binary_cluster_path <- function(sensor) {
  file.path(
    BINARY_RASTER_DIR,
    paste0(str_replace_all(str_to_lower(sensor), "[^a-z0-9]+", "_"), "cluster_binary.tif")
  )
}

binary_time_series <- function(stack, river, sensor, workflow) {
  river_projected <- project(river, crs(stack))
  corridor <- rasterize(river_projected, stack[[1]], field = 1, background = NA)
  stack <- mask(stack, corridor)
  date_names <- setdiff(names(stack), "baseline")
  dates <- as.Date(date_names, format = "%Y%m%d")
  if (anyNA(dates)) stop("Unparseable binary prediction dates for ", sensor, ".")
  cell_area_acres <- prod(res(stack[[1]])) * 0.00024710538146717

  baseline <- count_binary_codes(stack[["baseline"]]) |>
    transmute(class, baseline_pixels = n, baseline_area_acres = n * cell_area_acres)
  map2_dfr(date_names, dates, function(layer_name, date_value) {
    count_binary_codes(stack[[layer_name]]) |>
      transmute(date = date_value, class, pixels = n, area_acres = n * cell_area_acres)
  }) |>
    left_join(baseline, by = "class") |>
    arrange(class, date) |>
    group_by(class) |>
    mutate(
      between_date_change_acres = area_acres - lag(area_acres),
      baseline_change_acres = area_acres - baseline_area_acres
    ) |>
    ungroup() |>
    mutate(sensor = sensor, workflow = workflow, .before = 1)
}

run_binary_time_series <- function() {
  river <- vect(BINARY_RIVER_CORRIDOR)
  configs <- filter(BINARY_SENSOR_CONFIG, reference_table)
  output <- map_dfr(seq_len(nrow(configs)), function(i) {
    sensor <- configs$sensor[[i]]
    bind_rows(
      binary_time_series(rast(binary_cluster_path(sensor)), river, sensor, "Cluster-assisted"),
      binary_time_series(rast(configs$rf_prediction_path[[i]]), river, sensor, "Random forest")
    )
  })
  write_binary_csv(output, "binary_time_series_all_sensors.csv")
  output
}

evaluate_binary_against_hydraulics <- function() {
  river <- vect(BINARY_RIVER_CORRIDOR)
  hydraulic <- rast(BINARY_HYDRAULIC_REFERENCE)
  rf_config <- filter(BINARY_SENSOR_CONFIG, !reference_table)
  rf <- rast(rf_config$rf_prediction_path[[1]])
  cluster <- rast(binary_cluster_path(rf_config$sensor[[1]]))
  river_grid <- rasterize(project(river, crs(rf)), rf[[1]], field = 1, background = NA)
  cell_area_acres <- prod(res(rf[[1]])) * 0.00024710538146717

  dated <- pmap_dfr(BINARY_DATE_MAP, function(test_id, hydraulic_date, image_date) {
    layer_name <- format(image_date, "%Y%m%d")
    observed <- project(hydraulic[[test_id]], rf[[layer_name]], method = "near")
    observed_water <- ifel(!is.na(observed), 1, 0)
    common_domain <- ifel(
      !is.na(river_grid) & !is.na(cluster[[layer_name]]) & !is.na(rf[[layer_name]]),
      1,
      NA
    )
    observed_water <- mask(observed_water, common_domain)

    imap_dfr(
      list("Cluster-assisted" = cluster[[layer_name]], "Random forest" = rf[[layer_name]]),
      function(predicted, workflow) {
        predicted <- mask(predicted, common_domain)
        predicted_water <- ifel(predicted == 2L, 1, 0)
        confusion_from_binary_rasters(predicted_water, observed_water) |>
          mutate(
            test_id = test_id,
            hydraulic_reference_date = hydraulic_date,
            image_prediction_date = image_date,
            workflow = workflow,
            positive_class = "surface water",
            tp_acres = tp * cell_area_acres,
            tn_acres = tn * cell_area_acres,
            fp_acres = fp * cell_area_acres,
            fn_acres = fn * cell_area_acres,
            observed_water_acres = (tp + fn) * cell_area_acres,
            predicted_water_acres = (tp + fp) * cell_area_acres,
            extent_error_acres = predicted_water_acres - observed_water_acres,
            absolute_extent_error_acres = abs(extent_error_acres),
            .before = 1
          )
      }
    )
  })

  metric_names <- c(
    "accuracy", "sensitivity", "specificity", "precision", "npv", "f1",
    "mcc", "informedness_j", "balanced_accuracy"
  )
  summary <- dated |>
    pivot_longer(all_of(metric_names), names_to = "metric", values_to = "estimate") |>
    group_by(workflow, metric) |>
    group_modify(~ mean_ci_binary(.x$estimate)) |>
    ungroup()
  tests <- dated |>
    pivot_longer(all_of(metric_names), names_to = "metric", values_to = "estimate") |>
    group_by(metric) |>
    group_modify(~ paired_wilcox_binary(
      .x, "estimate", "workflow", "test_id", "Cluster-assisted", "Random forest"
    )) |>
    ungroup()

  mean_confusion_acres <- dated |>
    group_by(workflow) |>
    summarise(
      observed_land_predicted_land = mean(tn_acres),
      observed_land_predicted_water = mean(fp_acres),
      observed_water_predicted_land = mean(fn_acres),
      observed_water_predicted_water = mean(tp_acres),
      .groups = "drop"
    )

  pooled_confusion_counts <- dated |>
    group_by(workflow) |>
    summarise(
      tn = sum(tn),
      fp = sum(fp),
      fn = sum(fn),
      tp = sum(tp),
      .groups = "drop"
    )

  write_binary_csv(dated, "hydraulic_date_specific_validation.csv")
  write_binary_csv(summary, "hydraulic_date_specific_validation_summary_95ci.csv")
  write_binary_csv(tests, "hydraulic_date_specific_validation_paired_tests.csv")
  write_binary_csv(mean_confusion_acres, "hydraulic_confusion_mean_acres.csv")
  write_binary_csv(pooled_confusion_counts, "hydraulic_confusion_pooled_counts.csv")
  list(
    dated = dated,
    summary = summary,
    tests = tests,
    mean_confusion_acres = mean_confusion_acres,
    pooled_confusion_counts = pooled_confusion_counts
  )
}

run_binary_baseline_comparison <- function(dated_validation) {
  river <- vect(BINARY_RIVER_CORRIDOR)
  hydraulic <- rast(BINARY_HYDRAULIC_REFERENCE)
  rf_config <- filter(BINARY_SENSOR_CONFIG, !reference_table)
  rf <- rast(rf_config$rf_prediction_path[[1]])
  cluster <- rast(binary_cluster_path(rf_config$sensor[[1]]))
  river_grid <- rasterize(project(river, crs(rf)), rf[[1]], field = 1, background = NA)
  cell_area_acres <- prod(res(rf[[1]])) * 0.00024710538146717
  hydraulic_median <- project(hydraulic[["median"]], rf[[1]], method = "near")

  baseline_changes <- pmap_dfr(BINARY_DATE_MAP, function(test_id, hydraulic_date, image_date) {
    layer_name <- format(image_date, "%Y%m%d")
    observed_date <- project(hydraulic[[test_id]], rf[[layer_name]], method = "near")
    common_domain <- ifel(
      !is.na(river_grid) & !is.na(cluster[[layer_name]]) & !is.na(rf[[layer_name]]) &
        !is.na(cluster[["baseline"]]) & !is.na(rf[["baseline"]]),
      1,
      NA
    )
    observed_date_water <- mask(ifel(!is.na(observed_date), 1, 0), common_domain)
    observed_baseline_water <- mask(ifel(!is.na(hydraulic_median), 1, 0), common_domain)
    observed_date_acres <- global(observed_date_water, "sum", na.rm = TRUE)[1, 1] * cell_area_acres
    observed_baseline_acres <- global(observed_baseline_water, "sum", na.rm = TRUE)[1, 1] * cell_area_acres

    imap_dfr(
      list("Cluster-assisted" = cluster, "Random forest" = rf),
      function(prediction, workflow) {
        predicted_date_water <- mask(ifel(prediction[[layer_name]] == 2L, 1, 0), common_domain)
        predicted_baseline_water <- mask(ifel(prediction[["baseline"]] == 2L, 1, 0), common_domain)
        predicted_date_acres <- global(predicted_date_water, "sum", na.rm = TRUE)[1, 1] * cell_area_acres
        predicted_baseline_acres <- global(predicted_baseline_water, "sum", na.rm = TRUE)[1, 1] * cell_area_acres
        tibble(
          test_id = test_id,
          hydraulic_reference_date = hydraulic_date,
          image_prediction_date = image_date,
          workflow = workflow,
          predicted_date_water_acres = predicted_date_acres,
          predicted_baseline_water_acres = predicted_baseline_acres,
          predicted_baseline_change_acres = predicted_date_acres - predicted_baseline_acres,
          hydraulic_date_water_acres = observed_date_acres,
          hydraulic_median_water_acres = observed_baseline_acres,
          hydraulic_baseline_change_acres = observed_date_acres - observed_baseline_acres,
          baseline_change_error_acres = predicted_baseline_change_acres - hydraulic_baseline_change_acres
        )
      }
    )
  })

  comparison_summary <- baseline_changes |>
    group_by(workflow) |>
    group_modify(~ {
      result <- suppressWarnings(cor.test(
        .x$predicted_baseline_change_acres,
        .x$hydraulic_baseline_change_acres,
        method = "spearman",
        exact = TRUE
      ))
      tibble(
        n = nrow(.x),
        mean_absolute_change_error_acres = mean(abs(.x$baseline_change_error_acres)),
        root_mean_square_change_error_acres = sqrt(mean(.x$baseline_change_error_acres^2)),
        spearman_rho = unname(result$estimate),
        p_value = result$p.value
      )
    }) |>
    ungroup()
  write_binary_csv(baseline_changes, "baseline_change_hydraulic_comparison.csv")
  write_binary_csv(comparison_summary, "baseline_change_hydraulic_summary.csv")
  list(comparison = baseline_changes, summary = comparison_summary)
}

run_binary_preceding_change_comparison <- function(baseline_changes) {
  preceding_changes <- baseline_changes |>
    arrange(workflow, image_prediction_date) |>
    group_by(workflow) |>
    mutate(
      preceding_image_date = lag(image_prediction_date),
      preceding_hydraulic_date = lag(hydraulic_reference_date),
      predicted_preceding_change_acres = predicted_date_water_acres - lag(predicted_date_water_acres),
      hydraulic_preceding_change_acres = hydraulic_date_water_acres - lag(hydraulic_date_water_acres),
      preceding_change_error_acres = predicted_preceding_change_acres - hydraulic_preceding_change_acres
    ) |>
    ungroup() |>
    filter(!is.na(predicted_preceding_change_acres), !is.na(hydraulic_preceding_change_acres))

  comparison_summary <- preceding_changes |>
    group_by(workflow) |>
    group_modify(~ {
      result <- suppressWarnings(cor.test(
        .x$predicted_preceding_change_acres,
        .x$hydraulic_preceding_change_acres,
        method = "spearman",
        exact = TRUE
      ))
      tibble(
        n = nrow(.x),
        mean_absolute_change_error_acres = mean(abs(.x$preceding_change_error_acres)),
        root_mean_square_change_error_acres = sqrt(mean(.x$preceding_change_error_acres^2)),
        spearman_rho = unname(result$estimate),
        p_value = result$p.value
      )
    }) |>
    ungroup()

  write_binary_csv(preceding_changes, "preceding_change_hydraulic_comparison.csv")
  write_binary_csv(comparison_summary, "preceding_change_hydraulic_summary.csv")
  list(comparison = preceding_changes, summary = comparison_summary)
}

run_binary_hydrologic_correlations <- function(dated_validation) {
  usgs <- read_csv("_data/Tables/USGS/meanData.csv", show_col_types = FALSE) |>
    mutate(Date = as.Date(Date), wamego_cfs = .data[["6887500"]])
  joined <- dated_validation |>
    left_join(select(usgs, Date, wamego_cfs), by = c("hydraulic_reference_date" = "Date"))
  correlations <- joined |>
    group_by(workflow) |>
    group_modify(~ bind_rows(
      {
        result <- suppressWarnings(cor.test(.x$predicted_water_acres, .x$wamego_cfs, method = "spearman", exact = TRUE))
        tibble(comparison = "predicted water versus discharge", n = nrow(.x), rho = unname(result$estimate), p_value = result$p.value)
      },
      {
        result <- suppressWarnings(cor.test(.x$predicted_water_acres, .x$observed_water_acres, method = "spearman", exact = TRUE))
        tibble(comparison = "predicted water versus hydraulic extent", n = nrow(.x), rho = unname(result$estimate), p_value = result$p.value)
      }
    )) |>
    ungroup()
  write_binary_csv(joined, "hydraulic_water_extent_and_discharge.csv")
  write_binary_csv(correlations, "hydraulic_water_extent_correlations.csv")
  correlations
}

run_binary_postprocessing <- function() {
  time_series <- run_binary_time_series()
  hydraulic <- evaluate_binary_against_hydraulics()
  baseline <- run_binary_baseline_comparison(hydraulic$dated)
  preceding <- run_binary_preceding_change_comparison(baseline$comparison)
  correlations <- run_binary_hydrologic_correlations(hydraulic$dated)
  correlation_summary <- correlations |>
    arrange(comparison, workflow)
  write_binary_csv(correlation_summary, "spearman_correlations_summary.csv")
  message("Binary surface-water post-processing complete.")
  invisible(list(
    time_series = time_series,
    hydraulic = hydraulic,
    baseline = baseline,
    preceding = preceding,
    correlations = correlation_summary
  ))
}

if (sys.nframe() == 0L) run_binary_postprocessing()
