source("_scripts/03_data_analysis/00_config.R")
source("_scripts/03_data_analysis/01_helpers.R")

count_class_codes <- function(layer, class_codes) {
  # as.numeric() removes the category table while preserving the underlying
  # integer class codes. catalyze() can reorder codes alphabetically.
  numeric_layer <- if (terra::is.factor(layer)) as.numeric(layer) else layer
  frequencies <- freq(numeric_layer)
  observed <- if (is.null(frequencies) || !nrow(frequencies)) {
    tibble(code = integer(), n = numeric())
  } else {
    tibble(code = as.integer(frequencies$value), n = as.numeric(frequencies$count))
  }
  tibble(code = class_codes) |>
    left_join(observed, by = "code") |>
    mutate(n = replace_na(n, 0))
}

normalise_zero_one <- function(x) {
  limits <- range(x, na.rm = TRUE)
  if (!all(is.finite(limits)) || diff(limits) == 0) return(rep(0, length(x)))
  (x - limits[[1]]) / diff(limits)
}

time_series_from_stack <- function(stack, river, method, code_to_class) {
  river_projected <- project(river, crs(stack))
  stack <- mask(stack, river_projected)
  if (!"baseline" %in% names(stack)) stop("Prediction stack lacks a baseline layer.")
  date_names <- setdiff(names(stack), "baseline")
  dates <- as.Date(date_names, format = "%Y%m%d")
  if (anyNA(dates)) stop("One or more prediction layers are not named as YYYYMMDD dates.")
  class_codes <- as.integer(names(code_to_class))
  cell_area_acres <- prod(res(stack[[1]])) * 0.00024710538146717

  baseline <- count_class_codes(stack[["baseline"]], class_codes) |>
    mutate(n_percent_baseline = 100 * n / sum(n)) |>
    transmute(
      class = unname(code_to_class[as.character(code)]),
      n_baseline = n,
      n_percent_baseline = n_percent_baseline,
      area_baseline = n * cell_area_acres
    )

  dated <- map2_dfr(date_names, dates, function(layer_name, date_value) {
    count_class_codes(stack[[layer_name]], class_codes) |>
      mutate(date = date_value, .before = 1)
  }) |>
    mutate(class = unname(code_to_class[as.character(code)])) |>
    group_by(date) |>
    mutate(n_percent = 100 * n / sum(n)) |>
    ungroup() |>
    mutate(area = n * cell_area_acres) |>
    left_join(baseline, by = "class") |>
    arrange(class, date) |>
    group_by(class) |>
    mutate(
      area_diff = area - lag(area),
      diff_percent = 100 * area_diff / lag(area),
      area_diff_baseline = area - area_baseline,
      diff_percent_baseline = 100 * area_diff_baseline / area_baseline,
      diff_normalized = normalise_zero_one(area_diff),
      diff_normalized_baseline = normalise_zero_one(area_diff_baseline)
    ) |>
    ungroup() |>
    mutate(method = method) |>
    select(
      date, class, n, n_percent, area,
      n_baseline, n_percent_baseline, area_baseline,
      area_diff, diff_percent, diff_normalized,
      area_diff_baseline, diff_percent_baseline, diff_normalized_baseline,
      method
    )
  dated
}

KMEANS_RECLASS <- list(
  "Landsat-9" = c(1L, 5L, 3L, 2L, 1L, 1L),
  "Sentinel-2" = c(2L, 1L, 1L, 5L, 3L, 1L),
  "PlanetScope" = c(1L, 3L, 5L, 2L, 1L, 1L)
)

KMEANS_PATHS <- c(
  "Landsat-9" = "_data/GIS/Raster/Analysis/Kmeans/km_landsat_22_core4.tif",
  "Sentinel-2" = "_data/GIS/Raster/Analysis/Kmeans/km_sentinel_22_core4.tif",
  "PlanetScope" = "_data/GIS/Raster/Analysis/Kmeans/km_planet_22.tif"
)

TIME_SERIES_FILENAMES <- c(
  "Landsat-9" = "resultsTimeSeries_landsat_core4_balanced.csv",
  "Sentinel-2" = "resultsTimeSeries_sentinel_core4_balanced.csv",
  "PlanetScope" = "resultsTimeSeries_planet_balanced.csv"
)

run_landcover_time_series <- function() {
  river <- vect(RIVER_CORRIDOR)
  outputs <- map_dfr(seq_len(nrow(sensor_config)), function(i) {
    sensor <- sensor_config$sensor[[i]]
    rf <- rast(sensor_config$prediction_path[[i]])
    km_raw <- rast(KMEANS_PATHS[[sensor]])
    if (!identical(names(rf), names(km_raw))) {
      stop("Layer names differ between balanced RF and cluster-assisted stacks for ", sensor)
    }
    reclass_matrix <- cbind(seq_along(KMEANS_RECLASS[[sensor]]), KMEANS_RECLASS[[sensor]])
    km <- classify(km_raw, reclass_matrix)
    names(km) <- names(rf)

    rf_series <- time_series_from_stack(
      rf,
      river,
      "randomforest",
      setNames(LANDCOVER_LEVELS, seq_along(LANDCOVER_LEVELS))
    )
    km_series <- time_series_from_stack(
      km,
      river,
      "kmeans",
      c("1" = "bareland", "2" = "agricultural_land", "3" = "forest", "5" = "water")
    )
    combined <- bind_rows(km_series, rf_series)
    write_balanced_csv(combined, TIME_SERIES_FILENAMES[[sensor]])
    combined |> mutate(sensor = sensor, .before = 1)
  })
  write_balanced_csv(outputs, "landcover_time_series_all_sensors.csv")
  outputs
}

summarize_landcover_results <- function(time_series) {
  exposed_classes <- c("bareland", "agricultural_land", "urban")
  grouped <- time_series |>
    mutate(
      comparison_class = case_when(
        class == "water" ~ "surface_water",
        class %in% exposed_classes ~ "exposed_land",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(comparison_class)) |>
    group_by(sensor, method, date, comparison_class) |>
    summarise(
      area = sum(area),
      area_diff = sum(area_diff),
      area_diff_baseline = sum(area_diff_baseline),
      .groups = "drop"
    )

  ranges <- grouped |>
    group_by(sensor, method, comparison_class) |>
    summarise(minimum_acres = min(area), maximum_acres = max(area), .groups = "drop")

  wide <- grouped |>
    pivot_wider(
      names_from = comparison_class,
      values_from = c(area, area_diff, area_diff_baseline)
    )

  kendall <- map_dfr(c("area_diff", "area_diff_baseline"), function(definition) {
    water_column <- paste0(definition, "_surface_water")
    exposed_column <- paste0(definition, "_exposed_land")
    wide |>
      group_by(sensor, method) |>
      group_modify(~ {
        keep <- complete.cases(.x[[water_column]], .x[[exposed_column]])
        result <- suppressWarnings(stats::cor.test(
          .x[[water_column]][keep],
          .x[[exposed_column]][keep],
          method = "kendall",
          exact = FALSE
        ))
        tibble(
          change_definition = ifelse(definition == "area_diff", "between-date", "time-aggregated-baseline-relative"),
          n = sum(keep),
          tau = unname(result$estimate),
          p_value = result$p.value
        )
      }) |>
      ungroup()
  })
  write_balanced_csv(ranges, "landcover_area_ranges.csv")
  write_balanced_csv(kendall, "landcover_water_exposed_kendall.csv")
  list(grouped = grouped, ranges = ranges, kendall = kendall)
}

MESO_VALIDATION_DATES <- tribble(
  ~test_id, ~hydraulic_date, ~image_date,
  "01_Dec22_2021", as.Date("2021-12-22"), as.Date("2021-12-22"),
  "02_Feb12_2022", as.Date("2022-02-12"), as.Date("2022-02-12"),
  "03_April17_2022", as.Date("2022-04-17"), as.Date("2022-04-18"),
  "04_May31_2022", as.Date("2022-05-31"), as.Date("2022-05-31"),
  "05_June20_2022", as.Date("2022-06-20"), as.Date("2022-06-20"),
  "06_July18_2022", as.Date("2022-07-18"), as.Date("2022-07-18"),
  "07_Sep28_2022", as.Date("2022-09-28"), as.Date("2022-09-28")
)

run_water_validation <- function() {
  river <- vect(RIVER_CORRIDOR)
  hydraulic <- rast("_data/GIS/Raster/Clean/mesohabitats_v2.tif")
  km <- rast("_data/GIS/Raster/Analysis/Kmeans/km_planet_7.tif")
  rf <- rast("_data/GIS/Raster/Analysis/RandomForest/balanced/rf_planet_7_balanced.tif")
  river_grid <- rasterize(project(river, crs(rf)), rf[[1]], field = 1, background = NA)
  cell_area_acres <- prod(res(rf[[1]])) * 0.00024710538146717

  dated <- pmap_dfr(MESO_VALIDATION_DATES, function(test_id, hydraulic_date, image_date) {
    prediction_name <- format(image_date, "%Y%m%d")
    observed <- project(hydraulic[[test_id]], rf[[prediction_name]], method = "near")
    observed_water <- ifel(!is.na(observed), 1, 0)
    common_domain <- ifel(
      !is.na(river_grid) & !is.na(km[[prediction_name]]) & !is.na(rf[[prediction_name]]),
      1,
      NA
    )
    observed_water <- mask(observed_water, common_domain)

    model_inputs <- list(
      "Cluster-assisted" = list(raster = km[[prediction_name]], water_code = 3L),
      "Random forest" = list(raster = rf[[prediction_name]], water_code = 5L)
    )
    map_dfr(names(model_inputs), function(model_name) {
      input <- model_inputs[[model_name]]
      predicted <- mask(input$raster, common_domain)
      predicted_water <- ifel(predicted == input$water_code, 1, 0)
      confusion_from_binary_rasters(predicted_water, observed_water) |>
        mutate(
          test_id = test_id,
          hydraulic_reference_date = hydraulic_date,
          image_prediction_date = image_date,
          model = model_name,
          tp_acres = tp * cell_area_acres,
          tn_acres = tn * cell_area_acres,
          fp_acres = fp * cell_area_acres,
          fn_acres = fn * cell_area_acres,
          observed_water_acres = (tp + fn) * cell_area_acres,
          predicted_water_acres = (tp + fp) * cell_area_acres,
          .before = 1
        )
    })
  })

  metric_names <- c("accuracy", "sensitivity", "specificity", "precision", "npv", "f1", "mcc", "informedness_j", "balanced_accuracy")
  summary <- dated |>
    pivot_longer(all_of(metric_names), names_to = "metric", values_to = "estimate") |>
    group_by(model, metric) |>
    group_modify(~ mean_ci(.x$estimate)) |>
    ungroup()
  tests <- dated |>
    pivot_longer(all_of(metric_names), names_to = "metric", values_to = "estimate") |>
    group_by(metric) |>
    group_modify(~ paired_wilcox(.x, "estimate", "model", "test_id", "Cluster-assisted", "Random forest")) |>
    ungroup()

  write_balanced_csv(dated, "date_specific_water_validation.csv")
  write_balanced_csv(summary, "date_specific_water_validation_summary_95ci.csv")
  write_balanced_csv(tests, "date_specific_water_validation_paired_tests.csv")
  list(dated = dated, summary = summary, tests = tests)
}

run_median_water_validation <- function() {
  river <- vect(RIVER_CORRIDOR)
  hydraulic_median <- rast("_data/GIS/Raster/Clean/mesohabitats_v2.tif")[["median"]]
  km <- rast("_data/GIS/Raster/Analysis/Kmeans/km_planet_7.tif")[["baseline"]]
  rf <- rast("_data/GIS/Raster/Analysis/RandomForest/balanced/rf_planet_7_balanced.tif")[["baseline"]]
  river_projected <- project(river, crs(rf))
  river_grid <- rasterize(river_projected, rf, field = 1, background = NA)
  observed <- project(hydraulic_median, rf, method = "near")
  observed_water <- ifel(!is.na(observed), 1, 0)
  common_domain <- ifel(!is.na(river_grid) & !is.na(km) & !is.na(rf), 1, NA)
  observed_water <- mask(observed_water, common_domain)
  cell_area_acres <- prod(res(rf)) * 0.00024710538146717

  results <- bind_rows(
    confusion_from_binary_rasters(ifel(mask(km, common_domain) == 3L, 1, 0), observed_water) |>
      mutate(model = "Cluster-assisted", .before = 1),
    confusion_from_binary_rasters(ifel(mask(rf, common_domain) == 5L, 1, 0), observed_water) |>
      mutate(model = "Random forest", .before = 1)
  ) |>
    mutate(
      tp_acres = tp * cell_area_acres,
      tn_acres = tn * cell_area_acres,
      fp_acres = fp * cell_area_acres,
      fn_acres = fn * cell_area_acres
    )
  write_balanced_csv(results, "median_reference_water_validation.csv")
  results
}

run_hydrologic_correlations <- function(dated_validation) {
  usgs <- read_csv("_data/Tables/USGS/meanData.csv", show_col_types = FALSE) |>
    mutate(Date = as.Date(Date), wamego_cfs = .data[["6887500"]])
  joined <- dated_validation |>
    left_join(select(usgs, Date, wamego_cfs), by = c("image_prediction_date" = "Date"))

  correlations <- joined |>
    group_by(model) |>
    group_modify(~ bind_rows(
      {
        test <- suppressWarnings(cor.test(.x$predicted_water_acres, .x$wamego_cfs, method = "spearman", exact = TRUE))
        tibble(comparison = "predicted water versus discharge", n = nrow(.x), rho = unname(test$estimate), p_value = test$p.value)
      },
      {
        test <- suppressWarnings(cor.test(.x$predicted_water_acres, .x$observed_water_acres, method = "spearman", exact = TRUE))
        tibble(comparison = "predicted water versus hydraulic extent", n = nrow(.x), rho = unname(test$estimate), p_value = test$p.value)
      }
    )) |>
    ungroup()
  write_balanced_csv(joined, "date_specific_water_extent_and_discharge.csv")
  write_balanced_csv(correlations, "water_extent_spearman_correlations.csv")
  correlations
}

run_postprocessing <- function() {
  time_series <- run_landcover_time_series()
  landcover_summary <- summarize_landcover_results(time_series)
  water_validation <- run_water_validation()
  median_validation <- run_median_water_validation()
  hydrology <- run_hydrologic_correlations(water_validation$dated)
  message("Balanced post-processing complete.")
  invisible(list(
    time_series = time_series,
    landcover = landcover_summary,
    water = water_validation,
    median = median_validation,
    hydrology = hydrology
  ))
}

if (sys.nframe() == 0L) run_postprocessing()
