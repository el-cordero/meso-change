# Reviewer-response post-processing for the manuscript revision
#
# This script uses existing rasters, cached training tables, and saved
# leave-one-date-out metrics. It does not fit or refit any model, and it does
# not create or modify figures. All derived tables are written to
# _data/Tables/reviewer_revision.

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
})

set.seed(42)

out_dir <- "_data/Tables/reviewer_revision"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write_out <- function(x, filename) {
  readr::write_csv(x, file.path(out_dir, filename), na = "")
}

mean_ci <- function(x, conf = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0) {
    return(tibble(
      n = 0L, mean = NA_real_, sd = NA_real_, ci_low = NA_real_,
      ci_high = NA_real_, minimum = NA_real_, maximum = NA_real_
    ))
  }
  m <- mean(x)
  s <- stats::sd(x)
  half_width <- if (n > 1) stats::qt((1 + conf) / 2, df = n - 1) * s / sqrt(n) else NA_real_
  tibble(
    n = n,
    mean = m,
    sd = s,
    ci_low = m - half_width,
    ci_high = m + half_width,
    minimum = min(x),
    maximum = max(x)
  )
}

paired_wilcox <- function(data, value_col, group_col, id_col, group_1, group_2) {
  wide <- data |>
    select(all_of(c(id_col, group_col, value_col))) |>
    pivot_wider(names_from = all_of(group_col), values_from = all_of(value_col)) |>
    filter(is.finite(.data[[group_1]]), is.finite(.data[[group_2]]))

  if (nrow(wide) < 2) {
    return(tibble(
      group_1 = group_1,
      group_2 = group_2,
      n_pairs = nrow(wide),
      median_group_1 = if (nrow(wide) > 0) median(wide[[group_1]]) else NA_real_,
      median_group_2 = if (nrow(wide) > 0) median(wide[[group_2]]) else NA_real_,
      median_paired_difference = if (nrow(wide) > 0) median(wide[[group_1]] - wide[[group_2]]) else NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      test = "paired Wilcoxon signed-rank test (insufficient finite pairs)"
    ))
  }

  test <- stats::wilcox.test(
    wide[[group_1]], wide[[group_2]],
    paired = TRUE, exact = FALSE, conf.int = FALSE
  )

  tibble(
    group_1 = group_1,
    group_2 = group_2,
    n_pairs = nrow(wide),
    median_group_1 = median(wide[[group_1]]),
    median_group_2 = median(wide[[group_2]]),
    median_paired_difference = median(wide[[group_1]] - wide[[group_2]]),
    statistic = unname(test$statistic),
    p_value = test$p.value,
    test = "paired Wilcoxon signed-rank test"
  )
}

# -----------------------------------------------------------------------------
# 1. Authentic leave-one-date-out results
# -----------------------------------------------------------------------------

loo_path <- "_data/Tables/meso_classification/loo_fold_metrics.csv"
loo <- read_csv(loo_path, show_col_types = FALSE)

loo_summary_ci <- loo |>
  group_by(model, .metric) |>
  group_modify(~ mean_ci(.x$.estimate)) |>
  ungroup()

loo_tests <- loo |>
  group_by(.metric) |>
  group_modify(~ paired_wilcox(
    .x,
    value_col = ".estimate",
    group_col = "model",
    id_col = "test_id",
    group_1 = "SemiSupervised",
    group_2 = "SupervisedRF"
  )) |>
  ungroup()

loo_provenance <- tribble(
  ~file, ~status, ~interpretation,
  "_data/Tables/meso_classification/loo_fold_metrics.csv",
  "authoritative",
  "Metrics saved during the seven-date leave-one-date-out run; each date was evaluated using a model trained without that date.",
  "_data/Tables/meso_classification/loo_fold_metrics_FROM_SAVED.csv",
  "not a holdout estimate",
  "Metrics reconstructed from FINAL prediction rasters produced by models trained on all seven dates; these values describe fitted-model predictions and must not be reported as leave-one-date-out performance.",
  "_data/Tables/meso_classification/loo_summary_FROM_SAVED.csv",
  "not a holdout estimate",
  "Summary of the FINAL-raster metrics above; not evidence of temporal generalization."
)

write_out(loo, "loo_fold_metrics_authoritative.csv")
write_out(loo_summary_ci, "loo_summary_with_95ci.csv")
write_out(loo_tests, "loo_paired_model_tests.csv")
write_out(loo_provenance, "loo_metric_provenance.csv")

# -----------------------------------------------------------------------------
# 2. Mesohabitat class support from cached labelled pixels
# -----------------------------------------------------------------------------

date_map <- tribble(
  ~test_id, ~prediction_date,
  "01_Dec22_2021", as.Date("2021-12-22"),
  "02_Feb12_2022", as.Date("2022-02-12"),
  "03_April17_2022", as.Date("2022-04-18"),
  "04_May31_2022", as.Date("2022-05-31"),
  "05_June20_2022", as.Date("2022-06-20"),
  "06_July18_2022", as.Date("2022-07-18"),
  "07_Sep28_2022", as.Date("2022-09-28")
)

xy_dir <- "_data/Cache/meso_classification_v2/xy_tables"
xy_paths <- file.path(xy_dir, paste0("xy_fact1_", date_map$test_id, ".rds"))
stopifnot(all(file.exists(xy_paths)))

meso_support <- map2_dfr(xy_paths, date_map$test_id, function(path, test_id) {
  x <- readRDS(path)
  count(x, meso, name = "n_pixels") |>
    mutate(test_id = test_id, .before = 1)
}) |>
  mutate(meso = as.character(meso)) |>
  group_by(test_id) |>
  mutate(percent_within_date = 100 * n_pixels / sum(n_pixels)) |>
  ungroup() |>
  left_join(date_map, by = "test_id")

meso_fold_support <- crossing(
  test_id = date_map$test_id,
  meso = sort(unique(meso_support$meso))
) |>
  left_join(
    meso_support |>
      select(test_id, meso, test_n_pixels = n_pixels),
    by = c("test_id", "meso")
  ) |>
  mutate(test_n_pixels = replace_na(test_n_pixels, 0L)) |>
  group_by(meso) |>
  mutate(all_dates_n_pixels = sum(test_n_pixels)) |>
  ungroup() |>
  mutate(
    train_n_pixels = all_dates_n_pixels - test_n_pixels,
    rf_train_n_after_per_class_cap = pmin(train_n_pixels, 50000L)
  ) |>
  left_join(date_map, by = "test_id")

write_out(meso_support, "mesohabitat_labelled_pixel_support_by_date.csv")
write_out(meso_fold_support, "mesohabitat_fold_train_test_support.csv")

# -----------------------------------------------------------------------------
# 3. Land-cover reference support
# -----------------------------------------------------------------------------

classes <- rast("_data/GIS/Raster/Analysis/classes.tif")
class_values <- as.data.frame(classes, na.rm = TRUE)

landcover_pixels <- class_values |>
  transmute(class = as.character(class), split_code = training) |>
  count(class, split_code, name = "n_pixels") |>
  mutate(
    split = case_when(
      split_code == 1 ~ "training",
      split_code == 0 ~ "validation",
      TRUE ~ paste0("code_", split_code)
    )
  ) |>
  select(class, split, n_pixels) |>
  arrange(class, split)

reference_polygons <- vect("_data/GIS/Vector/userDelineated.shp") |>
  as.data.frame() |>
  count(class, name = "n_polygons") |>
  arrange(class)

write_out(landcover_pixels, "landcover_reference_pixel_counts.csv")
write_out(reference_polygons, "landcover_reference_polygon_counts.csv")

# -----------------------------------------------------------------------------
# 4. Median-surface binary water metrics, with surface water as the positive class
# -----------------------------------------------------------------------------

median_cm <- read_csv("_data/Tables/confusion_matrix_baselines.csv", show_col_types = FALSE)

binary_metrics <- function(tp, tn, fp, fn) {
  n <- tp + tn + fp + fn
  accuracy <- (tp + tn) / n
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  precision <- tp / (tp + fp)
  npv <- tn / (tn + fn)
  f1 <- 2 * precision * sensitivity / (precision + sensitivity)
  mcc_denom <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  mcc <- ifelse(mcc_denom > 0, (tp * tn - fp * fn) / mcc_denom, NA_real_)
  informedness <- sensitivity + specificity - 1
  balanced_accuracy <- (sensitivity + specificity) / 2
  prevalence <- (tp + fn) / n

  tibble(
    tp = tp, tn = tn, fp = fp, fn = fn,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    npv = npv,
    f1 = f1,
    mcc = mcc,
    informedness_j = informedness,
    balanced_accuracy = balanced_accuracy,
    prevalence = prevalence
  )
}

median_water_metrics <- median_cm |>
  group_by(dataset) |>
  group_modify(function(x, key) {
    tp <- x$water[x$predictions == "water"]
    fn <- x$water[x$predictions == "not water"]
    fp <- x$`not water`[x$predictions == "water"]
    tn <- x$`not water`[x$predictions == "not water"]
    binary_metrics(tp, tn, fp, fn)
  }) |>
  ungroup() |>
  mutate(
    model = recode(dataset, km = "Cluster-assisted", rf = "Supervised RF"),
    confusion_units = "acres",
    positive_class = "surface water"
  ) |>
  select(model, positive_class, confusion_units, everything(), -dataset)

write_out(median_water_metrics, "median_reference_water_positive_metrics.csv")

# -----------------------------------------------------------------------------
# 5. Date-specific validation against corresponding hydraulic surfaces
# -----------------------------------------------------------------------------

river <- vect("_data/GIS/Vector/Clean/river_corridor.shp")
hydraulic <- rast("_data/GIS/Raster/Clean/mesohabitats_v2.tif")[[date_map$test_id]]
km <- rast("_data/GIS/Raster/Analysis/Kmeans/km_planet_7.tif")
rf <- rast("_data/GIS/Raster/Analysis/RandomForest/rf_planet_7.tif")

river_prediction <- project(river, crs(rf))
river_grid <- rasterize(river_prediction, rf[[1]], field = 1, background = NA)
cell_area_acres <- prod(res(rf[[1]])) * 0.00024710538146717

confusion_from_rasters <- function(pred_water, obs_water) {
  code <- pred_water * 2 + obs_water
  f <- freq(code)
  counts <- setNames(rep(0, 4), as.character(0:3))
  if (!is.null(f) && nrow(f) > 0) {
    counts[as.character(f$value)] <- f$count
  }
  binary_metrics(
    tp = unname(counts["3"]),
    tn = unname(counts["0"]),
    fp = unname(counts["2"]),
    fn = unname(counts["1"])
  )
}

dated_validation <- map_dfr(seq_len(nrow(date_map)), function(i) {
  test_id <- date_map$test_id[i]
  pred_name <- format(date_map$prediction_date[i], "%Y%m%d")

  obs_on_prediction_grid <- project(hydraulic[[test_id]], rf[[pred_name]], method = "near")
  obs_water <- ifel(!is.na(obs_on_prediction_grid), 1, 0)

  # Use one common evaluation domain for both classifiers: the river corridor
  # where both saved prediction rasters contain a class value.
  valid_domain <- ifel(
    !is.na(river_grid) & !is.na(km[[pred_name]]) & !is.na(rf[[pred_name]]),
    1,
    NA
  )
  obs_water <- mask(obs_water, valid_domain)

  model_inputs <- list(
    "Cluster-assisted" = list(raster = km[[pred_name]], water_code = 3),
    "Supervised RF" = list(raster = rf[[pred_name]], water_code = 5)
  )

  map_dfr(names(model_inputs), function(model_name) {
    model_input <- model_inputs[[model_name]]
    pred <- mask(model_input$raster, valid_domain)
    pred_water <- ifel(pred == model_input$water_code, 1, 0)
    pred_water <- mask(pred_water, valid_domain)

    metrics <- confusion_from_rasters(pred_water, obs_water)
    metrics |>
      mutate(
        test_id = test_id,
        hydraulic_reference_date = case_when(
          test_id == "03_April17_2022" ~ as.Date("2022-04-17"),
          TRUE ~ date_map$prediction_date[i]
        ),
        image_prediction_date = date_map$prediction_date[i],
        model = model_name,
        tp_acres = tp * cell_area_acres,
        tn_acres = tn * cell_area_acres,
        fp_acres = fp * cell_area_acres,
        fn_acres = fn * cell_area_acres,
        observed_water_acres = (tp + fn) * cell_area_acres,
        predicted_water_acres = (tp + fp) * cell_area_acres,
        positive_class = "surface water"
      )
  })
}) |>
  select(
    test_id, hydraulic_reference_date, image_prediction_date,
    model, positive_class,
    tp, tn, fp, fn, tp_acres, tn_acres, fp_acres, fn_acres,
    observed_water_acres, predicted_water_acres,
    accuracy, sensitivity, specificity, precision, npv, f1, mcc,
    informedness_j, balanced_accuracy, prevalence
  )

dated_metric_names <- c(
  "accuracy", "sensitivity", "specificity", "precision", "npv", "f1",
  "mcc", "informedness_j", "balanced_accuracy"
)

dated_validation_summary <- dated_validation |>
  pivot_longer(all_of(dated_metric_names), names_to = "metric", values_to = "estimate") |>
  group_by(model, metric) |>
  group_modify(~ mean_ci(.x$estimate)) |>
  ungroup()

dated_validation_tests <- dated_validation |>
  pivot_longer(all_of(dated_metric_names), names_to = "metric", values_to = "estimate") |>
  group_by(metric) |>
  group_modify(~ paired_wilcox(
    .x,
    value_col = "estimate",
    group_col = "model",
    id_col = "test_id",
    group_1 = "Cluster-assisted",
    group_2 = "Supervised RF"
  )) |>
  ungroup()

write_out(dated_validation, "date_specific_water_validation.csv")
write_out(dated_validation_summary, "date_specific_water_validation_summary_95ci.csv")
write_out(dated_validation_tests, "date_specific_water_validation_paired_tests.csv")

# -----------------------------------------------------------------------------
# 6. Hydrologic responsiveness of observed and predicted water extent
# -----------------------------------------------------------------------------

usgs <- read_csv("_data/Tables/USGS/meanData.csv", show_col_types = FALSE) |>
  mutate(
    Date = as.Date(Date),
    wamego_cfs = .data[["6887500"]],
    fort_riley_cfs = .data[["6879100"]],
    manhattan_cfs = .data[["6887000"]],
    upstream_sum_cfs = fort_riley_cfs + manhattan_cfs
  )

extent_discharge <- dated_validation |>
  select(test_id, image_prediction_date, model, observed_water_acres, predicted_water_acres) |>
  left_join(
    usgs |>
      select(
        Date, wamego_cfs, fort_riley_cfs, manhattan_cfs,
        upstream_sum_cfs, meanDischarge
      ),
    by = c("image_prediction_date" = "Date")
  )

spearman_result <- function(x, y) {
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  test <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = TRUE))
  tibble(n = length(x), rho = unname(test$estimate), p_value = test$p.value)
}

hydrologic_correlations <- extent_discharge |>
  pivot_longer(
    c(wamego_cfs, fort_riley_cfs, manhattan_cfs, upstream_sum_cfs, meanDischarge),
    names_to = "discharge_series",
    values_to = "discharge"
  ) |>
  group_by(model, discharge_series) |>
  group_modify(~ bind_rows(
    spearman_result(.x$observed_water_acres, .x$discharge) |>
      mutate(extent_series = "hydraulic reference"),
    spearman_result(.x$predicted_water_acres, .x$discharge) |>
      mutate(extent_series = "predicted surface water"),
    spearman_result(.x$predicted_water_acres, .x$observed_water_acres) |>
      mutate(extent_series = "predicted versus hydraulic extent")
  )) |>
  ungroup() |>
  select(model, extent_series, discharge_series, n, rho, p_value)

write_out(extent_discharge, "date_specific_water_extent_and_discharge.csv")
write_out(hydrologic_correlations, "water_extent_spearman_correlations.csv")

# -----------------------------------------------------------------------------
# 7. Recalculate land-cover trend tests consistently
# -----------------------------------------------------------------------------

landcover_paths <- tribble(
  ~sensor, ~path,
  "Landsat-9", "_data/Tables/resultsTimeSeries_landsat_core4.csv",
  "Sentinel-2", "_data/Tables/resultsTimeSeries_sentinel_core4.csv",
  "PlanetScope", "_data/Tables/resultsTimeSeries_planet.csv"
)

target_classes <- c("bareland", "cropland", "urban")

kendall_one <- function(x, y) {
  keep <- !is.na(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  if (length(y) < 3 || stats::sd(y) == 0) {
    return(tibble(n = length(y), tau = NA_real_, p_value = NA_real_))
  }
  test <- suppressWarnings(stats::cor.test(as.numeric(x), y, method = "kendall", exact = FALSE))
  tibble(n = length(y), tau = unname(test$estimate), p_value = test$p.value)
}

landcover_kendall <- map2_dfr(landcover_paths$sensor, landcover_paths$path, function(sensor, path) {
  x <- read_csv(path, show_col_types = FALSE) |>
    mutate(date = as.Date(date)) |>
    filter(class %in% target_classes) |>
    group_by(method, date) |>
    summarise(
      area_diff = sum(area_diff, na.rm = FALSE),
      area_diff_baseline = sum(area_diff_baseline, na.rm = FALSE),
      .groups = "drop"
    )

  x |>
    pivot_longer(c(area_diff, area_diff_baseline), names_to = "change_definition", values_to = "area_change") |>
    group_by(method, change_definition) |>
    group_modify(~ kendall_one(.x$date, .x$area_change)) |>
    ungroup() |>
    mutate(sensor = sensor, classes_combined = "bareland + cropland + urban", .before = 1)
})

write_out(landcover_kendall, "landcover_kendall_recalculated.csv")

# Kendall association between surface-water change and the combined exposed
# classes used in the manuscript (bareland + cropland + urban).
landcover_water_exposed_kendall <- map2_dfr(
  landcover_paths$sensor,
  landcover_paths$path,
  function(sensor, path) {
    x <- read_csv(path, show_col_types = FALSE) |>
      mutate(
        date = as.Date(date),
        comparison_class = case_when(
          class == "water" ~ "surface_water",
          class %in% target_classes ~ "exposed_land",
          TRUE ~ NA_character_
        )
      ) |>
      filter(!is.na(comparison_class)) |>
      group_by(method, date, comparison_class) |>
      summarise(
        area_diff = sum(area_diff, na.rm = FALSE),
        area_diff_baseline = sum(area_diff_baseline, na.rm = FALSE),
        .groups = "drop"
      ) |>
      pivot_wider(
        names_from = comparison_class,
        values_from = c(area_diff, area_diff_baseline)
      )

    bind_rows(
      x |>
        group_by(method) |>
        group_modify(~ {
          keep <- complete.cases(.x$area_diff_surface_water, .x$area_diff_exposed_land)
          test <- suppressWarnings(stats::cor.test(
            .x$area_diff_surface_water[keep],
            .x$area_diff_exposed_land[keep],
            method = "kendall",
            exact = FALSE
          ))
          tibble(
            change_definition = "between-date",
            n = sum(complete.cases(.x$area_diff_surface_water, .x$area_diff_exposed_land)),
            tau = unname(test$estimate),
            p_value = test$p.value
          )
        }),
      x |>
        group_by(method) |>
        group_modify(~ {
          keep <- complete.cases(.x$area_diff_baseline_surface_water, .x$area_diff_baseline_exposed_land)
          test <- suppressWarnings(stats::cor.test(
            .x$area_diff_baseline_surface_water[keep],
            .x$area_diff_baseline_exposed_land[keep],
            method = "kendall",
            exact = FALSE
          ))
          tibble(
            change_definition = "baseline-relative",
            n = sum(complete.cases(.x$area_diff_baseline_surface_water, .x$area_diff_baseline_exposed_land)),
            tau = unname(test$estimate),
            p_value = test$p.value
          )
        })
    ) |>
      ungroup() |>
      mutate(sensor = sensor, .before = 1)
  }
)

write_out(landcover_water_exposed_kendall, "landcover_water_exposed_kendall.csv")

# -----------------------------------------------------------------------------
# 8. Processing-resolution documentation for the revised methods table
# -----------------------------------------------------------------------------

resolution_documentation <- tribble(
  ~source_or_workflow, ~native_resolution, ~resampling_or_alignment, ~analysis_resolution,
  "Landsat-9 OLI-2", "30 m multispectral", "Bands retained on the native grid", "30 m",
  "Sentinel-2 MSI", "10 m for B2, B3, B4, B8; 20 m for B8A", "B8A bilinearly resampled to the 10 m grid; categorical outputs use nearest-neighbor resampling", "10 m",
  "PlanetScope SuperDove", "approximately 3 m", "Bands retained on the PlanetScope grid", "3 m",
  "Hydraulic mesohabitat reference", "3 US survey feet (approximately 0.91 m)", "Categorical hydraulic classes projected with nearest-neighbor resampling to the PlanetScope grid", "3 m for image-to-reference validation",
  "Mesohabitat spectral predictors", "PlanetScope approximately 3 m", "Continuous reflectance bands aligned bilinearly where grids differed; categorical reference and predictions used nearest-neighbor resampling", "3 m"
)

write_out(resolution_documentation, "processing_resolution_documentation.csv")

message("Reviewer-response tables written to: ", out_dir)
