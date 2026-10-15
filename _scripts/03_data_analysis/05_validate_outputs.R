source("_scripts/03_data_analysis/00_config.R")

validate_equal_class_support <- function(data, model_columns, fitted_column) {
  failures <- data |>
    group_by(across(all_of(model_columns))) |>
    summarise(
      n_fitted_values = n_distinct(.data[[fitted_column]]),
      fitted = first(.data[[fitted_column]]),
      target = first(balance_target),
      available_minimum = min(n_training_available),
      .groups = "drop"
    ) |>
    filter(n_fitted_values != 1L | fitted != target | target != available_minimum)
  if (nrow(failures)) {
    stop("One or more fitted models do not have equal per-class training support.")
  }
  invisible(TRUE)
}

validate_balanced_outputs <- function() {
  land_balance <- read_csv(
    file.path(BALANCED_TABLE_DIR, "landcover_training_balance_by_model.csv"),
    show_col_types = FALSE
  )
  meso_balance <- read_csv(
    file.path(BALANCED_TABLE_DIR, "mesohabitat_training_balance.csv"),
    show_col_types = FALSE
  )

  validate_equal_class_support(
    land_balance,
    c("sensor", "model"),
    "n_training_fitted"
  )
  validate_equal_class_support(
    meso_balance,
    "test_id",
    "n_training_fitted"
  )

  if (any(grepl("cropland|herbaceous", land_balance$class, ignore.case = TRUE))) {
    stop("Legacy cropland/herbaceous terminology remains in the balanced outputs.")
  }

  required <- c(
    landcover_run_config$prediction_path,
    file.path(BALANCED_TABLE_DIR, c(
      "landcover_validation_metrics.csv",
      "landcover_reference_support_by_sensor.csv",
      "mesohabitat_loo_fold_metrics.csv",
      "mesohabitat_loo_confusion_pooled.csv",
      "date_specific_water_validation.csv",
      "water_extent_spearman_correlations.csv"
    ))
  )
  missing <- required[!file.exists(required)]
  if (length(missing)) stop("Missing balanced outputs: ", paste(missing, collapse = ", "))

  input_files <- c(
    LANDCOVER_POLYGONS,
    sub("\\.shp$", ".dbf", LANDCOVER_POLYGONS),
    sub("\\.shp$", ".shx", LANDCOVER_POLYGONS)
  )
  land_outputs <- c(
    landcover_run_config$prediction_path,
    file.path(BALANCED_TABLE_DIR, c(
      "landcover_training_balance_by_model.csv",
      "landcover_validation_metrics.csv",
      "landcover_reference_support_by_sensor.csv"
    ))
  )
  latest_input <- max(file.info(input_files)$mtime, na.rm = TRUE)
  stale_land_outputs <- land_outputs[
    file.exists(land_outputs) & file.info(land_outputs)$mtime < latest_input
  ]
  if (length(stale_land_outputs)) {
    stop(
      "Balanced land-cover outputs are older than the revised reference polygons: ",
      paste(stale_land_outputs, collapse = ", ")
    )
  }

  report <- c(
    "Balanced analysis validation: PASS",
    paste0("Land-cover models checked: ", n_distinct(interaction(land_balance$sensor, land_balance$model))),
    paste0("Mesohabitat fits checked: ", n_distinct(meso_balance$test_id)),
    "Every fitted training set contains the same number of pixels from each class.",
    "Validation and held-out observations were not undersampled.",
    "Land-cover outputs are newer than the revised reference-polygon inputs."
  )
  writeLines(report, file.path(BALANCED_TABLE_DIR, "analysis_validation.txt"))
  message(paste(report, collapse = "\n"))
  invisible(TRUE)
}

if (sys.nframe() == 0L) validate_balanced_outputs()
