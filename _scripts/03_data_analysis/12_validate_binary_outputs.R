source("_scripts/03_data_analysis/08_binary_config.R")

validate_binary_outputs <- function() {
  balance <- read_csv(
    file.path(BINARY_TABLE_DIR, "binary_training_balance_by_model.csv"),
    show_col_types = FALSE
  )
  failures <- balance |>
    group_by(sensor, model) |>
    summarise(
      classes = n_distinct(class),
      fitted_counts = n_distinct(n_training_fitted),
      fitted = first(n_training_fitted),
      target = first(balance_target),
      available_minimum = min(n_training_available),
      .groups = "drop"
    ) |>
    filter(classes != 2L | fitted_counts != 1L | fitted != target | target != available_minimum)
  if (nrow(failures)) stop("One or more binary fits failed the equal-support audit.")

  required <- c(
    BINARY_SENSOR_CONFIG$rf_prediction_path,
    map_chr(BINARY_SENSOR_CONFIG$sensor, ~ file.path(
      BINARY_RASTER_DIR,
      paste0(str_replace_all(str_to_lower(.x), "[^a-z0-9]+", "_"), "cluster_binary.tif")
    )),
    file.path(BINARY_TABLE_DIR, c(
      "binary_validation_metrics.csv",
      "binary_reference_support_by_sensor.csv",
      "binary_time_series_all_sensors.csv",
      "hydraulic_date_specific_validation.csv",
      "hydraulic_date_specific_validation_summary_95ci.csv",
      "hydraulic_water_extent_correlations.csv",
      "baseline_change_hydraulic_comparison.csv",
      "baseline_change_hydraulic_summary.csv",
      "preceding_change_hydraulic_comparison.csv",
      "preceding_change_hydraulic_summary.csv",
      "spearman_correlations_summary.csv",
      "multisensor_surface_water_streamflow_inputs.csv",
      "multisensor_surface_water_streamflow_spearman.csv",
      "multisensor_surface_water_streamflow_spearman_table.csv"
    ))
  )
  missing <- required[!file.exists(required)]
  if (length(missing)) stop("Missing binary outputs: ", paste(missing, collapse = ", "))

  raster_paths <- c(
    BINARY_SENSOR_CONFIG$rf_prediction_path,
    map_chr(BINARY_SENSOR_CONFIG$sensor, ~ file.path(
      BINARY_RASTER_DIR,
      paste0(str_replace_all(str_to_lower(.x), "[^a-z0-9]+", "_"), "cluster_binary.tif")
    ))
  )
  invalid <- keep(raster_paths, function(path) {
    observed <- unique(na.omit(values(rast(path), mat = FALSE)))
    any(!observed %in% 1:2)
  })
  if (length(invalid)) stop("Nonbinary class codes found in: ", paste(invalid, collapse = ", "))

  preceding <- read_csv(
    file.path(BINARY_TABLE_DIR, "preceding_change_hydraulic_comparison.csv"),
    show_col_types = FALSE
  )
  preceding_counts <- preceding |>
    count(workflow, name = "n")
  if (nrow(preceding_counts) != 2L || any(preceding_counts$n != 6L)) {
    stop("Preceding-change comparison must contain six chronological intervals per workflow.")
  }

  correlations <- read_csv(
    file.path(BINARY_TABLE_DIR, "spearman_correlations_summary.csv"),
    show_col_types = FALSE
  )
  if (nrow(correlations) != 4L || any(!is.finite(correlations$rho))) {
    stop("Spearman summary must contain two finite comparisons for both workflows.")
  }

  multisensor_correlations <- read_csv(
    file.path(BINARY_TABLE_DIR, "multisensor_surface_water_streamflow_spearman.csv"),
    show_col_types = FALSE
  )
  if (
    nrow(multisensor_correlations) != 6L ||
      any(multisensor_correlations$n != 22L) ||
      any(!is.finite(multisensor_correlations$rho)) ||
      any(!is.finite(multisensor_correlations$p_value))
  ) {
    stop("Multi-sensor Spearman summary must contain 22 finite pairs for both workflows and all three sensors.")
  }

  report <- c(
    "Binary surface-water analysis validation: PASS",
    paste0("Binary random-forest fits checked: ", n_distinct(interaction(balance$sensor, balance$model))),
    "Every fitted model used equal land and surface-water training support.",
    "Validation observations and hydraulic comparison domains were not undersampled.",
    "All saved prediction rasters contain only land and surface-water codes.",
    "The 22-date products include a time-stacked baseline and date-specific predictions.",
    "The seven PlanetScope dates were compared with their corresponding hydraulic inundation masks.",
    "Baseline-referenced and preceding matched-date change products were retained for the time-series analysis.",
    "Two seven-date PlanetScope Spearman comparisons were summarized for both workflows.",
    "Surface-water area was compared with three-gauge mean daily streamflow across 22 dates for all three sensors and both workflows."
  )
  writeLines(report, file.path(BINARY_TABLE_DIR, "analysis_validation.txt"))
  message(paste(report, collapse = "\n"))
  invisible(TRUE)
}

if (sys.nframe() == 0L) validate_binary_outputs()
