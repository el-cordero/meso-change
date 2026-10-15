library(dplyr)
library(readr)
library(tidyr)

calculate_multisensor_surface_water_spearman <- function() {
  table_dir <- "_data/Tables/binary_surface_water"

  surface_water <- read_csv(
    file.path(table_dir, "binary_time_series_all_sensors.csv"),
    show_col_types = FALSE
  ) |>
    filter(class == "water") |>
    transmute(
      sensor,
      workflow,
      date = as.Date(date),
      surface_water_acres = area_acres
    )

  streamflow <- read_csv(
    "_data/Tables/USGS/meanData.csv",
    show_col_types = FALSE
  ) |>
    transmute(
      date = as.Date(Date),
      mean_daily_streamflow_cfs = meanDischarge
    )

  inputs <- surface_water |>
    left_join(streamflow, by = "date") |>
    arrange(sensor, workflow, date)

  if (anyNA(inputs$mean_daily_streamflow_cfs)) {
    stop("Mean daily streamflow is missing for one or more satellite acquisition dates.")
  }

  pair_counts <- inputs |>
    count(sensor, workflow, name = "n")
  if (nrow(pair_counts) != 6L || any(pair_counts$n != 22L)) {
    stop("Expected 22 surface-water and streamflow pairs for each sensor and workflow.")
  }

  correlations <- inputs |>
    group_by(sensor, workflow) |>
    group_modify(~ {
      test <- suppressWarnings(cor.test(
        .x$surface_water_acres,
        .x$mean_daily_streamflow_cfs,
        method = "spearman",
        exact = FALSE
      ))
      tibble(
        n = nrow(.x),
        rho = unname(test$estimate),
        p_value = test$p.value
      )
    }) |>
    ungroup() |>
    arrange(factor(sensor, c("Landsat-9", "Sentinel-2", "PlanetScope")), workflow)

  manuscript_table <- correlations |>
    select(sensor, workflow, n, rho, p_value) |>
    pivot_wider(
      names_from = workflow,
      values_from = c(rho, p_value),
      names_glue = "{workflow}_{.value}"
    ) |>
    arrange(factor(sensor, c("Landsat-9", "Sentinel-2", "PlanetScope")))

  write_csv(
    inputs,
    file.path(table_dir, "multisensor_surface_water_streamflow_inputs.csv")
  )
  write_csv(
    correlations,
    file.path(table_dir, "multisensor_surface_water_streamflow_spearman.csv")
  )
  write_csv(
    manuscript_table,
    file.path(table_dir, "multisensor_surface_water_streamflow_spearman_table.csv")
  )

  message("Multi-sensor surface-water and streamflow Spearman correlations complete.")
  invisible(list(inputs = inputs, correlations = correlations, manuscript_table = manuscript_table))
}

if (sys.nframe() == 0L) calculate_multisensor_surface_water_spearman()
