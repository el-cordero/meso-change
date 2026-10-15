library(dplyr)
library(readr)

verify_surface_water_spearman <- function() {
  table_dir <- "_data/Tables/binary_surface_water"

  validation <- read_csv(
    file.path(table_dir, "hydraulic_date_specific_validation.csv"),
    show_col_types = FALSE
  ) |>
    mutate(hydraulic_reference_date = as.Date(hydraulic_reference_date))

  discharge <- read_csv("_data/Tables/USGS/meanData.csv", show_col_types = FALSE) |>
    transmute(
      hydraulic_reference_date = as.Date(Date),
      wamego_cfs = .data[["6887500"]]
    )

  inputs <- validation |>
    left_join(discharge, by = "hydraulic_reference_date") |>
    group_by(workflow) |>
    mutate(
      predicted_water_rank = rank(predicted_water_acres),
      hydraulic_water_rank = rank(observed_water_acres),
      discharge_rank = rank(wamego_cfs)
    ) |>
    ungroup()

  if (anyNA(inputs$wamego_cfs)) {
    stop("A Wamego discharge value is missing for one or more hydraulic reference dates.")
  }

  rank_audit <- inputs |>
    distinct(hydraulic_reference_date, observed_water_acres, wamego_cfs,
             hydraulic_water_rank, discharge_rank) |>
    summarise(
      n = n(),
      hydraulic_extent_discharge_rho = cor(
        observed_water_acres, wamego_cfs, method = "spearman"
      ),
      identical_rank_order = identical(hydraulic_water_rank, discharge_rank)
    )

  correlations <- inputs |>
    group_by(workflow) |>
    group_modify(~ bind_rows(
      {
        test <- suppressWarnings(cor.test(
          .x$predicted_water_acres, .x$wamego_cfs,
          method = "spearman", exact = TRUE
        ))
        tibble(
          comparison = "predicted water versus discharge",
          n = nrow(.x),
          rho = unname(test$estimate),
          p_value = test$p.value
        )
      },
      {
        test <- suppressWarnings(cor.test(
          .x$predicted_water_acres, .x$observed_water_acres,
          method = "spearman", exact = TRUE
        ))
        tibble(
          comparison = "predicted water versus hydraulic extent",
          n = nrow(.x),
          rho = unname(test$estimate),
          p_value = test$p.value
        )
      }
    )) |>
    ungroup() |>
    arrange(comparison, workflow)

  write_csv(inputs, file.path(table_dir, "surface_water_spearman_inputs.csv"))
  write_csv(rank_audit, file.path(table_dir, "surface_water_spearman_rank_audit.csv"))
  write_csv(correlations, file.path(table_dir, "spearman_correlations_summary.csv"))

  message("Surface-water Spearman correlations verified.")
  invisible(list(inputs = inputs, rank_audit = rank_audit, correlations = correlations))
}

if (sys.nframe() == 0L) verify_surface_water_spearman()
