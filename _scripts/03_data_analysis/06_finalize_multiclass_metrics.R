source("_scripts/03_data_analysis/00_config.R")
source("_scripts/03_data_analysis/01_helpers.R")

finalize_multiclass_metrics <- function() {
  land_confusion <- read_csv(
    file.path(BALANCED_TABLE_DIR, "landcover_validation_confusion.csv"),
    show_col_types = FALSE
  )
  land_metrics <- metrics_from_wide_confusion(
    land_confusion,
    c("sensor", "model")
  )
  write_balanced_csv(land_metrics, "landcover_validation_metrics.csv")

  meso_confusion <- read_csv(
    file.path(BALANCED_TABLE_DIR, "mesohabitat_loo_confusion_by_fold.csv"),
    show_col_types = FALSE
  )
  meso_metrics <- metrics_from_wide_confusion(
    meso_confusion,
    c("test_id", "model")
  )
  meso_summary <- meso_metrics |>
    group_by(model, .metric) |>
    group_modify(~ mean_ci(.x$.estimate)) |>
    ungroup()
  meso_tests <- meso_metrics |>
    group_by(.metric) |>
    group_modify(~ paired_wilcox(
      .x,
      ".estimate",
      "model",
      "test_id",
      "Cluster-assisted",
      "Random forest"
    )) |>
    ungroup()

  write_balanced_csv(meso_metrics, "mesohabitat_loo_fold_metrics.csv")
  write_balanced_csv(meso_summary, "mesohabitat_loo_summary_95ci.csv")
  write_balanced_csv(meso_tests, "mesohabitat_loo_paired_tests.csv")

  walk(unique(meso_metrics$test_id), function(test_id) {
    checkpoint_path <- file.path(MESO_MODEL_DIR, paste0("fold_summary_", test_id, ".rds"))
    if (!file.exists(checkpoint_path)) return(invisible(NULL))
    checkpoint <- readRDS(checkpoint_path)
    checkpoint$metrics <- meso_metrics |> filter(.data$test_id == test_id)
    saveRDS(checkpoint, checkpoint_path, compress = "gzip")
  })

  message("Multiclass metrics finalized from saved confusion tables.")
  invisible(list(landcover = land_metrics, mesohabitat = meso_metrics))
}

if (sys.nframe() == 0L) finalize_multiclass_metrics()
