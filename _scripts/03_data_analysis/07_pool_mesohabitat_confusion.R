pool_mesohabitat_confusion <- function() {
  input_path <- file.path(
    BALANCED_TABLE_DIR,
    "mesohabitat_loo_confusion_by_fold.csv"
  )
  if (!file.exists(input_path)) {
    stop("Missing fold-specific mesohabitat confusion output: ", input_path)
  }

  pooled <- readr::read_csv(input_path, show_col_types = FALSE) |>
    tidyr::pivot_longer(
      cols = -c(test_id, model, truth),
      names_to = "prediction",
      values_to = "count"
    ) |>
    dplyr::group_by(model, truth, prediction) |>
    dplyr::summarise(count = sum(count), .groups = "drop") |>
    dplyr::group_by(model, truth) |>
    dplyr::mutate(
      truth_total = sum(count),
      row_proportion = count / truth_total
    ) |>
    dplyr::ungroup()

  write_balanced_csv(
    pooled,
    "mesohabitat_loo_confusion_pooled.csv"
  )
  invisible(pooled)
}
