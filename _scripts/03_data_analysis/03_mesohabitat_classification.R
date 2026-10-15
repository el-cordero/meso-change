source("_scripts/03_data_analysis/00_config.R")
source("_scripts/03_data_analysis/01_helpers.R")

MESO_DATE_MAP <- tribble(
  ~test_id, ~planet_prefix,
  "01_Dec22_2021", "20211222",
  "02_Feb12_2022", "20220212",
  "03_April17_2022", "20220418",
  "04_May31_2022", "20220531",
  "05_June20_2022", "20220620",
  "06_July18_2022", "20220718",
  "07_Sep28_2022", "20220928"
)

normalize_meso_label <- function(x) {
  x <- str_squish(as.character(x))
  x[x == "Med Pool"] <- "Medium Pool"
  x[x == "Faster than a Raceway"] <- "Faster than Raceway"
  x[x == "Faster than a Deep Pool"] <- "Faster than Deep Pool"
  x
}

meso_cache_paths <- function(test_id) {
  list(
    feature = file.path(MESO_CACHE_DIR, "aligned_features", paste0("features_fact1_", test_id, ".tif")),
    xy = file.path(MESO_CACHE_DIR, "xy_tables", paste0("xy_fact1_", test_id, ".rds"))
  )
}

ensure_mesohabitat_cache <- function() {
  force_rebuild <- identical(Sys.getenv("MESO_FORCE_CACHE", "0"), "1")
  expected <- map(MESO_DATE_MAP$test_id, meso_cache_paths)
  complete <- all(map_lgl(expected, ~ file.exists(.x$feature) && file.exists(.x$xy)))
  if (complete && !force_rebuild) {
    message("Using validated mesohabitat feature and pixel caches.")
    return(invisible(NULL))
  }

  message("Rebuilding mesohabitat caches from mesohabitats_v2 and PlanetScope inputs.")
  dir.create(file.path(MESO_CACHE_DIR, "aligned_features"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(MESO_CACHE_DIR, "xy_tables"), recursive = TRUE, showWarnings = FALSE)

  meso_source <- rast("_data/GIS/Raster/Clean/mesohabitats_v2.tif")[[MESO_DATE_MAP$test_id]]
  planet <- rast("_data/GIS/Raster/Clean/planet_7dates.tif")
  original_levels <- levels(meso_source[[1]])[[1]]
  label_column <- setdiff(names(original_levels), "value")[[1]]
  meso_levels <- normalize_meso_label(original_levels[[label_column]])
  meso_codes <- as.integer(original_levels$value)
  meso_projected <- project(meso_source, planet[[1]], method = "near")
  names(meso_projected) <- MESO_DATE_MAP$test_id

  pwalk(MESO_DATE_MAP, function(test_id, planet_prefix) {
    paths <- meso_cache_paths(test_id)
    bands <- paste0(planet_prefix, "_", c("blue", "green", "red", "nir"))
    features <- planet[[bands]]
    names(features) <- c("blue", "green", "red", "nir")
    reference <- meso_projected[[test_id]]
    features <- mask(features, reference)
    writeRaster(features, paths$feature, overwrite = TRUE, wopt = list(gdal = "COMPRESS=LZW"))

    x <- values(features, mat = TRUE)
    y <- values(reference, mat = FALSE)
    keep <- apply(is.finite(x), 1, all) & !is.na(y) & y %in% meso_codes
    xy <- as_tibble(x[keep, , drop = FALSE])
    names(xy) <- c("blue", "green", "red", "nir")
    xy$meso <- factor(meso_levels[match(y[keep], meso_codes)], levels = meso_levels)
    saveRDS(xy, paths$xy, compress = "gzip")
  })
  invisible(NULL)
}

load_meso_xy <- function(test_id) {
  x <- readRDS(meso_cache_paths(test_id)$xy)
  x$meso <- factor(normalize_meso_label(x$meso), levels = MESO_LEVELS)
  x |>
    filter(if_all(-meso, is.finite), !is.na(meso))
}

fit_kmeans_mesohabitat <- function(training_balanced, seed) {
  x <- as.matrix(select(training_balanced, -meso))
  scaled <- scale(x)
  center <- attr(scaled, "scaled:center")
  scale_values <- attr(scaled, "scaled:scale")
  scale_values[!is.finite(scale_values) | scale_values == 0] <- 1
  set.seed(seed)
  fitted <- kmeans(
    scaled,
    centers = 12,
    nstart = 50,
    iter.max = 500,
    algorithm = "Hartigan-Wong"
  )
  cluster_labels <- tibble(cluster = fitted$cluster, meso = training_balanced$meso) |>
    count(cluster, meso, name = "n") |>
    group_by(cluster) |>
    slice_max(n, n = 1, with_ties = FALSE) |>
    ungroup()
  list(
    centers = fitted$centers,
    center = center,
    scale = scale_values,
    cluster_labels = cluster_labels,
    cluster_to_code = setNames(match(as.character(cluster_labels$meso), MESO_LEVELS), cluster_labels$cluster),
    kmeans = fitted
  )
}

predict_kmeans_labels <- function(model, new_data) {
  codes <- kmeans_predict_codes(model, select(new_data, -any_of("meso")))
  factor(MESO_LEVELS[codes], levels = MESO_LEVELS)
}

fit_rf_mesohabitat <- function(training_balanced, seed) {
  set.seed(seed)
  ranger::ranger(
    dependent.variable.name = "meso",
    data = training_balanced,
    num.trees = 500,
    mtry = 4,
    min.node.size = 5,
    importance = "impurity",
    num.threads = N_THREADS,
    seed = seed
  )
}

run_meso_fold <- function(test_id, xy_by_date) {
  message("Mesohabitat leave-one-date-out fold: ", test_id)
  training_raw <- bind_rows(xy_by_date[names(xy_by_date) != test_id])
  testing <- xy_by_date[[test_id]]
  balanced <- undersample_to_minimum(
    training_raw,
    meso,
    seed_for("mesohabitat", test_id, "undersample", base = MESO_SEED)
  )
  training_balanced <- balanced$data

  kmeans_model <- fit_kmeans_mesohabitat(
    training_balanced,
    seed_for("mesohabitat", test_id, "kmeans", base = MESO_SEED)
  )
  rf_model <- fit_rf_mesohabitat(
    training_balanced,
    seed_for("mesohabitat", test_id, "rf", base = MESO_SEED)
  )

  kmeans_prediction <- predict_kmeans_labels(kmeans_model, testing)
  rf_prediction <- predict(rf_model, data = select(testing, -meso), num.threads = N_THREADS)$predictions
  rf_prediction <- factor(rf_prediction, levels = MESO_LEVELS)

  metrics <- bind_rows(
    classification_metrics(testing$meso, kmeans_prediction) |> mutate(model = "Cluster-assisted", .before = 1),
    classification_metrics(testing$meso, rf_prediction) |> mutate(model = "Random forest", .before = 1)
  ) |>
    mutate(test_id = test_id, .before = 1)

  confusion <- bind_rows(
    multiclass_confusion(testing$meso, kmeans_prediction) |> mutate(model = "Cluster-assisted", .before = 1),
    multiclass_confusion(testing$meso, rf_prediction) |> mutate(model = "Random forest", .before = 1)
  ) |>
    mutate(test_id = test_id, .before = 1)

  balance_audit <- full_join(
    training_raw |> count(meso, name = "n_training_available"),
    training_balanced |> count(meso, name = "n_training_fitted"),
    by = "meso"
  ) |>
    full_join(testing |> count(meso, name = "n_holdout"), by = "meso") |>
    mutate(test_id = test_id, balance_target = balanced$target_n, .before = 1)

  saveRDS(kmeans_model, file.path(MESO_MODEL_DIR, paste0("cluster_assisted_", test_id, ".rds")), compress = "gzip")
  saveRDS(rf_model, file.path(MESO_MODEL_DIR, paste0("random_forest_", test_id, ".rds")), compress = "gzip")

  result <- list(metrics = metrics, confusion = confusion, balance = balance_audit)
  saveRDS(result, file.path(MESO_MODEL_DIR, paste0("fold_summary_", test_id, ".rds")), compress = "gzip")
  result
}

write_final_mesohabitat_predictions <- function(kmeans_model, rf_model) {
  category_table <- data.frame(value = seq_along(MESO_LEVELS), mesohabitat = MESO_LEVELS)
  pwalk(MESO_DATE_MAP, function(test_id, planet_prefix) {
    feature_raster <- rast(meso_cache_paths(test_id)$feature)
    kmeans_path <- file.path(MESO_PREDICTION_DIR, paste0("cluster_assisted_pred_FINAL_", test_id, ".tif"))
    rf_path <- file.path(MESO_PREDICTION_DIR, paste0("rf_pred_FINAL_", test_id, ".tif"))
    kmeans_temporary <- tempfile(pattern = "cluster_codes_", tmpdir = MESO_PREDICTION_DIR, fileext = ".tif")
    rf_temporary <- tempfile(pattern = "rf_codes_", tmpdir = MESO_PREDICTION_DIR, fileext = ".tif")
    on.exit(unlink(c(kmeans_temporary, rf_temporary)), add = TRUE)

    kmeans_raster <- predict(
      feature_raster,
      kmeans_model,
      fun = kmeans_predict_codes,
      filename = kmeans_temporary,
      overwrite = TRUE,
      na.rm = TRUE,
      wopt = list(datatype = "INT1U", gdal = "COMPRESS=LZW")
    )
    kmeans_raster <- as.factor(kmeans_raster)
    levels(kmeans_raster) <- category_table
    writeRaster(kmeans_raster, kmeans_path, overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = "COMPRESS=LZW"))

    rf_raster <- predict(
      feature_raster,
      rf_model,
      fun = predict_ranger_codes,
      class_levels = MESO_LEVELS,
      filename = rf_temporary,
      overwrite = TRUE,
      na.rm = TRUE,
      wopt = list(datatype = "INT1U", gdal = "COMPRESS=LZW")
    )
    rf_raster <- as.factor(rf_raster)
    levels(rf_raster) <- category_table
    writeRaster(rf_raster, rf_path, overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = "COMPRESS=LZW"))
  })
}

run_mesohabitat_analysis <- function() {
  ensure_mesohabitat_cache()
  first_xy <- readRDS(meso_cache_paths(MESO_DATE_MAP$test_id[[1]])$xy)
  MESO_LEVELS <<- levels(first_xy$meso)
  if (length(MESO_LEVELS) != 8L) stop("Expected eight mesohabitat classes.")
  xy_by_date <- setNames(map(MESO_DATE_MAP$test_id, load_meso_xy), MESO_DATE_MAP$test_id)

  force_refit <- identical(Sys.getenv("MESO_FORCE_REFIT", "0"), "1")
  fold_results <- map(MESO_DATE_MAP$test_id, function(test_id) {
    summary_path <- file.path(MESO_MODEL_DIR, paste0("fold_summary_", test_id, ".rds"))
    kmeans_path <- file.path(MESO_MODEL_DIR, paste0("cluster_assisted_", test_id, ".rds"))
    rf_path <- file.path(MESO_MODEL_DIR, paste0("random_forest_", test_id, ".rds"))
    if (!force_refit && all(file.exists(c(summary_path, kmeans_path, rf_path)))) {
      message("Using completed mesohabitat fold checkpoint: ", test_id)
      readRDS(summary_path)
    } else {
      run_meso_fold(test_id, xy_by_date)
    }
  })
  fold_metrics <- map_dfr(fold_results, "metrics")
  fold_confusion <- map_dfr(fold_results, "confusion")
  fold_balance <- map_dfr(fold_results, "balance") |>
    mutate(meso = as.character(meso))

  final_raw <- bind_rows(xy_by_date)
  final_balanced <- undersample_to_minimum(
    final_raw,
    meso,
    seed_for("mesohabitat", "final", "undersample", base = MESO_SEED)
  )
  final_training <- final_balanced$data
  final_kmeans_path <- file.path(MESO_MODEL_DIR, "cluster_assisted_FINAL.rds")
  final_rf_path <- file.path(MESO_MODEL_DIR, "random_forest_FINAL.rds")
  if (!force_refit && all(file.exists(c(final_kmeans_path, final_rf_path)))) {
    message("Using completed final mesohabitat model checkpoints.")
    final_kmeans <- readRDS(final_kmeans_path)
    final_rf <- readRDS(final_rf_path)
  } else {
    final_kmeans <- fit_kmeans_mesohabitat(
      final_training,
      seed_for("mesohabitat", "final", "kmeans", base = MESO_SEED)
    )
    final_rf <- fit_rf_mesohabitat(
      final_training,
      seed_for("mesohabitat", "final", "rf", base = MESO_SEED)
    )
    saveRDS(final_kmeans, final_kmeans_path, compress = "gzip")
    saveRDS(final_rf, final_rf_path, compress = "gzip")
  }
  write_final_mesohabitat_predictions(final_kmeans, final_rf)

  final_balance_audit <- full_join(
    final_raw |> count(meso, name = "n_training_available"),
    final_training |> count(meso, name = "n_training_fitted"),
    by = "meso"
  ) |>
    mutate(
      test_id = "FINAL_all_dates",
      balance_target = final_balanced$target_n,
      meso = as.character(meso),
      .before = 1
    )

  summary <- fold_metrics |>
    group_by(model, .metric) |>
    group_modify(~ mean_ci(.x$.estimate)) |>
    ungroup()
  tests <- fold_metrics |>
    group_by(.metric) |>
    group_modify(~ paired_wilcox(.x, ".estimate", "model", "test_id", "Cluster-assisted", "Random forest")) |>
    ungroup()

  write_balanced_csv(fold_metrics, "mesohabitat_loo_fold_metrics.csv")
  write_balanced_csv(summary, "mesohabitat_loo_summary_95ci.csv")
  write_balanced_csv(tests, "mesohabitat_loo_paired_tests.csv")
  write_balanced_csv(fold_confusion, "mesohabitat_loo_confusion_by_fold.csv")
  write_balanced_csv(bind_rows(fold_balance, final_balance_audit), "mesohabitat_training_balance.csv")
  write_balanced_csv(
    map_dfr(names(xy_by_date), ~ count(xy_by_date[[.x]], meso, name = "n_pixels") |> mutate(test_id = .x, .before = 1)),
    "mesohabitat_labelled_pixel_support_by_date.csv"
  )
  writeLines(capture.output(sessionInfo()), file.path(MESO_MODEL_DIR, "R_sessionInfo.txt"))
  message("Balanced mesohabitat analysis complete.")
  invisible(list(metrics = fold_metrics, summary = summary, tests = tests))
}

if (sys.nframe() == 0L) run_mesohabitat_analysis()
