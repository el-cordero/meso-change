source("_scripts/03_data_analysis/08_binary_config.R")
source("_scripts/03_data_analysis/09_binary_helpers.R")

prepare_binary_reference <- function(stack, sensor_name) {
  polygons <- vect(BINARY_POLYGONS)
  polygons$binary_class <- normalize_reference_class(polygons$class)
  if (anyNA(polygons$binary_class)) {
    stop(
      "Unrecognized reference-polygon values for ", sensor_name, ": ",
      paste(unique(polygons$class[is.na(polygons$binary_class)]), collapse = ", ")
    )
  }
  polygons$binary_class <- factor(polygons$binary_class, levels = BINARY_LEVELS)
  polygons$class_code <- match(as.character(polygons$binary_class), BINARY_LEVELS)
  polygons <- project(polygons, crs(stack))

  class_raster <- rasterize(polygons, stack[[1]], field = "class_code")
  labelled_cells <- which(!is.na(values(class_raster, mat = FALSE)))
  if (!length(labelled_cells)) stop("No reference-polygon cells intersect ", sensor_name, ".")

  scaled_stack <- scale(stack)
  predictor_values <- terra::extract(scaled_stack, labelled_cells)
  if ("ID" %in% names(predictor_values)) predictor_values$ID <- NULL
  class_codes <- terra::extract(class_raster, labelled_cells)[[1]]
  reference <- as_tibble(predictor_values) |>
    mutate(
      cell = labelled_cells,
      class = factor(BINARY_LEVELS[class_codes], levels = BINARY_LEVELS),
      .before = 1
    )

  polygon_counts <- as.data.frame(polygons) |>
    count(binary_class, name = "n_polygons") |>
    transmute(class = as.character(binary_class), n_polygons)

  list(
    scaled_stack = scaled_stack,
    reference = reference,
    polygon_counts = polygon_counts
  )
}

tune_binary_baseline <- function(training_balanced, sensor_name) {
  model_data <- select(training_balanced, -cell)
  predictor_count <- ncol(model_data) - 1L
  model_spec <- rand_forest(trees = 300, mtry = tune(), min_n = tune()) |>
    set_mode("classification") |>
    set_engine("ranger", importance = "impurity", num.threads = BINARY_THREADS)
  model_recipe <- recipe(class ~ ., data = model_data)
  model_workflow <- workflow() |>
    add_model(model_spec) |>
    add_recipe(model_recipe)

  set.seed(binary_seed_for(sensor_name, "binary", "folds"))
  folds <- vfold_cv(model_data, v = 5, strata = class)
  parameter_grid <- grid_regular(
    mtry(range = c(1L, min(25L, predictor_count))),
    min_n(range = c(2L, 20L)),
    levels = 5
  )
  foreach::registerDoSEQ()
  set.seed(binary_seed_for(sensor_name, "binary", "tuning"))
  tuning <- tune_grid(
    model_workflow,
    resamples = folds,
    grid = parameter_grid,
    metrics = metric_set(accuracy, bal_accuracy),
    control = control_grid(verbose = TRUE, allow_par = FALSE)
  )
  list(
    tuning = tuning,
    best = select_best(tuning, metric = "bal_accuracy")
  )
}

fit_binary_model <- function(model_data, sensor_name, model_name, mtry_value, min_n_value) {
  complete <- model_data |> filter(if_all(-c(cell, class), is.finite))
  if (n_distinct(complete$class) != 2L) {
    stop(sensor_name, " ", model_name, " does not contain both binary classes.")
  }

  set.seed(binary_seed_for(sensor_name, model_name, "binary", "split"))
  split <- initial_split(complete, prop = 0.80, strata = class)
  training_available <- training(split)
  validation <- testing(split)
  balanced <- undersample_binary(
    training_available,
    class,
    binary_seed_for(sensor_name, model_name, "binary", "undersample")
  )
  training_fitted <- balanced$data

  set.seed(binary_seed_for(sensor_name, model_name, "binary", "fit"))
  fitted <- ranger::ranger(
    dependent.variable.name = "class",
    data = select(training_fitted, -cell),
    num.trees = 500,
    mtry = min(as.integer(mtry_value), ncol(training_fitted) - 2L),
    min.node.size = as.integer(min_n_value),
    importance = "impurity",
    num.threads = BINARY_THREADS,
    seed = binary_seed_for(sensor_name, model_name, "binary", "ranger")
  )

  validation_prediction <- predict(
    fitted,
    data = select(validation, -cell, -class),
    num.threads = BINARY_THREADS
  )$predictions
  validation_prediction <- factor(validation_prediction, levels = BINARY_LEVELS)

  balance_audit <- full_join(
    complete |> count(class, name = "n_labelled"),
    training_available |> count(class, name = "n_training_available"),
    by = "class"
  ) |>
    full_join(training_fitted |> count(class, name = "n_training_fitted"), by = "class") |>
    full_join(validation |> count(class, name = "n_validation"), by = "class") |>
    mutate(
      sensor = sensor_name,
      model = model_name,
      balance_target = balanced$target_n,
      .before = 1
    )

  list(
    model = fitted,
    metrics = binary_metrics_from_factors(validation$class, validation_prediction) |>
      mutate(sensor = sensor_name, model = model_name, positive_class = "surface water", .before = 1),
    confusion = binary_confusion_wide(validation$class, validation_prediction) |>
      mutate(sensor = sensor_name, model = model_name, .before = 1),
    balance = balance_audit,
    training_fitted = training_fitted,
    validation = validation
  )
}

run_binary_sensor <- function(sensor_name, resolution_m, stack_path, rf_prediction_path, reference_table) {
  message("\n===== ", sensor_name, " binary surface-water analysis =====")
  raw_stack <- trim(rast(stack_path))
  dates <- unique(sub("^(\\d{8})_.*$", "\\1", names(raw_stack)))
  if (length(dates) * 4L != nlyr(raw_stack)) {
    stop(sensor_name, " stack is not a four-band-per-date stack.")
  }

  prepared <- prepare_binary_reference(raw_stack, sensor_name)
  reference <- prepared$reference
  scaled_stack <- prepared$scaled_stack
  baseline_columns <- c("cell", "class", names(scaled_stack))
  baseline_complete <- reference |>
    select(all_of(baseline_columns)) |>
    filter(if_all(-c(cell, class), is.finite))

  set.seed(binary_seed_for(sensor_name, "binary", "baseline", "split"))
  tuning_split <- initial_split(baseline_complete, prop = 0.80, strata = class)
  tuning_available <- training(tuning_split)
  tuning_balanced <- undersample_binary(
    tuning_available,
    class,
    binary_seed_for(sensor_name, "binary", "baseline", "undersample")
  )$data
  message("Tuning ", sensor_name, " with ", nrow(tuning_balanced), " balanced pixels.")
  tuned <- tune_binary_baseline(tuning_balanced, sensor_name)
  best_mtry <- tuned$best$mtry[[1]]
  best_min_n <- tuned$best$min_n[[1]]

  sensor_slug <- str_replace_all(str_to_lower(sensor_name), "[^a-z0-9]+", "_")
  saveRDS(tuned$tuning, file.path(BINARY_MODEL_DIR, paste0(sensor_slug, "tuning.rds")), compress = "gzip")
  sensor_cache <- file.path(BINARY_CACHE_DIR, sensor_slug)
  dir.create(sensor_cache, recursive = TRUE, showWarnings = FALSE)

  fitted_outputs <- list()
  raster_paths <- character()
  baseline_fit <- fit_binary_model(
    reference |> select(all_of(baseline_columns)),
    sensor_name,
    "baseline",
    best_mtry,
    best_min_n
  )
  fitted_outputs[["baseline"]] <- baseline_fit
  saveRDS(baseline_fit$model, file.path(BINARY_MODEL_DIR, paste0(sensor_slug, "baseline.rds")), compress = "gzip")
  baseline_path <- file.path(sensor_cache, "baseline.tif")
  predict(
    scaled_stack,
    baseline_fit$model,
    fun = predict_binary_codes,
    filename = baseline_path,
    overwrite = TRUE,
    na.rm = TRUE,
    wopt = list(datatype = "INT1U", gdal = "COMPRESS=LZW")
  )
  raster_paths <- c(raster_paths, baseline_path)

  for (date in dates) {
    message(sensor_name, " binary date model: ", date)
    band_names <- paste0(date, "_", c("blue", "green", "red", "nir"))
    date_fit <- fit_binary_model(
      reference |> select(cell, class, all_of(band_names)),
      sensor_name,
      date,
      mtry_value = min(2L, length(band_names)),
      min_n_value = 2L
    )
    fitted_outputs[[date]] <- date_fit
    date_path <- file.path(sensor_cache, paste0(date, ".tif"))
    predict(
      scaled_stack[[band_names]],
      date_fit$model,
      fun = predict_binary_codes,
      filename = date_path,
      overwrite = TRUE,
      na.rm = TRUE,
      wopt = list(datatype = "INT1U", gdal = "COMPRESS=LZW")
    )
    raster_paths <- c(raster_paths, date_path)
  }

  write_binary_stack(raster_paths, c("baseline", dates), rf_prediction_path)

  metrics <- map_dfr(fitted_outputs, "metrics")
  confusion <- map_dfr(fitted_outputs, "confusion")
  balance <- map_dfr(fitted_outputs, "balance") |> mutate(class = as.character(class))
  baseline_support <- balance |>
    filter(model == "baseline") |>
    left_join(prepared$polygon_counts, by = "class") |>
    mutate(resolution_m = resolution_m, reference_table = reference_table, .after = sensor)

  result <- list(
    metrics = metrics,
    confusion = confusion,
    balance = balance,
    baseline_support = baseline_support,
    tuning_metrics = collect_metrics(tuned$tuning) |> mutate(sensor = sensor_name, .before = 1),
    tuning_best = tuned$best |> mutate(sensor = sensor_name, .before = 1)
  )
  saveRDS(result, file.path(sensor_cache, "sensor_summary.rds"), compress = "gzip")
  result
}

write_binary_cluster_stacks <- function() {
  pmap(BINARY_SENSOR_CONFIG, function(sensor, resolution_m, stack_path, rf_prediction_path, cluster_path, cluster_water_code, reference_table) {
    output_path <- file.path(
      BINARY_RASTER_DIR,
      paste0(str_replace_all(str_to_lower(sensor), "[^a-z0-9]+", "_"), "cluster_binary.tif")
    )
    cluster <- rast(cluster_path)
    binary <- ifel(cluster == cluster_water_code, 2, 1)
    binary <- as.factor(binary)
    categories <- data.frame(value = 1:2, class = unname(BINARY_LABELS[BINARY_LEVELS]))
    for (i in seq_len(nlyr(binary))) levels(binary[[i]]) <- categories
    names(binary) <- names(cluster)
    writeRaster(
      binary,
      output_path,
      overwrite = TRUE,
      wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW", "PREDICTOR=2"))
    )
    tibble(sensor = sensor, cluster_binary_path = output_path)
  }) |>
    list_rbind() |>
    write_binary_csv("cluster_binary_paths.csv")
}

run_binary_landcover_models <- function() {
  results <- pmap(
    select(BINARY_SENSOR_CONFIG, sensor, resolution_m, stack_path, rf_prediction_path, reference_table),
    run_binary_sensor
  )
  write_binary_csv(map_dfr(results, "metrics"), "binary_validation_metrics.csv")
  write_binary_csv(map_dfr(results, "confusion"), "binary_validation_confusion.csv")
  write_binary_csv(map_dfr(results, "balance"), "binary_training_balance_by_model.csv")
  write_binary_csv(
    map_dfr(results, "baseline_support") |> filter(reference_table) |> select(-reference_table),
    "binary_reference_support_by_sensor.csv"
  )
  write_binary_csv(map_dfr(results, "tuning_metrics"), "binary_tuning_metrics.csv")
  write_binary_csv(map_dfr(results, "tuning_best"), "binary_tuning_selected.csv")
  write_binary_cluster_stacks()
  writeLines(capture.output(sessionInfo()), file.path(BINARY_MODEL_DIR, "R_sessionInfo.txt"))
  message("Binary land and surface-water models complete.")
  invisible(results)
}

if (sys.nframe() == 0L) run_binary_landcover_models()
