source("_scripts/03_data_analysis/00_config.R")
source("_scripts/03_data_analysis/01_helpers.R")

prepare_reference_data <- function(stack, sensor_name) {
  polygons <- vect(LANDCOVER_POLYGONS)
  polygons$class <- normalize_landcover_class(polygons$class)
  if (anyNA(polygons$class)) {
    stop("Unrecognized land-cover polygon values: ", paste(unique(polygons$class[is.na(polygons$class)]), collapse = ", "))
  }
  polygons$class <- factor(polygons$class, levels = LANDCOVER_LEVELS)
  polygons$class_code <- match(as.character(polygons$class), LANDCOVER_LEVELS)
  polygons <- project(polygons, crs(stack))

  class_raster <- rasterize(polygons, stack[[1]], field = "class_code")
  labelled_cells <- which(!is.na(values(class_raster, mat = FALSE)))
  if (!length(labelled_cells)) stop("No polygon cells intersect the ", sensor_name, " grid.")

  scaled_stack <- scale(stack)
  predictor_values <- terra::extract(scaled_stack, labelled_cells)
  if ("ID" %in% names(predictor_values)) predictor_values$ID <- NULL
  class_codes <- terra::extract(class_raster, labelled_cells)[[1]]
  reference <- as_tibble(predictor_values) |>
    mutate(
      cell = labelled_cells,
      class = factor(LANDCOVER_LEVELS[class_codes], levels = LANDCOVER_LEVELS),
      .before = 1
    )

  polygon_counts <- as.data.frame(polygons) |>
    count(class, name = "n_polygons") |>
    mutate(class = as.character(class))

  list(
    scaled_stack = scaled_stack,
    reference = reference,
    polygon_counts = polygon_counts
  )
}

tune_baseline <- function(train_balanced, sensor_name) {
  model_data <- select(train_balanced, -cell)
  predictor_count <- ncol(model_data) - 1L
  model_spec <- rand_forest(
    trees = 300,
    mtry = tune(),
    min_n = tune()
  ) |>
    set_mode("classification") |>
    set_engine("ranger", importance = "impurity", num.threads = N_THREADS)
  model_recipe <- recipe(class ~ ., data = model_data)
  model_workflow <- workflow() |>
    add_model(model_spec) |>
    add_recipe(model_recipe)

  set.seed(seed_for(sensor_name, "folds"))
  folds <- vfold_cv(model_data, v = 5, strata = class)
  parameter_grid <- grid_regular(
    mtry(range = c(1L, min(25L, predictor_count))),
    min_n(range = c(2L, 20L)),
    levels = 5
  )

  # Sequential resampling avoids platform-dependent socket failures. Ranger
  # still uses N_THREADS within each fitted forest.
  foreach::registerDoSEQ()

  set.seed(seed_for(sensor_name, "tuning"))
  tuning <- tune_grid(
    model_workflow,
    resamples = folds,
    grid = parameter_grid,
    metrics = metric_set(roc_auc, accuracy),
    control = control_grid(verbose = TRUE, allow_par = FALSE)
  )
  best <- select_best(tuning, metric = "roc_auc")
  list(tuning = tuning, best = best)
}

fit_landcover_model <- function(model_data, sensor_name, model_name, mtry_value, min_n_value, trees) {
  complete <- model_data |>
    filter(if_all(-c(cell, class), is.finite))
  if (n_distinct(complete$class) != length(LANDCOVER_LEVELS)) {
    stop(sensor_name, " ", model_name, " does not contain all five land-cover classes.")
  }

  set.seed(seed_for(sensor_name, model_name, "split"))
  split <- initial_split(complete, prop = 0.80, strata = class)
  training_available <- training(split)
  validation <- testing(split)
  balanced <- undersample_to_minimum(
    training_available,
    class,
    seed_for(sensor_name, model_name, "undersample")
  )
  training_balanced <- balanced$data

  set.seed(seed_for(sensor_name, model_name, "fit"))
  fitted <- ranger::ranger(
    dependent.variable.name = "class",
    data = select(training_balanced, -cell),
    num.trees = trees,
    mtry = min(as.integer(mtry_value), ncol(training_balanced) - 2L),
    min.node.size = as.integer(min_n_value),
    importance = "impurity",
    num.threads = N_THREADS,
    seed = seed_for(sensor_name, model_name, "ranger")
  )

  validation_prediction <- predict(
    fitted,
    data = select(validation, -cell, -class),
    num.threads = N_THREADS
  )$predictions
  validation_prediction <- factor(validation_prediction, levels = LANDCOVER_LEVELS)

  balance_audit <- full_join(
    complete |> count(class, name = "n_labelled"),
    training_available |> count(class, name = "n_training_available"),
    by = "class"
  ) |>
    full_join(training_balanced |> count(class, name = "n_training_fitted"), by = "class") |>
    full_join(validation |> count(class, name = "n_validation"), by = "class") |>
    mutate(
      sensor = sensor_name,
      model = model_name,
      balance_target = balanced$target_n,
      .before = 1
    )

  list(
    model = fitted,
    metrics = classification_metrics(validation$class, validation_prediction) |>
      mutate(sensor = sensor_name, model = model_name, .before = 1),
    confusion = multiclass_confusion(validation$class, validation_prediction) |>
      mutate(sensor = sensor_name, model = model_name, .before = 1),
    balance = balance_audit,
    training_balanced = training_balanced,
    validation = validation
  )
}

run_sensor <- function(sensor_name, stack_path, prediction_path, resolution_m, reference_table) {
  message("\n===== ", sensor_name, " land-cover analysis =====")
  raw_stack <- trim(rast(stack_path))
  dates <- unique(sub("^(\\d{8})_.*$", "\\1", names(raw_stack)))
  if (length(dates) * 4L != nlyr(raw_stack)) {
    stop(sensor_name, " stack is not a four-band-per-date stack.")
  }

  prepared <- prepare_reference_data(raw_stack, sensor_name)
  scaled_stack <- prepared$scaled_stack
  reference <- prepared$reference

  baseline_columns <- c("cell", "class", names(scaled_stack))
  baseline_complete <- reference |>
    select(all_of(baseline_columns)) |>
    filter(if_all(-c(cell, class), is.finite))
  set.seed(seed_for(sensor_name, "baseline", "split"))
  baseline_split_for_tuning <- initial_split(baseline_complete, prop = 0.80, strata = class)
  tuning_available <- training(baseline_split_for_tuning)
  tuning_balanced <- undersample_to_minimum(
    tuning_available,
    class,
    seed_for(sensor_name, "baseline", "undersample")
  )$data

  message("Tuning balanced baseline model for ", sensor_name, " with ", nrow(tuning_balanced), " pixels.")
  tuned <- tune_baseline(tuning_balanced, sensor_name)
  best_mtry <- tuned$best$mtry[[1]]
  best_min_n <- tuned$best$min_n[[1]]

  tuning_metrics <- collect_metrics(tuned$tuning) |>
    mutate(sensor = sensor_name, .before = 1)
  tuning_best <- tuned$best |>
    mutate(sensor = sensor_name, .before = 1)
  saveRDS(tuned$tuning, file.path(LANDCOVER_MODEL_DIR, paste0(str_replace_all(str_to_lower(sensor_name), "[^a-z0-9]+", "_"), "_tuning.rds")))

  fitted_outputs <- list()
  raster_paths <- character()
  raster_names <- c("baseline", dates)
  sensor_cache <- file.path(LANDCOVER_CACHE_DIR, str_replace_all(str_to_lower(sensor_name), "[^a-z0-9]+", "_"))
  dir.create(sensor_cache, recursive = TRUE, showWarnings = FALSE)

  baseline_fit <- fit_landcover_model(
    reference |> select(all_of(baseline_columns)),
    sensor_name,
    "baseline",
    best_mtry,
    best_min_n,
    trees = 500L
  )
  fitted_outputs[["baseline"]] <- baseline_fit
  saveRDS(baseline_fit$model, file.path(LANDCOVER_MODEL_DIR, paste0(str_replace_all(str_to_lower(sensor_name), "[^a-z0-9]+", "_"), "_baseline.rds")), compress = "gzip")

  baseline_raster_path <- file.path(sensor_cache, "baseline.tif")
  predict(
    scaled_stack,
    baseline_fit$model,
    fun = predict_ranger_codes,
    class_levels = LANDCOVER_LEVELS,
    filename = baseline_raster_path,
    overwrite = TRUE,
    na.rm = TRUE,
    wopt = list(datatype = "INT1U", gdal = "COMPRESS=LZW")
  )
  raster_paths <- c(raster_paths, baseline_raster_path)

  for (date in dates) {
    message(sensor_name, " date-specific model: ", date)
    band_names <- paste0(date, "_", c("blue", "green", "red", "nir"))
    date_fit <- fit_landcover_model(
      reference |> select(cell, class, all_of(band_names)),
      sensor_name,
      date,
      mtry_value = 2L,
      min_n_value = 2L,
      trees = 500L
    )
    fitted_outputs[[date]] <- date_fit
    date_path <- file.path(sensor_cache, paste0(date, ".tif"))
    predict(
      scaled_stack[[band_names]],
      date_fit$model,
      fun = predict_ranger_codes,
      class_levels = LANDCOVER_LEVELS,
      filename = date_path,
      overwrite = TRUE,
      na.rm = TRUE,
      wopt = list(datatype = "INT1U", gdal = "COMPRESS=LZW")
    )
    raster_paths <- c(raster_paths, date_path)
  }

  category_table <- data.frame(
    value = seq_along(LANDCOVER_LEVELS),
    class = unname(LANDCOVER_LABELS[LANDCOVER_LEVELS])
  )
  make_categorical_stack(raster_paths, raster_names, category_table, prediction_path)

  metrics <- map_dfr(fitted_outputs, "metrics")
  confusion <- map_dfr(fitted_outputs, "confusion")
  balance <- map_dfr(fitted_outputs, "balance") |>
    mutate(class = as.character(class))

  baseline_support <- balance |>
    filter(model == "baseline") |>
    left_join(prepared$polygon_counts, by = "class") |>
    mutate(
      resolution_m = resolution_m,
      reference_table = reference_table,
      .after = sensor
    )

  result <- list(
    metrics = metrics,
    confusion = confusion,
    balance = balance,
    baseline_support = baseline_support,
    tuning_metrics = tuning_metrics,
    tuning_best = tuning_best
  )
  saveRDS(result, file.path(sensor_cache, "sensor_summary.rds"), compress = "gzip")
  result
}

run_landcover_analysis <- function() {
  results <- pmap(
    landcover_run_config,
    ~ run_sensor(..1, ..3, ..4, ..2, ..5)
  )
  write_balanced_csv(map_dfr(results, "metrics"), "landcover_validation_metrics.csv")
  write_balanced_csv(map_dfr(results, "confusion"), "landcover_validation_confusion.csv")
  write_balanced_csv(map_dfr(results, "balance"), "landcover_training_balance_by_model.csv")
  write_balanced_csv(
    map_dfr(results, "baseline_support") |> filter(reference_table) |> select(-reference_table),
    "landcover_reference_support_by_sensor.csv"
  )
  write_balanced_csv(
    map_dfr(results, "baseline_support"),
    "landcover_reference_support_all_models.csv"
  )
  write_balanced_csv(map_dfr(results, "tuning_metrics"), "landcover_tuning_metrics.csv")
  write_balanced_csv(map_dfr(results, "tuning_best"), "landcover_tuning_selected.csv")
  writeLines(capture.output(sessionInfo()), file.path(LANDCOVER_MODEL_DIR, "R_sessionInfo.txt"))
  message("Balanced land-cover analysis complete.")
  invisible(results)
}

if (sys.nframe() == 0L) run_landcover_analysis()
