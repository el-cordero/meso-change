source("_scripts/03_data_analysis/08_binary_config.R")
source("_scripts/03_data_analysis/09_binary_helpers.R")

binary_path <- "_data/Tables/binary_surface_water/binary_validation_confusion.csv"
all_workflow_binary_path <- "_data/Tables/binary_surface_water/binary_all_workflow_validation_confusion.csv"
hydraulic_path <- "_data/Tables/binary_surface_water/hydraulic_confusion_pooled_counts.csv"
mesohabitat_path <- "_data/Tables/balanced_analysis/mesohabitat_loo_confusion_pooled.csv"
mesohabitat_fold_path <- "_data/Tables/balanced_analysis/mesohabitat_loo_confusion_by_fold.csv"
output_path <- "_data/Tables/consolidated_confusion_matrices.csv"

prepare_binary_reference_cells <- function(stack, sensor_name) {
  polygons <- vect(BINARY_POLYGONS)
  polygons$binary_class <- normalize_reference_class(polygons$class)
  if (anyNA(polygons$binary_class)) {
    stop("Unrecognized reference-polygon values for ", sensor_name, ".")
  }
  polygons$class_code <- match(polygons$binary_class, BINARY_LEVELS)
  polygons <- project(polygons, crs(stack))
  class_raster <- rasterize(polygons, stack[[1]], field = "class_code")
  labelled_cells <- which(!is.na(values(class_raster, mat = FALSE)))
  class_codes <- terra::extract(class_raster, labelled_cells)[[1]]
  river <- project(vect(BINARY_RIVER_CORRIDOR), crs(stack))
  corridor <- rasterize(river, stack[[1]], field = 1, background = NA)
  inside_river_corridor <- !is.na(terra::extract(corridor, labelled_cells)[[1]])
  tibble(
    cell = labelled_cells,
    class = factor(BINARY_LEVELS[class_codes], levels = BINARY_LEVELS),
    inside_river_corridor
  )
}

all_workflow_validation_confusion <- function(sensor_name, stack_path, rf_prediction_path) {
  stack <- trim(rast(stack_path))
  reference <- prepare_binary_reference_cells(stack, sensor_name)
  dates <- unique(sub("^(\\d{8})_.*$", "\\1", names(stack)))
  cluster_path <- file.path(
    BINARY_RASTER_DIR,
    paste0(str_replace_all(str_to_lower(sensor_name), "[^a-z0-9]+", "_"), "cluster_binary.tif")
  )
  cluster <- rast(cluster_path)
  random_forest <- rast(rf_prediction_path)
  if (
    !identical(dates, setdiff(names(cluster), "baseline")) ||
      !identical(dates, setdiff(names(random_forest), "baseline"))
  ) {
    stop("Prediction and predictor dates do not match for ", sensor_name, ".")
  }

  map_dfr(dates, function(date) {
    band_names <- paste0(date, "_", c("blue", "green", "red", "nir"))
    predictor_values <- terra::extract(stack[[band_names]], reference$cell)
    if ("ID" %in% names(predictor_values)) predictor_values$ID <- NULL
    complete <- bind_cols(reference, as_tibble(predictor_values)) |>
      filter(if_all(-c(cell, class, inside_river_corridor), is.finite))
    set.seed(binary_seed_for(sensor_name, date, "binary", "split"))
    validation_full <- testing(initial_split(complete, prop = 0.80, strata = class))
    validation <- validation_full |>
      filter(inside_river_corridor)
    if (!nrow(validation) || n_distinct(validation$class) != 2L) {
      stop("River-corridor validation lacks both classes for ", sensor_name, " ", date, ".")
    }
    cluster_labels <- terra::extract(cluster[[date]], validation$cell)[[1]]
    random_forest_labels <- terra::extract(random_forest[[date]], validation$cell)[[1]]
    cluster_codes <- match(as.character(cluster_labels), unname(BINARY_LABELS))
    random_forest_codes <- match(as.character(random_forest_labels), unname(BINARY_LABELS))
    common <- is.finite(cluster_codes) & is.finite(random_forest_codes)
    if (!any(common)) {
      stop("No common prediction support for ", sensor_name, " ", date, ".")
    }
    truth <- validation$class[common]
    predictions <- list(
      "Cluster-assisted" = factor(BINARY_LEVELS[cluster_codes[common]], levels = BINARY_LEVELS),
      "Random forest" = factor(BINARY_LEVELS[random_forest_codes[common]], levels = BINARY_LEVELS)
    )
    map_dfr(names(predictions), function(workflow_name) {
      binary_confusion_wide(truth, predictions[[workflow_name]]) |>
        mutate(
          sensor = sensor_name,
          model = date,
          workflow = workflow_name,
          full_partition_truth_total = vapply(
            as.character(truth),
            function(class_name) sum(validation_full$class == class_name),
            integer(1)
          ),
          partition_truth_total = vapply(
            as.character(truth),
            function(class_name) sum(validation$class == class_name),
            integer(1)
          ),
          n_common_evaluated = land + water,
          n_excluded = partition_truth_total - n_common_evaluated,
          .before = 1
        )
    })
  })
}

configured_sensors <- BINARY_SENSOR_CONFIG |>
  filter(reference_table) |>
  select(sensor, stack_path, rf_prediction_path)

all_workflow_confusion <- pmap_dfr(
  configured_sensors,
  all_workflow_validation_confusion
)

saved_random_forest_confusion <- read_csv(binary_path, show_col_types = FALSE) |>
  filter(
    sensor %in% configured_sensors$sensor,
    model != "baseline"
  )

reconstructed_partition_totals <- all_workflow_confusion |>
  distinct(sensor, model, truth, full_partition_truth_total) |>
  rename(partition_truth_total = full_partition_truth_total) |>
  arrange(sensor, model, truth)
saved_partition_totals <- saved_random_forest_confusion |>
  mutate(truth_total = land + water) |>
  transmute(sensor, model, truth, partition_truth_total = truth_total) |>
  arrange(sensor, model, truth)
if (!isTRUE(all.equal(reconstructed_partition_totals, saved_partition_totals, check.attributes = FALSE))) {
  stop("Reconstructed validation partitions do not match the saved random-forest partitions.")
}

all_workflow_confusion |>
  arrange(sensor, model, workflow, truth) |>
  write_csv(all_workflow_binary_path)

binary_raw <- read_csv(all_workflow_binary_path, show_col_types = FALSE)

binary_date_counts <- binary_raw |>
  distinct(sensor, workflow, model) |>
  count(sensor, workflow, name = "n_dates")

if (any(binary_date_counts$n_dates != 22L)) {
  stop("Every satellite system must contain exactly 22 date-specific validation matrices.")
}

binary <- binary_raw |>
  pivot_longer(c(land, water), names_to = "predicted_class", values_to = "count") |>
  group_by(sensor, workflow, truth, predicted_class) |>
  summarise(count = sum(count), .groups = "drop") |>
  group_by(sensor, workflow, truth) |>
  mutate(row_percent = count / sum(count)) |>
  ungroup() |>
  transmute(
    evaluation = "Surface-water classification against held-out reference pixels",
    sensor_or_dataset = sensor,
    workflow,
    n_dates = 22L,
    observed_class = recode(truth, land = "Land", water = "Surface water"),
    predicted_class = recode(predicted_class, land = "Land", water = "Surface water"),
    count,
    row_percent
  )

hydraulic <- read_csv(hydraulic_path, show_col_types = FALSE) |>
  pivot_longer(c(tn, fp, fn, tp), names_to = "cell", values_to = "count") |>
  mutate(
    observed_class = if_else(cell %in% c("tn", "fp"), "Land", "Surface water"),
    predicted_class = if_else(cell %in% c("tn", "fn"), "Land", "Surface water")
  ) |>
  group_by(workflow, observed_class) |>
  mutate(row_percent = count / sum(count)) |>
  ungroup() |>
  transmute(
    evaluation = "Surface-water classification against hydraulic masks",
    sensor_or_dataset = "PlanetScope-hydraulic",
    workflow,
    n_dates = 7L,
    observed_class,
    predicted_class,
    count,
    row_percent
  )

mesohabitat_folds <- read_csv(mesohabitat_fold_path, show_col_types = FALSE) |>
  distinct(test_id)

if (nrow(mesohabitat_folds) != 7L) {
  stop("Mesohabitat validation must contain exactly seven held-out-date folds.")
}

mesohabitat <- read_csv(mesohabitat_path, show_col_types = FALSE) |>
  transmute(
    evaluation = "Eight-class mesohabitat leave-one-date-out validation",
    sensor_or_dataset = "PlanetScope-hydraulic mesohabitat",
    workflow = model,
    n_dates = 7L,
    observed_class = truth,
    predicted_class = prediction,
    count,
    row_percent = row_proportion
  )

bind_rows(binary, hydraulic, mesohabitat) |>
  arrange(evaluation, sensor_or_dataset, workflow, observed_class, predicted_class) |>
  write_csv(output_path)

message("Wrote ", output_path)
