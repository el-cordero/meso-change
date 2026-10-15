# Land-cover reference support by sensor and analysis resolution
#
# This post-processing script reproduces only the reference-pixel accounting
# used by the existing random-forest workflows. It does not fit or refit a
# model. The same 49 QGIS polygons are rasterized independently to each
# sensor grid, complete-case pixels are retained across the 22-date core-four
# stack, and the existing seed-99 stratified 80:20 split is reconstructed.

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(purrr)
  library(readr)
  library(rsample)
  library(tibble)
})

output_file <- "_data/Tables/reviewer_revision/landcover_reference_support_by_sensor.csv"
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

sensor_inputs <- tribble(
  ~sensor, ~analysis_resolution_m, ~raster_path,
  "Landsat-9", 30, "_data/GIS/Raster/Clean/landsat_22dates_core4.tif",
  "Sentinel-2", 10, "_data/GIS/Raster/Clean/sentinel_22dates_core4.tif",
  "PlanetScope", 3, "_data/GIS/Raster/Clean/planet_22dates.tif"
)

reference_polygons <- vect("_data/GIS/Vector/userDelineated.shp")
class_order <- c("bareland", "cropland", "forest", "urban", "water")

polygon_counts <- reference_polygons |>
  as.data.frame() |>
  count(class, name = "n_polygons") |>
  mutate(class = as.character(class))

complete_reference_classes <- function(predictors, labels, label_cells, block_size = 8L) {
  complete <- rep(TRUE, length(label_cells))

  for (first_layer in seq.int(1L, nlyr(predictors), by = block_size)) {
    last_layer <- min(first_layer + block_size - 1L, nlyr(predictors))
    values_block <- terra::extract(
      predictors[[first_layer:last_layer]],
      label_cells
    )
    complete <- complete & stats::complete.cases(values_block)
  }

  label_values <- terra::extract(labels, label_cells)[[1]]
  label_values[complete]
}

summarize_sensor <- function(sensor, analysis_resolution_m, raster_path) {
  predictors <- trim(rast(raster_path))
  polygons <- project(reference_polygons, crs(predictors))
  polygons$class_code <- match(as.character(polygons$class), class_order)

  labels <- rasterize(polygons, predictors[[1]], field = "class_code")
  label_cells <- which(!is.na(values(labels, mat = FALSE)))
  clean_codes <- complete_reference_classes(predictors, labels, label_cells)

  clean_reference <- tibble(
    class = factor(class_order[clean_codes], levels = class_order)
  )

  set.seed(99)
  data_split <- initial_split(clean_reference, prop = 0.80, strata = class)
  training_counts <- training(data_split) |>
    count(class, name = "training_assigned")
  validation_counts <- testing(data_split) |>
    count(class, name = "validation")

  tibble(class = factor(class_order, levels = class_order)) |>
    left_join(count(clean_reference, class, name = "labeled_pixels"), by = "class") |>
    left_join(training_counts, by = "class") |>
    left_join(validation_counts, by = "class") |>
    mutate(
      across(c(labeled_pixels, training_assigned, validation), ~coalesce(.x, 0L)),
      training_fitted = pmin(training_assigned, 50000L),
      sensor = sensor,
      analysis_resolution_m = analysis_resolution_m,
      .before = 1
    ) |>
    mutate(class = as.character(class)) |>
    left_join(polygon_counts, by = "class") |>
    select(
      sensor,
      analysis_resolution_m,
      class,
      n_polygons,
      labeled_pixels,
      training_assigned,
      training_fitted,
      validation
    )
}

support <- pmap_dfr(sensor_inputs, summarize_sensor)
write_csv(support, output_file)
print(support, n = Inf)
