suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(stringr)
  library(rsample)
  library(recipes)
  library(parsnip)
  library(workflows)
  library(tune)
  library(dials)
  library(yardstick)
  library(ranger)
})

if (!file.exists("_docs/Documentation/_written/Manuscript.docx")) {
  stop("Run the binary workflow from the Meso-change project root.")
}

BINARY_SEED <- 99L
DETECTED_BINARY_CORES <- suppressWarnings(parallel::detectCores(logical = TRUE))
if (!is.finite(DETECTED_BINARY_CORES)) DETECTED_BINARY_CORES <- 4L
BINARY_THREADS <- max(1L, min(8L, as.integer(DETECTED_BINARY_CORES) - 1L))

BINARY_LEVELS <- c("land", "water")
BINARY_LABELS <- c(land = "Land", water = "Surface water")

normalize_reference_class <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))
  case_when(
    x %in% c("water", "watere", "surface water") ~ "water",
    x %in% c(
      "bareland", "bare land", "cropland", "agricultural land",
      "agricultural_land", "forest", "urban"
    ) ~ "land",
    TRUE ~ NA_character_
  )
}

BINARY_SENSOR_CONFIG <- tribble(
  ~sensor, ~resolution_m, ~stack_path, ~rf_prediction_path, ~cluster_path, ~cluster_water_code, ~reference_table,
  "Landsat-9", 30, "_data/GIS/Raster/Clean/landsat_22dates_core4.tif", "_data/GIS/Raster/Analysis/Binary/rf_landsat_binary.tif", "_data/GIS/Raster/Analysis/Kmeans/km_landsat_22_core4.tif", 2L, TRUE,
  "Sentinel-2", 10, "_data/GIS/Raster/Clean/sentinel_22dates_core4.tif", "_data/GIS/Raster/Analysis/Binary/rf_sentinel_binary.tif", "_data/GIS/Raster/Analysis/Kmeans/km_sentinel_22_core4.tif", 4L, TRUE,
  "PlanetScope", 3, "_data/GIS/Raster/Clean/planet_22dates.tif", "_data/GIS/Raster/Analysis/Binary/rf_planet_binary.tif", "_data/GIS/Raster/Analysis/Kmeans/km_planet_22.tif", 3L, TRUE,
  "PlanetScope (7-date hydraulic comparison)", 3, "_data/GIS/Raster/Clean/planet_7dates.tif", "_data/GIS/Raster/Analysis/Binary/rf_planet_7_binary.tif", "_data/GIS/Raster/Analysis/Kmeans/km_planet_7.tif", 3L, FALSE
)

BINARY_DATE_MAP <- tribble(
  ~test_id, ~hydraulic_date, ~image_date,
  "01_Dec22_2021", as.Date("2021-12-22"), as.Date("2021-12-22"),
  "02_Feb12_2022", as.Date("2022-02-12"), as.Date("2022-02-12"),
  "03_April17_2022", as.Date("2022-04-17"), as.Date("2022-04-18"),
  "04_May31_2022", as.Date("2022-05-31"), as.Date("2022-05-31"),
  "05_June20_2022", as.Date("2022-06-20"), as.Date("2022-06-20"),
  "06_July18_2022", as.Date("2022-07-18"), as.Date("2022-07-18"),
  "07_Sep28_2022", as.Date("2022-09-28"), as.Date("2022-09-28")
)

BINARY_POLYGONS <- "_data/GIS/Vector/userDelineated.shp"
BINARY_RIVER_CORRIDOR <- "_data/GIS/Vector/Clean/river_corridor.shp"
BINARY_HYDRAULIC_REFERENCE <- "_data/GIS/Raster/Clean/mesohabitats_v2.tif"
BINARY_TABLE_DIR <- "_data/Tables/binary_surface_water"
BINARY_MODEL_DIR <- "_data/Models/binary_surface_water"
BINARY_CACHE_DIR <- "_data/Cache/binary_surface_water"
BINARY_RASTER_DIR <- "_data/GIS/Raster/Analysis/Binary"

walk(
  c(BINARY_TABLE_DIR, BINARY_MODEL_DIR, BINARY_CACHE_DIR, BINARY_RASTER_DIR),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

binary_seed_for <- function(..., base = BINARY_SEED) {
  key <- paste(..., collapse = "|")
  code <- utf8ToInt(key)
  hash <- sum(code * seq_along(code))
  as.integer((base + hash) %% .Machine$integer.max)
}

write_binary_csv <- function(x, filename) {
  readr::write_csv(x, file.path(BINARY_TABLE_DIR, filename), na = "")
}
