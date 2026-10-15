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
  library(doParallel)
})

if (!file.exists("_docs/Documentation/_written/Manuscript.docx")) {
  stop("Run this workflow from the Meso-change project root.")
}

ANALYSIS_SEED <- 99L
MESO_SEED <- 42L
DETECTED_CORES <- suppressWarnings(parallel::detectCores(logical = TRUE))
if (!is.finite(DETECTED_CORES)) DETECTED_CORES <- 4L
N_THREADS <- max(1L, min(8L, as.integer(DETECTED_CORES) - 1L))
N_TUNING_WORKERS <- max(1L, min(4L, N_THREADS))

LANDCOVER_LEVELS <- c(
  "bareland",
  "agricultural_land",
  "forest",
  "urban",
  "water"
)

LANDCOVER_LABELS <- c(
  bareland = "Bare land",
  agricultural_land = "Agricultural land",
  forest = "Forest",
  urban = "Urban",
  water = "Surface water"
)

normalize_landcover_class <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))
  case_when(
    x %in% c("bareland", "bare land") ~ "bareland",
    x %in% c("cropland", "agricultural land", "agricultural_land") ~ "agricultural_land",
    x == "forest" ~ "forest",
    x == "urban" ~ "urban",
    x %in% c("water", "watere", "surface water") ~ "water",
    TRUE ~ NA_character_
  )
}

sensor_config <- tribble(
  ~sensor, ~resolution_m, ~stack_path, ~prediction_path,
  "Landsat-9", 30, "_data/GIS/Raster/Clean/landsat_22dates_core4.tif", "_data/GIS/Raster/Analysis/RandomForest/balanced/rf_landsat_core4_balanced.tif",
  "Sentinel-2", 10, "_data/GIS/Raster/Clean/sentinel_22dates_core4.tif", "_data/GIS/Raster/Analysis/RandomForest/balanced/rf_sentinel_core4_balanced.tif",
  "PlanetScope", 3, "_data/GIS/Raster/Clean/planet_22dates.tif", "_data/GIS/Raster/Analysis/RandomForest/balanced/rf_planet_balanced.tif"
)

landcover_run_config <- bind_rows(
  sensor_config |> mutate(reference_table = TRUE),
  tibble(
    sensor = "PlanetScope (7-date validation)",
    resolution_m = 3,
    stack_path = "_data/GIS/Raster/Clean/planet_7dates.tif",
    prediction_path = "_data/GIS/Raster/Analysis/RandomForest/balanced/rf_planet_7_balanced.tif",
    reference_table = FALSE
  )
)

LANDCOVER_POLYGONS <- "_data/GIS/Vector/userDelineated.shp"
RIVER_CORRIDOR <- "_data/GIS/Vector/Clean/river_corridor.shp"
BALANCED_TABLE_DIR <- "_data/Tables/balanced_analysis"
LANDCOVER_MODEL_DIR <- "_data/Models/balanced_analysis/landcover"
MESO_MODEL_DIR <- "_data/Models/balanced_analysis/mesohabitat"
LANDCOVER_CACHE_DIR <- "_data/Cache/balanced_analysis/landcover"
MESO_CACHE_DIR <- "_data/Cache/meso_classification_v2"
MESO_PREDICTION_DIR <- "_data/GIS/Raster/Outputs/meso_predictions_balanced"

dir.create(BALANCED_TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LANDCOVER_MODEL_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MESO_MODEL_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LANDCOVER_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MESO_PREDICTION_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(sensor_config$prediction_path[1]), recursive = TRUE, showWarnings = FALSE)

seed_for <- function(..., base = ANALYSIS_SEED) {
  key <- paste(..., collapse = "|")
  hash <- sum(utf8ToInt(key) * seq_along(utf8ToInt(key)))
  as.integer((base + hash) %% .Machine$integer.max)
}

write_balanced_csv <- function(x, filename) {
  readr::write_csv(x, file.path(BALANCED_TABLE_DIR, filename), na = "")
}
