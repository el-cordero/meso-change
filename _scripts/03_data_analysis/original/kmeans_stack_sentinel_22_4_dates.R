source('_scripts/00_libraries.R')
source('_scripts/01_functions/km_knn_raster.r')

# ---------------------------------------------------------
# Read raster
# ---------------------------------------------------------
r <- rast('_data/GIS/Raster/Clean/sentinel_22dates_core4.tif')

# trim outer NA values
r <- trim(r)

# Sentinel is already 10 m, so resampling is usually unnecessary
# s <- rast(ext = ext(r), crs = crs(r), res = 10)
# r <- resample(r, s)

# scale the values
r <- scale(r)

# get dates from layer names
dates <- sub("^(\\d{8})_.*$", "\\1", names(r))
dates <- unique(dates)

# It is important to set the seed because kmeans
# initializes centers in random locations
set.seed(99)

k_raster <- km_knn_raster(
  r = r,
  n_bands = 4,
  model_date = dates
)

# save the raster stack
writeRaster(
  k_raster,
  '_data/GIS/Raster/Analysis/Kmeans/km_sentinel_22_core4.tif',
  overwrite = TRUE
)

rm(list = ls())
