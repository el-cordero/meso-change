source('_scripts/00_libraries.R')
source('_scripts/01_functions/km_knn_raster.r')

# ---------------------------------------------------------
# Read Landsat core4 raster stack
# ---------------------------------------------------------
r <- rast('_data/GIS/Raster/Clean/landsat_22dates_core4.tif')

# trim outer NA values
r <- trim(r)

# scale the values
r <- scale(r)

# acquire the sampling dates from layer names
dates <- sub("^(\\d{8})_.*$", "\\1", names(r))
dates <- unique(dates)

print(dates)

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
  '_data/GIS/Raster/Analysis/Kmeans/km_landsat_22_core4.tif',
  overwrite = TRUE
)

rm(list = ls())