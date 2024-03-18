source('_scripts/00_libraries.R')
source('_scripts/01_functions/km_knn_raster.r')

# read raster
r <- rast('_data/GIS/Raster/Clean/planet_22dates.tif')

# trim an NA values
r <- trim(r)

# resample according to your needs/capacity
s <- rast(ext=ext(r),crs=crs(r),res=10)
r <- resample(r,s)

# scale the values
r <- scale(r)

dates <- sub("^(\\d{8})_.*$", "\\1", names(r))
dates <- unique(dates)

# It is important to set the seed generator because `kmeans`
# initiates the centers in random locations
set.seed(99)

k_raster <- km_knn_raster(
  r = r,
  n_bands = 4,
  model_date = dates
)

# save the raster stack as a dataset
writeRaster(k_raster, '_data/GIS/Raster/Analysis/Kmeans/km_planet_22.tif',
            overwrite=TRUE)
rm(list=ls())


