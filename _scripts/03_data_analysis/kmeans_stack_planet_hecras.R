source('_scripts/00_libraries.R')
source('_scripts/01_functions/km_knn_raster.r')

# read raster
r <- rast('_data/GIS/Raster/Clean/planet_hecras.tif')

# resample according to your needs/capacity
s <- rast(ext=ext(r),crs=crs(r),res=10)
r <- resample(r,s)

# scale the values
r <- scale(r)

dates <- c('01_Dec22_2021','02_Feb12_2022','03_Apr17_2022',
  '04_May31_2022','05_Jun20_2022','06_Jul18_2022','07_Sep28_2022')

# It is important to set the seed generator because `kmeans`
# initiates the centers in random locations
set.seed(99)


k_raster <- km_knn_raster(
  r = r,
  n_bands = 6,
  model_date = dates,
  optimal_k = 8
)

# save the raster stack as a dataset
writeRaster(k_raster, '_data/GIS/Raster/Analysis/Kmeans/km_planet_hecras.tif',
            overwrite=TRUE)
writeRaster(k_raster$baseline, '_data/GIS/Raster/Analysis/Kmeans/km_high_resolution_baseline.tif',
            overwrite=TRUE)

rm(list=ls())


