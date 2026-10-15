source('_scripts/00_libraries.R')
source('_scripts/01_functions/km_knn_raster.r')

# landsat files list
rasterFiles <- list.files(path='_data/GIS/Raster/Raw/Landsat/Raw',
                         pattern=".tif$", full.names = TRUE)

# set crs
crs <- 'EPSG:32614'

# read in rasters
r <- c()
# add raster files to a list
for (raster in rasterFiles){
    s <- rast(raster)
    s <- terra::project(s,crs)
    r <- c(r,s)
    rm(s)
}

# apply crop and mask over list elements
for (i in 1:length(r)) {
    r <- lapply(r, crop, y = r[[i]], ext = TRUE)
    r <- lapply(r, mask, mask = r[[i]])
}

r <- rast(r)

# trim an NA values
r <- trim(r)

r <- scale(r)

# acquire the sampling dates
dates <- sub(".*_(\\d{8}).*", "\\1", rasterFiles)

# It is important to set the seed generator because `kmeans`
# initiates the centers in random locations
set.seed(99)

k_raster <- km_knn_raster(
  r = r,
  n_bands = 7,
  model_date = dates
)

# save the raster stack as a dataset
writeRaster(k_raster, '_data/GIS/Raster/Analysis/Kmeans/km_landsat_22.tif',
            overwrite=TRUE)
rm(list=ls())