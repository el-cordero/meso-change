source('_scripts/00_libraries.R')
source('_scripts/01_functions/normalise.R')

# river mask
river <- vect('_data/GIS/Vector/Clean/river_corridor.shp')

# read in raster stacks
kmRaster <- rast('_data/GIS/Raster/Analysis/Kmeans/km_planet_22.tif')
rfRaster <- rast('_data/GIS/Raster/Analysis/RandomForest/rf_planet.tif')

# mask the rasters
river <- terra::project(river, crs(rfRaster))
rfRaster <- mask(rfRaster, river)
kmRaster <- mask(kmRaster, river)

levels(rfRaster[[1]])

# check if the names are equal
names(rfRaster) == names(kmRaster)

# 1 = water, 2 = mixed (urban/bareland), 3 = urban 
# 4 = cropland, 5 = forest, 6 = mixed (urban/bareland)
rcl <- cbind(c(1,2,3,4,5,6),c(2,1,1,4,3,1))
kmRaster <- classify(kmRaster,rcl)
kmRaster <- as.factor(kmRaster)
kmValues <- list(data.frame(1:4,c('bareland','water','forest','cropland')))
kmRaster <- categories(kmRaster,layer=0, value=rep(kmValues,nlyr(kmRaster)))
names(kmRaster) <- names(rfRaster)


kmDF <- time_series(kmRaster)
rfDF <- time_series(rfRaster)

kmDF$method <- 'kmeans'; rfDF$method <- 'randomforest'

mlDF <- rbind(kmDF,rfDF)

comparisonDF <- full_join(
  mlDF %>% 
    filter(method == 'kmeans') %>% 
    group_by(class) %>% 
    reframe(total_area = sum(area)),
  mlDF %>% 
    filter(method == 'randomforest') %>% 
    group_by(class) %>% 
    reframe(total_area = sum(area)),
  by = 'class')
names(comparisonDF) <- c('class','kmeans_area','randomforest_area')

# save dataset
write.csv(mlDF, '_data/Tables/resultsTimeSeries_planet.csv',
          row.names = FALSE)
write.csv(comparisonDF, '_data/Tables/comparisonArea_planet.csv',
          row.names = FALSE)

rm(list=ls())
