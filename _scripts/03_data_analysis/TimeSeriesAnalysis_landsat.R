source('_scripts/00_libraries.R')
source('_scripts/01_functions/normalise.R')
source('_scripts/01_functions/time_series.r')

# river mask
river <- vect('_data/GIS/Vector/Clean/river_corridor.shp')

# read in raster stacks
kmRaster <- rast('_data/GIS/Raster/Analysis/Kmeans/km_landsat_22.tif')
rfRaster <- rast('_data/GIS/Raster/Analysis/RandomForest/rf_landsat.tif')

# mask the rasters
river <- terra::project(river, crs(rfRaster))
rfRaster <- mask(rfRaster, river)
kmRaster <- mask(kmRaster, river)

# check if the names are equal
names(rfRaster) == names(kmRaster)

# 1 = mixed (urban/bareland) 2 = water 3 = forest 4 = cropland 
# 5 = mixed (urban/bareland) 6 = urban/bareland 
rcl <- cbind(c(1,2,3,4,5,6),c(1,2,3,4,1,1))
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
    reframe(total_area = sum(area),
            avg_area = mean(area)),
  mlDF %>% 
    filter(method == 'randomforest') %>% 
    group_by(class) %>% 
    reframe(total_area = sum(area),
            avg_area = mean(area)),
  by = 'class')
names(comparisonDF) <- c('class','kmeans_area_tot','kmeans_area_avg',
                         'randomforest_area_tot','randomforest_area_avg')
comparisonDF <- comparisonDF[c('class','kmeans_area_tot','randomforest_area_tot',
                               'kmeans_area_avg','randomforest_area_avg')]

# save dataset
write.csv(mlDF, '_data/Tables/resultsTimeSeries_landsat.csv',
          row.names = FALSE)
write.csv(comparisonDF, '_data/Tables/comparisonArea_landsat.csv',
          row.names = FALSE)

rm(list=ls())
