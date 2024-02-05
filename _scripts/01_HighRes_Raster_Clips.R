# load libraries 
library(terra)

# set path names
vector <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Vector/"
raster <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Raster/"

# load in river corridor polygon created previously
river <- vect(paste0(vector,'Clean/river_corridor.shp'))

# list the USGS orthoimagery and dem file names
rgbFiles <- list.files(path=paste0(raster,"USGS/Tiles"), pattern=".tif$")
demFiles <- list.files(path=paste0(raster,"USGS/DEM"), pattern=".tif$")

#### if the raster mosaic has not been completed, run the commented out code
# mosaic rasters
# rgbMosaic <- raster_mosaic(demFiles, paste0(raster, 'USGS/Tiles/'), river)
# demMosaic <-raster_mosaic(demFiles, paste0(raster, 'USGS/DEM/'), river)

# # save the raster object
# writeRaster(rgbMosaic,paste0(raster,'Clean/riverMosaic.tif'),overwrite=TRUE)
# writeRaster(demMosaic,paste0(raster,'Clean/dem.tif'),overwrite=TRUE)

# read in the rasters if already created
rgbMosaic <- rast(paste0(raster, 'Clean/riverMosaic.tif'))
demMosaic <- rast(paste0(raster, 'Clean/dem.tif'))

# read in mesohabitats raster
mesohabitats <- rast(paste0(raster, 'Clean/mesohabitats.tif'))

# calculate terrain statistics
slope <- terrain(demMosaic,'slope')
roughness <- terrain(demMosaic,'roughness')
flowDir <- terrain(demMosaic,'flowdir')

# merge dem and terrain rasters
dem.sprc <- c(demMosaic,slope,roughness,flowDir)

# reproject the dem raster because it has larger resolution
# mesohabitats dataset has the same crs as the rgbMosaic
dem.sprc <- terra::project(dem.sprc,crs(rgbMosaic))

# resample to the mesohabitats raster
dem.sprc <- resample(dem.sprc,mesohabitats)
rgbMosaic <- resample(rgbMosaic,mesohabitats)

# rename the layers
names(rgbMosaic) <- c('red','green','blue')
names(dem.sprc) <- c('dem','slope','roughness','flowdir')

# combine into one spatraster
highRes <- c(rgbMosaic,dem.sprc)

# reduce the dataset to mesohabitat mask that includes all mapped mesohabitats
mesohabitatsMask <- classify(mesohabitats,cbind(1:12,1))
mesohabitatsMask <- as.list(mesohabitatsMask)
mesohabitatsMask <- sprc(mesohabitatsMask)
mesohabitatsMask <- merge(mesohabitatsMask)
highRes <- mask(highRes, mesohabitatsMask)

# save the raster
writeRaster(highRes,paste0(raster, 'Clean/highResRast.tif'),overwrite=TRUE)





















