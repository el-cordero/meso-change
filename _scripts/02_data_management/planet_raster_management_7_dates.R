source('_scripts/00_libraries.R')

# load in the area of interest
aoi <- rast('_data/GIS/Raster/Clean/mesohabitats.tif',lyrs=1)


planet_files <- list.files(
    path = '_data/GIS/Raster/Raw/Planet/b',
    pattern = '_SR_clip.tif$',
    full.names = TRUE,
    recursive = TRUE
    )


# review list to make sure there are not duplicates
# these files need to be merged into one raster
# only the first is necessary 
planet_extra <- c(3,5,8,10)
planet_files_red <- planet_files[-planet_extra]

# reorganize according to date
planet_files_red <- planet_files_red[c(1,7,2:6)] 

# create a list of rasters that have been reprojected, resampled
# and cropped to the aoi
planet <- c()
for (raster in planet_files_red){
    s <- rast(raster)
    s <- terra::project(s,crs(aoi))
    s <- resample(s,aoi)
    s <- crop(s,aoi)
    planet <- c(planet,s)
    rm(s)
}

# merge the raster objects into one
planet <- rast(planet)

hecras_names <- list.files(path = '_data/GIS/Raster/Raw/HECRAS')
planet_names <- sort(rep(hecras_names,4))
planet_names <- paste0(planet_names,'_',names(planet))
names(planet) <- planet_names


writeRaster(planet,'_data/GIS/Raster/Clean/planet_7dates.tif', overwrite=TRUE)
tmpFiles(remove=TRUE)
rm(list=ls())
