source('_scripts/00_libraries.R')

# load in the area of interest
aoi <- rast('_data/GIS/Raster/Clean/mesohabitats.tif',lyrs=1)


planet_files <- list.files(
    path = '_data/GIS/Raster/Raw/Planet',
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
# and masked to the aoi
planet <- c()
for (raster in planet_files_red){
    s <- rast(raster)
    s <- terra::project(s,crs(aoi))
    s <- resample(s,aoi)
    s <- mask(s,aoi)
    planet <- c(planet,s)
    rm(s)
}

# merge the raster objects into one
planet <- rast(planet)

# same process for the hecras files...but slightly more complicated
hecras_files <- list.files(
    path = '_data/GIS/Raster/Raw/HECRAS',
    pattern = '.tif$',
    full.names = TRUE,
    recursive = TRUE
    )

# the hecras files do not have a projection
# based off talks with Aubrey, the projection is 
# ESRI:103284 NAD 1983 (CORS96) SPCS Kansas North (US Feet)
# provided via proj4 format
hecras_crs <- '+proj=lcc +lat_0=38.3333333333333 +lon_0=-98 +lat_1=38.7166666666667 +lat_2=39.7833333333333 +x_0=400000 +y_0=0 +ellps=GRS80 +units=us-ft +no_defs +type=crs'

depth <- c()
val <- 1
while (val < 15){
    s <- rast(hecras_files[val])
    crs(s) <- hecras_crs
    s <- terra::project(s,crs(aoi))
    s <- resample(s,aoi)
    s <- mask(s,aoi)
    depth <- c(depth,s)
    rm(s)
    val <- val + 2
}
depth <- rast(depth)

velocity <- c()
val <- 2
while (val < 15){
    s <- rast(hecras_files[val])
    crs(s) <- hecras_crs
    s <- terra::project(s,crs(aoi))
    s <- resample(s,aoi)
    s <- mask(s,aoi)
    velocity <- c(velocity,s)
    rm(s)
    val <- val + 2
}
velocity <- rast(velocity)

hecras_names <- list.files(path = '_data/GIS/Raster/Raw/HECRAS')
names(depth) <- paste0(hecras_names,'_x_depth')
names(velocity) <- paste0(hecras_names,'_x_velocity')

# the planet rasters have 4 bands (rgb and nir)
# rename according to date

planet_names <- sort(rep(hecras_names,4))
planet_names <- paste0(planet_names,'_',names(planet))
names(planet) <- planet_names

observations <- c(planet,depth,velocity)

observations <- observations[[sort(names(observations))]]

writeRaster(observations,'_data/GIS/Raster/Clean/planet_hecras.tif')
tmpFiles(remove=TRUE)
rm(depth,velocity,observations,aoi)