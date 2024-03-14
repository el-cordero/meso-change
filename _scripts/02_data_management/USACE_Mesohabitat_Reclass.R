source('_scripts/00_libraries.R')
source('_scripts/01_functions/normalise.R')

# read in rasters
r1 <- rast('_data/Data/GIS/Raster/Raw/MesohabitatReclass/June20_2022/jun22')
r2 <- rast('_data/Data/GIS/Raster/Raw/MesohabitatReclass/July22_2022/jul22')
r3 <- rast('_data/Data/GIS/Raster/Raw/MesohabitatReclass/November11_2022/nov22')
r4 <- rast('_data/Data/GIS/Raster/Raw/MesohabitatReclass/March03_2023/mar23')
r5 <- rast('_data/Data/GIS/Raster/Raw/MesohabitatReclass/November14_2023/nov23')


r <- c(r1,r2,r3,r4,r5)
rm(r1,r2,r3,r4,r5)

raster_files <- list.files(
    path = '_data/GIS/Raster/Raw/MesohabitatReclass',
    pattern = '.tif$',
    full.names = TRUE
    )

for (raster in raster_files){
    s <- rast(raster)
    s <- terra::project(s,r)
    r <- c(r,s)
    rm(s)
}

# load in the area of interest
aoi <- vect('_data/GIS/Vector/Confluence_AOI.shp')
aoi <- terra::project(aoi,crs(r))

r <- crop(r,aoi,ext=TRUE)
rm(aoi)

# 10 and 15 are undefined
oldClass <- c(1,8,6,4,2,3,9,12,10,15)
newClass <- c(3,rep(6,3),2,1,4,5,rep(7,2))
labs <- c(
    'Deep Pool',rep('Raceway',3),'Med Pool',
    'Shallow Pool','Slow Riffle','Fast Riffle',
    rep('Undefined',2))
rcl <- data.frame(label=labs,old=oldClass,new=newClass)
rcl <- rcl[order(rcl$new),]

r <- classify(r, rcl[,c('old','new')])

r$median <- median(r,na.rm=TRUE)
r$mean <- mean(r,na.rm=TRUE)

rcl_levels <- rcl[!duplicated(rcl$label),c('new','label')]
rcl_levels <- rep(list(rcl_levels),nlyr(r))

raster_names <- names(r)

levels(r) <- rcl_levels

names(r) <- c(
    'Jun 20, 2022','Jul 22, 2022','Nov 11, 2022',
    'Mar 03, 2023','Nov 14, 2023',"Apr 17, 2022", "Dec 22, 2021", 
    "Feb 12, 2022", "July 18, 2022", "Jun 20, 2022", "May 31, 2022", 
    "Sep 28, 2022", 
    'median','mean'
    )

writeRaster(r,'_data/GIS/Raster/Clean/mesohabitats.tif', 
    overwrite=TRUE)

