source('_scripts/00_libraries.R')

raster_files <- list.files(
    path = '_data/GIS/Raster/Raw/MesohabitatReclass',
    pattern = '.tif$',
    full.names = TRUE
    )

r <- c()
for (raster in raster_files){
    s <- rast(raster)
    r <- c(r,s)
    rm(s)
}
r <- rast(r)

# load in the area of interest
aoi <- vect('_data/GIS/Vector/Confluence_AOI.shp')
aoi <- terra::project(aoi,crs(r))

r <- crop(r,aoi,ext=TRUE)

# reorganize by date
r_names <- c("Dec22_2021","Feb12_2022","Apr17_2022",
    "May31_2022","Jun20_2022","July18_2022","Sep28_2022")
r <- r[[r_names]]
names(r) <- paste0(rep(0,7),1:7,'_',names(r))

# 10 and 15 are undefined
oldClass <- c(1,8,6,4,2,3,9,12,10,15)
newClass <- c(3,rep(6,3),2,1,4,5,7,8)
labs <- c(
    'Deep Pool',rep('Raceway',3),'Med Pool',
    'Shallow Pool','Slow Riffle','Fast Riffle',
    'Faster than a Raceway','Faster than a Deep Pool')
rcl <- data.frame(label=labs,old=oldClass,new=newClass)
rcl <- rcl[order(rcl$new),]

r <- classify(r, rcl[,c('old','new')])

r$median <- median(r,na.rm=TRUE)
r$median_low <- round(r$median - 0.1)
r$median_high <- round(r$median + 0.1)

rcl_levels <- rcl[!duplicated(rcl$label),c('new','label')]
rcl_levels_rep <- rep(list(rcl_levels),nlyr(r))
r_names <- names(r)
r <- categories(r,layer=0,rcl_levels_rep) 

names(r) <- r_names

writeRaster(r,'_data/GIS/Raster/Clean/mesohabitats.tif', 
    overwrite=TRUE)
write.csv(rcl_levels,'_data/Tables/mesohabitat_reclass.csv', row.names=FALSE)

rm(list=ls())
