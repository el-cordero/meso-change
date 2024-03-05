source("~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_libraries.R")
source("~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_funcNormalise.R")

## filepaths
raster <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Raster/"
vector <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Vector/"

# read in rasters

r1 <- rast(paste0(raster,'MesohabitatReclass_011624/June20_2022/jun22'))
r2 <- rast(paste0(raster,'MesohabitatReclass_011624/July22_2022/jul22'))
r3 <- rast(paste0(raster,'MesohabitatReclass_011624/November11_2022/nov22'))
r4 <- rast(paste0(raster,'MesohabitatReclass_011624/March03_2023/mar23'))
r5 <- rast(paste0(raster,'MesohabitatReclass_011624/November14_2023/nov23'))

r <- c(r1,r2,r3,r4,r5)

# load in the area of interest
aoi <- vect(paste0(vector, 'Confluence_AOI.shp'))
aoi <- terra::project(aoi,crs(r))

r <- crop(r,aoi,ext=TRUE)

oldClass <- c(1,8,6,4,2,3,9,12)
newClass <- c(3,rep(6,3),2,1,4,5)
labs <- c('Deep Pool',rep('Raceway',3),'Med Pool',
              'Shallow Pool','Slow Riffle','Fast Riffle')
rcl <- data.frame(label=labs,old=oldClass,new=newClass)
rcl <- rcl[order(rcl$new),]

r <- classify(r, rcl[,c('old','new')])
rMedian <- median(r,na.rm=TRUE)
rMean <- mean(r,na.rm=TRUE)
r$median <- rMedian
r$mean <- rMean

names(r) <- c('June 20, 2022','July 22, 2022','Nov 11, 2022','March 03, 2023','November 14, 2023',
              'median','mean')

writeRaster(r,paste0(raster,'Clean/mesohabitats.tif'), overwrite=TRUE)

