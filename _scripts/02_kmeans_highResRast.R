source("~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_libraries.R")
source('~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_funcRasterPrep.R')

# kmeans

## filepaths
raster <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Raster/"
vector <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Vector/"

highRes <- rast(paste0(raster, 'Clean/highResRast.tif'))

# convert the raster to a data.frame
# remote flowdir as this seems to reduce the quality of the analysis
nr <- as.data.frame(highRes[[-7]], cell=TRUE, na.rm=TRUE)

# It is important to set the seed generator because `kmeans`
# initiates the centers in random locations
set.seed(99)

# Create XX clusters, allow 500 iterations,
# start with 50 random sets using "Hartigan-Wong" method. 
# Do not use the first column (cell number).
kmncluster05 <- kmeans(nr[,-1], centers=5, iter.max = 500, 
                     nstart = 50, algorithm="Hartigan-Wong")
kmncluster06 <- kmeans(nr[,-1], centers=6, iter.max = 500, 
                     nstart = 50, algorithm="Hartigan-Wong")
kmncluster07 <- kmeans(nr[,-1], centers=7, iter.max = 500, 
                     nstart = 50, algorithm="Hartigan-Wong")
kmncluster08 <- kmeans(nr[,-1], centers=8, iter.max = 500, 
                     nstart = 50, algorithm="Hartigan-Wong")
kmncluster09 <- kmeans(nr[,-1], centers=9, iter.max = 500, 
                     nstart = 50, algorithm="Hartigan-Wong")
kmncluster10 <- kmeans(nr[,-1], centers=10, iter.max = 500, 
                     nstart = 50, algorithm="Hartigan-Wong")
kmncluster11 <- kmeans(nr[,-1], centers=11, iter.max = 500, 
                     nstart = 50, algorithm="Hartigan-Wong")

kmr05 <- rast(highRes, nlyr=1); kmr05[nr$cell] <- kmncluster05$cluster
kmr06 <- rast(highRes, nlyr=1); kmr06[nr$cell] <- kmncluster06$cluster
kmr07 <- rast(highRes, nlyr=1); kmr07[nr$cell] <- kmncluster07$cluster
kmr08 <- rast(highRes, nlyr=1); kmr08[nr$cell] <- kmncluster08$cluster
kmr09 <- rast(highRes, nlyr=1); kmr09[nr$cell] <- kmncluster09$cluster
kmr10 <- rast(highRes, nlyr=1); kmr10[nr$cell] <- kmncluster10$cluster
kmr11 <- rast(highRes, nlyr=1); kmr11[nr$cell] <- kmncluster11$cluster

kmr <- c(kmr05,kmr06,kmr07,kmr08,kmr09,kmr10,kmr11)
names(kmr) <- c(paste0('kmr_0',5:9), paste0('kmr_',10:11))

writeRaster(kmr, paste0(raster,'Analysis/Kmeans/combined/kmeansHighResStack.tif'),
            overwrite=TRUE)


