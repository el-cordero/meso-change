source("~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_libraries.R")
source('~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_funcRasterPrep.R')

# kmeans

## filepaths
raster <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Raster/"
vector <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Vector/"

# landsat files list
file.names <- list.files(path=paste0(raster,'Landsat/Raw/'),
                         pattern=".tif$")

crs <- "EPSG:32614"

# river polygon
river <- vect(paste0(vector,'Clean/river_corridor.shp'))
river <- terra::project(river, crs)

# read in the training raster
# in this case, the raster is the first one within the list
# this raster has the lowest cloud cover (<1%)
r <- rast(paste0(raster,'Landsat/Raw/',file.names[1]))
r <- raster_prep(r, river, crs)

for (i in 2:length(file.names)){
  # read in and prepare raster
  r2 <- rast(paste0(raster,'Landsat/Raw/',file.names[i]))
  r2 <- raster_prep(r2, river, crs)
  r <- c(r,r2)
}

# convert the raster to a data.frame
nr <- as.data.frame(r, cell=TRUE, na.rm=TRUE)


# It is important to set the seed generator because `kmeans`
# initiates the centers in random locations
set.seed(99)

# Create 5 clusters, allow 500 iterations, 
# start with 50 random sets using "Hartigan-Wong" method. 
# Do not use the first column (cell number).
kmncluster <- kmeans(nr[,-1], centers=5, iter.max = 500, 
                     nstart = 50, algorithm="Hartigan-Wong")


kmr <- rast(r, nlyr=1)
kmr[nr$cell] <- kmncluster$cluster

# Use the training raster object to set the cluster values to a new raster
# with the same amount of layers as there are rasters
knr <- rast(r,nlyr=length(file.names))

# remove training raster
rm(r); rm(nr)

# Apply the same kmeans model to the other rasters
# starting at the second raster
for (i in 1:length(file.names)){
  # read in and prepare raster
  r2 <- rast(paste0(raster,'Landsat/Raw/',file.names[i]))
  r2 <- raster_prep(r2, river, crs)
  
  # convert the raster into a dataframe
  nr2 <- as.data.frame(r2, cell=TRUE, na.rm=TRUE)
  
  # apply the model to the test raster data
  pred.knn <- FNN::get.knnx(kmncluster$center[,(1:7)+(7*(i-1))], 
                            nr2[,-1], 1)$nn.index[,1]
  
  # set cluster values to the raster layer
  layerName <- paste0("lyr",i)
  knr[[layerName]][nr2$cell] <- pred.knn

  # remove testing rasters
  rm(r2); rm(nr2)
}

# set the layer names for the raster stack
names(knr) <- sub(".tif.*", "", file.names)
names(kmr) <- 'date_all'

# save kmeans raster
kmeansRaster <- c(kmr,knr)
rm(kmr);rm(knr)

# save the raster stack as a dataset
write.csv(kmeansRaster, paste0(raster,'Analysis/Kmeans/combined/kmeansStack05.csv'))
writeRaster(kmeansRaster, paste0(raster,'Analysis/Kmeans/combined/kmeansStack05.tif'),
            overwrite=TRUE)


# # set the colors for the rasters
# mycolors <- c('#cfd186', '#CD533B','#275DAD', '#9EB3C2',  '#C2EABD')
# coltb <- data.frame(value=1:5, mycolors)

# # save the rasters individually
# r.list <- c() # create an empty list
# for (i in 1:length(file.names)){
#   r.list <- c(r.list,knr[[i]])
#   coltab(r.list[[i]]) <- coltb
#   writeRaster(r.list[[i]], paste0(raster,'Analysis/Kmeans/05stack/',file.names[i]),
#               overwrite=TRUE)
# }