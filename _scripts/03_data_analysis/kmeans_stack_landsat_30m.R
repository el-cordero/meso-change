source('_scripts/00_libraries.R')
source('_scripts/01_functions/raster_prep.R')

# kmeans

# landsat files list
file.names <- list.files(path='_data/GIS/Raster/Raw/Landsat/Raw',
                         pattern=".tif$")

crs <- "EPSG:32614"

# load in the area of interest
aoi <- vect('_data/GIS/Vector/Clean/aoi.shp')
aoi <- terra::project(aoi,crs)
aoi <- rast(ext=ext(aoi),crs=crs(aoi),res=30)

r <- c()
for (filename in file.names){
  # read in and prepare raster
  s <- rast(paste0('_data/GIS/Raster/Raw/Landsat/Raw/',filename))
  s <- terra::project(s,aoi)
  s <- crop(s,aoi,ext=TRUE)
  r <- c(r,s)
  rm(s)
}

r <- rast(r)

r <- scale(r)

# convert the raster to a data.frame
nr <- as.data.frame(r, cell=TRUE, na.rm=TRUE)


# It is important to set the seed generator because `kmeans`
# initiates the centers in random locations
set.seed(99)

# Create 5 clusters, allow 500 iterations, 
# start with 50 random sets using "Hartigan-Wong" method. 
# Do not use the first column (cell number).
kmncluster <- kmeans(nr[,-1], centers=6, iter.max = 500, 
                     nstart = 50, algorithm="Hartigan-Wong")


kmr <- rast(r, nlyr=1)
kmr[nr$cell] <- kmncluster$cluster

# Use the training raster object to set the cluster values to a new raster
# with the same amount of layers as there are rasters
knr <- rast(r,nlyr=nlyr(r)/7)

# remove training raster
# rm(r); rm(nr)

# Apply the same kmeans model to the other rasters
# starting at the second raster
for (i in 1:nlyr(knr)){
  # convert the raster into a dataframe
  nr2 <- nr[,c(1,(1:7)+(7*(i-1))+1)]
  kmnc2 <- kmncluster$center[,(1:7)+(7*(i-1))]
  
  # apply the model to the test raster data
  pred.knn <- FNN::get.knnx(data=kmnc2, 
                            query=nr2[,-1], 
                            k=1)
  pred.knn <- pred.knn$nn.index[,1]
  
  # set cluster values to the raster layer
  layerName <- paste0("lyr",i)
  knr[[layerName]][nr2$cell] <- pred.knn

  # remove testing rasters
  rm(nr2)
}

# set the layer names for the raster stack
names(knr) <- sub(".tif.*", "", file.names)
names(kmr) <- 'baseline'

# save kmeans raster
kmeansRaster <- c(kmr,knr)
rm(kmr);rm(knr)

# save the raster stack as a dataset
writeRaster(kmeansRaster, '_data/GIS/Raster/Analysis/Kmeans/Kmeans_landsat_22.tif',
            overwrite=TRUE)
tmpFiles(remove=TRUE)


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