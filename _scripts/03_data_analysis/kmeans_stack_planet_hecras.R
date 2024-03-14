source('_scripts/00_libraries.R')


# kmeans

# read in the training raster
r <- rast('_data/GIS/Raster/Clean/planet_hecras.tif')
r <- trim(r)

s <- rast(ext=ext(r),crs=crs(r),res=10)
r <- resample(r,s)

for (i in 1:nlyr(r)) r <- mask(r,r[[i]])

r <- scale(r)

# convert the raster to a data.frame
nr <- as.data.frame(r, cell=TRUE, na.rm=TRUE)

# It is important to set the seed generator because `kmeans`
# initiates the centers in random locations
set.seed(99)

# Create 15 clusters, allow 500 iterations, 
# 15 clusters based off the elbow method
# start with 50 random sets using "Hartigan-Wong" method. 
# Do not use the first column (cell number).
kmncluster <- kmeans(nr[,-1], centers=15, iter.max = 500, 
                     nstart = 50, algorithm="Hartigan-Wong")


kmr <- rast(r, nlyr=1)
kmr[nr$cell] <- kmncluster$cluster

# Use the training raster object to set the cluster values to a new raster
# with the same amount of layers as there are rasters
knr <- rast(r,nlyr=nlyr(r)/6)

# remove training raster

# Apply the same kmeans model to the other rasters
# starting at the second raster
for (i in 1:nlyr(knr)){
  # convert the raster into a dataframe
  nr2 <- nr[,c(1,(1:6)+(6*(i-1))+1)]
  kmnc2 <- kmncluster$center[,(1:6)+(6*(i-1))]
  
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
knr
# set the layer names for the raster stack
names(knr) <- c('01_Dec22_2021','02_Feb12_2022','03_Apr17_2022',
  '04_May31_2022','05_Jun20_2022','06_Jul18_2022','07_Sep28_2022')
names(kmr) <- 'baseline'

# save kmeans raster
kmeansRaster <- c(kmr,knr)
rm(kmr);rm(knr)

# save the raster stack as a dataset
writeRaster(kmeansRaster, '_data/GIS/Raster/Analysis/Kmeans/km_high_res.tif',
            overwrite=TRUE)


