source("~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_libraries.R")
source('~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_funcRasterPrep.R')

## filepaths
raster <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Raster/"
vector <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Vector/"

# landsat files list
rasterFiles <- list.files(path=paste0(raster,'Landsat/Raw/'),
                         pattern=".tif$", full.names = TRUE)
# landsat files list
vectorFiles <- list.files(path=paste0(vector,'userDelineated/'),
                         pattern=".shp$", full.names = TRUE)

# crs
crs <- "EPSG:32614"

# river polygon
river <- vect(paste0(vector,'Clean/river_corridor.shp'))
river <- terra::project(river, crs)

# inputed user polygons with observations
userData <- vect(paste0(vector,'userDelineated.shp'))
userData <- terra::project(userData, crs)

# read in the raster stack
r <- rast(rasterFiles[1])
r <- raster_prep(r, river, crs, doMask = FALSE)

# add raster files to raster stack
# this creates a combined raster with all bands for all dates
for (i in 2:length(rasterFiles)){
  # read in and prepare raster
  r2 <- rast(rasterFiles[i])
  r2 <- raster_prep(r2, river, crs, doMask = FALSE)
  r <- c(r,r2)
  rm(r2)
}

# change userData classes to factor to retain class values
userData$class <- as.factor(userData$class)

# add as a raster layer to raster
r[['class']]  <- rasterize(userData, r, field = "class")

# create dataframe from raster stack
df <- as.data.frame(r, cell=FALSE, na.rm=TRUE)

# transfer the names from df to raster stack
names(r) <- names(df)

# change classes to factor to retain class values
df$class <- as.factor(df$class)

# move class column to the 1st column
df <- df[,c(ncol(df),1:(ncol(df)-1))]

# set the seed
set.seed(9)

# Partition the data for training and testing
# 80% of the data will be used for the training
inTraining <- createDataPartition(df$class, p=0.80,list=FALSE)
training <- df[inTraining,]
testing <- df[-inTraining,]

# set training control parameters
fitControl <- trainControl(method="repeatedcv", number=5, repeats=5)

# set clusters
cl <- makePSOCKcluster(5)

# parallel programming
registerDoParallel(cl)

# train the random forest model for the complete dataframe
rfModel <- train(class~.,data=training, method="rf",
                  trControl=fitControl,
                  prox=TRUE,
                  fitBest = FALSE,
                  returnData = TRUE)

# stop the clusters
stopCluster(cl)

# save the model for future use
pathRF <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/ML/Models/Random Forest/"
saveRDS(rfModel, file = paste0(pathRF,"rf_model_all.rds"))

# create training raster
df <- as.data.frame(r[['class']], cell=TRUE, na.rm=TRUE)
df$training <- 0
df[inTraining,]$training <- 1
r[['training']] <- NA
r[['training']][df$cell] <- df[,c('training')]

# save the raster for future use
writeRaster(r[[c('class','training')]], paste0(raster,'Analysis/classes.tif'),
            overwrite=TRUE)

# remove class raster
r <- r[[-(155:156)]]

# run the model on the raster
rasterRF <- predict(r,rfModel,na.rm=TRUE)

# rename the raster layer
names(rasterRF[[1]]) <- 'date_all'

# train random forest model for each date and apply model to that date
for (i in 1:length(rasterFiles)){
  
  # subset class column AND the 7 columns pertaining to bands for a date
  subsetDate.df <- c(1,(2:8)+7*(i-1))
  
  # parallel processing
  registerDoParallel(cl)
  
  # train for date subset
  rfModel.dt <- train(class~.,data=training[,subsetDate.df], 
                           method="rf",
                            trControl=fitControl,
                            prox=TRUE,
                            fitBest = FALSE,
                            returnData = TRUE)
  
  # save the random forest model
  saveRDS(rfModel.dt, file = paste0(pathRF,"rf_model_",i,".rds"))
  
  # # run the model on testing data
  # pred_rf <- predict(rf_model_single$finalModel, 
  #                    newdata = testing[,subsetDate.df])
  
  # subset for date
  subsetDate.r <- (1:7)+7*(i-1)
  
  # run the model on the raster
  # add as a raster layer
  rfDate <- paste0("date_",i)
  rasterRF[[rfDate]] <- predict(r[[subsetDate.r]],rfModel.dt,
                                na.rm=TRUE)
  
  # stop the cluster
  stopCluster(cl)
}

# landsat files list
rasterNames <- list.files(path=paste0(raster,'Landsat/Raw/'),
                          pattern=".tif$", full.names = FALSE)
names(rasterRF) <- sub(".tif.*", "", rasterNames)

for (i in 1:length(names(rasterRF))){
  plot(rasterRF[[i]])
}




writeRaster(rasterRF,paste0(raster,'Analysis/RandomForest/combined/rfStack.tif'),
            overwrite=TRUE)
