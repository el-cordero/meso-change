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

userData <- vect(paste0(vector,'userDelineated.shp'))
userData <- terra::project(userData, crs)

# read in the training raster
r <- rast(rasterFiles[1])
r <- raster_prep(r, river, crs, doMask = FALSE)

for (i in 2:length(rasterFiles)){
  # read in and prepare raster
  r2 <- rast(rasterFiles[i])
  r2 <- raster_prep(r2, river, crs, doMask = FALSE)
  r <- c(r,r2)
  rm(r2)
}

userData$class <- as.factor(userData$class)
r[['class']]  <- rasterize(userData, r, field = "class")

df <- as.data.frame(r, cell=FALSE, na.rm=TRUE)
df$class <- as.factor(df$class)
df <- df[,c(ncol(df),1:(ncol(df)-1))]

set.seed(9)
inTraining <- createDataPartition(df$class, p=0.80,list=FALSE)
training <- df[ inTraining,]
testing <- df[-inTraining,]

fitControl<- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)

cl <- makePSOCKcluster(5)
registerDoParallel(cl)

timeStart <- proc.time()

rf_model <- train(class~.,data=training, method="rf",
                  trControl=fitControl,
                  prox=TRUE,
                  fitBest = FALSE,
                  returnData = TRUE)

stopCluster(cl)
proc.time() - timeStart

pathRF <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/ML/Models/Random Forest/"
saveRDS(rf_model, file = paste0(pathRF,"rf_model_all.rds"))

pred_rf <- predict(rf_model$finalModel, newdata = testing)
names(r)[1:154] <- names(df)[2:155]
r <- r[[-155]]
rasterRF <- predict(r,rf_model,na.rm=TRUE)

for (i in 3:length(rasterFiles)){
# for (i in 1:length(rasterFiles)){
  subsetDate.df <- c(1,(2:8)+7*(i-1))
  cl <- makePSOCKcluster(5)
  registerDoParallel(cl)
  
  timeStart <- proc.time()
  rf_model_single <- train(class~.,data=training[,subsetDate.df], 
                           method="rf",
                            trControl=fitControl,
                            prox=TRUE,
                            fitBest = FALSE,
                            returnData = TRUE)
  stopCluster(cl)
  print(proc.time() - timeStart)
  saveRDS(rf_model_single, file = paste0(pathRF,"rf_model_",i,".rds"))
  pred_rf <- predict(rf_model_single$finalModel, 
                     newdata = testing[,subsetDate.df])
  subsetDate.r <- (1:7)+7*(i-1)
  rasterRF[[paste0("date_",i)]] <- predict(r[[subsetDate.r]],
                                           rf_model_single,na.rm=TRUE)
}


names(rasterRF[[1]]) <- 'date_all'

for (i in 1:23){
  plot(rasterRF[[i]])
}

writeRaster(rasterRF,paste0(raster,'Analysis/RandomForest/combined/rfStack.tif'),
            overwrite=TRUE)
# model_list[12+24*21]



""             "method"       "modelInfo"    "modelType"   
[5] "results"      "pred"         "bestTune"     "call"        
[9] "dots"         "metric"       "control"      "finalModel"  
[13] "preProcess"   "trainingData" "ptype"        "resample"    
[17] "resampledCM"  "perfNames"    "maximize"     "yLimits"     
[21] "times"        "levels"       "terms"        "coefnames"   
[25] "xlevels"      



















LC_ 84_Multdate <- gplot(LC_rf_84_Multdate) + geom_ra
ster(aes(fill = factor(value, labels=c("Agriculture", "
Bareland", "Green Spaces", "Urban", "Water")))) + scale
_fill_manual(values = c("yellow", "grey", "green3", "re
d", "blue3"), name= "Land Cover") + ggtitle("Random For
est Classification") +theme(plot.title = element_text(l
                                                      ineheight=.4, face="bold")) + coord_equal()







for (i in 2:length(sampleDates)){
  userPolygons.date <- userPolygons[userPolygons$date == sampleDates[i],]
  userPolygons.date <- rasterize(userPolygons.date, r, field = "class")
  userPolygons.r <- c(userPolygons.r,userPolygons.date)
}

names(userPolygons.r) <- sampleDates
userPolygons.r <- mask(userPolygons.r, river)

s <- rast(nrows=nrow(r),ncols=ncol(r),crs=crs(r),
          extent=ext(r), resolution=res(r))
userPolygons.r <- resample(userPolygons.r, s)

userPoints <- as.points(userPolygons.r)
userPoints$`2021-11-21` <- as.integer(userPoints$`2021-11-21`)

imagePoints <- extract(r,userPoints[,'2021-11-21'], ID=FALSE)
imagePoints$class <- userPoints$`2021-11-21`


idxTrain <- createDataPartition(imagePoints$class, p=0.7,list=FALSE)
dtTrain <- imagePoints[idxTrain,]
dtTest <- imagePoints[-idxTrain,]

# create cross-validation folds (splits the data into n random groups)
n_folds <- 10
set.seed(321)
folds <- createFolds(1:nrow(dtTrain), k = n_folds)
# Set the seed at each resampling iteration. Useful when running CV in parallel.
seeds <- vector(mode = "list", length = n_folds + 1) # +1 for the final model
for(i in 1:n_folds) seeds[[i]] <- sample.int(1000, n_folds)
seeds[n_folds + 1] <- sample.int(1000, 1) # seed for the final model

ctrl <- trainControl(summaryFunction = multiClassSummary,
                     method = "cv",
                     number = n_folds,
                     search = "grid",
                     classProbs = TRUE, # not implemented for SVM; will just get a warning
                     savePredictions = TRUE,
                     index = folds,
                     seeds = seeds)

cl <- makeCluster(3/4 * detectCores())
doParallel::registerDoParallel(cl)

dtTrain$class <- make.names(dtTrain$class)
dtTrain$class <- as.factor(dtTrain$class)

model_rf <- caret::train(class ~ . , method = "rf", data = dtTrain,
                         importance = TRUE, # passed to randomForest()
                         # run CV process in parallel;
                         # see https://stackoverflow.com/a/44774591/5193830
                         allowParallel = TRUE,
                         tuneGrid = data.frame(mtry = c(2, 3, 4, 5, 8)),
                         trControl = ctrl)
stopCluster(cl); remove(cl)
# Unregister the doParallel cluster so that we can use sequential operations
# if needed; details at https://stackoverflow.com/a/25110203/5193830
registerDoSEQ()
saveRDS(model_rf, file = "/Users/EC13/Documents/Projects/USACE/ML Mesohabitats/Data/ML/Models/model_rf.rds")

model_rf$times$everything 
plot(model_rf)

dtTest$class <- make.names(dtTest$class)
dtTest$class <- as.factor(dtTest$class)
cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dtTest),
                         dtTest$class)
cm_rf

model_rf$finalModel
r1 <- terra::predict(r, model_rf,na.rm=TRUE)







