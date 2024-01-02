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

userPolygons <- vect(vectorFiles[1])
userPolygons$id <- 1

for (i in 2:length(vectorFiles)){
  userPolygons.addition <- vect(vectorFiles[i])
  userPolygons.addition$id <- i
  userPolygons <- rbind(userPolygons,userPolygons.addition)
}

userPolygons <- terra::project(userPolygons,river)
userPolygons$date <- as.Date(userPolygons$date)

r <- rast(rasterFiles[1])
r <- raster_prep(r, river, crs)

sampleDates <- unique(userPolygons$date)
userPolygons.r <- userPolygons[userPolygons$date == sampleDates[1],]
userPolygons.r <- rasterize(userPolygons.r, r, field = "class")

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







