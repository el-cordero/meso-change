source('_scripts/00_libraries.R')
source('_scripts/01_functions/raster_prep.R')

r <- rast('_data/GIS/Raster/Clean/planet_22dates.tif')
r <- trim(r)

s <- rast(ext=ext(r),crs=crs(r),res=50)
r <- resample(r,s)

# for (i in 1:nlyr(r)) r <- mask(r,r[[i]])
r <- scale(r)

# inputed user polygons with observations
userData <- vect('_data/GIS/Vector/userDelineated.shp')
userData <- terra::project(userData, crs(r))

# change userData classes to factor to retain class values
userData$class <- as.factor(userData$class)

# add as a raster layer to raster
r[['class']]  <- rasterize(userData, r, field = "class")

# create dataframe from raster stack
df_original <- as.data.frame(r, cell=FALSE,na.rm=FALSE)

# change classes to factor to retain class values
df_original$class <- as.factor(df_original$class)

# move class column to the 1st column
df_original <- df_original[,c(ncol(df_original),1:(ncol(df_original)-1))]

# Assuming 'df' is your dataframe
na_rows <- which(apply(df_original[-1], 1, function(x) any(is.na(x))))

# 'na_rows' now contains the indices of rows with any NA values
df_clean <- na.omit(df_original[-na_rows,])

# set the seed
set.seed(99)

# Partition the data for training and testing
# 80% of the data will be used for the training
data_split <- initial_split(df_clean, prop = 0.80)  # Adjust the proportion as needed
train_data <- training(data_split)
test_data <- testing(data_split)

rf_model_spec <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("classification")  # Use "regression" for a regression problem

rf_recipe <- recipe(class ~ ., data = train_data) %>%
  step_normalize(all_predictors())  # Example step to normalize predictors

rf_workflow <- workflow() %>%
  add_model(rf_model_spec) %>%
  add_recipe(rf_recipe) %>%
  fit(data = train_data)

predictions <- predict(rf_workflow, new_data = df_original[-na_rows,-1])  # Combine predictions with true outcomes for evaluation
df_original$predictions <- NULL
df_original[-na_rows,'predictions'] <- predictions$.pred_class

rf_raster <- init(r[[1]],NA)
values(rf_raster) <- df_original$predictions
names(rf_raster) <- 'randomforest'

# For classification, you might calculate accuracy, ROC AUC, etc.
rf_results <- predictions %>%
  metrics(truth = class, estimate = .pred_class)  # Adjust based on your outcome variable and predictions

# For detailed class-specific metrics
rf_class_metrics <- predictions %>%
  roc_auc(class, .pred_class)  # Adjust based on your needs



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


# remove class raster
r <- r[[-(nlyr(r))]]

# run the model on the raster
rasterRF <- predict(r,rf_workflow,na.rm=TRUE)

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
