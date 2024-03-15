source('_scripts/00_libraries.R')
source('_scripts/01_functions/raster_prep.R')

planet <- rast('_data/GIS/Raster/Clean/planet_22dates.tif')
planet <- trim(planet)

s <- rast(ext=ext(planet),crs=crs(planet),res=10)
planet <- resample(planet,s)

# for (i in 1:nlyr(planet)) planet <- mask(planet,planet[[i]])
planet <- scale(planet)

dates <- sub("^(\\d{8})_.*$", "\\1", names(planet))
dates <- unique(dates)

# inputed user polygons with observations
userData <- vect('_data/GIS/Vector/userDelineated.shp')
userData <- terra::project(userData, crs(planet))

# change userData classes to factor to retain class values
userData$class <- as.factor(userData$class)

# add as a raster layer to raster
planet[['class']]  <- rasterize(userData, planet, field = "class")

# create dataframe from raster stack
df_original <- as.data.frame(planet, cell=FALSE,na.rm=FALSE)

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
data_split <- initial_split(df_clean, prop = 0.80, strata=class)  # Adjust the proportion as needed
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

rf_results <- predict(rf_workflow, new_data = test_data) %>%
  bind_cols(test_data) %>%  
  metrics(truth = class, estimate = .pred_class)
rf_results$model <- 'baseline'

predictions <- predict(rf_workflow, new_data = df_original[-na_rows,-1])  # Combine predictions with true outcomes for evaluation
df_original$predictions <- NULL
df_original[-na_rows,'predictions'] <- predictions$.pred_class

rf_raster <- init(planet[[1]],NA)
values(rf_raster) <- df_original$predictions
names(rf_raster) <- 'baseline'

# set the seed
set.seed(99)

for (i in 1:22){
  subset_cols <- c(1,1+(1:4)+4*(i-1))
  subset_df <- df_original[,subset_cols]
  # 'na_rows' now contains the indices of rows with any NA values
  subset_df_clean <- na.omit(subset_df[-na_rows,])

  # Partition the data for training and testing
  # 80% of the data will be used for the training
  subset_data_split <- initial_split(subset_df_clean, 
    prop = 0.80, ,strata=class)
  subset_train_data <- training(subset_data_split)
  subset_test_data <- testing(subset_data_split)

  subset_recipe <- recipe(class ~ ., data = subset_train_data) %>%
    step_normalize(all_predictors())  

  subset_workflow <- workflow() %>%
    add_model(rf_model_spec) %>%
    add_recipe(subset_recipe) %>%
    fit(data = subset_train_data)

  subset_pred <- predict(subset_workflow, new_data = subset_df[-na_rows,-1])  # Combine predictions with true outcomes for evaluation
  subset_df$predictions <- NULL
  subset_df[-na_rows,'predictions'] <- subset_pred$.pred_class

  rf_raster[[dates[i]]] <- init(rf_raster[[1]],NA)
  values(rf_raster[[dates[i]]]) <- subset_df$predictions

  subset_rf_results <- 
    predict(subset_workflow, new_data = subset_test_data) %>%
    bind_cols(subset_test_data) %>%  
    metrics(truth = class, estimate = .pred_class)

  subset_rf_results$model <- dates[i]
  rf_results <- rbind(rf_results,subset_rf_results)
}


# check the performance
acc <- rf_results %>% filter(.metric == 'accuracy', model != 'baseline')
kapa <- rf_results %>% filter(.metric == 'kap', model != 'baseline')
plot(acc$.estimate,type='l')
plot(kapa$.estimate,type='l')

writeRaster(rf_raster,paste0(raster,'Analysis/RandomForest/combined/rfStack.tif'),
            overwrite=TRUE)
