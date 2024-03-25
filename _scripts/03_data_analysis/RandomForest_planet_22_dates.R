source('_scripts/00_libraries.R')
source('_scripts/01_functions/random_forest_raster.R')

# read raster
r <- rast('_data/GIS/Raster/Clean/planet_22dates.tif')

# trim an NA values
r <- trim(r)

# resample according to your needs/capacity
s <- rast(ext=ext(r),crs=crs(r),res=10)
r <- resample(r,s)

# scale the values
r <- scale(r)

# acquire the sampling dates
dates <- sub("^(\\d{8})_.*$", "\\1", names(r))
dates <- unique(dates)

# import user polygons with observations
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

# set the seed
set.seed(99)

# #hyperparameter tuning
# df_clean <- na.omit(df_original[-na_rows,])

# # Partition the data for training and testing
# # 80% of the data will be used for the training
# data_split <- initial_split(df_clean, prop = 0.80, strata=class)  # Adjust the proportion as needed
# train_data <- training(data_split)
# test_data <- testing(data_split)

# rf_model_spec <- rand_forest(
#   mtry = tune(),
#   trees = 1000,
#   min_n = tune()
#   ) %>%
#     set_engine("ranger") %>%
#     set_mode("classification") 

# rf_recipe <- recipe(class ~ ., data = train_data) #%>%
#     # step_normalize(all_predictors())  # Example step to normalize predictors

# rf_workflow <- workflow() %>%
#   add_model(rf_model_spec) %>%
#   add_recipe(rf_recipe)

# df_folds <- vfold_cv(train_data)

# doParallel::registerDoParallel()
# set.seed(123)
# rf_grid <- tune_grid(
#   rf_workflow,
#   resamples = df_folds,
#   grid = 20
# )
### this led to mtry = 78 and min_n = 6 for a res of 100

# run the rf model on all the data
rf_output <- random_forest_raster(
  r = r, 
  df = df_original, 
  na_rows = na_rows, 
  model_date = 'baseline'
  )

# run a new rf model for each sampling date
for (i in 1:22){
  # subset columns with the class column being first
  # adjust according to # of bands - 4 in this case
  subset_cols <- c(1,1+(1:4)+4*(i-1))

  # run model
  subset_rf_output <- random_forest_raster(
    r = r, 
    df = df_original[subset_cols], 
    na_rows = na_rows, 
    model_date = dates[i]
    )

  # save to raster stack
  rf_output$raster[[dates[i]]] <- subset_rf_output$raster

  # append results to results table
  rf_output$results <- rbind(rf_output$results,subset_rf_output$results)
}

# save data
writeRaster(rf_output$raster,'_data/GIS/Raster/Analysis/RandomForest/rf_planet.tif',
            overwrite=TRUE)
write.csv(rf_output$results,'_data/Tables/rf_results_landsat.csv')

# remove junk
rm(list = ls())
