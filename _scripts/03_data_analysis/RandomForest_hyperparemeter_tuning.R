source('_scripts/00_libraries.R')
source('_scripts/01_functions/random_forest_raster.R')

# read raster
r <- rast('_data/GIS/Raster/Clean/planet_22dates.tif')

# trim an NA values
r <- trim(r)

# resample according to your needs/capacity
s <- rast(ext=ext(r),crs=crs(r),res=50)
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
df_clean <- na.omit(df_original[-na_rows,])

# # Partition the data for training and testing
# # 80% of the data will be used for the training
data_split <- initial_split(df_clean, prop = 0.80, strata=class)  # Adjust the proportion as needed
train_data <- training(data_split)
test_data <- testing(data_split)

rf_model_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
  ) %>%
    set_engine("ranger") %>%
    set_mode("classification") 

rf_recipe <- recipe(class ~ ., data = train_data) 

rf_workflow <- workflow() %>%
  add_model(rf_model_spec) %>%
  add_recipe(rf_recipe)

df_folds <- vfold_cv(train_data)

doParallel::registerDoParallel()
set.seed(123)
tune_res <- tune_grid(
  rf_workflow,
  resamples = df_folds,
  grid = 20
)

tune_res_data <- tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) 

tune_res_data %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC") +
  theme_bw()


rf_grid <- grid_regular(
  mtry(range = c(1, 25)),
  min_n(range = c(2, 20)),
  levels = 5
)

set.seed(321)
regular_res <- tune_grid(
  rf_workflow,
  resamples = df_folds,
  grid = rf_grid
)

regular_res_data <- regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) 

regular_res_data %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC", x = 'mtry') +
  theme_bw()

best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  rf_model_spec,
  best_auc
)

final_rf

final_wf <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(data_split)

final_res_data <- final_res %>%
  collect_metrics()
### this led to mtry = 78 and min_n = 6 for a res of 100
### for a res of 50
# Random Forest Model Specification (classification)
# 
# Main Arguments:
#   mtry = 7
# trees = 1000
# min_n = 6
# 
# Computational engine: ranger 
stopImplicitCluster()


write.csv(tune_res_data,'_data/Tables/rf_tuning_results50m.csv', row.names = FALSE)
write.csv(regular_res_data,'_data/Tables/rf_tuning_results50m_refined.csv',row.names = FALSE)
write.csv(final_res_data,'_data/Tables/rf_tuning_results50m_final_res.csv', row.names = FALSE)


rm(list = ls())

