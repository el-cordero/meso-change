# =========================================================
# Random forest hyperparameter tuning script
# Updated / cleaned / faster version
#
# Main changes:
# - removes redundant na_rows logic
# - uses fewer trees during tuning for speed
# - uses 5-fold CV instead of default
# - registers parallel backend explicitly
# - keeps full baseline stack tuning logic
# - clarifies that tuned mtry applies to the baseline stacked model
#   and should not be reused blindly for 4-band per-date models
# =========================================================

source('_scripts/00_libraries.R')
source('_scripts/01_functions/random_forest_raster.R')

# ---------------------------------------------------------
# Read raster
# ---------------------------------------------------------
r <- rast('_data/GIS/Raster/Clean/planet_22dates.tif')

# trim outer NA values
r <- trim(r)

# resample according to your needs/capacity
# s <- rast(ext = ext(r), crs = crs(r), res = 10)
# r <- resample(r, s)

# scale the values
r <- scale(r)

# acquire the sampling dates
dates <- sub("^(\\d{8})_.*$", "\\1", names(r))
dates <- unique(dates)

# ---------------------------------------------------------
# Import user polygons with observations
# ---------------------------------------------------------
userData <- vect('_data/GIS/Vector/userDelineated.shp')
userData <- terra::project(userData, crs(r))

# retain class values as factor
userData$class <- as.factor(userData$class)

# rasterize user polygons to raster stack
r[['class']] <- rasterize(userData, r, field = "class")

# ---------------------------------------------------------
# Create dataframe from raster stack
# ---------------------------------------------------------
df_original <- as.data.frame(r, cell = FALSE, na.rm = FALSE)

# ensure class is factor
df_original$class <- as.factor(df_original$class)

# move class column to first position
df_original <- df_original[, c(ncol(df_original), 1:(ncol(df_original) - 1))]

# keep only complete rows for tuning
df_clean <- na.omit(df_original)

nrow(df_clean)
ncol(df_clean)
table(df_clean$class)

# ---------------------------------------------------------
# Set seed
# ---------------------------------------------------------
set.seed(99)

# ---------------------------------------------------------
# Split labeled data
# ---------------------------------------------------------
data_split <- initial_split(df_clean, prop = 0.80, strata = class)
train_data <- training(data_split)
test_data  <- testing(data_split)

train_data_tune <- train_data %>%
  group_by(class) %>%
  group_modify(~ {
    if (nrow(.x) <= 10000) {
      .x
    } else {
      .x[sample.int(nrow(.x), 10000), , drop = FALSE]
    }
  }) %>%
  ungroup()

# ---------------------------------------------------------
# Random forest specification for tuning
# NOTE:
# This tunes the BASELINE STACKED MODEL with all predictors.
# The resulting mtry can be large and should not be reused for
# single-date 4-band models without adjustment.
# ---------------------------------------------------------
rf_model_spec <- rand_forest(
  mtry  = tune(),
  trees = 300,      # reduced from 1000 for faster tuning
  min_n = tune()
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

rf_recipe <- recipe(class ~ ., data = train_data_tune)

rf_workflow <- workflow() %>%
  add_model(rf_model_spec) %>%
  add_recipe(rf_recipe)

# ---------------------------------------------------------
# Cross-validation folds
# ---------------------------------------------------------
df_folds <- vfold_cv(train_data_tune, v = 5, strata = class)

# ---------------------------------------------------------
# Parallel backend
# ---------------------------------------------------------
n_cores <- max(1, parallel::detectCores() - 1)
cl <- parallel::makePSOCKcluster(n_cores)
doParallel::registerDoParallel(cl)

# ---------------------------------------------------------
# Initial coarse tuning
# ---------------------------------------------------------
set.seed(123)

t1 <- Sys.time()
tune_res <- tune_grid(
  rf_workflow,
  resamples = df_folds,
  grid = 10,
  metrics = metric_set(roc_auc, accuracy)
)
Sys.time() - t1

tune_res_data <- tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(
    min_n:mtry,
    values_to = "value",
    names_to = "parameter"
  )

tune_res_plot <- tune_res_data %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC") +
  theme_bw()

print(tune_res_plot)

# ---------------------------------------------------------
# Refined regular grid
# Adjust ranges if your first plot suggests otherwise
# ---------------------------------------------------------
rf_grid <- grid_regular(
  mtry(range = c(1, min(25, ncol(train_data) - 1))),
  min_n(range = c(2, 20)),
  levels = 5
)

set.seed(321)

regular_res <- tune_grid(
  rf_workflow,
  resamples = df_folds,
  grid = rf_grid,
  metrics = metric_set(roc_auc, accuracy)
)

regular_res_data <- regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n))

regular_res_plot <- regular_res_data %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, linewidth = 1.2) +
  geom_point() +
  labs(y = "AUC", x = "mtry") +
  theme_bw()

print(regular_res_plot)

# ---------------------------------------------------------
# Select best model by AUC
# ---------------------------------------------------------
best_auc <- select_best(regular_res, metric = "roc_auc")

final_rf <- finalize_model(
  rf_model_spec,
  best_auc
)

print(final_rf)

# ---------------------------------------------------------
# Final workflow and final fit
# ---------------------------------------------------------
final_wf <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(data_split)

final_res_data <- final_res %>%
  collect_metrics()

print(final_res_data)

# ---------------------------------------------------------
# Stop parallel backend
# ---------------------------------------------------------
parallel::stopCluster(cl)
stopImplicitCluster()

# ---------------------------------------------------------
# Save outputs
# ---------------------------------------------------------
write.csv(
  tune_res_data,
  '_data/Tables/rf_tuning_results10m.csv',
  row.names = FALSE
)

write.csv(
  regular_res_data,
  '_data/Tables/rf_tuning_results10m_refined.csv',
  row.names = FALSE
)

write.csv(
  final_res_data,
  '_data/Tables/rf_tuning_results10m_final_res.csv',
  row.names = FALSE
)

# Optional: save plots
ggsave(
  filename = '_data/Tables/rf_tuning_results10m_plot.png',
  plot = tune_res_plot,
  width = 7,
  height = 4,
  dpi = 300
)

ggsave(
  filename = '_data/Tables/rf_tuning_results10m_refined_plot.png',
  plot = regular_res_plot,
  width = 7,
  height = 4,
  dpi = 300
)

# ---------------------------------------------------------
# Notes
# ---------------------------------------------------------
cat("\nBest tuned parameters for BASELINE stacked model:\n")
print(best_auc)

cat("\nReminder:\n")
cat("- This tuned mtry applies to the baseline stacked model with many predictors.\n")
cat("- Do not reuse this mtry directly for single-date models with only 4 predictors.\n")
cat("- For single-date 4-band models, use mtry <= 4.\n")

# ---------------------------------------------------------
# Clean up
# ---------------------------------------------------------
rm(list = ls())