source('_scripts/00_libraries.R')
source('_scripts/01_functions/random_forest_raster.R')

# ---------------------------------------------------------
# Read Landsat core4 raster stack
# ---------------------------------------------------------
r <- rast('_data/GIS/Raster/Clean/landsat_22dates_core4.tif')

# trim outer NA values
r <- trim(r)

# scale the values
r <- scale(r)

# acquire the sampling dates from layer names
dates <- sub("^(\\d{8})_.*$", "\\1", names(r))
dates <- unique(dates)

print(dates)

# ---------------------------------------------------------
# Import user polygons with observations
# ---------------------------------------------------------
userData <- vect('_data/GIS/Vector/userDelineated.shp')
userData <- terra::project(userData, crs(r))

# retain class values
userData$class <- as.factor(userData$class)

# rasterize user polygons to raster stack
r[['class']] <- rasterize(userData, r, field = "class")

# ---------------------------------------------------------
# Create dataframe from raster stack
# ---------------------------------------------------------
df_original <- as.data.frame(r, cell = FALSE, na.rm = FALSE)

# retain class values
df_original$class <- as.factor(df_original$class)

# move class column to first position
df_original <- df_original[, c(ncol(df_original), 1:(ncol(df_original) - 1))]

# rows with NA predictors
na_rows <- which(apply(df_original[-1], 1, function(x) any(is.na(x))))

# ---------------------------------------------------------
# Set seed
# ---------------------------------------------------------
set.seed(99)

# ---------------------------------------------------------
# Baseline RF model using all dates / all predictors
# ---------------------------------------------------------
rf_output <- random_forest_raster(
  r = r,
  trees = 500,
  df = df_original,
  na_rows = na_rows,
  model_date = 'baseline',
  mtry = 13,   # baseline stacked model; adjust if you want to use tuned value
  min_n = 2
)

# ---------------------------------------------------------
# Single-date RF models
# Landsat core4 has 4 bands per date:
# blue, green, red, nir
# ---------------------------------------------------------
for (i in seq_along(dates)) {
  t1 <- Sys.time()
  
  # class column + 4 predictor bands for date i
  subset_cols <- c(1, 1 + (1:4) + 4 * (i - 1))
  
  subset_df <- df_original[, subset_cols, drop = FALSE]
  
  subset_na_rows <- which(
    apply(subset_df[-1], 1, function(x) any(is.na(x)))
  )
  
  subset_rf_output <- random_forest_raster(
    r = r,
    trees = 300,
    df = subset_df,
    na_rows = subset_na_rows,
    model_date = dates[i],
    mtry = 2,   # must be <= 4 for single-date core4 models
    min_n = 2
  )
  
  # save to raster stack
  rf_output$raster[[dates[i]]] <- subset_rf_output$raster
  
  # append results to results table
  rf_output$results <- rbind(rf_output$results, subset_rf_output$results)
  
  print(Sys.time() - t1)
}

# ---------------------------------------------------------
# Save outputs
# ---------------------------------------------------------
writeRaster(
  rf_output$raster,
  '_data/GIS/Raster/Analysis/RandomForest/rf_landsat_core4.tif',
  overwrite = TRUE
)

write.csv(
  rf_output$results,
  '_data/Tables/rf_results_landsat_core4.csv',
  row.names = FALSE
)

# ---------------------------------------------------------
# Clean up
# ---------------------------------------------------------
rm(list = ls())