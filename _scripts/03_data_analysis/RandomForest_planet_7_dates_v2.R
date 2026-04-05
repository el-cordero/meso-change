source('_scripts/00_libraries.R')
source('_scripts/01_functions/random_forest_raster.R')

# read raster
r <- rast('_data/GIS/Raster/Clean/planet_7dates.tif')

# trim an NA values
r <- trim(r)

# scale the values
r <- scale(r)

# acquire the sampling dates
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

r
names(r)

# run the rf model on all the data
rf_output <- random_forest_raster(
  r = r, 
  trees = 500,
  df = df_original, 
  na_rows = na_rows, 
  model_date = 'baseline',
  mtry = 7,
  min_n = 2
)

# run a new rf model for each sampling date
for (i in 1:7) {
  t1 <- Sys.time()
  # subset columns with the class column being first
  # adjust according to # of bands - 4 in this case
  subset_cols <- c(1, 1 + (1:4) + 4 * (i - 1))
  
  subset_df <- df_original[, subset_cols, drop = FALSE]
  
  subset_na_rows <- which(
    apply(subset_df[-1], 1, function(x) any(is.na(x)))
  )
  
  subset_rf_output <- random_forest_raster(
    r = r, 
    trees = 500,
    df = subset_df, 
    na_rows = subset_na_rows, 
    model_date = dates[i],
    mtry = 2,
    min_n = 2
  )
  
  # save to raster stack
  rf_output$raster[[dates[i]]] <- subset_rf_output$raster
  
  # append results to results table
  rf_output$results <- rbind(rf_output$results, subset_rf_output$results)
  Sys.time() - t1
}

# save data
writeRaster(rf_output$raster,'_data/GIS/Raster/Analysis/RandomForest/rf_planet_7.tif',
            overwrite=TRUE)
write.csv(rf_output$results,'_data/Tables/rf_results_planet_7.csv')

# remove junk
rm(list = ls())
