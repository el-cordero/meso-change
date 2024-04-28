source('_scripts/00_libraries.R')
source('_scripts/01_functions/random_forest_raster.R')

# read raster
r <- rast('_data/GIS/Raster/Clean/planet_7dates.tif')

# trim an NA values
r <- trim(r)

# resample according to your needs/capacity
s <- rast(ext=ext(r),crs=crs(r),res=10)
r <- resample(r,s)

# scale the values
r <- scale(r)

# acquire the sampling dates
dates <- c('01_Dec22_2021','02_Feb12_2022','03_Apr17_2022',
  '04_May31_2022','05_Jun20_2022','06_Jul18_2022','07_Sep28_2022')
  dates <- gsub("^(\\d{2})_([A-Za-z]{3})(\\d{2})_(\\d{4})$", "\\2 \\3 \\4", dates)
dates <- as.Date(dates, format = "%b%d%Y")
dates <- gsub("-", "", dates)
dates <- sub("^(\\d{8})_.*$", "\\1",dates)

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

# run the rf model on all the data
rf_output <- random_forest_raster(
  r = r, 
  df = df_original, 
  na_rows = na_rows, 
  model_date = 'baseline',
  trees = 1000,
  mtry = 7,
  min_n = 6
  )

# run a new rf model for each sampling date
for (i in 1:7){
  # subset columns with the class column being first
  # adjust according to # of bands - 4 in this case
  subset_cols <- c(1,1+(1:4)+4*(i-1))

  # run model
  subset_rf_output <- random_forest_raster(
    r = r, 
    df = df_original[subset_cols], 
    na_rows = na_rows, 
    model_date = dates[i],
    trees = 1000,
    mtry = 7,
    min_n = 6
    )

  # save to raster stack
  rf_output$raster[[dates[i]]] <- subset_rf_output$raster

  # append results to results table
  rf_output$results <- rbind(rf_output$results,subset_rf_output$results)
}

names(rf_output$raster) <- c('baseline', dates)

# save data
writeRaster(rf_output$raster,'_data/GIS/Raster/Analysis/RandomForest/rf_planet_7.tif',
            overwrite=TRUE)
write.csv(rf_output$results,'_data/Tables/rf_results_planet_7.csv')

# remove junk
rm(list = ls())
