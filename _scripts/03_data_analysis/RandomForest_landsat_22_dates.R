source('_scripts/00_libraries.R')
source('_scripts/01_functions/random_forest_raster.R')

# landsat files list
rasterFiles <- list.files(path='_data/GIS/Raster/Raw/Landsat/Raw',
                         pattern=".tif$", full.names = TRUE)

# set crs
crs <- 'EPSG:32614'

# read in rasters
r <- c()
# add raster files to a list
for (raster in rasterFiles){
    s <- rast(raster)
    s <- terra::project(s,crs)
    r <- c(r,s)
    rm(s)
}

# apply crop and mask over list elements
for (i in 1:length(r)) {
    r <- lapply(r, crop, y = r[[i]], ext = TRUE)
    r <- lapply(r, mask, mask = r[[i]])
}

# combine into one raster
r <- rast(r)

# trim an NA values
r <- trim(r)

# scale the values
r <- scale(r)

# acquire the sampling dates
dates <- sub(".*_(\\d{8}).*", "\\1", rasterFiles)

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
  model_date = 'baseline'
  )

# run a new rf model for each sampling date
for (i in 1:22){
  # subset columns with the class column being first
  # adjust according to # of bands - 7 in this case
  subset_cols <- c(1,1+(1:7)+7*(i-1))

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
writeRaster(rf_output$raster,'_data/GIS/Raster/Analysis/RandomForest/rf_landsat.tif',
            overwrite=TRUE)
write.csv(rf_output$results,'_data/Tables/rf_results_landsat.csv')

# remove junk
rm(list = ls())
