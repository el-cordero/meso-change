source('_scripts/00_libraries.R')
source('_scripts/01_functions/normalise.R')
source('_scripts/01_functions/time_series.r')

# ---------------------------------------------------------
# River mask
# ---------------------------------------------------------
river <- vect('_data/GIS/Vector/Clean/river_corridor.shp')

# ---------------------------------------------------------
# Read Sentinel raster stacks
# ---------------------------------------------------------
kmRaster <- rast('_data/GIS/Raster/Analysis/Kmeans/km_sentinel_22_core4.tif')
rfRaster <- rast('_data/GIS/Raster/Analysis/RandomForest/rf_sentinel_core4.tif')

plot(kmRaster[[1]])
plot(kmRaster[[2]])
# ---------------------------------------------------------
# Mask the rasters
# ---------------------------------------------------------
river <- terra::project(river, crs(rfRaster))

rfRaster <- mask(rfRaster, river)
kmRaster <- mask(kmRaster, river)

# ---------------------------------------------------------
# Sanity checks
# ---------------------------------------------------------
levels(rfRaster[[1]])

if (!all(names(rfRaster) == names(kmRaster))) {
  stop("RF and KM raster layer names do not match.")
}

# ---------------------------------------------------------
# Reclassify K-means output
# ---------------------------------------------------------
# Original classes:
# 1 = cropland, 2 = mixed, 3 = mixed, 4 = water, 5 = forest, 6 = mixed

rcl <- cbind(
  c(1, 2, 3, 4, 5, 6),
  c(4, 1, 1, 2, 3, 1)
)

kmRaster <- classify(kmRaster, rcl)
kmRaster <- as.factor(kmRaster)

kmValues <- list(
  data.frame(
    1:4,
    c('bareland', 'water', 'forest','cropland')
  )
)

kmRaster <- categories(
  kmRaster,
  layer = 0,
  value = rep(kmValues, nlyr(kmRaster))
)

# force name alignment (critical)
names(kmRaster) <- names(rfRaster)
plot(kmRaster[[1]])
# ---------------------------------------------------------
# Time series extraction
# ---------------------------------------------------------
kmDF <- time_series(kmRaster)
rfDF <- time_series(rfRaster)

kmDF$method <- 'kmeans'
rfDF$method <- 'randomforest'

mlDF <- rbind(kmDF, rfDF)

# ---------------------------------------------------------
# Area comparison summary
# ---------------------------------------------------------
comparisonDF <- full_join(
  mlDF %>%
    filter(method == 'kmeans') %>%
    group_by(class) %>%
    reframe(
      total_area = sum(area),
      avg_area   = mean(area)
    ),
  mlDF %>%
    filter(method == 'randomforest') %>%
    group_by(class) %>%
    reframe(
      total_area = sum(area),
      avg_area   = mean(area)
    ),
  by = 'class'
)

names(comparisonDF) <- c(
  'class',
  'kmeans_area_tot',
  'kmeans_area_avg',
  'randomforest_area_tot',
  'randomforest_area_avg'
)

comparisonDF <- comparisonDF[
  , c(
    'class',
    'kmeans_area_tot',
    'randomforest_area_tot',
    'kmeans_area_avg',
    'randomforest_area_avg'
  )
]

# ---------------------------------------------------------
# Save outputs
# ---------------------------------------------------------
write.csv(
  mlDF,
  '_data/Tables/resultsTimeSeries_sentinel_core4.csv',
  row.names = FALSE
)

write.csv(
  comparisonDF,
  '_data/Tables/comparisonArea_sentinel_core4.csv',
  row.names = FALSE
)

# ---------------------------------------------------------
# Clean up
# ---------------------------------------------------------
rm(list = ls())