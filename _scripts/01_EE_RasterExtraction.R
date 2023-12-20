source("~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_libraries.R")

# rgee::ee_install_upgrade()

# Initialize just Earth Engine
ee_clean_user_credentials()
ee_Authenticate()
ee_Initialize(drive = TRUE)
# Earth Engine account: users/ecordero 
# Python Path: /Users/EC13/.virtualenvs/rgee/bin/python 

# set path names
vector <- "~/Documents/Projects/USACE/NextGen IPR/Data/GIS/Kansas/Vector/"
raster <- "~/Documents/Projects/USACE/NextGen IPR/Data/GIS/Kansas/Raster/"

river <- st_read(paste0(vector,'Clean/river_corridor.shp')) %>%
  sf_as_ee()

aoi <- st_read(paste0(vector,'Clean/aoi.shp')) %>%
  sf_as_ee()

images <- ee$ImageCollection('LANDSAT/LC09/C02/T1_L2') %>%
  ee$ImageCollection$filterBounds(aoi) %>%
  ee$ImageCollection$filterDate('2020-01-01', '2023-12-01') %>%
  ee$ImageCollection$filter(ee$Filter$lt('CLOUD_COVER',20)) %>%
  ee$ImageCollection$select('SR_B[1-7]')

images_original <- images
images <- clip(images,river, return_tidyee = FALSE)

training <- images$sort('Cloud Cover')$first()$sample(
  region = river,
  scale = 30,
  numPixels = 2000,
  seed = 0,
  geometries = TRUE
)

n_clusters = 5

clusterer = ee$Clusterer$wekaKMeans(
  nClusters = n_clusters,
  distanceFunction = 'Manhattan'
)

clusterer = clusterer$train(training)

multi_cluster <- function(image){
  image <- image %>% ee$Image$cluster(clusterer)
  return(image)
}

results <- images$map(multi_cluster)

# Move results from Earth Engine to Drive
task_img <- ee_imagecollection_to_local(
  ic = results,
  region = river$geometry(),
  dsn = "/Users/EC13/Documents/Projects/USACE/NextGen IPR/Data/GIS/Kansas/Raster/Analysis/Kmeans/Raw/ten/"
)

task_img <- ee_imagecollection_to_local(
  ic = images_original,
  region = aoi,
  dsn = "/Users/EC13/Documents/Projects/USACE/NextGen IPR/Data/GIS/Kansas/Raster/Landsat/Raw/"
)

pnts <- vect(ee_as_sf(training))
write.csv(pnts, paste0(vector,'Analysis/Kmeans/','training.csv'), 
          row.names = FALSE)
writeVector(pnts,paste0(vector,'Analysis/Kmeans/','training_pnts.shp'),
            overwrite = TRUE)

