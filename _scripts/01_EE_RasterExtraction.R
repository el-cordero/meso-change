source("~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_libraries.R")

# rgee::ee_install_upgrade()

# Initialize just Earth Engine
ee_clean_user_credentials()
ee_Authenticate()
ee_Initialize(drive = TRUE)
# Earth Engine account: users/ecordero 
# Python Path: /Users/EC13/.virtualenvs/rgee/bin/python 

# set path names
vector <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Vector/"

# read in AOI as an sf vector and convert to ee
aoi <- vect(paste0(vector,'Clean/aoi.shp'))
aoi <- ee$Geometry$Rectangle(c(xmin(aoi),ymin(aoi),xmax(aoi),ymax(aoi)))

# extract image data
images <- ee$ImageCollection('LANDSAT/LC09/C02/T1_L2') %>%
  ee$ImageCollection$filterBounds(aoi) %>%
  ee$ImageCollection$filterDate('2020-01-01', '2023-12-01') %>%
  ee$ImageCollection$filter(ee$Filter$lt('CLOUD_COVER',20)) %>%
  ee$ImageCollection$select('SR_B[1-7]')

img_scaling <- function(image){
  image <- image$multiply(0.0000275)$add(-0.2)
  return(image)
}

images <- images$map(img_scaling)


task_img <- ee_imagecollection_to_local(
  ic = images,
  region = aoi,
  dsn = "/Users/EC13/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Raster/Landsat/Raw/"
)

# extract image data
images <- ee$ImageCollection('LANDSAT/LC09/C02/T1') %>%
  ee$ImageCollection$filterBounds(aoi) %>%
  ee$ImageCollection$filterDate('2022-08-08', '2023-12-01') %>%
  ee$ImageCollection$filter(ee$Filter$lt('CLOUD_COVER',20)) %>%
  ee$ImageCollection$select('B[2-4]')

task_img <- ee_imagecollection_to_local(
  ic = images,
  region = aoi,
  dsn = "/Users/EC13/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Raster/Landsat/Color/"
)
