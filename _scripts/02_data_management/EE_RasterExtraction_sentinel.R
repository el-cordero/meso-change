source("~/Documents/Projects/USACE/Meso-change/_scripts/00_libraries.R")

# library(reticulate)
# library(rgee)
# 
# # See what Python rgee/reticulate is using
# py_config()
# 
# # Install Earth Engine into the current env if needed
# py_install(
#   packages = c("earthengine-api"),
#   envname = "/Users/ec/.virtualenvs/r-reticulate",
#   pip = TRUE
# )
# 
# # optional but often useful
# py_install(
#   packages = c("google-auth", "google-auth-oauthlib", "google-cloud-storage"),
#   envname = "/Users/ec/.virtualenvs/r-reticulate",
#   pip = TRUE
# )

library(rgee)

ee_clean_user_credentials()
ee_Authenticate()

ee$Initialize(project = "ee-cordero")
ee$Image("USGS/SRTMGL1_003")$getInfo()

# -------------------------------------------------
# Paths
# -------------------------------------------------
vector_dir <- "~/Documents/Projects/USACE/Meso-change/_data/GIS/Vector"
out_dir    <- "~/Documents/Projects/USACE/Meso-change/_data/GIS/Raster/Sentinel/Raw/"
out_dir    <- "~/Documents/Projects/USACE/Meso-change/_data/GIS/Raster/Raw/Sentinel_B8A/"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
# -------------------------------------------------
# AOI
# -------------------------------------------------
aoi_vect <- vect(file.path(vector_dir, "Clean/aoi.shp"))
aoi <- ee$Geometry$Rectangle(
  c(
    xmin(aoi_vect),
    ymin(aoi_vect),
    xmax(aoi_vect),
    ymax(aoi_vect)
  )
)

# -------------------------------------------------
# Parameters
# -------------------------------------------------
start_date      <- "2020-01-01"
end_date        <- "2023-12-01"
scene_cloud_max <- 20
cloud_prob_max  <- 20
bands           <- c("B2", "B3", "B4", "B8", "B11", "B12")

# -------------------------------------------------
# Join Sentinel-2 SR with cloud probability
# -------------------------------------------------
# get_s2_sr_cld_col <- function(aoi, start_date, end_date, scene_cloud_max = 20) {
#   
#   s2_sr <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
#     filterBounds(aoi)$
#     filterDate(start_date, end_date)$
#     filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", scene_cloud_max))
#   
#   s2_clouds <- ee$ImageCollection("COPERNICUS/S2_CLOUD_PROBABILITY")$
#     filterBounds(aoi)$
#     filterDate(start_date, end_date)
#   
#   ee$ImageCollection(
#     ee$Join$saveFirst("cloud_mask")$apply(
#       primary = s2_sr,
#       secondary = s2_clouds,
#       condition = ee$Filter$equals(
#         leftField  = "system:index",
#         rightField = "system:index"
#       )
#     )
#   )
# }

# -------------------------------------------------
# Build collection
# -------------------------------------------------
images <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED") %>%
  ee$ImageCollection$filterBounds(aoi) %>%
  # ee$ImageCollection$filterDate("2020-01-01", "2023-12-01") %>%
  ee$ImageCollection$filterDate("2021-07-30", "2023-12-01") %>%
  ee$ImageCollection$filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 20)) %>%
  # ee$ImageCollection$select(c("B2", "B3", "B4", "B8", "B11", "B12"))
  ee$ImageCollection$select("B8A")
  
# # -------------------------------------------------
# # Cloud mask + scale reflectance
# # -------------------------------------------------
# process_s2 <- function(img) {
#   
#   cloud_img  <- ee$Image(img$get("cloud_mask"))
#   cloud_prob <- cloud_img$select("probability")
#   clear_mask <- cloud_prob$lt(cloud_prob_max)
#   
#   img$
#     select(bands)$
#     updateMask(clear_mask)$
#     divide(10000)$
#     copyProperties(img, img$propertyNames())
# }

process_s2 <- function(img) {
  img$divide(10000)
}

images <- images$map(process_s2)

# -------------------------------------------------
# Optional: inspect collection size
# -------------------------------------------------
n_imgs <- images$size()$getInfo()
print(paste("Number of images:", n_imgs))



ee_path <- "~/.config/earthengine/rgee_sessioninfo.txt"

dir.create(dirname(ee_path), showWarnings = FALSE, recursive = TRUE)

write.table(
  data.frame(
    email = "cordero.elvin@gmail.com",
    user = "users/EC_meso_change",
    drive_cre = NA,
    gcs_cre = NA,
    stringsAsFactors = FALSE
  ),
  ee_path,
  row.names = FALSE
)

# -------------------------------------------------
# Export locally at 10 m
# -------------------------------------------------
task_img <- ee_imagecollection_to_local(
  ic          = images,
  region      = aoi,
  dsn         = out_dir,
  scale       = 10,
  maxPixels   = 1e13,
  timePrefix  = FALSE
)

task_img