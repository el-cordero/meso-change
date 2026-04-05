library(terra)

# the hecras files do not have a projection
# based off talks with Aubrey, the projection is
# ESRI:103284 NAD 1983 (CORS96) SPCS Kansas North (US Feet)
# provided via proj4 format
hecras_crs <- '+proj=lcc +lat_0=38.3333333333333 +lon_0=-98 +lat_1=38.7166666666667 +lat_2=39.7833333333333 +x_0=400000 +y_0=0 +ellps=GRS80 +units=us-ft +no_defs +type=crs'

aoi <- vect('_data/GIS/Vector/Confluence_AOI.shp')
aoi <- terra::project(aoi, hecras_crs)

# =========================================================
# Paths
# =========================================================
in_dir  <- "_data/GIS/Raster/Raw/HECRAS"
out_dir <- "_data/GIS/Raster/Raw/Mesohabitats_v2"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# =========================================================
# Date folders
# =========================================================
date_dirs <- list.dirs(in_dir, recursive = FALSE, full.names = TRUE)
date_dirs <- date_dirs[basename(date_dirs) != ".DS_Store"]

print(basename(date_dirs))

# =========================================================
# Class definitions
# =========================================================
meso_levels <- data.frame(
  value = 1:8,
  label = c(
    "Shallow Pool",
    "Medium Pool",
    "Deep Pool",
    "Slow Riffle",
    "Fast Riffle",
    "Raceway",
    "Faster than Raceway",
    "Faster than Deep Pool"
  ),
  stringsAsFactors = FALSE
)

# =========================================================
# Helper to find depth / velocity tif
# =========================================================
find_depth_velocity <- function(folder) {
  files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
  
  depth_file <- files[grepl("^Depth ", basename(files)) | grepl("^Depth\\b", basename(files))]
  vel_file   <- files[grepl("^Velocity ", basename(files)) | grepl("^Velocity\\b", basename(files))]
  
  if (length(depth_file) != 1) {
    stop("Expected exactly 1 depth tif in: ", folder, "\nFound:\n", paste(depth_file, collapse = "\n"))
  }
  if (length(vel_file) != 1) {
    stop("Expected exactly 1 velocity tif in: ", folder, "\nFound:\n", paste(vel_file, collapse = "\n"))
  }
  
  list(depth = depth_file, velocity = vel_file)
}

# =========================================================
# Mesohabitat classification function
# =========================================================
classify_mesohabitat_fast <- function(depth_r, vel_r) {
  if (!terra::compareGeom(depth_r, vel_r, stopOnError = FALSE)) {
    vel_r <- terra::project(vel_r, depth_r, method = "bilinear")
    vel_r <- terra::resample(vel_r, depth_r, method = "bilinear")
  }
  
  meso <- ifel(
    is.na(depth_r) | is.na(vel_r), NA,
    ifel(
      depth_r < 2,
      ifel(vel_r < 1, 1,
           ifel(vel_r < 2, 4, 5)),
      ifel(
        depth_r < 4.5,
        ifel(vel_r < 1, 2,
             ifel(vel_r < 2, 6, 7)),
        ifel(vel_r < 1, 3, 8)
      )
    )
  )
  
  meso <- as.factor(meso)
  levels(meso) <- meso_levels
  meso
}

# =========================================================
# Process all dates
# =========================================================
meso_list  <- vector("list", length(date_dirs))
depth_list <- vector("list", length(date_dirs))
vel_list   <- vector("list", length(date_dirs))

names(meso_list)  <- basename(date_dirs)
names(depth_list) <- basename(date_dirs)
names(vel_list)   <- basename(date_dirs)

for (i in seq_along(date_dirs)) {
  folder_name <- basename(date_dirs[i])
  cat("\nProcessing:", folder_name, "\n")
  
  f <- find_depth_velocity(date_dirs[i])
  
  depth_r <- rast(f$depth)
  vel_r   <- rast(f$velocity)
  
  crs(depth_r) <- hecras_crs
  crs(vel_r)   <- hecras_crs
  
  depth_r <- crop(depth_r, aoi)
  vel_r   <- crop(vel_r, aoi)
  
  # ensure geometry match once here
  if (!terra::compareGeom(depth_r, vel_r, stopOnError = FALSE)) {
    vel_r <- terra::project(vel_r, depth_r, method = "bilinear")
    vel_r <- terra::resample(vel_r, depth_r, method = "bilinear")
  }
  
  # store hydraulic rasters for later median computation
  names(depth_r) <- folder_name
  names(vel_r)   <- folder_name
  
  depth_list[[i]] <- depth_r
  vel_list[[i]]   <- vel_r
  
  # classify individual date
  meso_r <- classify_mesohabitat_fast(depth_r, vel_r)
  names(meso_r) <- folder_name
  
  out_file <- file.path(out_dir, paste0(folder_name, "_mesohabitats.tif"))
  
  writeRaster(
    meso_r,
    out_file,
    overwrite = TRUE,
    wopt = list(datatype = "INT1U", gdal = "COMPRESS=LZW")
  )
  
  meso_list[[i]] <- meso_r
  
  cat("Saved:", out_file, "\n")
}

# =========================================================
# Build median depth and velocity rasters
# =========================================================
depth_stack <- rast(depth_list)
vel_stack   <- rast(vel_list)

names(depth_stack) <- basename(date_dirs)
names(vel_stack)   <- basename(date_dirs)

depth_median <- median(depth_stack, na.rm = TRUE)
vel_median   <- median(vel_stack, na.rm = TRUE)

names(depth_median) <- "median_depth"
names(vel_median)   <- "median_velocity"

writeRaster(
  depth_median,
  file.path(out_dir, "median_depth.tif"),
  overwrite = TRUE,
  wopt = list(gdal = "COMPRESS=LZW")
)

writeRaster(
  vel_median,
  file.path(out_dir, "median_velocity.tif"),
  overwrite = TRUE,
  wopt = list(gdal = "COMPRESS=LZW")
)

# =========================================================
# Classify median hydraulics into mesohabitat median
# =========================================================
meso_median <- classify_mesohabitat_fast(depth_median, vel_median)
names(meso_median) <- "median"

writeRaster(
  meso_median,
  file.path(out_dir, "median_mesohabitats.tif"),
  overwrite = TRUE,
  wopt = list(datatype = "INT1U", gdal = "COMPRESS=LZW")
)

# =========================================================
# Build final stacked raster
# =========================================================
meso_stack <- rast(c(meso_list, list(meso_median)))
names(meso_stack) <- c(basename(date_dirs), "median")

stack_file <- file.path("_data/GIS/Raster/Clean", "mesohabitats_v2.tif")

writeRaster(
  meso_stack,
  stack_file,
  overwrite = TRUE,
  wopt = list(datatype = "INT1U", gdal = "COMPRESS=LZW")
)

cat("\nSaved stack:", stack_file, "\n")

# =========================================================
# Optional summary
# =========================================================
print(meso_stack)
print(levels(meso_stack)[[1]])
plot(meso_stack)