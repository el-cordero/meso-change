library(terra)
target_crs <- "EPSG:32614"

# ---------------------------------------------------------
# Load AOI
# ---------------------------------------------------------
aoi <- terra::vect("_data/GIS/Vector/Clean/aoi.shp")
aoi <- terra::project(aoi, target_crs)

# ---------------------------------------------------------
# List Landsat tif files only
# ---------------------------------------------------------
landsat_files <- list.files(
  path = "_data/GIS/Raster/Raw/Landsat/Raw",
  pattern = "\\.tif$",
  full.names = TRUE
)

# extract dates
landsat_dates <- basename(landsat_files)
landsat_dates <- sub("^.*_(\\d{8})\\.tif$", "\\1", landsat_dates)

# keep valid only
keep <- grepl("^\\d{8}$", landsat_dates)
landsat_files <- landsat_files[keep]
landsat_dates <- landsat_dates[keep]

# order
ord <- order(landsat_dates, landsat_files)
landsat_files <- landsat_files[ord]
landsat_dates <- landsat_dates[ord]

print(data.frame(date = landsat_dates, file = landsat_files))

# ---------------------------------------------------------
# Read + process rasters
# ---------------------------------------------------------
ras_list <- vector("list", length(landsat_files))

for (i in seq_along(landsat_files)) {
  
  message("Processing: ", basename(landsat_files[i]))
  
  r <- terra::rast(landsat_files[i])
  
  # 🔥 keep only RGB + NIR (bands 2–5)
  r <- r[[2:5]]
  
  r <- terra::project(r, target_crs)
  r <- terra::crop(r, aoi, ext = TRUE)
  r <- terra::mask(r, aoi)
  
  names(r) <- paste0(
    landsat_dates[i],
    c("_blue", "_green", "_red", "_nir")
  )
  
  ras_list[[i]] <- r
}

# ---------------------------------------------------------
# Align to first raster
# ---------------------------------------------------------
template <- ras_list[[1]]

ras_list <- lapply(ras_list, function(x) {
  if (!terra::compareGeom(template, x, stopOnError = FALSE)) {
    x <- terra::resample(x, template, method = "bilinear")
  }
  x
})

# ---------------------------------------------------------
# Combine into stack
# ---------------------------------------------------------
landsat_22dates <- terra::rast(ras_list)

# ---------------------------------------------------------
# Median composite
# ---------------------------------------------------------
blue_stack  <- landsat_22dates[[grep("_blue$",  names(landsat_22dates))]]
green_stack <- landsat_22dates[[grep("_green$", names(landsat_22dates))]]
red_stack   <- landsat_22dates[[grep("_red$",   names(landsat_22dates))]]
nir_stack   <- landsat_22dates[[grep("_nir$",   names(landsat_22dates))]]

landsat_median <- c(
  terra::app(blue_stack,  median, na.rm = TRUE),
  terra::app(green_stack, median, na.rm = TRUE),
  terra::app(red_stack,   median, na.rm = TRUE),
  terra::app(nir_stack,   median, na.rm = TRUE)
)

names(landsat_median) <- c("blue", "green", "red", "nir")

# ---------------------------------------------------------
# Save outputs
# ---------------------------------------------------------
terra::writeRaster(
  landsat_22dates,
  "_data/GIS/Raster/Clean/landsat_22dates_core4.tif",
  overwrite = TRUE
)

terra::writeRaster(
  landsat_median,
  "_data/GIS/Raster/Clean/landsat_22dates_core4_median.tif",
  overwrite = TRUE
)

terra::tmpFiles(remove = TRUE)

# ---------------------------------------------------------
# Check
# ---------------------------------------------------------
print(landsat_22dates)
print(names(landsat_22dates))
print(unique(sub("^(\\d{8})_.*$", "\\1", names(landsat_22dates))))