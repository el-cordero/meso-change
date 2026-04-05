source("_scripts/00_libraries.R")

target_crs <- "EPSG:32614"

# ---------------------------------------------------------
# Load AOI
# ---------------------------------------------------------
aoi <- vect("_data/GIS/Vector/Clean/aoi.shp")
aoi <- terra::project(aoi, target_crs)

# ---------------------------------------------------------
# List Planet files
# ---------------------------------------------------------
planet_files <- list.files(
  path = "_data/GIS/Raster/Raw/Planet/b",
  pattern = "_SR_clip.tif$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(planet_files) == 0) {
  stop("No Planet files found.")
}

# Extract dates from filenames
planet_dates <- basename(planet_files)
planet_dates <- sub("^([0-9]{8}).*$", "\\1", planet_dates)

# Order files by date
ord <- order(planet_dates, planet_files)
planet_files <- planet_files[ord]
planet_dates <- planet_dates[ord]

# Quick check
print(data.frame(date = planet_dates, file = planet_files))

# ---------------------------------------------------------
# Build one mosaicked raster per date
# ---------------------------------------------------------
unique_dates <- unique(planet_dates)

planet_by_date <- vector("list", length(unique_dates))
names(planet_by_date) <- unique_dates

for (i in seq_along(unique_dates)) {
  this_date <- unique_dates[i]
  this_files <- planet_files[planet_dates == this_date]
  
  message("Processing date: ", this_date, " (", length(this_files), " scene(s))")
  
  # Read all rasters for this date
  ras_list <- lapply(this_files, function(f) {
    r <- rast(f)
    r <- terra::project(r, target_crs)
    r <- crop(r, aoi, ext = TRUE)
    r
  })
  
  # If more than one scene on this date, mosaic them
  if (length(ras_list) == 1) {
    r_date <- ras_list[[1]]
  } else {
    r_date <- do.call(terra::mosaic, c(ras_list, fun = "mean"))
  }
  
  # Mask to AOI after mosaic
  r_date <- mask(r_date, aoi)
  
  # Rename bands
  band_names <- c("blue", "green", "red", "nir")
  if (nlyr(r_date) != 4) {
    stop("Unexpected number of bands for date ", this_date, ": ", nlyr(r_date))
  }
  names(r_date) <- paste0(this_date, "_", band_names)
  
  planet_by_date[[i]] <- r_date
}


# ---------------------------------------------------------
# Combine all 7 dates into one raster stack
# ---------------------------------------------------------
planet_7dates <- rast(planet_by_date)
date_ids <- unique(sub("^(\\d{8})_.*$", "\\1", names(planet_7dates)))
new_names <- unlist(lapply(date_ids, function(d) {
  paste0(d, c("_blue", "_green", "_red", "_nir"))
}))
names(planet_7dates) <- new_names
# ---------------------------------------------------------
# Median composite across dates
# Since each date has 4 bands, compute median by band position
# ---------------------------------------------------------
blue_stack  <- planet_7dates[[grep("_blue$",  names(planet_7dates))]]
green_stack <- planet_7dates[[grep("_green$", names(planet_7dates))]]
red_stack   <- planet_7dates[[grep("_red$",   names(planet_7dates))]]
nir_stack   <- planet_7dates[[grep("_nir$",   names(planet_7dates))]]

planet_median <- c(
  app(blue_stack,  median, na.rm = TRUE),
  app(green_stack, median, na.rm = TRUE),
  app(red_stack,   median, na.rm = TRUE),
  app(nir_stack,   median, na.rm = TRUE)
)

names(planet_median) <- c("blue", "green", "red", "nir")

# ---------------------------------------------------------
# Write outputs
# ---------------------------------------------------------
writeRaster(
  planet_7dates,
  "_data/GIS/Raster/Clean/planet_7dates.tif",
  overwrite = TRUE
)

writeRaster(
  planet_median,
  "_data/GIS/Raster/Clean/planet_7dates_median.tif",
  overwrite = TRUE
)

tmpFiles(remove = TRUE)

# ---------------------------------------------------------
# Check result
# ---------------------------------------------------------
print(planet_7dates)
print(names(planet_7dates))
print(unique(sub("^(\\d{8})_.*$", "\\1", names(planet_7dates))))