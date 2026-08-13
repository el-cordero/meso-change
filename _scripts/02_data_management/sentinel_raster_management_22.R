source("_scripts/00_libraries.R")

target_crs <- "EPSG:32614"

# ---------------------------------------------------------
# User settings
# ---------------------------------------------------------
target_dates <- c(
  "2021-11-21", "2022-01-11", "2022-02-12", "2022-04-01", "2022-06-20",
  "2022-07-22", "2022-08-07", "2022-08-23", "2022-09-08", "2022-09-24",
  "2022-10-10", "2022-11-11", "2022-12-29", "2023-03-03", "2023-03-19",
  "2023-04-04", "2023-05-06", "2023-06-07", "2023-07-09", "2023-08-10",
  "2023-09-27", "2023-11-14"
)

target_dates_compact <- gsub("-", "", target_dates)

band_codes <- c("B2", "B3", "B4", "B8", "B11", "B12")
band_names <- c("blue", "green", "red", "nir", "swir1", "swir2")

# ---------------------------------------------------------
# Load AOI
# ---------------------------------------------------------
aoi <- terra::vect("_data/GIS/Vector/Clean/aoi.shp")
aoi <- terra::project(aoi, target_crs)

# ---------------------------------------------------------
# List Sentinel files
# Adjust path/pattern if needed
# ---------------------------------------------------------
sentinel_files <- list.files(
  path = "_data/GIS/Raster/Raw/Sentinel",
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(sentinel_files) == 0) {
  stop("No Sentinel files found.")
}

# ---------------------------------------------------------
# Extract dates from filenames
# Supports either YYYYMMDD or YYYY-MM-DD in filename
# ---------------------------------------------------------
extract_date <- function(x) {
  b <- basename(x)
  
  d1 <- stringr::str_extract(b, "\\d{8}")
  d2 <- stringr::str_extract(b, "\\d{4}-\\d{2}-\\d{2}")
  
  if (!is.na(d2)) {
    return(d2)
  } else if (!is.na(d1)) {
    return(paste0(substr(d1, 1, 4), "-", substr(d1, 5, 6), "-", substr(d1, 7, 8)))
  } else {
    return(NA_character_)
  }
}

sentinel_dates <- vapply(sentinel_files, extract_date, character(1))

# ---------------------------------------------------------
# Match Sentinel files to target dates (±2 days)
# ---------------------------------------------------------
sentinel_dates_date <- as.Date(sentinel_dates)
target_dates_date   <- as.Date(target_dates)

matched_files <- vector("list", length(target_dates))
matched_dates <- vector("list", length(target_dates))

for (i in seq_along(target_dates_date)) {
  td <- target_dates_date[i]
  
  # ±2 day window
  idx <- which(abs(sentinel_dates_date - td) <= 11)
  
  if (length(idx) == 0) {
    warning("No Sentinel match within ±11 days for: ", td)
    next
  }
  
  # 🔥 pick BEST match (closest in time)
  best_idx <- idx[which.min(abs(sentinel_dates_date[idx] - td))]
  
  matched_files[[i]] <- sentinel_files[best_idx]
  matched_dates[[i]] <- as.character(td)
}

# Clean up
keep <- !sapply(matched_files, is.null)
sentinel_files <- unlist(matched_files[keep])
sentinel_dates <- unlist(matched_dates[keep])

# Order
ord <- order(sentinel_dates)
sentinel_files <- sentinel_files[ord]
sentinel_dates <- sentinel_dates[ord]

print(data.frame(target_date = sentinel_dates, file = sentinel_files))

if (length(sentinel_files) == 0) {
  stop("No Sentinel files with parseable dates found.")
}

# Keep only the 22 target dates
keep_target <- sentinel_dates %in% target_dates
sentinel_files <- sentinel_files[keep_target]
sentinel_dates <- sentinel_dates[keep_target]

if (length(sentinel_files) == 0) {
  stop("No Sentinel files matched the 22 target dates.")
}

# Order files by date, then filename
ord <- order(sentinel_dates, sentinel_files)
sentinel_files <- sentinel_files[ord]
sentinel_dates <- sentinel_dates[ord]

# Quick check
print(data.frame(date = sentinel_dates, file = sentinel_files))

# ---------------------------------------------------------
# Build one mosaicked raster per target date
# ---------------------------------------------------------
unique_dates <- target_dates[target_dates %in% unique(sentinel_dates)]

sentinel_by_date <- vector("list", length(unique_dates))
names(sentinel_by_date) <- unique_dates

for (i in seq_along(unique_dates)) {
  this_date <- unique_dates[i]
  this_files <- sentinel_files[sentinel_dates == this_date]
  
  if (length(this_files) == 0) {
    warning("No Sentinel file found for target date: ", this_date)
    next
  }
  
  message("Processing date: ", this_date, " (", length(this_files), " scene(s))")
  
  ras_list <- lapply(this_files, function(f) {
    r <- terra::rast(f)
    
    # Expecting a 6-band exported Sentinel raster:
    # B2, B3, B4, B8, B11, B12
    if (terra::nlyr(r) != 6) {
      stop(
        "Unexpected number of bands in file ", basename(f),
        ". Expected 6 bands (B2, B3, B4, B8, B11, B12), found ",
        terra::nlyr(r), "."
      )
    }
    
    r <- terra::project(r, target_crs)
    r <- terra::crop(r, aoi, ext = TRUE)
    
    r
  })
  
  # If more than one scene on this date, mosaic them
  if (length(ras_list) == 1) {
    r_date <- ras_list[[1]]
  } else {
    r_date <- do.call(terra::mosaic, c(ras_list, fun = "mean"))
  }
  
  # Mask to AOI after mosaic
  r_date <- terra::mask(r_date, aoi)
  
  if (terra::nlyr(r_date) != 6) {
    stop("Unexpected number of bands for date ", this_date, ": ", terra::nlyr(r_date))
  }
  
  names(r_date) <- paste0(gsub("-", "", this_date), "_", band_names)
  
  sentinel_by_date[[i]] <- r_date
}

# Drop NULL dates if any were missing
sentinel_by_date <- sentinel_by_date[!vapply(sentinel_by_date, is.null, logical(1))]

if (length(sentinel_by_date) == 0) {
  stop("No valid Sentinel rasters were processed.")
}

# ---------------------------------------------------------
# Combine all dates into one raster stack
# ---------------------------------------------------------
sentinel_22dates <- terra::rast(sentinel_by_date)

# date_ids <- unique(sub("^(\\d{8})_.*$", "\\1", names(sentinel_22dates)))
band_names <- c("blue", "green", "red", "nir", "swir1", "swir2")

old_names <- names(sentinel_22dates)

new_names <- sapply(old_names, function(n) {
  parts <- strsplit(n, "_")[[1]]
  
  date <- parts[1]
  band_index <- as.integer(parts[2])
  
  paste0(gsub("-", "", date), "_", band_names[band_index])
})

names(sentinel_22dates) <- new_names

# ---------------------------------------------------------
# Median composite across dates
# ---------------------------------------------------------
blue_stack  <- sentinel_22dates[[grep("_blue$",  names(sentinel_22dates))]]
green_stack <- sentinel_22dates[[grep("_green$", names(sentinel_22dates))]]
red_stack   <- sentinel_22dates[[grep("_red$",   names(sentinel_22dates))]]
nir_stack   <- sentinel_22dates[[grep("_nir$",   names(sentinel_22dates))]]
swir1_stack <- sentinel_22dates[[grep("_swir1$", names(sentinel_22dates))]]
swir2_stack <- sentinel_22dates[[grep("_swir2$", names(sentinel_22dates))]]

sentinel_median <- c(
  terra::app(blue_stack,  median, na.rm = TRUE),
  terra::app(green_stack, median, na.rm = TRUE),
  terra::app(red_stack,   median, na.rm = TRUE),
  terra::app(nir_stack,   median, na.rm = TRUE),
  terra::app(swir1_stack, median, na.rm = TRUE),
  terra::app(swir2_stack, median, na.rm = TRUE)
)

names(sentinel_median) <- c("blue", "green", "red", "nir", "swir1", "swir2")

# ---------------------------------------------------------
# Check result
# ---------------------------------------------------------
print(sentinel_22dates)
print(names(sentinel_22dates))
print(unique(sub("^(\\d{8})_.*$", "\\1", names(sentinel_22dates))))
print(sentinel_median)
print(names(sentinel_median))
# ---------------------------------------------------------
# Write outputs
# ---------------------------------------------------------
terra::writeRaster(
  sentinel_22dates,
  "_data/GIS/Raster/Clean/sentinel_22dates.tif",
  overwrite = TRUE
)

terra::writeRaster(
  sentinel_median,
  "_data/GIS/Raster/Clean/sentinel_22dates_median.tif",
  overwrite = TRUE
)



terra::tmpFiles(remove = TRUE)
