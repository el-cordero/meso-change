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

band_names <- c("blue", "green", "red", "nir")

# ---------------------------------------------------------
# Load AOI
# ---------------------------------------------------------
aoi <- terra::vect("_data/GIS/Vector/Clean/aoi.shp")
aoi <- terra::project(aoi, target_crs)

# ---------------------------------------------------------
# Helper: extract date from filename
# ---------------------------------------------------------
extract_date <- function(x) {
  b <- basename(x)
  d1 <- stringr::str_extract(b, "\\d{8}")
  if (!is.na(d1)) {
    return(paste0(substr(d1, 1, 4), "-", substr(d1, 5, 6), "-", substr(d1, 7, 8)))
  } else {
    return(NA_character_)
  }
}

# ---------------------------------------------------------
# List files
# ---------------------------------------------------------
visual_files <- list.files(
  path = "_data/GIS/Raster/Raw/Sentinel",
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

b8a_files <- list.files(
  path = "_data/GIS/Raster/Raw/Sentinel_B8A",
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(visual_files) == 0) stop("No Sentinel visual files found.")
if (length(b8a_files) == 0) stop("No Sentinel B8A files found.")

visual_dates <- vapply(visual_files, extract_date, character(1))
b8a_dates    <- vapply(b8a_files, extract_date, character(1))

visual_dates_date <- as.Date(visual_dates)
b8a_dates_date    <- as.Date(b8a_dates)
target_dates_date <- as.Date(target_dates)

# ---------------------------------------------------------
# Match each target date to closest visual + B8A within ±11 days
# ---------------------------------------------------------
matched_visual_files <- vector("list", length(target_dates))
matched_b8a_files    <- vector("list", length(target_dates))
matched_dates        <- vector("list", length(target_dates))

for (i in seq_along(target_dates_date)) {
  td <- target_dates_date[i]
  
  visual_idx <- which(abs(visual_dates_date - td) <= 11)
  b8a_idx    <- which(abs(b8a_dates_date - td) <= 11)
  
  if (length(visual_idx) == 0) {
    warning("No Sentinel visual match within ±11 days for: ", td)
    next
  }
  
  if (length(b8a_idx) == 0) {
    warning("No Sentinel B8A match within ±11 days for: ", td)
    next
  }
  
  best_visual_idx <- visual_idx[which.min(abs(visual_dates_date[visual_idx] - td))]
  best_b8a_idx    <- b8a_idx[which.min(abs(b8a_dates_date[b8a_idx] - td))]
  
  matched_visual_files[[i]] <- visual_files[best_visual_idx]
  matched_b8a_files[[i]]    <- b8a_files[best_b8a_idx]
  matched_dates[[i]]        <- as.character(td)
}

keep <- !sapply(matched_visual_files, is.null) & !sapply(matched_b8a_files, is.null)

matched_visual_files <- unlist(matched_visual_files[keep])
matched_b8a_files    <- unlist(matched_b8a_files[keep])
matched_dates        <- unlist(matched_dates[keep])

ord <- order(matched_dates)
matched_visual_files <- matched_visual_files[ord]
matched_b8a_files    <- matched_b8a_files[ord]
matched_dates        <- matched_dates[ord]

print(data.frame(
  target_date = matched_dates,
  visual_file = matched_visual_files,
  b8a_file = matched_b8a_files
))

# ---------------------------------------------------------
# Build one raster per target date
# ---------------------------------------------------------
sentinel_by_date <- vector("list", length(matched_dates))
names(sentinel_by_date) <- matched_dates

for (i in seq_along(matched_dates)) {
  this_date <- matched_dates[i]
  visual_f  <- matched_visual_files[i]
  b8a_f     <- matched_b8a_files[i]
  
  message("Processing date: ", this_date)
  
  visual_r <- terra::rast(visual_f,lyrs=1:3)
  b8a_r    <- terra::rast(b8a_f)
  
  if (terra::nlyr(visual_r) != 3) {
    stop("Visual file does not have 3 bands: ", basename(visual_f))
  }
  
  if (terra::nlyr(b8a_r) != 1) {
    stop("B8A file does not have 1 band: ", basename(b8a_f))
  }
  
  # only crop/mask to AOI
  visual_r <- terra::crop(visual_r, aoi, ext = TRUE)
  visual_r <- terra::mask(visual_r, aoi)
  
  b8a_r <- terra::crop(b8a_r, aoi, ext = TRUE)
  b8a_r <- terra::mask(b8a_r, aoi)
  
  # combine directly
  r_date <- c(visual_r, b8a_r)
  
  names(r_date) <- paste0(gsub("-", "", this_date), "_", band_names)
  
  sentinel_by_date[[i]] <- r_date
}

sentinel_by_date <- sentinel_by_date[!vapply(sentinel_by_date, is.null, logical(1))]

if (length(sentinel_by_date) == 0) {
  stop("No valid Sentinel visual+B8A rasters were processed.")
}

# ---------------------------------------------------------
# Combine into one raster stack
# ---------------------------------------------------------
sentinel_22dates_visual_b8a <- terra::rast(sentinel_by_date)

# ---------------------------------------------------------
# Median composite
# ---------------------------------------------------------

band_names <- c("blue", "green", "red", "nir")

old_names <- names(sentinel_22dates_visual_b8a)

new_names <- sapply(old_names, function(n) {
  parts <- strsplit(n, "_")[[1]]
  
  date <- parts[1]
  band_index <- as.integer(parts[2])
  
  paste0(gsub("-", "", date), "_", band_names[band_index])
})

names(sentinel_22dates_visual_b8a) <- new_names

print(names(sentinel_22dates_visual_b8a))

blue_stack  <- sentinel_22dates_visual_b8a[[grep("_blue$",  names(sentinel_22dates_visual_b8a))]]
green_stack <- sentinel_22dates_visual_b8a[[grep("_green$", names(sentinel_22dates_visual_b8a))]]
red_stack   <- sentinel_22dates_visual_b8a[[grep("_red$",   names(sentinel_22dates_visual_b8a))]]
b8a_stack   <- sentinel_22dates_visual_b8a[[grep("_nir$",   names(sentinel_22dates_visual_b8a))]]

sentinel_visual_b8a_median <- c(
  terra::app(blue_stack,  median, na.rm = TRUE),
  terra::app(green_stack, median, na.rm = TRUE),
  terra::app(red_stack,   median, na.rm = TRUE),
  terra::app(b8a_stack,   median, na.rm = TRUE)
)

names(sentinel_visual_b8a_median) <- c("blue", "green", "red", "nir")

# ---------------------------------------------------------
# Write outputs
# ---------------------------------------------------------
terra::writeRaster(
  sentinel_22dates_visual_b8a,
  "_data/GIS/Raster/Clean/sentinel_22dates_core4.tif",
  overwrite = TRUE
)

terra::writeRaster(
  sentinel_visual_b8a_median,
  "_data/GIS/Raster/Clean/sentinel_22dates_visual_core4.tif",
  overwrite = TRUE
)

terra::tmpFiles(remove = TRUE)
