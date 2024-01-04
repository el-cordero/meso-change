# create a function for preparing the raster
raster_prep <- function(r, mask.polygon, crs, doMask=TRUE){
  # reproject to crs
  r <- terra::project(r, crs, method="near")
  
  # crop to the extent
  # this insures that the raster grid remains the same across rasters
  r <- crop(r, mask.polygon, ext=TRUE)

  if (doMask == TRUE){
  # then mask to the river
    r <- mask(r, mask.polygon)
  }
  
  return(r)
}
