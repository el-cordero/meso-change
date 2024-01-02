# create a function for preparing the raster
raster_prep <- function(r, mask.polygon, crs){
  # reproject to crs
  r <- terra::project(r, crs, method="near")
  
  # crop to the extent then mask to the river
  # this insures that the raster grid remains the same across rasters
  r <- crop(r, mask.polygon, ext=TRUE)
  r <- mask(r, mask.polygon)
  
  return(r)
}