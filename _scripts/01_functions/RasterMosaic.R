# create a function for masking and merging rasters
raster_mosaic <- function(fileNames, filePath, maskLayer, is.rgb=FALSE){
  
  # create a raster list with imagery data
  r.list <- c()
  
  # loop the masked rasters into the list vector
  for (i in 1:length(fileNames)){
    
    # read in raster
    y <- rast(paste0(filePath,fileNames[i]))
    # mask raster to river corridor
    y <- mask(y, project(maskLayer, y))
    
    
    if (is.rgb == TRUE){
      # Identify to columns as RGB
      names(y) <- 1:3
      RGB(y) <- 1:3
    }
    
    # append to list
    r.list <- c(r.list, y)
    
    # remove the raster
    rm(y)
  }
  
  # Merge the rasters
  rsrc <- sprc(r.list) # SpatRaster Collection
  r.mosaic <- merge(rsrc)
  
  r.mosaic <- crop(r.mosaic, project(maskLayer, r.mosaic),ext=TRUE)
  
  return(r.mosaic)
}
