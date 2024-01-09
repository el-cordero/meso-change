### Raw Landsat Image

# load libraries 
library(terra)

# set path names
raster <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Raster/"
vector <- "~/Documents/Projects/USACE/NextGen IPR/Data/GIS/Kansas/Vector/"

r <- rast(paste0(raster,"Landsat/Raw/LC09_027033_20211121.tif"))

# inputed user polygons with observations
userData <- vect(paste0(vector,'userDelineated.shp'))
userData <- terra::project(userData, crs(r))
userData <- crop(userData, r, ext=TRUE)


file.output <- '~/Documents/Projects/USACE/ML Mesohabitats/Documentation/Written/media/fig_Training_polygons.png'
png(filename=file.output, height=4,width=6,units="in",res=300)

par(mfrow=c(1,1))

plotRGB(r, r=4, g=3, b=2, stretch='hist',main="True Color")
plot(userData, 'class',add=TRUE)

# Close jpeg creation
invisible(dev.off())
