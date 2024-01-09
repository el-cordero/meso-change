### Raw Landsat Image

# load libraries 
library(terra)

# set path names
raster <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Raster/"

r <- rast(paste0(raster,"Landsat/Raw/LC09_027033_20211121.tif"))

# band.names <- c('Band 1','Band 2','Band 3','Band 4',
#                'Band 5','Band 6','Band 7')

band.names <- c("Coastal Aerosol", "Blue", "Green", "Red", "Near Infrared",
"Shortwave Infrared", "Shortwave Infrared")

file.output <- '~/Documents/Projects/USACE/ML Mesohabitats/Documentation/Written/media/fig_landsat_bands.png'
png(filename=file.output, height=5,width=12,units="in",res=300)

par(mfrow=c(2,4), mar = c(0.1, 0.1, 0.1, 0.1))

plotRGB(r, r=4, g=3, b=2, stretch='hist', main="True Color")

for (i in 1:length(names(r))){
  plot(r[[i]], main = band.names[i], axes = FALSE)
}

# Close jpeg creation
invisible(dev.off())
