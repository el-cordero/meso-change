# baseline rasters
# load libraries 
library(terra)

## filepaths
raster <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Raster/"

# read in raster stacks
kmRaster <- rast(paste0(raster,'Analysis/Kmeans/combined/kmeansStack05.tif'))
rfRaster <- rast(paste0(raster,'Analysis/RandomForest/combined/rfStack.tif'))

r <- c(kmRaster[[1]],rfRaster[[1]])

lcKm <- c('cropland', 'bareland', 'mixed', 'forest', 'water')
colorsKm <- c('#cfd186', '#CD533B','#9EB3C2', '#C2EABD','#4b6ab4')
coltbKm <- data.frame(value=1:5, colorsKm)
coltab(r[[1]]) <- coltbKm

lcRf <- unique(r[[2]]) # print and verify
lcRf <- c("bareland", "cropland", "forest",   "urban", "water")
colorsRf <- c('#CD533B','#cfd186','#C2EABD', '#a65997','#4b6ab4')
coltbRf <- data.frame(value=1:5, colorsRf)
coltab(r[[2]]) <- coltbRf

names(r) <- c('(a) k-means', '(b) random forest')




lc <- c('cropland', 'bareland', 'mixed',  "urban", 'forest', 'water')
colors <- c('#cfd186', '#CD533B','#9EB3C2', '#a65997', '#C2EABD','#4b6ab4')


file.output <- '~/Documents/Projects/USACE/ML Mesohabitats/Documentation/Written/media/fig_baselineRasters.png'
png(filename=file.output, height=4,width=6.5,units="in",res=300)

par(mfrow=c(1,2), mar = c(0.1, 0.1, 0.1, 0.1))

# plotRGB(r, r=4, g=3, b=2, stretch='hist', main="True Color")

# Add legend
plot(r, axes = FALSE, legend=FALSE)
legend("left", legend=lc, box.lwd = 0, 
       lwd=NA, col=NA,  border=NA, fill=colors, bg=NA, 
       cex=0.7,xjust=0,yjust=0,x.intersp=-1.8)


# Close jpeg creation
invisible(dev.off())
