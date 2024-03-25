source('_scripts/00_libraries.R')

mesohabitats <- rast('_data/GIS/Raster/Clean/mesohabitats.tif',lyrs=1:7)
km <- rast('_data/GIS/Raster/Analysis/Kmeans/km_planet_hecras.tif',lyrs=2:8)

# check names
names(km) == names(mesohabitats)

old_class <- c(1:8)
labs <- c(
  'Raceway','Med Pool','Fast Rifle','Deep Pool',
  'Undefined','Shallow Pool','Slow Riffle','Shallow Pool'
  )
new_class <- c(6,2,5,3,99,1,4,1) # 99 is bridge crossings
rcl <- data.frame(label=labs,old=old_class,new=new_class)
rcl <- rcl[order(rcl$new),]

km <- classify(km,rcl[c('old','new')])

km_cats <- rcl[-duplicated(rcl$label),c('new','label')]
km <- categories(km, layer=0, rep(list(km_cats),nlyr(km)))
names(km) <- names(mesohabitats)

meso_median <- median(mesohabitats,na.rm = TRUE)
vals <- unique(meso_median)
round(vals)

# km <- terra::project(km,mesohabitats,method='near')
mesohabitats <- terra::project(mesohabitats,km,method='near')


df <- as.data.frame(c(km,mesohabitats),na.rm=NA,cells=TRUE)
km_df <- df[,c(1,2:8)]; meso_df <- df[,c(1,9:15)]
rm(df)





rm(list=ls())