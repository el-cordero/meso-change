source('_scripts/00_libraries.R')

mesohabitats <- rast('_data/GIS/Raster/Clean/mesohabitats.tif',lyrs=8:10)
km <- rast('_data/GIS/Raster/Analysis/Kmeans/km_planet_hecras.tif',lyrs=1)

# old_class <- c(1:8)
# labs <- c(
#   'Raceway','Med Pool','Fast Riffle','Deep Pool',
#   'Undefined','Shallow Pool','Slow Riffle','Shallow Pool'
#   )
# new_class <- c(6,2,5,3,99,1,4,1) # 99 is bridge crossings
# rcl <- data.frame(label=labs,old=old_class,new=new_class)
# rcl <- rcl[order(rcl$new),]

old_class <- c(1:8)
labs <- c(
  'Faster than a Raceway','Fast Riffle','Raceway','Fast Riffle',
  rep('Raceway',4)
  )
new_class <- c(1,2,3,2,3,3,3,3) # 99 is bridge crossings
rcl <- data.frame(label=labs,old=old_class,new=new_class)
rcl <- rcl[order(rcl$new),]

km <- classify(km,rcl[c('old','new')])

km_cats <- rcl[!duplicated(rcl$new),c('new','label')]
km_cats <- na.omit(km_cats)
# km <- categories(km, layer=0, rep(list(km_cats),nlyr(km)))
levels(km) <- km_cats
names(km) <- 'km'
# km <- terra::project(km,mesohabitats,method='near')
mesohabitats <- terra::project(mesohabitats,km,method='near')

df <- as.data.frame(c(km,mesohabitats),na.rm=NA)

all_levels <- union(levels(df$km), levels(df$median_low))
df$km <- factor(df$km, levels = all_levels)
df$median_low <- factor(df$median_low, levels = all_levels)
df$median_high <- factor(df$median_high, levels = all_levels)

df %>% conf_mat(truth = median_low, estimate = km)
df %>% conf_mat(truth = median_high, estimate = km)






rm(list=ls())