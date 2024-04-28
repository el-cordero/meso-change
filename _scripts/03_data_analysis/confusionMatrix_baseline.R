source('_scripts/00_libraries.R')

# river mask
river <- vect('_data/GIS/Vector/Clean/river_corridor.shp')

mesohabitats <- rast('_data/GIS/Raster/Clean/mesohabitats.tif',lyrs=8:10)
km <- rast('_data/GIS/Raster/Analysis/Kmeans/km_planet_7.tif',lyrs=1)
rf <- rast('_data/GIS/Raster/Analysis/RandomForest/rf_planet_7.tif',lyrs=1)

# mask the rasters
river <- terra::project(river, crs(rf))
rf <- mask(rf, river)
km <- crop(km, rf)
km <- mask(km,river)

obs <- mask(mesohabitats$median_low, river)
obs <- terra::project(obs, rf, method='near')

obs <- mask(obs, river)
obs <- classify(obs,cbind(1:8,1))
obs <- classify(obs,cbind(NA,0))

obs <- mask(obs,river)

extent <- ext(c(1720603, 1736513, 308455.5, 321400))
obs <- crop(obs,extent)
km <- crop(km,obs)
rf <- crop(rf,obs)

# rf 
old_class <- c(1:5)
labs <- c(rep('not water',4),'water')
new_class <- c(rep(0,4),1) 
rcl <- data.frame(label=labs,old=old_class,new=new_class)
rcl <- rcl[order(rcl$new),]

rf <- classify(rf,rcl[c('old','new')])

rf_cats <- rcl[!duplicated(rcl$new),c('new','label')]
rf_cats <- na.omit(rf_cats)
levels(rf) <- rf_cats
names(rf) <- 'rf'

# km 
old_class <- c(1:6)
labs <- c(rep('not water',2),'water',rep('not water',3)) # water = 3
new_class <- c(rep(0,2),1,rep(0,3)) 
rcl <- data.frame(label=labs,old=old_class,new=new_class)
rcl <- rcl[order(rcl$new),]

km <- classify(km,rcl[c('old','new')])

km_cats <- rcl[!duplicated(rcl$new),c('new','label')]
levels(km) <- na.omit(km_cats)
names(km) <- 'km'

# water
obs_cats <- data.frame(new=0:1,label=c('not water','water'))
levels(obs) <- obs_cats
names(obs) <- 'obs'

df <- as.data.frame(c(rf,km,obs),na.rm=NA)

df$rf <- as.factor(df$rf); df$km <- as.factor(df$km); df$obs <- as.factor(df$obs)

cm_rf <- df %>% conf_mat(truth = obs, estimate = rf)
cm_km <- df %>% conf_mat(truth = obs, estimate = km)

cm_summary <- cbind(summary(cm_rf), summary(cm_km))
cm_summary <- cm_summary[,c(1,3,6)]
names(cm_summary) <- c('metric','estimate_rf','estimate_km')

cm <- data.frame(rbind(cm_rf$table,cm_km$table),row.names = NULL)
names(cm) <- c('not water', 'water')
cm$predictions <- rep(row.names(cm_rf$table),2)
cm$dataset <- c(rep('rf',2),rep('km',2))

meanCellSize <- cellSize(rf, mask=TRUE)
meanCellSize <- mean(values(meanCellSize),na.rm=TRUE)
cm[c(1:2)] <- cm[c(1:2)] * meanCellSize * 0.00024711

write.csv(cm,  '_data/Tables/confusion_matrix_baselines.csv',
          row.names = FALSE)
write.csv(cm_summary, '_data/Tables/confusion_matrix_baselines_summary.csv',
          row.names = FALSE)

rm(list=ls())
