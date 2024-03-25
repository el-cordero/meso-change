source('_scripts/00_libraries.R')

mesohabitats <- rast('_data/GIS/Raster/Clean/mesohabitats.tif',lyrs=8:10)
km <- rast('_data/GIS/Raster/Analysis/Kmeans/km_planet_hecras.tif',lyrs=1)

names(km) <- 'km'

mesohabitats <- terra::project(mesohabitats,km,method='near')

df <- as.data.frame(c(km,mesohabitats),na.rm=NA)


km_summary_low <- df %>% 
    group_by(km, median_low) %>% 
    summarize(n = n()) %>%
    arrange(km,desc(n)) %>%
    na.omit()

# km_summary_high <- df %>% 
#     group_by(km, median_high) %>% 
#     summarize(n = n()) %>%
#     arrange(km,desc(n)) %>%
#     na.omit()

# km_summary <- cbind(km_summary_low,km_summary_high)
# km_summary <- km_summary[-4] # check table before doing this
# names(km_summary) <- c('km','mesohabitat_low','n_median_low','mesohabitat_high','n_median_high')
    
write.csv(km_summary_low,'_data/Tables/kmeans_cluster_interpretation_full_table.csv',row.names=FALSE)

median_low <- df %>% 
    group_by(km, median_low) %>% 
    summarize(n = n()) %>%
    arrange(km,desc(n)) %>%
    na.omit() %>%
    group_by(km) %>%
    filter(n == max(n))

median_high <- df %>% 
    group_by(km, median_high) %>% 
    summarize(n = n()) %>%
    arrange(km,desc(n)) %>%
    na.omit() %>%
    group_by(km) %>%
    filter(n == max(n))

median <- cbind(median_high,median_low[2:3])
median <- median[-4] # check table before doing this
names(median) <- c('km','mesohabitat','n_median_low','n_median_high')
write.csv(median,'_data/Tables/kmeans_cluster_interpretation.csv',row.names=FALSE)


