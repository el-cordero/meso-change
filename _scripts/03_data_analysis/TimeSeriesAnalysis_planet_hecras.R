source('_scripts/00_libraries.R')
source('_scripts/01_functions/normalise.R')
source('_scripts/01_functions/time_series.r')

km <- rast('_data/GIS/Raster/Analysis/Kmeans/km_planet_hecras.tif')
mesohabitats <- rast('_data/GIS/Raster/Clean/mesohabitats.tif',lyrs=c(9,1:7))

dates <- gsub("^(\\d{2})_([A-Za-z]{3})(\\d{2})_(\\d{4})$", "\\2 \\3 \\4", names(km)[-1])
dates <- as.Date(dates, format = "%b %d %Y")
dates <- gsub("-", "", dates)

names(km) <- c(names(km)[1],dates)


km_df <- time_series(km)

lvls <- levels(mesohabitats)
mesohabitats <- classify(mesohabitats,cbind(4,3)) # remove the slow riffle bc
levels(mesohabitats) <- lvls

names(mesohabitats) <- names(km)
mesohabitats <- resample(mesohabitats,km)

meso_df <- time_series(mesohabitats)



km_df$dataset <- 'kmeans'; meso_df$dataset <- 'mesohabitats'
km_df$class <- as.character(km_df$class)

ml_DF <- rbind(km_df,meso_df)

comparison_DF <- full_join(
  ml_DF %>% 
    filter(dataset == 'kmeans') %>% 
    group_by(class) %>% 
    reframe(total_area = sum(area)),
  ml_DF %>% 
    filter(dataset == 'mesohabitats') %>% 
    group_by(class) %>% 
    reframe(total_area = sum(area)),
  by = 'class')
names(comparison_DF) <- c('class','kmeans_area','mesohabitats_area')

# save dataset
write.csv(ml_DF, '_data/Tables/resultsTimeSeries_planet_hecras.csv',
          row.names = FALSE)
write.csv(comparison_DF, '_data/Tables/comparisonArea_planet_hecras.csv',
          row.names = FALSE)

rm(list=ls())
