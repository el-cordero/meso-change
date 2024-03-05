source("~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_libraries.R")
source("~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_funcNormalise.R")

## filepaths
raster <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Raster/"

# read in raster stacks
kmRaster <- rast(paste0(raster,'Analysis/Kmeans/combined/kmeansStack05.tif'))
rfRaster <- rast(paste0(raster,'Analysis/RandomForest/combined/rfStack.tif'))

# mask the randomForest Raster
rfRaster <- mask(rfRaster, kmRaster)

names(rfRaster) <- names(kmRaster)

# for (i in 2:length(names(kmRaster))){plot(kmRaster[[i]], main = names(kmRaster)[i])}
for (i in 2:length(names(rfRaster))){plot(rfRaster[[i]], main = names(rfRaster)[i])}

timeSeries <- function(r){
    # extract dates
  
  sample.Dates <- sub(".*\\_", "", names(r)) 
  sample.Dates <- gsub("^(.{4})(.*)$","\\1-\\2", sample.Dates)
  sample.Dates <- gsub("^(.{7})(.*)$","\\1-\\2", sample.Dates)
  
  # rename columns
  # give 'all_dates' a unique date name for now
  names(r) <- sample.Dates
  names(r) <- c('2021-11-01',sample.Dates[-1])

  # extract raster as a dataframe
  df <- as.data.frame(r)
  
  # get the counts of each class for each column
  df <- map(names(df), ~ df %>% 
             dplyr::select(all_of(.x)) %>% 
             count(!! rlang::sym(.x)))
  
  # tidy the dataset
  for (x in 1:length(df)){
    df[[x]]$date <- as.Date(names(df[[x]])[1])
    names(df[[x]])[1] <- 'class'
    df[[x]]$n_percent <- 100 * df[[x]]$n / sum(df[[x]]$n)
    
    # shift columns date, class, n, n_percent
    df[[x]] <- df[[x]][,c(3,1,2,4)]
  }
  
  # row bind the list elements into one dataframe
  df <- do.call("rbind", df)
  
  # calculate the average cell size and area per class
  meanCellSize <- cellSize(r, mask=TRUE)
  meanCellSize <- mean(values(meanCellSize),na.rm=TRUE)
  df$area <- df$n * meanCellSize * 0.00024711 # sq meters to Acre
  
  # tidy data
  df <- df %>% 
    filter(!is.na(class)) %>% # remove NA 
    arrange(class, date) %>% 
    group_by(class) 
  

  dfAlldates <-   df %>% filter(date == '2021-11-01') 
  n <- length(names(r)) - 1
  dfAlldates <- do.call("rbind", replicate(n, dfAlldates, simplify = FALSE))
  dfAlldates <- dfAlldates %>% arrange(class) %>% group_by(class)
  
  # remove dummy date as a seperate dataframe and add the columns
  df <- df %>% filter(date != '2021-11-01' ) 
  df[,c('n_baseline','n_percent_baseline','area_baseline')] <- 
    dfAlldates[,c('n','n_percent','area')]
  
  # Calculate changes over time
  # change in area from one date to the following date
  # percent difference in area
  df <- df %>% 
    mutate(area_diff = area - lag(area)) %>%
    mutate(diff_percent = area_diff * 100 / lag(area) ) %>%
    mutate(diff_normalized = normalise(diff_percent)) %>%
    mutate(area_diff_baseline = area - area_baseline) %>%
    mutate(diff_percent_baseline = area_diff_baseline * 100 / lag(area)) %>%
    mutate(diff_normalized_baseline = normalise(diff_percent_baseline))
  
  return(df)
}

kmDF <- timeSeries(kmRaster)
rfDF <- timeSeries(rfRaster)

kmDF$class <- c(rep('cropland',22), rep('bareland',22),rep('mixed',22),
  rep('forest',22),rep('water',22))

kmDF$method <- 'kmeans'; rfDF$method <- 'randomforest'

mlDF <- rbind(kmDF,rfDF)

comparisonDF <- full_join(
  mlDF %>% filter(method == 'kmeans') %>% group_by(class) %>% reframe(total_area = sum(area)),
  mlDF %>% filter(method == 'randomforest') %>% group_by(class) %>% reframe(total_area = sum(area)),
          by = 'class')
names(comparisonDF) <- c('class','kmeans_area','randomforest_area')

# save dataset
write.csv(mlDF, '~/Documents/Projects/USACE/ML Mesohabitats/Data/Tables/resultsTimeSeries.csv',
          row.names = FALSE)
write.csv(comparisonDF, '~/Documents/Projects/USACE/ML Mesohabitats/Data/Tables/comparisonArea.csv',
          row.names = FALSE)




