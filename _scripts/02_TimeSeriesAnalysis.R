source("~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_libraries.R")
source("~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_funcNormalise.R")

## filepaths
raster <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Raster/"

# read in raster stack
r <- rast(paste0(raster,'Analysis/Kmeans/kmeansStack05.tif'))

# extract dates
sample.Dates <- sub(".*\\_", "", names(r)) 
sample.Dates <- gsub("^(.{4})(.*)$","\\1-\\2", sample.Dates)
sample.Dates <- gsub("^(.{7})(.*)$","\\1-\\2", sample.Dates)
dateRange <- c(sample.Dates[1],tail(sample.Dates,1))

# rename columns
names(r) <- sample.Dates

# review plots
for (i in 1:length(sample.Dates)){plot(r[[i]], main = sample.Dates[i])}

# relabel according to your preference
classDefs <- data.frame(Class = 1:5,
                        Label = c('Water','Sand/Mixed','Mixed',
                                  'Forest','Sand'),
                        newClass = c(1,3,4,5,2)) 

# reclassify according to new class labels
r <- classify(r, classDefs[,c('Class','newClass')])

# review plots again
for (i in 1:length(sample.Dates)){plot(r[[i]], main = sample.Dates[i])}

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

# Calculate changes over time
# change in area from one date to the following date
# percent difference in area
df <- df %>%
  mutate(area_diff = area - lag(area)) %>% 
  mutate(diff_percent = area_diff * 100 / lag(area) ) %>% 
  mutate(diff_normalized = normalise(diff_percent))

# save dataset
write.csv(df, '~/Documents/Projects/USACE/ML Mesohabitats/Data/Tables/resultsTimeSeries.csv',
          row.names = FALSE)

