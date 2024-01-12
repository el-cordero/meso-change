library(ggplot2)
library(scales)
library(ggpubr)

# save dataset
mlDF <- read.csv('~/Documents/Projects/USACE/ML Mesohabitats/Data/Tables/resultsTimeSeries.csv')
# meanData <- read.csv('~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/USGS/meanData.csv')

mlDF$date <- as.Date(mlDF$date)
# meanData$Date <- as.Date(meanData$Date)
dateRange <- range(mlDF$date)

mlDF$zero <- 0

file.output <- '~/Documents/Projects/USACE/ML Mesohabitats/Documentation/Written/media/fig_areaTimeSeries.png'

png(filename=file.output, height=7,width=16,units="in",res=400)

a <- mlDF %>% filter(class != 'mixed' & class != 'urban') %>%
  ggplot(aes(date, area, color=method)) + 
  geom_line(size = 0.5) + 
  facet_grid(class ~ .) +
  geom_point(size = 0.8) +
  scale_x_date(date_breaks = "6 month",
               labels=date_format("%b-%Y"),
               limits = dateRange) +
  labs(title= "",x = "Date", y = "Total Area (acres)") +
  theme_classic() +
  theme(legend.position="none")

b <- mlDF %>% filter(class != 'mixed' & class != 'urban') %>%
  ggplot(aes(method, area_diff, color=method)) + 
  geom_boxplot() + 
  facet_grid(class ~ .)  +
  labs(title= "",x = "Method", y = "Change in Area (acres) - Between Dates") +
  theme_classic() +
  theme(legend.position="none")

c <- mlDF %>% filter(class != 'mixed' & class != 'urban') %>%
  ggplot(aes(method, area_diff_baseline, color=method)) + 
  geom_boxplot() + 
  facet_grid(class ~ .)  +
  labs(title= "",x = "Method", y = "Change in Area (acres) - Baseline v. Instance ") +
  theme_classic() +
  theme(legend.position="none")

pl <- ggarrange(a,b,c, font.label=0.9, labels=letters[1:3],
                common.legend = TRUE,
                ncol = 3)
pl

invisible(dev.off())


# > names(mlDF)
# [1] "date"                     "class"                   
# [3] "n"                        "n_percent"               
# [5] "area"                     "n_baseline"              
# [7] "n_percent_baseline"       "area_baseline"           
# [9] "area_diff"                "diff_percent"            
# [11] "diff_normalized"          "area_diff_baseline"      
# [13] "diff_percent_baseline"    "diff_normalized_baseline"
# [15] "method"  


# b <- mlDF %>% filter(class != 'mixed' & class != 'urban') %>%
#   ggplot(aes(date, area_diff, color=method)) + 
#   geom_line(size = 0.5) + 
#   facet_grid(class ~ .) +
#   geom_point(size = 0.8) +
#   geom_line(aes(date,zero), col = "black",
#             size = 0.1, show.legend = FALSE) +
#   scale_x_date(date_breaks = "6 month",
#                labels=date_format("%b-%Y"),
#                limits = dateRange) +
#   labs(title = "", x = "Date", y = "Change in Area (acres) - Between Dates") +
#   theme_classic() +
#   theme(legend.position="top")
# 
# c <- mlDF %>% filter(class != 'mixed' & class != 'urban') %>%
#   ggplot(aes(date, area_diff_baseline, color=method)) + 
#   geom_line(size = 0.5) + 
#   facet_grid(class ~ .) +
#   geom_point(size = 0.8) +
#   geom_line(aes(date,zero), col = "black",
#             size = 0.1, show.legend = FALSE) +
#   scale_x_date(date_breaks = "6 month",
#                labels=date_format("%b-%Y"),
#                limits = dateRange) +
#   labs(title = "", x = "Date", y = "Change in Area (acres) - Baseline v. Instance ") +
#   theme_classic() +
#   theme(legend.position="none")