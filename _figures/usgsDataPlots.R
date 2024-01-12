usgs <- read.csv('~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/USGS/usgsData.csv')
meanData <- read.csv('~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/USGS/meanData.csv')

usgs$Date <- as.Date(usgs$Date)
meanData$Date <- as.Date(meanData$Date)

lbls <- c(`6879100` = 'Fort Riley',
          `6887000` = 'Manhattan',
          `6887500` = 'Wamego')

file.output <- '~/Documents/Projects/USACE/ML Mesohabitats/Documentation/Written/media/fig_usgsData.png'
png(filename=file.output, height=6,width=9,units="in",res=300)

ggplot(usgs, aes(Date, avgDischarge)) + 
  geom_line() + 
  geom_line(data = meanData, aes(Date,meanDischarge), col = "blue", 
            size = 0.2, show.legend = TRUE, linetype = 'dashed') + 
  facet_grid(site_no ~., labeller = as_labeller(lbls)) + 
  scale_x_date(date_breaks = "3 month", 
               labels=date_format("%b-%Y"),
               limits = as.Date(range(usgs$Date))) + 
  labs(x = "Date", y = "Mean Daily Stream Flow (cubic-ft/s)") +
  theme_classic()

# Close jpeg creation
invisible(dev.off())