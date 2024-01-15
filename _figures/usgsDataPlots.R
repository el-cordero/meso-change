usgs <- read.csv('~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/USGS/usgsData.csv')
meanData <- read.csv('~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/USGS/meanData.csv')

usgs$Date <- as.Date(usgs$Date)
meanData$Date <- as.Date(meanData$Date)

lbls <- c(`6879100` = 'Fort Riley',
          `6887000` = 'Manhattan',
          `6887500` = 'Wamego')

dates <- data.frame(cbind(c("2021-11-21", "2022-01-11", "2022-02-12", "2022-04-01", "2022-06-20",
  "2022-07-22", "2022-08-07", "2022-08-23", "2022-09-08", "2022-09-24",
  "2022-10-10", "2022-11-11", "2022-12-29", "2023-03-03", "2023-03-19",
  "2023-04-04", "2023-05-06", "2023-06-07", "2023-07-09", "2023-08-10",
  "2023-09-27", "2023-11-14"),1))
names(dates) <- c('date','value')
dates$date <- as.Date(dates$date)

dates <- c("2021-11-21", "2022-01-11", "2022-02-12", "2022-04-01", "2022-06-20",
  "2022-07-22", "2022-08-07", "2022-08-23", "2022-09-08", "2022-09-24",
  "2022-10-10", "2022-11-11", "2022-12-29", "2023-03-03", "2023-03-19",
  "2023-04-04", "2023-05-06", "2023-06-07", "2023-07-09", "2023-08-10",
  "2023-09-27", "2023-11-14")
usgs$sampleDate <- NULL
dates <- usgs[usgs$Date %in% dates,]
dates$zero <- 0



file.output <- '~/Documents/Projects/USACE/ML Mesohabitats/Documentation/Written/media/fig_usgsData.png'
png(filename=file.output, height=6,width=9,units="in",res=300)

ggplot(usgs, aes(Date, avgDischarge)) + 
  geom_line() + 
  geom_line(data = meanData, aes(Date,meanDischarge), col = "blue", 
            size = 0.2, show.legend = TRUE, linetype = 'dashed') + 
  # geom_line(data = dates, aes(Date,zero), col = "black", 
  #           size = 0.1) +
  geom_point(data = dates, aes(Date,zero), col = "black", 
             size = 0.5) + 
  facet_grid(site_no ~., labeller = as_labeller(lbls)) +
  scale_x_date(date_breaks = "3 month", 
               labels=date_format("%b-%Y"),
               limits = as.Date(range(usgs$Date))) + 
  labs(x = "Date", y = "Mean Daily Stream Flow (cubic-ft/s)") +
  theme_classic()

# Close jpeg creation
invisible(dev.off())

dates <- c("2021-11-21", "2022-01-11", "2022-02-12", "2022-04-01", "2022-06-20",
           "2022-07-22", "2022-08-07", "2022-08-23", "2022-09-08", "2022-09-24",
           "2022-10-10", "2022-11-11", "2022-12-29", "2023-03-03", "2023-03-19",
           "2023-04-04", "2023-05-06", "2023-06-07", "2023-07-09", "2023-08-10",
           "2023-09-27", "2023-11-14")
write.csv(dates, '~/Documents/Projects/USACE/ML Mesohabitats/Data/Tables/samplingdates.csv',
          row.names = FALSE)
