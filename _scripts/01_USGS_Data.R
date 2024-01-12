source("~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_libraries.R")
source("~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_funcNormalise.R")

# https://help.waterdata.usgs.gov/parameter_cd?group_cd=PHY
# USGS 06887500 KANSAS R AT WAMEGO, KS
# USGS 06879100 KANSAS R AT FORT RILEY, KS
# USGS 06887000 Big Blue R NR Manhattan, KS
siteNo <- c("06887500", "06879100", "06887000")
pCode <- "00060" # discharge -  mean daily stream flow (ft3/s)
start.date <- "2021-11-20"
end.date <- "2023-12-15"

# extract the usgs gage data
usgsData <- readNWISuv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)

# tidy data
names(usgsData)[4:5] <- c('discharge','discharge_cd')
usgsData$Date <- as.Date(usgsData$dateTime)

# reduce data to average daily flow
usgsData <- usgsData %>%
  group_by(site_no,Date) %>%
  summarise(avgDischarge = mean(discharge, na.rm = TRUE))

# normalize the data
usgsData <- usgsData %>%
  mutate(dischargeNormalized = normalise(avgDischarge))

# calculate mean discharge over three sites
siteNo <- as.integer(siteNo)
columns <- c('Date','avgDischarge')
meanData <- usgsData[usgsData$site_no==siteNo[1],columns] %>%
  left_join(usgsData[usgsData$site_no==siteNo[2],columns], by=c("Date")) %>%
  left_join(usgsData[usgsData$site_no==siteNo[3],columns], by=c("Date")) 

names(meanData) <- c('Date',siteNo)
meanData$meanDischarge <- rowMeans(meanData[,-1])

# normalize mean discharge
meanData <- meanData %>%
  mutate(dischargeNormalized = normalise(meanDischarge))

# save data
write.csv(usgsData,'~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/USGS/usgsData.csv',
          row.names = FALSE)
write.csv(meanData,'~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/USGS/meanData.csv',
          row.names = FALSE)
