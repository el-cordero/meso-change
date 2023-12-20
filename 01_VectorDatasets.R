source("~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_libraries.R")

# set path names
vector <- "~/Documents/Projects/USACE/NextGen IPR/Data/GIS/Kansas/Vector/"

# load in the area of interest
aoi <- vect(paste0(vector, 'Confluence_AOI.shp'))
aoi <- project(aoi,crs)

# load in the cleaned hydrography dataset containing waterbodies
# taken from "NHDPLUS_H_1027_HU4_GDB/NHDPLUS_H_1027_HU4_GDB.gdb"
# layer = "NHDPlusBurnWaterbody"
river <- vect(paste0(vector, 'Clean/river.shp'))
river <- project(river, crs)
river <- crop(river, aoi) # crop to aoi
river <- aggregate(river, dissolve = TRUE)
river$FID <- 1


# buffer the river to create a corridor
river.corridor <- buffer(river,100)
river.corridor <- crop(river.corridor, aoi)


# save the aoi, river, river corridor, sampling locations
writeVector(aoi,paste0(vector,'Clean/aoi.shp'),
            overwrite=TRUE)
writeVector(river,paste0(vector,'Clean/river_updated.shp'),
            overwrite=TRUE)
writeVector(river.corridor,paste0(vector,'Clean/river_corridor.shp'),
            overwrite=TRUE)

