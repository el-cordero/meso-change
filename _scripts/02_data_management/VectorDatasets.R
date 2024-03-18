source('_scripts/00_libraries.R')

# load in the area of interest
aoi <- vect('_data/GIS/Vector/Clean/aoi.shp')

# load in the cleaned hydrography dataset containing waterbodies
# taken from "NHDPLUS_H_1027_HU4_GDB/NHDPLUS_H_1027_HU4_GDB.gdb"
# layer = "NHDPlusBurnWaterbody"
river <- vect('_data/GIS/Vector/Clean/river.shp')
river <- terra::project(river, crs(aoi))
river <- crop(river, aoi) # crop to aoi
river <- aggregate(river, dissolve = TRUE)
river$FID <- 1


# buffer the river to create a corridor
river.corridor <- buffer(river,30)
river.corridor <- crop(river.corridor, aoi)


# save the aoi, river, river corridor, sampling locations
writeVector(aoi,paste0(vector,'Clean/aoi.shp'),
            overwrite=TRUE)
writeVector(river,paste0(vector,'Clean/river_updated.shp'),
            overwrite=TRUE)
writeVector(river.corridor,'_data/GIS/Vector/Clean/river_corridor.shp',
            overwrite=TRUE)

