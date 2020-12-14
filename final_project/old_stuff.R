require(tiff)
require(rgdal)
require(raster)
setwd(choose.dir())
source("helper_functions_final.R")
file_location <- "CMS_Pantropical_Forest_Biomass_1337/data/AGLB_Deforested_Tropical_Asia_2000.tif"
mangrove.file_location <-"CMS_Mangrove_Cover_1670/data/mangrove_cover_2000-2016_CanGio.tif"
mangrove.raster_image <- raster(mangrove.file_location)

#------------------------------------------------
# Metadata info
#------------------------------------------------
GDALinfo(file_location)
crs(raw_asia_tiff) 
nlayers(raw_asia_tiff)


#------------------------------------------------
# Plotting
#------------------------------------------------
plot(raw_asia_tiff, main = "Deforestation 2000 Asia")
plot(mangrove.raster_image)


# index is NDVI
# https://www.youtube.com/watch?v=jvWCKfctXSs&t=113s&ab_channel=ThilankaMunasinghe
