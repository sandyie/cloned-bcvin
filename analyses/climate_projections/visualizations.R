library(raster)
library(tidyverse)
library(tmap)
library(tmaptools)
library(leaflet)
library(rgdal)
####
#Haven't started yet
#script is mostly a placeholder
#############################################
####   Maps of BCVin   ####
#this is also the home for any other visualizations 

#### Groups of Climate Projections ####
#
# (1) Growing season thermal sum: GDD_05_10_* & GDD_04_09_*
# (2) Harvest climate: tmin_09_*, tmin_10_*, GDD_09_*, GDD_10_* 
# (3) Extreme heat in growing season: tmax_06_*, tmax_07_*, tmax_08_*, tmax_09_*
# (4) Extreme lows in dormancy to budburst period: tmin_03 & tmin_04, tmin_12_03

##converting BCvin20m from CDEM to the mask of bcvinshapefile
test <- raster("C:/Users/adamfong/Desktop/Ecology Lab/Climatebc_v630/bcvin20m/bcvin20m.asc")
shp <- readOGR("C:/Users/adamfong/Desktop/Ecology Lab/Climatebc_v630/bcvinshapefile/bcvin.shp")
bcvin75 <- raster("Bcvin/Data/bcvin_raster/bcvin_raster.asc")
latlong <- "+proj=longlat +datum=WGS84 +no_defs"
test2 <- projectRaster(test, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", res = 0.0002)

mask <- mask(test2, shp)

writeRaster(mask, "bcvin20m_mask.asc")

bcvin50 <- raster("final_climate_proj/bcvin_raster/bcvin50m/bcvin50m.asc")
bcvin50_2 <- projectRaster(bcvin50, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", res = 0.000449)
mask2 <- mask(bcvin50_2, shp)
writeRaster(mask2, "bcvin50m_mask.asc")

###########################################################
#Variation


