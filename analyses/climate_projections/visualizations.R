#source("C:/Users/adamfong/Desktop/Ecology Lab/R/bcvin_git/analyses/climate_projections/functions_externaldrive.R")#in a different directory than the data
source("C:/Ecology Lab/R/bcvin_git/bcvin/analyses/climate_projections/functions_externaldrive.R")

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

#Notes:
# * using a translucent pattern / crosshatch to display SD in categories (https://github.com/mtennekes/tmap/issues/49) <- at the very bottom
# * to categorize well, make sure to do this in a dataframe and make conditionals from there (https://cran.r-project.org/web/packages/spup/vignettes/DEM_v3.html)




test <-
  tm_view(alpha = 1, basemaps = "Esri.WorldTopoMap")+
  tm_shape(tmin_02_2087)+
  tm_raster(alpha = .8, title = "February 2087", palette = get_brewer_pal("Blues")) +
  tm_scale_bar() +
  tm_minimap() + 
  tm_layout(title = "Mean Minimum Temperature")

testbm <- tm_basemap("Esri.WorldImagery")



###########################################################
#Variation

# (1) Histograms of PPT_09, PPT_10

plot_histogram_allmembers("ppt_09")
