#source("C:/Users/adamfong/Desktop/Ecology Lab/R/bcvin_git/analyses/climate_projections/functions_externaldrive.R")#in a different directory than the data
source("C:/Ecology Lab/R/bcvin_git/bcvin/analyses/climate_projections/functions_externaldrive.R")

library(raster)
library(tidyverse)
library(tmap)
library(tmaptools)
library(rgdal)
library(leaflet)
library(htmlwidgets)

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

variable_list <- list.files("E:/climate_projection_data/bcvin_raster/anomaly", pattern = ".asc")
setwd("E:/climate_projection_data/bcvin_raster/anomaly")

mw_hw_objects <- lapply(variable_list, raster)
mw_hw_objects_names <- substr(variable_list, 0 , length(variable_list)) %>%
  lapply(. , str_remove, pattern = ".asc") %>%
  unlist()

for ( i in 1:length(mw_hw_objects_names)){
  assign(
    x = paste(mw_hw_objects_names[i]),
    value = mw_hw_objects[[i]]
  )
}

sd_list <- list.files("E:/climate_projection_data/bcvin_raster/combined", pattern = c("sd", "cv"))
setwd("E:/climate_projection_data/bcvin_raster/combined")

sd_objects <- lapply(sd_list, raster)
sd_objects_names <- substr(sd_list, 0, length(sd_list)) %>%
  lapply(. , str_remove, pattern = ".asc") %>%
  unlist()

for ( i in 1:length(sd_objects_names)){
  assign(
    x = paste(sd_objects_names[i]),
    value = sd_objects[[i]]
  )
}


#####DIFFERENT STYLES OF MAPS#####

#This map displays the values and standard deviations for both warming scenarios. Every layer can be toggled on and off
#Pros: Can interact with the map. The minimap adds the benefit of context. Can measure distances between two points. Can change the basemaps between imagery and topographic maps.
#Cons: Can't view SD & values simultaneously. UI is clunky for the layers as you have to turn off a layer and turn on a layer, otherwise two layers overlap. No main title (can just title the portion on the website).  

palette_tmin_12_hw <- colorNumeric("Blues", domain = seq(6, 7.5, by = 0.5), na.color = "transparent")
palette_tmin_12_mw <- colorNumeric("Blues", domain = seq(3.5, 5, by = 0.5), na.color = "transparent")

palette_tmin_12_hw_sd <- colorNumeric("Greys", domain = seq(.8, 1.1, by = 0.1),
                         na.color = "transparent", reverse = TRUE)
palette_tmin_12_mw_sd <- colorNumeric("Greys", domain = seq(.5, .8, by = 0.1),
                                      na.color = "transparent", reverse = TRUE)

crs(tmin_12_hw_anomaly) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(tmin_12_mw_anomaly) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(tmin_12_hw_combined_sd) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(tmin_12_mw_combined_sd) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


tmin_12 <- leaflet()%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery")%>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri World Topographic") %>%
  addRasterImage(tmin_12_hw_anomaly, colors= palette_tmin_12_hw, group = "High Warming", opacity = .8) %>%
  addRasterImage(tmin_12_mw_anomaly, colors= palette_tmin_12_mw, group = "Moderate Warming", opacity = .8) %>%
  addRasterImage(tmin_12_hw_combined_sd, colors = palette_tmin_12_hw_sd, group = "Standard Deviation High Warming", opacity = .8 )%>%
  addRasterImage(tmin_12_mw_combined_sd, colors = palette_tmin_12_mw_sd, group = "Standard Deviation Moderate Warming", opacity = .8 )%>%
  addLayersControl(baseGroups = c("Esri World Imagery", "Esri World Topographic"),
                   overlayGroups = c("High Warming", "Moderate Warming", "Standard Deviation High Warming", "Standard Deviation Moderate Warming"))%>%
  addLegend(position = "topright", pal = palette_tmin_12_hw, title = "Temperature (degrees C)", group = "High Warming", values = seq(6, 7.5, by = 0.5))%>%
  addLegend(position = "topright", pal = palette_tmin_12_mw, title = "Temperature (degrees C)", group = "Moderate Warming", values = seq(3.5, 5, by = 0.5))%>%
  addLegend(position = "topright", pal = palette_tmin_12_hw_sd, title = "Standard Deviation", group = "Standard Deviation High Warming", values = seq(.8, 1.1, by = 0.1))%>%
  addLegend(position = "topright", pal = palette_tmin_12_mw_sd, title = "Standard Deviation", group = "Standard Deviation Moderate Warming", values = seq(.5, .8, by = 0.1))%>%
  addMeasure(
    position = "topleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479") %>%
  addMiniMap(position = "bottomleft", toggleDisplay = TRUE) %>%
  hideGroup("Standard Deviation Moderate Warming") %>%
  hideGroup("Standard Deviation High Warming") %>%
  hideGroup("Moderate Warming")

saveWidget(tmin_12, "interactive_tmin_12_example.html", selfcontained = TRUE)

##Static comparison map
##Pros: Can view multiple maps simultaneously. No UI. Simple and clean.
##Cons: No context of location (I'm yet to find a good basemap that is free and a fine resolution). Scales are different (Hard to compare differences between Moderate and Hig warming scenarios). 

tmin_stack <- stack(tmin_12_mw_anomaly, tmin_12_mw_combined_sd, tmin_12_hw_anomaly, tmin_12_hw_combined_sd)


static_comparison <- tmap_mode('plot') +
  tm_shape(tmin_stack) + 
  tm_raster(palette = list(get_brewer_pal("Blues"),get_brewer_pal("Greys"), get_brewer_pal("Blues"), get_brewer_pal("Greys")))+
  tm_facets(ncol = 4, free.scales = TRUE) +
  tm_layout(legend.title.color = "white", 
            legend.title.size = .001, 
            title = c("Temperature °C", "Standard Deviation", "Temperature °C", "Standard Deviation"), 
            title.size = .9,
            panel.labels = c("Moderate Warming", "***Moderate Warming SD***", "High Warming", "***HW Standard Devation***"), 
            panel.label.size = 1.1,
            main.title = "Mean Minimum Temperature Anomalies in December", 
            main.title.fontface = 2,
            compass.type = "arrow", attr.outside = TRUE, legend.outside = FALSE)+
  tm_scale_bar(position = c("RIGHT", "TOP"))+
  tm_compass(position = c("LEFT", "TOP")) 



##Static Comparison 2 sets of small multiples
##Pros: Easily compare between maps because they have the same breaks in legends
##Cons: May have an odd layout in the website. Both groups side by side? One above the other? Text inbetween? 


test_stack <- stack(tmin_12_mw_anomaly, tmin_12_hw_anomaly)
test_stack_sd <- stack(tmin_12_mw_combined_sd, tmin_12_hw_combined_sd)

static_warmingScenario_comparison <- tmap_mode('plot') +
  tm_shape(test_stack) + 
  tm_raster(palette = get_brewer_pal("Blues"), breaks = seq(3.5, 8, by = 0.5))+
  tm_facets(ncol = 2, free.scales.raster = FALSE) +
  tm_layout(panel.labels = c("Moderate Warming", "High Warming"), 
            panel.label.size = 1.1,
            legend.title.color = "white", 
            legend.title.size = .001, 
            title = "Temperature °C", main.title = "Mean Minimum Temperature Anomalies in December", 
            compass.type = "arrow", 
            attr.outside = TRUE, 
            legend.outside = TRUE)+
  tm_scale_bar(position = c("RIGHT", "TOP"))+
  tm_compass(position = c("LEFT", "TOP"))

static_warmingScenario_comparison_sd <- tmap_mode('plot') +
  tm_shape(test_stack_sd) + 
  tm_raster(palette = get_brewer_pal("Greys"), breaks = seq(0.5, 1.2, by = 0.1))+
  tm_facets(ncol = 2, free.scales.raster = FALSE) +
  tm_layout(panel.labels = c("Moderate Warming", "High Warming"), 
            panel.label.size = 1.1,
            legend.title.color = "white", 
            legend.title.size = .001, 
            title = "Standard Deviation", 
            main.title = "Mean Minimum Temperature Anomalies in December", 
            main.title.fontface = 2,
            compass.type = "arrow", 
            attr.outside = TRUE, 
            legend.outside = TRUE)+
  tm_scale_bar(position = c("RIGHT", "TOP"))+
  tm_compass(position = c("LEFT", "TOP"))


##Static Histogram Legend
##Pros: The legend has a histogram which displays the count for each legend category. It may be nice to display precipitation with different members this way. 
##Cons:Not very useful if the data is mostly normal 

static_histogram_legend <- tmap_mode("plot") +
  tm_shape(ppt_09_mw_r5)+
  tm_raster(palette = get_brewer_pal("Blues"), legend.hist = TRUE, title = "Precipitation Change (mm)") +
  tm_layout(main.title = "High Warming Precipitation Anomalies in September", 
            attr.outside = TRUE, 
            legend.outside = TRUE,
            legend.hist.width = 1.5)+
  tm_scale_bar(position = c("RIGHT", "TOP"))+
  tm_compass(position = c("LEFT", "TOP"))

###########################################################
#Variation

# (1) Histograms of PPT_09, PPT_10

open_members("ppt_09_mw")
open_members("ppt_10_mw")
open_members("ppt_09_hw")
open_members("ppt_10_hw")

plot_histogram_allmembers("ppt_09_mw", "stack")
plot_histogram_allmembers("ppt_09_mw", "facet")
plot_histogram_allmembers("ppt_09_mw", "density")

plot_histogram_allmembers("ppt_10_mw", "stack")
plot_histogram_allmembers("ppt_10_mw", "facet")
plot_histogram_allmembers("ppt_10_mw", "density")

plot_histogram_allmembers("ppt_09_hw", "stack")
plot_histogram_allmembers("ppt_09_hw", "facet")
plot_histogram_allmembers("ppt_09_hw", "density")

plot_histogram_allmembers("ppt_10_hw", "stack")
plot_histogram_allmembers("ppt_10_hw", "facet")
plot_histogram_allmembers("ppt_10_hw", "density")
