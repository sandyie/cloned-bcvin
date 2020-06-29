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
# (2) Harvest climate: tmin_09_*, tmin_10_*, ppt_09_*, ppt_10_* 
# (3) Extreme heat in growing season: tmax_06_*, tmax_07_*, tmax_08_*, tmax_09_*
# (4) Extreme lows in dormancy to budburst period: tmin_03 & tmin_04, tmin_12_03


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

sd_list <- list.files("E:/climate_projection_data/bcvin_raster/combined", pattern = c("sd|cv"))
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

tmin_12 <- {leaflet()%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery")%>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri World Topographic") %>%
  addRasterImage(tmin_12_hw_anomaly, colors= palette_tmin_12_hw, group = "High Warming", opacity = .8) %>%
  addRasterImage(tmin_12_mw_anomaly, colors= palette_tmin_12_mw, group = "Moderate Warming", opacity = .8) %>%
  addRasterImage(tmin_12_hw_combined_sd, colors = palette_tmin_12_hw_sd, group = "Standard Deviation High Warming", opacity = .8 )%>%
  addRasterImage(tmin_12_mw_combined_sd, colors = palette_tmin_12_mw_sd, group = "Standard Deviation Moderate Warming", opacity = .8 )%>%
  addLayersControl(baseGroups = c("Esri World Imagery", "Esri World Topographic"),
                   overlayGroups = c("High Warming", "Moderate Warming", "Standard Deviation High Warming", "Standard Deviation Moderate Warming"))%>%
  addLegend(position = "topright", pal = palette_tmin_12_hw, title = "Temperature (°C)", group = "High Warming", values = seq(6, 7.5, by = 0.5))%>%
  addLegend(position = "topright", pal = palette_tmin_12_mw, title = "Temperature (°C)", group = "Moderate Warming", values = seq(3.5, 5, by = 0.5))%>%
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
  hideGroup("Moderate Warming")}

saveWidget(tmin_12, "interactive_tmin_12_example.html", selfcontained = TRUE)

##Static comparison map (4 panels)
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
  tm_raster(palette = get_brewer_pal("Blues"))+
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
  tm_scale_bar(position = c("RIGHT", "TOP") )+
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

open_members("GDD5_05_10_mw")
open_members("GDD5_05_10_hw")
open_members("GDD5_04_09_mw")
open_members("GDD5_04_09_hw")

plot_histogram_allmembers("GDD5_05_10_mw", "stack")
plot_histogram_allmembers("GDD5_05_10_mw", "facet")
plot_histogram_allmembers("GDD5_05_10_mw", "density")

plot_histogram_allmembers("GDD5_05_10_hw", "stack")
plot_histogram_allmembers("GDD5_05_10_hw", "facet")
plot_histogram_allmembers("GDD5_05_10_hw", "density")

plot_histogram_allmembers("GDD5_04_09_mw", "stack")
plot_histogram_allmembers("GDD5_04_09_mw", "facet")
plot_histogram_allmembers("GDD5_04_09_mw", "density")

plot_histogram_allmembers("GDD5_04_09_hw", "stack")
plot_histogram_allmembers("GDD5_04_09_hw", "facet")
plot_histogram_allmembers("GDD5_04_09_hw", "density")

open_members("tmin_12_03_mw")
open_members("tmin_12_03_hw")

plot_histogram_allmembers("tmin_12_03_mw", "stack")
plot_histogram_allmembers("tmin_12_03_mw", "facet")
plot_histogram_allmembers("tmin_12_03_mw", "density")

plot_histogram_allmembers("tmin_12_03_hw", "stack")
plot_histogram_allmembers("tmin_12_03_hw", "facet")
plot_histogram_allmembers("tmin_12_03_hw", "density")

###Leaflet function
#myDescriptiveVariable does not include warming scenario or sd / cv
#still need to test for GDD but Tmin, Tmax, ppt seem good...


loadLeaflet <- function(myDescriptiveVariable) {
  if(str_detect(myDescriptiveVariable, "tmax")){
    getObject_mw <- get(paste(myDescriptiveVariable, "_mw_anomaly", sep = ""))
    getObject_hw <- get(paste(myDescriptiveVariable, "_hw_anomaly", sep = ""))
    getObject_mw_sd <- get(paste(myDescriptiveVariable, "_mw_combined_sd", sep = ""))
    getObject_hw_sd <- get(paste(myDescriptiveVariable, "_hw_combined_sd", sep = ""))
    
    crs(getObject_mw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    crs(getObject_hw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    crs(getObject_mw_sd) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    crs(getObject_hw_sd) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    seq1 <- getObject_mw %>%
      cellStats(. , range)
    seq2 <- getObject_hw %>%
      cellStats(. , range)
    seq_sd1 <- getObject_mw_sd %>%
      cellStats(. , range)
    seq_sd2 <- getObject_hw_sd %>%
      cellStats(. , range)
    
    objectPalette1 <- colorBin("Oranges", domain = c(seq1[1], seq2[2]), bins = 5, pretty = TRUE, na.color = "transparent")
    objectPalette2 <- colorBin("Oranges", domain = c(seq1[1], seq2[2]), bins = 5, pretty = TRUE, na.color = "transparent")
    objectPalette_sd1 <- colorBin("Greys", domain = c(min(seq_sd1[1],seq_sd2[1]), max(seq_sd1[2], seq_sd2[2])), bins = 5, pretty = TRUE, na.color = "transparent")
    
    
  assign(  
    x = myDescriptiveVariable,
    envir = .GlobalEnv,
  value = leaflet()%>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri World Topographic")%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  addRasterImage(getObject_mw, colors= objectPalette1, group = "Moderate Warming", opacity = .8) %>%
  addRasterImage(getObject_hw, colors= objectPalette2, group = "High Warming", opacity = .8) %>%
  addRasterImage(getObject_mw_sd, colors = objectPalette_sd1, group = "Standard Deviation Moderate Warming", opacity = .8 )%>%
  addRasterImage(getObject_hw_sd, colors = objectPalette_sd1, group = "Standard Deviation High Warming", opacity = .8 )%>%
  addLayersControl(baseGroups = c("Esri World Topographic", "Esri World Imagery"),
                   overlayGroups = c("Moderate Warming", "High Warming", "Standard Deviation Moderate Warming", "Standard Deviation High Warming"))%>%
  addLegend(position = "topright", pal = objectPalette1, title = "Temperature<br>(°C)", group = "Moderate Warming", values = values(getObject_mw))%>%
  addLegend(position = "topright", pal = objectPalette2, title = "Temperature<br>(°C)", group = "High Warming", values = values(getObject_hw))%>%
  addLegend(position = "topright", pal = objectPalette_sd1, title = "Standard Deviation", group = "Standard Deviation Moderate Warming", values = values(getObject_mw_sd))%>%
  addLegend(position = "topright", pal = objectPalette_sd1, title = "Standard Deviation", group = "Standard Deviation High Warming", values = values(getObject_hw_sd))%>%
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
  )
  }
  else if (str_detect(myDescriptiveVariable, "tmin|minTemp")){
    getObject_mw <- get(paste(myDescriptiveVariable, "_mw_anomaly", sep = ""))
    getObject_hw <- get(paste(myDescriptiveVariable, "_hw_anomaly", sep = ""))
    getObject_mw_sd <- get(paste(myDescriptiveVariable, "_mw_combined_sd", sep = ""))
    getObject_hw_sd <- get(paste(myDescriptiveVariable, "_hw_combined_sd", sep = ""))
    
    crs(getObject_mw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    crs(getObject_hw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    crs(getObject_mw_sd) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    crs(getObject_hw_sd) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    seq1 <- getObject_mw %>%
      cellStats(. , range)
    seq2 <- getObject_hw %>%
      cellStats(. , range)
    seq_sd1 <- getObject_mw_sd %>%
      cellStats(. , range)
    seq_sd2 <- getObject_hw_sd %>%
      cellStats(. , range)
    
    objectPalette1 <- colorBin("Blues", domain = c(seq1[1], seq2[2]), bins = 5, pretty = TRUE, na.color = "transparent")
    objectPalette2 <- colorBin("Blues", domain = c(seq1[1], seq2[2]), bins = 5, pretty = TRUE, na.color = "transparent")
    objectPalette_sd1 <- colorBin("Greys", domain = c(min(seq_sd1[1],seq_sd2[1]), max(seq_sd1[2], seq_sd2[2])), bins = 5, pretty = TRUE, na.color = "transparent")
  
    assign(
      x = myDescriptiveVariable,
      envir = .GlobalEnv,
      value = leaflet()%>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri World Topographic")%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
      addRasterImage(getObject_mw, colors= objectPalette1, group = "Moderate Warming", opacity = .8) %>%
      addRasterImage(getObject_hw, colors= objectPalette2, group = "High Warming", opacity = .8) %>%
      addRasterImage(getObject_mw_sd, colors = objectPalette_sd1, group = "Standard Deviation Moderate Warming", opacity = .8 )%>%
      addRasterImage(getObject_hw_sd, colors = objectPalette_sd1, group = "Standard Deviation High Warming", opacity = .8 )%>%
      addLayersControl(baseGroups = c("Esri World Topographic", "Esri World Imagery"),
                       overlayGroups = c("Moderate Warming", "High Warming", "Standard Deviation Moderate Warming", "Standard Deviation High Warming"))%>%
      addLegend(position = "topright", pal = objectPalette1, title = "Temperature<br>(°C)", group = "Moderate Warming", values = values(getObject_mw))%>%
      addLegend(position = "topright", pal = objectPalette2, title = "Temperature<br>(°C)", group = "High Warming", values = values(getObject_hw))%>%
      addLegend(position = "topright", pal = objectPalette_sd1, title = "Standard Deviation", group = "Standard Deviation Moderate Warming", values = values(getObject_mw_sd))%>%
      addLegend(position = "topright", pal = objectPalette_sd1, title = "Standard Deviation", group = "Standard Deviation High Warming", values = values(getObject_hw_sd))%>%
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
    )
  }
  else if(str_detect(myDescriptiveVariable, "GDD")){
    getObject_mw <- get(paste(myDescriptiveVariable, "_mw_anomaly", sep = ""))
    getObject_hw <- get(paste(myDescriptiveVariable, "_hw_anomaly", sep = ""))
    getObject_mw_sd <- get(paste(myDescriptiveVariable, "_mw_combined_sd", sep = ""))
    getObject_hw_sd <- get(paste(myDescriptiveVariable, "_hw_combined_sd", sep = ""))
    
    crs(getObject_mw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    crs(getObject_hw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    crs(getObject_mw_sd) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    crs(getObject_hw_sd) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    seq1 <- getObject_mw %>%
      cellStats(. , range)
    seq2 <- getObject_hw %>%
      cellStats(. , range)
    seq_sd1 <- getObject_mw_sd %>%
      cellStats(. , range)
    seq_sd2 <- getObject_hw_sd %>%
      cellStats(. , range)
    
    objectPalette1 <- colorBin("Oranges", domain = c(seq1[1], seq2[2]), bins = 5, pretty = TRUE, na.color = "transparent")
    objectPalette2 <- colorBin("Oranges", domain = c(seq1[1], seq2[2]), bins = 5, pretty = TRUE, na.color = "transparent")
    objectPalette_sd1 <- colorBin("Greys", domain = c(min(seq_sd1[1],seq_sd2[1]), max(seq_sd1[2], seq_sd2[2])), bins = 5, pretty = TRUE, na.color = "transparent")
    

    assign(  
      x = myDescriptiveVariable,
      envir = .GlobalEnv,
      value = leaflet()%>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri World Topographic")%>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
        addRasterImage(getObject_mw, colors= objectPalette1, group = "Moderate Warming", opacity = .8) %>%
        addRasterImage(getObject_hw, colors= objectPalette2, group = "High Warming", opacity = .8) %>%
        addRasterImage(getObject_mw_sd, colors = objectPalette_sd1, group = "Standard Deviation Moderate Warming", opacity = .8 )%>%
        addRasterImage(getObject_hw_sd, colors = objectPalette_sd1, group = "Standard Deviation High Warming", opacity = .8 )%>%
        addLayersControl(baseGroups = c("Esri World Topographic", "Esri World Imagery"),
                         overlayGroups = c("Moderate Warming", "High Warming", "Standard Deviation Moderate Warming", "Standard Deviation High Warming"))%>%
        addLegend(position = "topright", pal = objectPalette1, title = "GDD > 5", group = "Moderate Warming", values = values(getObject_mw))%>%
        addLegend(position = "topright", pal = objectPalette2, title = "GDD > 5", group = "High Warming", values = values(getObject_hw))%>%
        addLegend(position = "topright", pal = objectPalette_sd1, title = "SD", group = "Standard Deviation Moderate Warming", values = values(getObject_mw_sd))%>%
        addLegend(position = "topright", pal = objectPalette_sd1, title = "SD", group = "Standard Deviation High Warming", values = values(getObject_hw_sd))%>%
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
    )
    
  }
  else{
    getPpt_mw <- get(paste(myDescriptiveVariable, "_mw_anomaly", sep = ""))
    getPpt_hw <- get(paste(myDescriptiveVariable, "_hw_anomaly", sep = ""))
    getPpt_mw_cv <- get(paste(myDescriptiveVariable, "_mw_combined_cv", sep = ""))
    getPpt_hw_cv <- get(paste(myDescriptiveVariable, "_hw_combined_cv", sep = ""))
    
    crs(getPpt_mw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    crs(getPpt_hw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    crs(getPpt_mw_cv) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    crs(getPpt_hw_cv) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    seq1 <- getPpt_mw %>%
      cellStats(. , range)
    seq2 <- getPpt_hw %>%
      cellStats(. , range)
    seq_cv1 <- getPpt_mw_cv %>%
      cellStats(. , range)
    seq_cv2 <- getPpt_hw_cv %>%
      cellStats(. , range)
    
    pptPalette1 <- colorBin("Blues", domain = c(seq1[1], seq2[2]), bins = 5, pretty = TRUE, na.color = "transparent")
    pptPalette2 <- colorBin("Blues", domain = c(seq1[1], seq2[2]), bins = 5, pretty = TRUE, na.color = "transparent")
    pptPalette_cv1 <- colorBin("Greys", domain = c(min(seq_cv1[1], seq_cv2[1]), max(seq_cv2[2], seq_cv1[2])), bins = 5, pretty = TRUE, na.color = "transparent")

    assign(
      x = myDescriptiveVariable,
      envir = .GlobalEnv,
    value = leaflet()%>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri World Topographic")%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
      addRasterImage(getPpt_mw, colors= pptPalette1, group = "Moderate Warming", opacity = .8) %>%
      addRasterImage(getPpt_hw, colors= pptPalette2, group = "High Warming", opacity = .8) %>%
      addRasterImage(getPpt_mw_cv, colors = pptPalette_cv1, group = "Coefficient of Variation: Moderate Warming", opacity = .8 )%>%
      addRasterImage(getPpt_hw_cv, colors = pptPalette_cv1, group = "Coefficient of Variation: High Warming", opacity = .8 )%>%
      addLayersControl(baseGroups = c("Esri World Topographic", "Esri World Imagery"),
                       overlayGroups = c("Moderate Warming", "High Warming", "Coefficient of Variation: Moderate Warming", "Coefficient of Variation: High Warming"))%>%
      addLegend(position = "topright", pal = pptPalette1, title = "Precipitation<br>(mm)", group = "Moderate Warming", values = values(getPpt_mw))%>%
      addLegend(position = "topright", pal = pptPalette2, title = "Precipitation<br>(mm)", group = "High Warming", values = values(getPpt_hw))%>%
      addLegend(position = "topright", pal = pptPalette_cv1, title = "CV", group = "Coefficient of Variation: Moderate Warming", values = values(getPpt_mw_cv))%>%
      addLegend(position = "topright", pal = pptPalette_cv1, title = "CV", group = "Coefficient of Variation: High Warming", values = values(getPpt_hw_cv))%>%
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479") %>%
      addMiniMap(position = "bottomleft", toggleDisplay = TRUE) %>%
      hideGroup("Coefficient of Variation: Moderate Warming") %>%
      hideGroup("Coefficient of Variation: High Warming") %>%
      hideGroup("High Warming")
    )
  }
}
#creating and writing all interactive maps

###Creating a function for static comparison maps (4 panel)
#myDescriptiveVariable is a string without anomaly or warming scenario: "tmax_06"

loadStatic <- function(myDescriptiveVariable){ #need to remove the scale bar from the 4 static maps
  if(str_detect(myDescriptiveVariable, "tmax")){
 #start 4 panel graphs
    
    raster1 <-  get(paste(myDescriptiveVariable, "_mw_anomaly", sep = ""))
    raster2 <- get(paste(myDescriptiveVariable, "_mw_combined_sd", sep = ""))
    raster3 <- get(paste(myDescriptiveVariable, "_hw_anomaly", sep = ""))
    raster4 <- get(paste(myDescriptiveVariable, "_hw_combined_sd", sep = ""))
    
     dataStack <- stack(raster1, raster2, raster3, raster4)
  
  palMw <- get_brewer_pal("Oranges", n = 6, plot = TRUE)
  palMw_sd <- get_brewer_pal("Greys", n = 6, plot = TRUE)
  palHw <- get_brewer_pal("Oranges", n = 6, plot = TRUE)
  palHw_sd <- get_brewer_pal("Greys", n = 6, plot = TRUE)
  
  
  static_comparison <- tmap_mode('plot') +
    tm_shape(dataStack) + 
    tm_raster(palette = list(palMw, palMw_sd, palHw, palHw_sd)) +
    tm_facets(ncol = 4, free.scales = TRUE) +
    tm_layout(legend.title.color = "white", 
              legend.title.size = .001, 
              title = c("Temperature\n(°C)", "SD", "Temperature\n(°C)", "SD"), 
              title.size = .9,
              panel.labels = c("MW", "MW SD", "HW", "HW SD"),
              panel.label.size = 1.1,
              scale = 1.2,
              compass.type = "arrow", attr.outside = TRUE, legend.outside = FALSE)+
    tm_compass(position = c("LEFT", "TOP")) 
  
  #end 4 panel
  
  #start 2 panel
  
  stack2 <- stack(raster1, raster3)
  stack2_sd <- stack(raster2, raster4)
  
  static_vals <- tmap_mode('plot') +
    tm_shape(stack2) + 
    tm_raster(palette = get_brewer_pal("Oranges", plot = TRUE))+
    tm_facets(ncol = 2, free.scales.raster = FALSE) +
    tm_layout(panel.labels = c("MW", "HW"), 
              panel.label.size = 1.1,
              legend.title.color = "white", 
              legend.title.size = .001, 
              title = "Temperature\n(°C)",
              compass.type = "arrow",
              scale = 1.2,
              attr.outside = TRUE, 
              legend.outside = TRUE)+
    tm_scale_bar(position = c("RIGHT", "TOP") )+
    tm_compass(position = c("LEFT", "TOP"))
  
  static_variation <- tmap_mode('plot') +
    tm_shape(stack2_sd) + 
    tm_raster(palette = get_brewer_pal("Greys", plot = TRUE))+
    tm_facets(ncol = 2, free.scales.raster = FALSE) +
    tm_layout(panel.labels = c("MW", "HW"), 
              panel.label.size = 1.1,
              legend.title.color = "white", 
              legend.title.size = .001, 
              title = "SD", 
              compass.type = "arrow", 
              scale = 1.2,
              attr.outside = TRUE, 
              legend.outside = TRUE)+
    tm_scale_bar(position = c("RIGHT", "TOP") )+
    tm_compass(position = c("LEFT", "TOP"))
  
  #end 2 panel
    
  }
  
  else if(str_detect(myDescriptiveVariable, "tmin|minTemp")){
    
    raster1 <-  get(paste(myDescriptiveVariable, "_mw_anomaly", sep = ""))
    raster2 <- get(paste(myDescriptiveVariable, "_mw_combined_sd", sep = ""))
    raster3 <- get(paste(myDescriptiveVariable, "_hw_anomaly", sep = ""))
    raster4 <- get(paste(myDescriptiveVariable, "_hw_combined_sd", sep = ""))
    
    
    dataStack <- stack(raster1, raster2, raster3, raster4)
    
    palMw <- get_brewer_pal("Blues", n = 6, plot = TRUE)
    palMw_sd <- get_brewer_pal("Greys", n = 6, plot = TRUE)
    palHw <- get_brewer_pal("Blues", n = 6, plot = TRUE)
    palHw_sd <- get_brewer_pal("Greys", n = 6, plot = TRUE)
    
    
    static_comparison <- tmap_mode('plot') +
      tm_shape(dataStack) + 
      tm_raster(palette = list(palMw, palMw_sd, palHw, palHw_sd)) +
      tm_facets(ncol = 4, free.scales = TRUE) +
      tm_layout(legend.title.color = "white", 
                legend.title.size = .001, 
                title = c("Temperature\n(°C)", "SD", "Temperature\n(°C)", "SD"), 
                title.size = .9,
                scale = 1.2,
                panel.labels = c("MW", "MW SD", "HW", "HW SD"),
                panel.label.size = 1.1,
                compass.type = "arrow", attr.outside = TRUE, legend.outside = FALSE)+
      tm_compass(position = c("LEFT", "TOP")) 
    ##end 4 panel
    
    ##start 2 panel
    stack2 <- stack(raster1, raster3)
    
    stack2_sd <- stack(raster2, raster4)
    
    static_vals <- tmap_mode('plot') +
      tm_shape(stack2) + 
      tm_raster(palette = get_brewer_pal("Blues", plot = TRUE))+
      tm_facets(ncol = 2, free.scales.raster = FALSE) +
      tm_layout(panel.labels = c("MW", "HW"), 
                panel.label.size = 1.1,
                legend.title.color = "white", 
                legend.title.size = .001, 
                scale = 1.2,
                title = "Temperature\n(°C)", 
                compass.type = "arrow", 
                attr.outside = TRUE, 
                legend.outside = TRUE)+
      tm_scale_bar(position = c("RIGHT", "TOP") )+
      tm_compass(position = c("LEFT", "TOP"))
    
    static_variation <- tmap_mode('plot') +
      tm_shape(stack2_sd) + 
      tm_raster(palette = get_brewer_pal("Greys", plot = TRUE))+
      tm_facets(ncol = 2, free.scales.raster = FALSE) +
      tm_layout(panel.labels = c("MW", "HW"), 
                panel.label.size = 1.1,
                scale = 1.2,
                legend.title.color = "white", 
                legend.title.size = .001, 
                title = "SD", 
                compass.type = "arrow", 
                attr.outside = TRUE, 
                legend.outside = TRUE)+
      tm_scale_bar(position = c("RIGHT", "TOP") )+
      tm_compass(position = c("LEFT", "TOP"))
    
    ##end 2 panel
  }
  
  else if(str_detect(myDescriptiveVariable, "ppt")){
    
    raster1 <- get(paste(myDescriptiveVariable, "_mw_anomaly", sep = ""))
    raster2 <- get(paste(myDescriptiveVariable, "_mw_combined_cv", sep = ""))
    raster3 <- get(paste(myDescriptiveVariable, "_hw_anomaly", sep = ""))
    raster4 <- get(paste(myDescriptiveVariable, "_hw_combined_cv", sep = ""))
    
    dataStack <- stack(raster1, raster2, raster3, raster4)
    
    palMw <- get_brewer_pal("Blues", plot = TRUE)
    palMw_cv <- get_brewer_pal("Greys", plot = TRUE)
    palHw <- get_brewer_pal("Blues", plot = TRUE)
    palHw_cv <- get_brewer_pal("Greys", plot = TRUE)
    
    
    static_comparison <- tmap_mode('plot') +
      tm_shape(dataStack) + 
      tm_raster(palette = list(palMw, palMw_cv, palHw, palHw_cv)) +
      tm_facets(ncol = 4, free.scales = TRUE) +
      tm_layout(legend.title.color = "white", 
                legend.title.size = .001, 
                scale = 1.2,
                title = c("Precipitation (mm)", "CV", "Precipitation (mm)", "CV"), 
                title.size = .9,
                panel.labels = c("MW", "MW CV", "HW", "HW CV"),
                panel.label.size = 1.1,
                compass.type = "arrow", attr.outside = TRUE, legend.outside = FALSE)+
      tm_compass(position = c("LEFT", "TOP")) 
    
    #end 4 panel
    
    #start 2 panel
    stack2 <- stack(
      raster1,
      raster3
    )
    stack2_cv <- stack(
      raster2,
      raster4
    )
    
    static_vals <- tmap_mode('plot') +
      tm_shape(stack2) + 
      tm_raster(palette = get_brewer_pal("Blues", plot = TRUE))+
      tm_facets(ncol = 2, free.scales.raster = FALSE) +
      tm_layout(panel.labels = c("MW", "HW"), 
                panel.label.size = 1.1,
                legend.title.color = "white", 
                legend.title.size = .001, 
                title = "Precipitation (mm)", 
                scale = 1.2,
                compass.type = "arrow", 
                attr.outside = TRUE, 
                legend.outside = TRUE)+
      tm_scale_bar(position = c("RIGHT", "TOP") )+
      tm_compass(position = c("LEFT", "TOP"))
    
    static_variation <- tmap_mode('plot') +
      tm_shape(stack2_cv) + 
      tm_raster(palette = get_brewer_pal("Greys", plot = TRUE))+
      tm_facets(ncol = 2, free.scales.raster = FALSE) +
      tm_layout(panel.labels = c("MW", "HW"), 
                panel.label.size = 1.1,
                legend.title.color = "white",
                scale = 1.2,
                legend.title.size = .001, 
                title = "CV", 
                compass.type = "arrow", 
                attr.outside = TRUE, 
                legend.outside = TRUE)+
      tm_scale_bar(position = c("RIGHT", "TOP") )+
      tm_compass(position = c("LEFT", "TOP"))
    
    
    #end 2 panel
    
    
  }
  else {
    
    
    raster1 <- get(paste(myDescriptiveVariable, "_mw_anomaly", sep = ""))
    raster2 <- get(paste(myDescriptiveVariable, "_mw_combined_sd", sep = ""))
    raster3 <- get(paste(myDescriptiveVariable, "_hw_anomaly", sep = ""))
    raster4 <- get(paste(myDescriptiveVariable, "_hw_combined_sd", sep = ""))
    
    dataStack <- stack(
      raster1,
      raster2,
      raster3,
      raster4
    )
    
    palMw <- get_brewer_pal("Oranges", n = 6, plot = TRUE)
    palMw_sd <- get_brewer_pal("Greys", n = 6, plot = TRUE)
    palHw <- get_brewer_pal("Oranges", n = 6, plot = TRUE)
    palHw_sd <- get_brewer_pal("Greys", n = 6, plot = TRUE)
    
    
    static_comparison <- tmap_mode('plot') +
      tm_shape(dataStack) + 
      tm_raster(palette = list(palMw, palMw_sd, palHw, palHw_sd)) +
      tm_facets(ncol = 4, free.scales = TRUE) +
      tm_layout(legend.title.color = "white", 
                scale = 1.2,
                legend.title.size = .001, 
                title = c("GDD > 5", "SD", "GDD > 5", "SD"), 
                title.size = .9,
                panel.labels = c("MW", "MW SD", "HW", "HW SD"),
                panel.label.size = 1.1,
                compass.type = "arrow", attr.outside = TRUE, legend.outside = FALSE)+
      tm_compass(position = c("LEFT", "TOP")) 
    
#start 2 panel     
    
    stack2 <- stack(
      raster1,
      raster3
    )
    stack2_sd <- stack(
      raster2,
      raster4
    )
    
    static_vals <- tmap_mode('plot') +
      tm_shape(stack2) + 
      tm_raster(palette = get_brewer_pal("Oranges", plot = TRUE))+
      tm_facets(ncol = 2, free.scales.raster = FALSE) +
      tm_layout(panel.labels = c("MW", "HW"), 
                panel.label.size = 1.1,
                legend.title.color = "white", 
                legend.title.size = .001, 
                scale = 1.2,
                title = "GDD > 5", 
                compass.type = "arrow", 
                attr.outside = TRUE, 
                legend.outside = TRUE)+
      tm_scale_bar(position = c("RIGHT", "TOP") )+
      tm_compass(position = c("LEFT", "TOP"))
    
    static_variation <- tmap_mode('plot') +
      tm_shape(stack2_sd) + 
      tm_raster(palette = get_brewer_pal("Greys", plot = TRUE))+
      tm_facets(ncol = 2, free.scales.raster = FALSE) +
      tm_layout(panel.labels = c("MW", "HW"), 
                panel.label.size = 1.1,
                scale = 1.2,
                legend.title.color = "white", 
                legend.title.size = .001, 
                title = "SD", 
                compass.type = "arrow", 
                attr.outside = TRUE, 
                legend.outside = TRUE)+
      tm_scale_bar(position = c("RIGHT", "TOP") )+
      tm_compass(position = c("LEFT", "TOP"))
    
   #end 2 panel
    
  }
  
  assign(
    x = paste(myDescriptiveVariable, "_4static", sep = ""),
    value = static_comparison,
    envir = .GlobalEnv
  )
  
  assign(
    x = paste(myDescriptiveVariable, "_2static_vals", sep = ""),
    value = static_vals,
    envir = .GlobalEnv
  )
  
  
  if(str_detect(myDescriptiveVariable, "ppt")){
  assign(
    x = paste(myDescriptiveVariable, "_2static_cv", sep = ""),
    value = static_variation,
    envir = .GlobalEnv
  )
  }
  else{
    assign(
      x = paste(myDescriptiveVariable, "_2static_sd", sep = ""),
      value = static_variation,
      envir = .GlobalEnv
    )
  }
}



leafletList <- c("GDD5_05_10", "GDD5_04_09", "tmin_09", "tmin_10", "ppt_09", "ppt_10", "tmax_06", "tmax_07", "tmax_08", "tmax_09", "tmin_03", "tmin_04", "minTemp_12_03")

lapply(leafletList, FUN = loadLeaflet)

setwd("E:/climate_projection_data/bcvin_raster")
for(i in 1:length(leafletList)){
  saveWidget(get(paste(leafletList[i])), paste(leafletList[i], ".html", sep = ""), selfcontained = TRUE)
}


setwd("E:/climate_projection_data/bcvin_raster")
for(i in 1:length(thistime)){
  saveWidget(get(paste(thistime[i])), paste(thistime[i], ".html", sep = ""), selfcontained = TRUE)
}


lapply(leafletList, FUN = loadStatic)

ppt_gdd_list <- grep("GDD5|ppt", leafletList) 
staticList_ppt_gdd <- leafletList[ppt_gdd_list]

tmax_tmin_list <- grep("tmax|tmin|minTemp", leafletList)
staticList_tmax_tmin <- leafletList[tmax_tmin_list]


#the unfortunate replacement to the for loop I had written. For loops don't work in this scenario as you must wait for the png to load on your viewer. No good alternative was found readily
#this solution is a bit clunky. Refer to staticList[i] for what variable each static map refers to 
#Start ppt & gdd
#1
get(paste(staticList_ppt_gdd[1], "_2static_vals", sep = ""))
get(paste(staticList_ppt_gdd[1], "_2static_cv", sep = ""))
get(paste(staticList_ppt_gdd[1], "_4static", sep = ""))

#2
get(paste(staticList_ppt_gdd[2], "_2static_vals", sep = ""))
get(paste(staticList_ppt_gdd[2], "_2static_cv", sep = ""))
get(paste(staticList_ppt_gdd[2], "_4static", sep = ""))

#3
get(paste(staticList_ppt_gdd[3], "_2static_vals", sep = ""))
get(paste(staticList_ppt_gdd[3], "_2static_cv", sep = ""))
get(paste(staticList_ppt_gdd[3], "_4static", sep = ""))

#4
get(paste(staticList_ppt_gdd[4], "_2static_vals", sep = ""))
get(paste(staticList_ppt_gdd[4], "_2static_cv", sep = ""))
get(paste(staticList_ppt_gdd[4], "_4static", sep = ""))
#end ppt & gdd

#start tmax & tmin
#1
get(paste(staticList_tmax_tmin[1], "_2static_vals", sep = ""))
get(paste(staticList_tmax_tmin[1], "_2static_sd", sep = ""))
get(paste(staticList_tmax_tmin[1], "_4static", sep = ""))

#2
get(paste(staticList_tmax_tmin[2], "_2static_vals", sep = ""))
get(paste(staticList_tmax_tmin[2], "_2static_sd", sep = ""))
get(paste(staticList_tmax_tmin[2], "_4static", sep = ""))

#3
get(paste(staticList_tmax_tmin[3], "_2static_vals", sep = ""))
get(paste(staticList_tmax_tmin[3], "_2static_sd", sep = ""))
get(paste(staticList_tmax_tmin[3], "_4static", sep = ""))

#4
get(paste(staticList_tmax_tmin[4], "_2static_vals", sep = ""))
get(paste(staticList_tmax_tmin[4], "_2static_sd", sep = ""))
get(paste(staticList_tmax_tmin[4], "_4static", sep = ""))

#5
get(paste(staticList_tmax_tmin[5], "_2static_vals", sep = ""))
get(paste(staticList_tmax_tmin[5], "_2static_sd", sep = ""))
get(paste(staticList_tmax_tmin[5], "_4static", sep = ""))

#6
get(paste(staticList_tmax_tmin[6], "_2static_vals", sep = ""))
get(paste(staticList_tmax_tmin[6], "_2static_sd", sep = ""))
get(paste(staticList_tmax_tmin[6], "_4static", sep = ""))

#7
get(paste(staticList_tmax_tmin[7], "_2static_vals", sep = ""))
get(paste(staticList_tmax_tmin[7], "_2static_sd", sep = ""))
get(paste(staticList_tmax_tmin[7], "_4static", sep = ""))

#8
get(paste(staticList_tmax_tmin[8], "_2static_vals", sep = ""))
get(paste(staticList_tmax_tmin[8], "_2static_sd", sep = ""))
get(paste(staticList_tmax_tmin[8], "_4static", sep = ""))

#9
get(paste(staticList_tmax_tmin[9], "_2static_vals", sep = ""))
get(paste(staticList_tmax_tmin[9], "_2static_sd", sep = ""))
get(paste(staticList_tmax_tmin[9], "_4static", sep = ""))

get(paste(anotherLeafletList[1], "_2static_vals", sep = ""))
get(paste(anotherLeafletList[1], "_2static_cv", sep = ""))
get(paste(anotherLeafletList[1], "_4static", sep = ""))

get(paste(anotherLeafletList[2], "_2static_vals", sep = ""))
get(paste(anotherLeafletList[2], "_2static_cv", sep = ""))
get(paste(anotherLeafletList[2], "_4static", sep = ""))

get(paste(anotherLeafletList[3], "_2static_vals", sep = ""))
get(paste(anotherLeafletList[3], "_2static_sd", sep = ""))
get(paste(anotherLeafletList[3], "_4static", sep = ""))


