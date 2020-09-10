 
source("functions_externaldrive.R")

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
# (4) Extreme lows in dormancy to budburst perioF: tmin_03 & tmin_04, tmin_12_03


variable_list <- list.files("../../../../climate_projection_data/bcvin_raster/anomaly", pattern = ".asc")
setwd("../../../../climate_projection_data/bcvin_raster/anomaly")

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

combined_list <- list.files("../../../../climate_projection_data/bcvin_raster/combined", pattern = ".asc")
setwd("../../../../climate_projection_data/bcvin_raster/combined")

combined_objects <- lapply(combined_list, raster)
combined_objects_names <- substr(combined_list, 0, length(combined_list)) %>%
  lapply(. , str_remove, pattern = ".asc") %>%
  unlist()

for ( i in 1:length(combined_objects_names)){
  assign(
    x = paste(combined_objects_names[i]),
    value = combined_objects[[i]]
  )
}

historical_list <- list.files("../../../../climate_projection_data/bcvin_raster/historical", pattern = ".asc")
setwd("../../../../climate_projection_data/bcvin_raster/historical")

historical_objects <- lapply(historical_list, raster)
historical_objects_names <- substr(historical_list, 0, length(historical_list)) %>%
  lapply(. , str_remove, pattern = ".asc") %>%
  unlist()

for(i in 1:length(historical_objects_names)){
  assign(
    x = paste(historical_objects_names[i]),
    value = historical_objects[[i]]
  )
}

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

###writes all of the interactive maps 
#myDescriptiveVariable does not include warming scenario or sd / cv
#still need to test for GDD but Tmin, Tmax, ppt seem good...

loadLeaflet <- function(myDescriptiveVariable) {
  
  getObject_nw <- get(paste(myDescriptiveVariable, "_nw", sep = ""))
  crs(getObject_nw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  seq3 <- getObject_nw %>%
    cellStats(. , range)
  objectPalette3 <- colorBin("RdYlBu", domain = c(seq3[1], seq3[2]), bins = 8, pretty = TRUE, na.color = "transparent", reverse = TRUE)
  
  if(str_detect(myDescriptiveVariable, "tmax")){
    getObject_mw <- get(paste(myDescriptiveVariable, "_mw_combined", sep = ""))
    getObject_hw <- get(paste(myDescriptiveVariable, "_hw_combined", sep = ""))

    
    crs(getObject_mw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    crs(getObject_hw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

    seq1 <- getObject_mw %>%
      cellStats(. , range)
    seq2 <- getObject_hw %>%
      cellStats(. , range)
  
    objectPalette1 <- colorBin("Oranges", domain = c(seq1[1], seq1[2]), bins = 8, pretty = TRUE, na.color = "transparent")
    objectPalette2 <- colorBin("Oranges", domain = c(seq2[1], seq2[2]), bins = 8, pretty = TRUE, na.color = "transparent")

  assign(  
    x = myDescriptiveVariable,
    envir = .GlobalEnv,
  value = leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri World Topographic")%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  addRasterImage(getObject_mw, colors= objectPalette1, group = "Moderate Warming", opacity = .8) %>%
  addRasterImage(getObject_hw, colors= objectPalette2, group = "High Warming", opacity = .8) %>%
  addRasterImage(getObject_nw, colors = objectPalette3, group = "Historical", opacity = .8) %>%
  addLayersControl(baseGroups = c("Esri World Topographic", "Esri World Imagery"),
                   overlayGroups = c("Historical", "Moderate Warming", "High Warming"))%>%
  addLegend(position = "topright", pal = objectPalette1, title = "Temperature<br>(°C)", group = "Moderate Warming", values = values(getObject_mw))%>%
  addLegend(position = "topright", pal = objectPalette2, title = "Temperature<br>(°C)", group = "High Warming", values = values(getObject_hw))%>%
  addLegend(position =  "topright", pal = objectPalette3, title = "Temperature<br>(°C)", group = "Historical", values = values(getObject_nw)) %>%
  addMeasure(
    position = "topleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479") %>%
  addMiniMap(position = "bottomleft", toggleDisplay = TRUE) %>%
    hideGroup("Moderate Warming")%>%
    hideGroup("High Warming")
  )
  }
  else if (str_detect(myDescriptiveVariable, "tmin|minTemp")){
    getObject_mw <- get(paste(myDescriptiveVariable, "_mw_combined", sep = ""))
    getObject_hw <- get(paste(myDescriptiveVariable, "_hw_combined", sep = ""))
    
    crs(getObject_mw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    crs(getObject_hw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    seq1 <- getObject_mw %>%
      cellStats(. , range)
    seq2 <- getObject_hw %>%
      cellStats(. , range)
    
    objectPalette1 <- colorBin("Blues", domain = c(seq1[1], seq1[2]), bins = 8, pretty = TRUE, na.color = "transparent")
    objectPalette2 <- colorBin("Blues", domain = c(seq2[1], seq2[2]), bins = 8, pretty = TRUE, na.color = "transparent")
    
    assign(
      x = myDescriptiveVariable,
      envir = .GlobalEnv,
      value = leaflet()%>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri World Topographic")%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
      addRasterImage(getObject_mw, colors= objectPalette1, group = "Moderate Warming", opacity = .8) %>%
      addRasterImage(getObject_hw, colors= objectPalette2, group = "High Warming", opacity = .8) %>%
      addRasterImage(getObject_nw, colors = objectPalette3, group = "Historical", opacity = .8) %>%
      addLayersControl(baseGroups = c("Esri World Topographic", "Esri World Imagery"),
                       overlayGroups = c("Historical", "Moderate Warming", "High Warming"))%>%
      addLegend(position = "topright", pal = objectPalette1, title = "Temperature<br>(°C)", group = "Moderate Warming", values = values(getObject_mw))%>%
      addLegend(position = "topright", pal = objectPalette2, title = "Temperature<br>(°C)", group = "High Warming", values = values(getObject_hw))%>%
      addLegend(position =  "topright", pal = objectPalette3, title = "Temperature<br>(°C)", group = "Historical", values = values(getObject_nw)) %>%
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479") %>%
      addMiniMap(position = "bottomleft", toggleDisplay = TRUE) %>%
      hideGroup("Moderate Warming") %>%
      hideGroup("High Warming")
    )
  }
  else if(str_detect(myDescriptiveVariable, "GDD")){
    getObject_mw <- get(paste(myDescriptiveVariable, "_mw_combined", sep = ""))
    getObject_hw <- get(paste(myDescriptiveVariable, "_hw_combined", sep = ""))
   
    crs(getObject_mw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    crs(getObject_hw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
   
    seq1 <- getObject_mw %>%
      cellStats(. , range)
    seq2 <- getObject_hw %>%
      cellStats(. , range)
    
    
    objectPalette1 <- colorBin("Oranges", domain = c(seq1[1], seq1[2]), bins = 8, pretty = TRUE, na.color = "transparent")
    objectPalette2 <- colorBin("Oranges", domain = c(seq2[1], seq2[2]), bins = 8, pretty = TRUE, na.color = "transparent")

    assign(  
      x = myDescriptiveVariable,
      envir = .GlobalEnv,
      value = leaflet()%>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri World Topographic")%>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
        addRasterImage(getObject_mw, colors= objectPalette1, group = "Moderate Warming", opacity = .8) %>%
        addRasterImage(getObject_hw, colors= objectPalette2, group = "High Warming", opacity = .8) %>%
        addRasterImage(getObject_nw, colors = objectPalette3, group = "Historical", opacity = .8) %>%
        addLayersControl(baseGroups = c("Esri World Topographic", "Esri World Imagery"),
                         overlayGroups = c("Historical", "Moderate Warming", "High Warming"))%>%
        addLegend(position = "topright", pal = objectPalette1, title = "GDD > 5", group = "Moderate Warming", values = values(getObject_mw))%>%
        addLegend(position = "topright", pal = objectPalette2, title = "GDD > 5", group = "High Warming", values = values(getObject_hw))%>%
        addLegend(position =  "topright", pal = objectPalette3, title = "GDD > 5", group = "Historical", values = values(getObject_nw)) %>%
        addMeasure(
          position = "topleft",
          primaryLengthUnit = "meters",
          primaryAreaUnit = "sqmeters",
          activeColor = "#3D535D",
          completedColor = "#7D4479") %>%
        addMiniMap(position = "bottomleft", toggleDisplay = TRUE) %>%
        hideGroup("Moderate Warming")%>%
        hideGroup("High Warming")
    )
    
  }
  else{
    getPpt_mw <- get(paste(myDescriptiveVariable, "_mw_combined", sep = ""))
    getPpt_hw <- get(paste(myDescriptiveVariable, "_hw_combined", sep = ""))
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
    
    pptPalette1 <- colorBin("Blues", domain = c(seq1[1], seq1[2]), bins = 5, pretty = TRUE, na.color = "transparent")
    pptPalette2 <- colorBin("Blues", domain = c(seq2[1], seq2[2]), bins = 5, pretty = TRUE, na.color = "transparent")
    pptPalette_cv1 <- colorBin("Greys", domain = c(seq_cv1[1], seq_cv1[2]), bins = 5, pretty = TRUE, na.color = "transparent")
    pptPalette_cv2 <- colorBin("Greys", domain = c(seq_cv2[1], seq_cv2[2]), bins = 5, pretty = TRUE, na.color = "transparent")
    
    
    assign(
      x = myDescriptiveVariable,
      envir = .GlobalEnv,
    value = leaflet()%>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri World Topographic")%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
      addRasterImage(getPpt_mw, colors= pptPalette1, group = "Moderate Warming", opacity = .8) %>%
      addRasterImage(getPpt_hw, colors= pptPalette2, group = "High Warming", opacity = .8) %>%
      addRasterImage(getObject_nw, colors = objectPalette3, group = "Historical", opacity = .8) %>%
      addRasterImage(getPpt_mw_cv, colors = pptPalette_cv1, group = "Coefficient of Variation: Moderate Warming", opacity = .8 )%>%
      addRasterImage(getPpt_hw_cv, colors = pptPalette_cv2, group = "Coefficient of Variation: High Warming", opacity = .8 )%>%
      addLayersControl(baseGroups = c("Esri World Topographic", "Esri World Imagery"),
                       overlayGroups = c("Historical", "Moderate Warming", "High Warming", "Coefficient of Variation: Moderate Warming", "Coefficient of Variation: High Warming"))%>%
      addLegend(position = "topright", pal = objectPalette3, title = "Precipitation<br>(mm)", group = "Historical", values = values(getObject_nw))%>%
      addLegend(position = "topright", pal = pptPalette1, title = "Precipitation<br>(mm)", group = "Moderate Warming", values = values(getPpt_mw))%>%
      addLegend(position = "topright", pal = pptPalette2, title = "Precipitation<br>(mm)", group = "High Warming", values = values(getPpt_hw))%>%
      addLegend(position = "topright", pal = pptPalette_cv1, title = "CV", group = "Coefficient of Variation: Moderate Warming", values = values(getPpt_mw_cv))%>%
      addLegend(position = "topright", pal = pptPalette_cv2, title = "CV", group = "Coefficient of Variation: High Warming", values = values(getPpt_hw_cv))%>%
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479") %>%
      addMiniMap(position = "bottomleft", toggleDisplay = TRUE) %>%
      hideGroup("Coefficient of Variation: Moderate Warming") %>%
      hideGroup("Coefficient of Variation: High Warming") %>%
      hideGroup("Moderate Warming")%>%
      hideGroup("High Warming")
    )
  }
}

###Creating a function for static comparison maps (4 panel)
#myDescriptiveVariable is a string without anomaly or warming scenario: "tmax_06"
#this function creates all static maps 
loadStatic <- function(myDescriptiveVariable){ 
  historical <- get(paste0(myDescriptiveVariable, "_nw"))
    palHist <- get_brewer_pal("-RdYlBu", n = 8, plot = TRUE)
    
  if(str_detect(myDescriptiveVariable, "tmax")){
 #start anomalies graphs
    
    raster1 <-  get(paste(myDescriptiveVariable, "_mw_combined", sep = ""))
    raster3 <- get(paste(myDescriptiveVariable, "_hw_combined", sep = ""))

    anomaly_Hw <- get(paste0(myDescriptiveVariable, "_hw_anomaly"))
    anomaly_Mw <- get(paste0(myDescriptiveVariable, "_mw_anomaly"))
    anomalyStack <- stack(anomaly_Mw, anomaly_Hw)
    
    crs(anomalyStack) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    
  palMw <- get_brewer_pal("Oranges", n = 6, plot = TRUE)
  palHw <- get_brewer_pal("Oranges", n = 6, plot = TRUE)
  
  anomaly_1 <- tmap_mode("plot") +
    tm_shape(anomaly_Mw)+
    tm_raster(palette = "Oranges", n = 6) +
    tm_layout(
      title = "Temperature\n(°C)",
      scale = 1,
      attr.outside = FALSE,
      legend.title.color = "white", 
      legend.title.size = .001,
      main.title = "MW Anomalies",
      main.title.fontface = 2,
      frame = TRUE,
      legend.width = 1.5, 
      legend.text.size = 1
    ) + 
    tm_compass(position = c("LEFT", "BOTTOM"))
  
  anomaly_2 <- tmap_mode("plot") +
    tm_shape(anomaly_Hw)+
    tm_raster(palette = "Oranges", n = 6) +
    tm_layout(
      title = "Temperature\n(°C)",
      scale = 1,
      attr.outside = FALSE,
      legend.title.color = "white", 
      legend.title.size = .001,
      main.title = "HW Anomalies",
      main.title.fontface = 2,
      frame = TRUE,
      legend.width = 1.5, 
      legend.text.size = 1
    ) + 
    tm_scale_bar(position = c("RIGHT", "BOTTOM"))
  
  #end anomalies
  
  #start 3 panel

  static_vals_1 <- tmap_mode("plot") +
    tm_shape(raster1) +
    tm_raster(palette = get_brewer_pal("Oranges", n = 8, plot = TRUE, stretch = TRUE), breaks = seq(floor(cellStats(raster1, range)[1]), cellStats(raster1, range)[2] + 1, by = 1)) + 
    tm_layout(
      title = "Temperature\n(°C)",
      scale = 1, 
      attr.outside = TRUE,
      legend.outside = FALSE,
      legend.title.color = "white", 
      legend.title.size = .001,
      main.title = "Moderate Warming",
      main.title.fontface = 2,
      frame = TRUE,
      legend.width = 1.5, 
      legend.text.size = 1
    ) 
    
  static_vals_3 <- tmap_mode("plot")+
    tm_shape(raster3) +
    tm_raster(palette = get_brewer_pal("Reds", n = 8, plot = TRUE, stretch = TRUE), breaks = seq(floor(cellStats(raster3, range)[1]), cellStats(raster3, range)[2] + 1, by = 1)) +
    tm_layout(
      title = "Temperature\n(°C)",
      scale = 1, 
      attr.outside = FALSE,
      legend.outside = FALSE,
      legend.title.color = "white", 
      legend.title.size = .001,
      main.title = "High Warming",
      main.title.fontface = 2,
      frame = TRUE,
      legend.width = 1.5, 
      legend.text.size = 1
    ) +
    tm_scale_bar(position = c("RIGHT", "BOTTOM"))
    
  
  historic_vals <- tmap_mode("plot") +
    tm_shape(historical) + 
    tm_raster(palette = palHist, breaks = seq(floor(cellStats(historical, range)[1]), cellStats(historical, range)[2] + 1, by = 1))+
    tm_layout(legend.title.size = .001,
              title = "Temperature\n(°C)",
              scale = 1, 
              attr.outside = FALSE,
              legend.outside = FALSE, 
              main.title = "Historical",
              legend.title.color = "white",
              main.title.fontface = 2,
              frame = TRUE,
              legend.width = 1.5, 
              legend.text.size = 1 
              ) +
    tm_compass(position = c("LEFT", "BOTTOM"))
  #end 3 panel
    
  }
  
  else if(str_detect(myDescriptiveVariable, "tmin")){
    
    raster1 <-  get(paste(myDescriptiveVariable, "_mw_combined", sep = ""))
    raster3 <- get(paste(myDescriptiveVariable, "_hw_combined", sep = ""))
    
    anomaly_Hw <- get(paste0(myDescriptiveVariable, "_hw_anomaly"))
    anomaly_Mw <- get(paste0(myDescriptiveVariable, "_mw_anomaly"))
    anomalyStack <- stack(anomaly_Mw, anomaly_Hw)
    crs(anomalyStack) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    
    palMw <- get_brewer_pal("Blues", n = 6, plot = TRUE)
    palHw <- get_brewer_pal("Blues", n = 6, plot = TRUE)
    
    anomaly_1 <- tmap_mode("plot") +
      tm_shape(anomaly_Mw)+
      tm_raster(palette = "Blues", n = 6) +
      tm_layout(
        title = "Temperature\n(°C)",
        scale = 1,
        attr.outside = FALSE,
        legend.title.color = "white", 
        legend.title.size = .001,
        main.title = "MW Anomalies",
        main.title.fontface = 2,
        frame = TRUE,
        legend.width = 1.5, 
        legend.text.size = 1
      ) + 
      tm_compass(position = c("LEFT", "BOTTOM"))
    
    anomaly_2 <- tmap_mode("plot") +
      tm_shape(anomaly_Hw)+
      tm_raster(palette = "Blues", n = 6) +
      tm_layout(
        title = "Temperature\n(°C)",
        scale = 1,
        attr.outside = FALSE,
        legend.title.color = "white", 
        legend.title.size = .001,
        main.title = "HW Anomalies",
        main.title.fontface = 2,
        frame = TRUE,
        legend.width = 1.5, 
        legend.text.size = 1
      ) + 
      tm_scale_bar(position = c("RIGHT", "BOTTOM"))
    ##end anomalies
    
    ##start 2 panel
    
    static_vals_1 <- tmap_mode("plot") +
      tm_shape(raster1) +
      tm_raster(palette = get_brewer_pal("Blues", n = 8, plot = TRUE, stretch = TRUE), breaks = seq(floor(cellStats(raster1, range)[1]), cellStats(raster1, range)[2] + 1, by = 1)) + 
      tm_layout(
        title = "Temperature\n(°C)",
        scale = 1, 
        attr.outside = TRUE,
        legend.outside = FALSE,
        legend.title.color = "white", 
        legend.title.size = .001,
        main.title = "Moderate Warming",
        main.title.fontface = 2,
        frame = TRUE,
        legend.width = 1.5, 
        legend.text.size = 1 
      ) 
    
    static_vals_3 <- tmap_mode("plot")+
      tm_shape(raster3) +
      tm_raster(palette = get_brewer_pal("Purples", n = 8, plot = TRUE, stretch = TRUE), breaks = seq(floor(cellStats(raster3, range)[1]), cellStats(raster3, range)[2] + 1, by = 1)) +
      tm_layout(
        title = "Temperature\n(°C)",
        scale = 1, 
        attr.outside = FALSE,
        legend.outside = FALSE,
        legend.title.color = "white", 
        legend.title.size = .001,
        main.title = "High Warming",
        main.title.fontface = 2,
        frame = TRUE,
        legend.width = 1.5, 
        legend.text.size = 1 
      ) +
      tm_scale_bar(position = c("RIGHT", "BOTTOM"))
    
    
    historic_vals <- tmap_mode("plot") +
      tm_shape(historical) + 
      tm_raster(palette = palHist, breaks = seq(floor(cellStats(historical, range)[1]), cellStats(historical, range)[2] + 1, by = 1))+
      tm_layout(legend.title.size = .001,
                title = "Temperature\n(°C)",
                scale = 1, 
                attr.outside = FALSE,
                legend.outside = FALSE, 
                main.title = "Historical",
                legend.title.color = "white",
                main.title.fontface = 2,
                frame = TRUE,
                legend.width = 1.5, 
                legend.text.size = 1 
      ) +
      tm_compass(position = c("LEFT", "BOTTOM"))
    
    ##end 2 panel
  }
    else if(str_detect(myDescriptiveVariable, "minTemp")){
      
      raster1 <-  get(paste(myDescriptiveVariable, "_mw_combined", sep = ""))
      raster3 <- get(paste(myDescriptiveVariable, "_hw_combined", sep = ""))
      
      anomaly_Hw <- get(paste0(myDescriptiveVariable, "_hw_anomaly"))
      anomaly_Mw <- get(paste0(myDescriptiveVariable, "_mw_anomaly"))
      anomalyStack <- stack(anomaly_Mw, anomaly_Hw)
      crs(anomalyStack) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

      anomaly_1 <- tmap_mode("plot") +
        tm_shape(anomaly_Mw)+
        tm_raster(palette = "Blues", n = 6) +
        tm_layout(
          title = "Temperature\n(°C)",
          scale = 1,
          attr.outside = FALSE,
          legend.title.color = "white", 
          legend.title.size = .001,
          main.title = "MW Anomalies",
          main.title.fontface = 2,
          frame = TRUE,
          legend.width = 1.5, 
          legend.text.size = 1
        ) + 
        tm_compass(position = c("LEFT", "BOTTOM"))
      
      anomaly_2 <- tmap_mode("plot") +
        tm_shape(anomaly_Hw)+
        tm_raster(palette = "Blues", n = 6) +
        tm_layout(
          title = "Temperature\n(°C)",
          scale = 1,
          attr.outside = FALSE,
          legend.title.color = "white", 
          legend.title.size = .001,
          main.title = "HW Anomalies",
          main.title.fontface = 2,
          frame = TRUE,
          legend.width = 1.5, 
          legend.text.size = 1
        ) + 
        tm_scale_bar(position = c("RIGHT", "BOTTOM"))
      ##end anomalies
      
      ##start 2 panel
      
      static_vals_1 <- tmap_mode("plot") +
        tm_shape(raster1) +
        tm_raster(palette = get_brewer_pal("-RdBu", n = 8, plot = TRUE, stretch = TRUE), breaks = seq(floor(cellStats(raster1, range)[1]), cellStats(raster1, range)[2] + 1, by = 1)) + 
        tm_layout(
          title = "Temperature\n(°C)",
          scale = 1, 
          attr.outside = TRUE,
          legend.outside = FALSE,
          legend.title.color = "white", 
          legend.title.size = .001,
          main.title = "Moderate Warming",
          main.title.fontface = 2,
          frame = TRUE,
          legend.width = 1.5, 
          legend.text.size = 1 
        ) 
      
      static_vals_3 <- tmap_mode("plot")+
        tm_shape(raster3) +
        tm_raster(palette = get_brewer_pal("-PuOr", n = 8, plot = TRUE, stretch = TRUE), breaks = seq(floor(cellStats(raster3, range)[1]), cellStats(raster3, range)[2] + 1, by = 1)) +
        tm_layout(
          title = "Temperature\n(°C)",
          scale = 1, 
          attr.outside = FALSE,
          legend.outside = FALSE,
          legend.title.color = "white", 
          legend.title.size = .001,
          main.title = "High Warming",
          main.title.fontface = 2,
          frame = TRUE,
          legend.width = 1.5, 
          legend.text.size = 1 
        ) +
        tm_scale_bar(position = c("RIGHT", "BOTTOM"))
      
      
      historic_vals <- tmap_mode("plot") +
        tm_shape(historical) + 
        tm_raster(palette = palHist, breaks = seq(floor(cellStats(historical, range)[1]), cellStats(historical, range)[2] + 1, by = 1))+
        tm_layout(legend.title.size = .001,
                  title = "Temperature\n(°C)",
                  scale = 1, 
                  attr.outside = FALSE,
                  legend.outside = FALSE, 
                  main.title = "Historical",
                  legend.title.color = "white",
                  main.title.fontface = 2,
                  frame = TRUE,
                  legend.width = 1.5, 
                  legend.text.size = 1 
        ) +
        tm_compass(position = c("LEFT", "BOTTOM"))
      
      ##end 2 panel
    }
  else if(str_detect(myDescriptiveVariable, "ppt")){
    
    raster1 <- get(paste(myDescriptiveVariable, "_mw_combined", sep = ""))
    raster3 <- get(paste(myDescriptiveVariable, "_hw_combined", sep = ""))
    
    anomaly_Hw <- get(paste0(myDescriptiveVariable, "_hw_anomaly"))
    anomaly_Mw <- get(paste0(myDescriptiveVariable, "_mw_anomaly"))
    anomalyStack <- stack(anomaly_Mw, anomaly_Hw)
    crs(anomalyStack) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    
    palMw <- get_brewer_pal("Blues", plot = TRUE)
    palHw <- get_brewer_pal("Blues", plot = TRUE)
    
    static_comparison <- tmap_mode('plot') +
      tm_shape(anomalyStack) + 
      tm_raster(palette = list(palMw, palHw)) +
      tm_facets(ncol = 2, free.scales = TRUE) +
      tm_layout(legend.title.color = "white", 
                legend.title.size = .001, 
                scale = 1,
                title = c("Precipitation (mm)", "Precipitation (mm)"), 
                title.size = .9,
                panel.labels = c("MW Anomalies", "HW Anomalies"),
                panel.label.size = 1.1,
                compass.type = "arrow", attr.outside = TRUE, legend.outside = FALSE,
                legend.width = 1.5, 
                legend.text.size = 1 )+
      tm_scale_bar(position = c("RIGHT", "TOP") )+
      tm_compass(position = c("LEFT", "TOP")) 
    
    anomaly_1 <- tmap_mode("plot") +
      tm_shape(anomaly_Mw)+
      tm_raster(palette = "Blues", n = 6) +
      tm_layout(
        title = "Precipitation (mm)",
        scale = 1,
        attr.outside = FALSE,
        legend.title.color = "white", 
        legend.title.size = .001,
        main.title = "MW Anomalies",
        main.title.fontface = 2,
        frame = TRUE,
        legend.width = 1.5, 
        legend.text.size = 1
      ) + 
      tm_compass(position = c("LEFT", "BOTTOM"))
    
    anomaly_2 <- tmap_mode("plot") +
      tm_shape(anomaly_Hw)+
      tm_raster(palette = "Blues", n = 6) +
      tm_layout(
        title = "Precipitation (mm)",
        scale = 1,
        attr.outside = FALSE,
        legend.title.color = "white", 
        legend.title.size = .001,
        main.title = "HW Anomalies",
        main.title.fontface = 2,
        frame = TRUE,
        legend.width = 1.5, 
        legend.text.size = 1
      ) + 
      tm_scale_bar(position = c("RIGHT", "BOTTOM"))
    #end anomalies
    
    #start 2 panel
    static_vals_1 <- tmap_mode("plot") +
      tm_shape(raster1) +
      tm_raster(palette = get_brewer_pal("Blues", n = 8, plot = TRUE, stretch = TRUE), breaks = seq(floor(cellStats(raster1, range)[1]), cellStats(raster1, range)[2] + 5, by = 5)) + 
      tm_layout(
        title = "Precipitation (mm)",
        scale = 1, 
        attr.outside = TRUE,
        legend.outside = FALSE,
        legend.title.color = "white", 
        legend.title.size = .001,
        main.title = "Moderate Warming",
        main.title.fontface = 2,
        frame = TRUE,
        legend.width = 1.5 , 
        legend.text.size = 1
      ) 
    
    static_vals_3 <- tmap_mode("plot")+
      tm_shape(raster3) +
      tm_raster(palette = get_brewer_pal("Purples", n = 8, plot = TRUE, stretch = TRUE), breaks = seq(floor(cellStats(raster3, range)[1]), cellStats(raster3, range)[2] + 5, by = 5)) +
      tm_layout(
        title = "Precipitation (mm)",
        scale = 1, 
        attr.outside = FALSE,
        legend.outside = FALSE,
        legend.title.color = "white", 
        legend.title.size = .001,
        main.title = "High Warming",
        main.title.fontface = 2,
        frame = TRUE,
        legend.width = 1.5, 
        legend.text.size = 1 
      ) +
      tm_scale_bar(position = c("RIGHT", "BOTTOM"))
    
    
    historic_vals <- tmap_mode("plot") +
      tm_shape(historical) + 
      tm_raster(palette = palHist, breaks = seq(floor(cellStats(historical, range)[1]), cellStats(historical, range)[2] + 5, by = 5))+
      tm_layout(legend.title.size = .001,
                title = "Precipitation (mm)",
                scale = 1, 
                attr.outside = FALSE,
                legend.outside = FALSE, 
                main.title = "Historical",
                legend.title.color = "white",
                main.title.fontface = 2,
                frame = TRUE,
                legend.width = 1.5, 
                legend.text.size = 1 
      ) +
      tm_compass(position = c("LEFT", "BOTTOM"))
    
    #end 2 panel
    
    
  }
  else {
    
    raster1 <- get(paste(myDescriptiveVariable, "_mw_combined", sep = ""))
    raster3 <- get(paste(myDescriptiveVariable, "_hw_combined", sep = ""))

    anomaly_Hw <- get(paste0(myDescriptiveVariable, "_hw_anomaly"))
    anomaly_Mw <- get(paste0(myDescriptiveVariable, "_mw_anomaly"))
    anomalyStack <- stack(anomaly_Mw, anomaly_Hw)
    crs(anomalyStack) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    palMw <- get_brewer_pal("Oranges", n = 6, plot = TRUE)
    palHw <- get_brewer_pal("Oranges", n = 6, plot = TRUE)
    
    anomaly_1 <- tmap_mode("plot") +
      tm_shape(anomaly_Mw)+
      tm_raster(palette = "Oranges", n = 6) +
      tm_layout(
        title = "GDD > 5",
        scale = 1,
        attr.outside = FALSE,
        legend.title.color = "white", 
        legend.title.size = .001,
        main.title = "MW Anomalies",
        main.title.fontface = 2,
        frame = TRUE,
        legend.width = 1.5, 
        legend.text.size = 1
      ) + 
      tm_compass(position = c("LEFT", "BOTTOM"))
    
    anomaly_2 <- tmap_mode("plot") +
      tm_shape(anomaly_Hw)+
      tm_raster(palette = "Oranges", n = 6) +
      tm_layout(
        title = "GDD > 5",
        scale = 1,
        attr.outside = FALSE,
        legend.title.color = "white", 
        legend.title.size = .001,
        main.title = "HW Anomalies",
        main.title.fontface = 2,
        frame = TRUE,
        legend.width = 1.5, 
        legend.text.size = 1
      ) + 
      tm_scale_bar(position = c("RIGHT", "BOTTOM"))
    
#start 2 panel     
    
    static_vals_1 <- tmap_mode("plot") +
      tm_shape(raster1) +
      tm_raster(palette = get_brewer_pal("Oranges", n = 8, plot = TRUE, stretch = TRUE), breaks = seq(floor(cellStats(raster1, range)[1]), cellStats(raster1, range)[2] + 200, by = 200)) + 
      tm_layout(
        title = "Temperature\n(°C)",
        scale = 1, 
        attr.outside = TRUE,
        legend.outside = FALSE,
        legend.title.color = "white", 
        legend.title.size = .001,
        main.title = "Moderate Warming",
        main.title.fontface = 2,
        frame = TRUE,
        legend.width = 1.5,
        legend.text.size = 1 
      ) 
    
    static_vals_3 <- tmap_mode("plot")+
      tm_shape(raster3) +
      tm_raster(palette = get_brewer_pal("Reds", n = 8, plot = TRUE, stretch = TRUE), breaks = seq(floor(cellStats(raster3, range)[1]), cellStats(raster3, range)[2] + 200, by = 200)) +
      tm_layout(
        title = "Temperature\n(°C)",
        scale = 1, 
        attr.outside = FALSE,
        legend.outside = FALSE,
        legend.title.color = "white", 
        legend.title.size = .001,
        main.title = "High Warming",
        main.title.fontface = 2,
        frame = TRUE,
        legend.width = 1.5,
        legend.text.size = 1 
      ) +
      tm_scale_bar(position = c("RIGHT", "BOTTOM"))
    
    
    historic_vals <- tmap_mode("plot") +
      tm_shape(historical) + 
      tm_raster(palette = palHist, breaks = seq(floor(cellStats(historical, range)[1]), cellStats(historical, range)[2] + 200, by = 200))+
      tm_layout(legend.title.size = .001,
                title = "Temperature\n(°C)",
                scale = 1, 
                attr.outside = FALSE,
                legend.outside = FALSE, 
                main.title = "Historical",
                legend.title.color = "white",
                main.title.fontface = 2,
                frame = TRUE,
                legend.width = 1.5,
                legend.text.size = 1 
      ) +
      tm_compass(position = c("LEFT", "BOTTOM"))
    
   #end 2 panel
    
  }
  
  assign(
    x = paste(myDescriptiveVariable, "_anomalies_v5", sep = ""),
    value = tmap_arrange(anomaly_1, anomaly_2),
    envir = .GlobalEnv
  )

  assign(#need to make an ifelse for ppt if we want to keep CV
    x = paste(myDescriptiveVariable, "_raw_v5", sep = ""),
    value = tmap_arrange(historic_vals, static_vals_1, static_vals_3),
    envir = .GlobalEnv
      
    )
    
  
}



leafletList <- c("GDD5_05_10", "GDD5_04_09", "tmin_09", "tmin_10", "ppt_09", "ppt_10", "tmax_06", "tmax_07", "tmax_08", "tmax_09", "tmin_03", "tmin_04", "minTemp_12_03")

#load and save all of the interactive maps 
lapply(leafletList, FUN = loadLeaflet)

setwd("F:/climate_projection_data/bcvin_raster")
for(i in 1:length(leafletList)){
  saveWidget(get(paste(leafletList[i])), paste(leafletList[i], "_v5.html", sep = ""), selfcontained = TRUE) #added V# because wordpress doesn't like replacing files of the same name for media
}

#comprehensive list of static maps. Must manually open and save each png
lapply(leafletList, FUN = loadStatic)
GDD5_05_10_anomalies_v5
GDD5_05_10_raw_v5
GDD5_04_09_anomalies_v5
GDD5_04_09_raw_v5
tmin_09_anomalies_v5
tmin_09_raw_v5
tmin_10_anomalies_v5
tmin_10_raw_v5
ppt_09_anomalies_v5
ppt_09_raw_v5
ppt_10_anomalies_v5
ppt_10_raw_v5
tmax_06_anomalies_v5
tmax_06_raw_v5
tmax_07_anomalies_v5
tmax_07_raw_v5
tmax_08_anomalies_v5
tmax_08_raw_v5
tmax_09_anomalies_v5
tmax_09_raw_v5
tmin_03_anomalies_v5
tmin_03_raw_v5
tmin_04_anomalies_v5
tmin_04_raw_v5
minTemp_12_03_anomalies_v5
minTemp_12_03_raw_v5


ppt_gdd_list <- grep("GDD5|ppt", leafletList) 
staticList_ppt_gdd <- leafletList[ppt_gdd_list]

tmax_tmin_list <- grep("tmax|tmin|minTemp", leafletList)
staticList_tmax_tmin <- leafletList[tmax_tmin_list]

getPpt_mw <- get(paste("ppt_09", "_mw_anomaly", sep = ""))
getPpt_hw <- get(paste("ppt_09", "_hw_anomaly", sep = ""))
getPpt_mw_cv <- get(paste("ppt_09", "_mw_combined_cv", sep = ""))
getPpt_hw_cv <- get(paste("ppt_09", "_hw_combined_cv", sep = ""))

seq1 <- getPpt_mw %>%
  cellStats(. , range)
seq2 <- getPpt_hw %>%
  cellStats(. , range)
seq_cv1 <- getPpt_mw_cv %>%
  cellStats(. , range)
seq_cv2 <- getPpt_hw_cv %>%
  cellStats(. , range)


#example for non anomaly visualization

setwd("F:/climate_projection_data/bcvin_raster/combined")

tmax_06_hw <- raster("tmax_06_hw_combined.asc")
tmax_06_hw_sd <- raster("tmax_06_hw_combined_sd.asc")
tmax_06_mw <- raster("tmax_06_mw_combined.asc")
tmax_06_mw_sd <- raster("tmax_06_mw_combined_sd.asc")

crs(tmax_06_hw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(tmax_06_hw_sd) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(tmax_06_mw) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(tmax_06_mw_sd) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

palette_hw <- colorBin("Oranges", domain = c(cellStats(tmax_06_hw, min), cellStats(tmax_06_hw, max)), na.color = "transparent", bins = 7)
palette_mw <- colorBin("Oranges", domain = c(cellStats(tmax_06_mw, min), cellStats(tmax_06_mw, max)), na.color = "transparent", bins = 7)
palette_sd_hw <-colorBin("Greys", domain = c(cellStats(tmax_06_hw_sd, min), cellStats(tmax_06_hw_sd, max)), na.color = "transparent", bins = 5)
palette_sd_mw <-colorBin("Greys", domain = c(cellStats(tmax_06_mw_sd, min), cellStats(tmax_06_mw_sd, max)), na.color = "transparent", bins = 5)

tmax_06_leaflet_raw <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri World Topographic")%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  addRasterImage(tmax_06_mw, color = palette_mw, group = "Moderate Warming", opacity = .8) %>%
  addRasterImage(tmax_06_hw, color = palette_hw, group = "High Warming", opacity = .8) %>%
    addRasterImage(tmax_06_mw_sd, color = palette_sd_mw, group = "Standard Deviation Moderate Warming", opacity = .8 )%>%
  addRasterImage(tmax_06_hw_sd, color = palette_sd_hw, group = "Standard Deviation High Warming", opacity = .8 )%>%
  addLayersControl(baseGroups = c("Esri World Topographic", "Esri World Imagery"),
                   overlayGroups = c("Moderate Warming", "High Warming", "Standard Deviation Moderate Warming", "Standard Deviation High Warming"))%>%
  addLegend(position = "topright",  pal = palette_mw, title = "Temperature (°C)", group = "Moderate Warming", values = values(tmax_06_mw))%>%
  addLegend(position = "topright",  pal = palette_hw,title = "Temperature (°C)", group = "High Warming", values = values(tmax_06_hw))%>%
  addLegend(position = "topright",  pal = palette_sd_mw, title = "SD", group = "Standard Deviation Moderate Warming", values = values(tmax_06_mw_sd))%>%
  addLegend(position = "topright",  pal = palette_sd_hw, title = "SD", group = "Standard Deviation High Warming", values = values(tmax_06_hw_sd))%>%
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

mw_anomalies <- ls()[grep("mw_anomaly", ls())]
hw_anomalies <- ls()[grep("hw_anomaly", ls())]

for(i in 1:length(mw_anomalies)){
  assign(
    x = paste0(mw_anomalies[i], "_mean"),
    value = cellStats(get(mw_anomalies[i]), mean)
  )
  
}

for(j in 1:length(hw_anomalies)){
  assign(
    x = paste0(hw_anomalies[j], "_mean"),
    value = cellStats(get(hw_anomalies[j]), mean)
  )
  
}


values_list <- c()
mean_list <- grep("_mean", x = ls())

#keeping this for archive purposes. If Geoff says we should use CV (or once I can successfully interpret it) and decide we need it, we can use this script as a template for CV again
#loadStatic <- function(myDescriptiveVariable){ #this doesn't work quite yet. Still need more info on whether or not to keep CV 
#  if(str_detect(myDescriptiveVariable, "tmax")){
#    #start 4 panel graphs
#    
#    raster1 <-  get(paste(myDescriptiveVariable, "_mw_combined", sep = ""))
#    raster3 <- get(paste(myDescriptiveVariable, "_hw_combined", sep = ""))
#    
#    dataStack <- stack(raster1, raster2, raster3, raster4)
#    
#    palMw <- get_brewer_pal("Oranges", n = 6, plot = TRUE)
#    palHw <- get_brewer_pal("Oranges", n = 6, plot = TRUE)
#    
#    
#    static_comparison <- tmap_mode('plot') +
#      tm_shape(dataStack) + 
#      tm_raster(palette = list(palMw, palHw)) +
#      tm_facets(ncol = 2, free.scales = TRUE) +
#      tm_layout(legend.title.color = "white", 
#                legend.title.size = .001, 
#                title = c("Temperature\n(°C)", "Temperature\n(°C)"), 
#                title.size = .9,
#                panel.labels = c("MW", "HW"),
#                panel.label.size = 1.1,
#                scale = 1.2,
#                compass.type = "arrow", attr.outside = TRUE, legend.outside = FALSE)+
#      tm_compass(position = c("LEFT", "TOP")) 
#    
#    #end 4 panel
#    
#    #start 2 panel
#    
#    stack2 <- stack(raster1, raster3)
#    
#    static_vals <- tmap_mode('plot') +
#      tm_shape(stack2) + 
 #     tm_raster(palette = get_brewer_pal("Oranges", plot = TRUE))+
#      tm_facets(ncol = 2, free.scales.raster = FALSE) +
#      tm_layout(panel.labels = c("MW", "HW"), 
#                panel.label.size = 1.1,
#                legend.title.color = "white", 
#               legend.title.size = .001, 
#                title = "Temperature\n(°C)",
#                compass.type = "arrow",
#                scale = 1.2,
#                attr.outside = TRUE, 
#                legend.outside = FALSE)+
#      tm_scale_bar(position = c("RIGHT", "TOP") )+
#      tm_compass(position = c("LEFT", "TOP"))
#    
#    #end 2 panel
#    
#  }
#  
#  else if(str_detect(myDescriptiveVariable, "tmin|minTemp")){
#    
#    raster1 <-  get(paste(myDescriptiveVariable, "_mw_combined", sep = ""))
#    raster3 <- get(paste(myDescriptiveVariable, "_hw_combined", sep = ""))
#    
#    
#    dataStack <- stack(raster1, raster3)
#    
#    palMw <- get_brewer_pal("Blues", n = 6, plot = TRUE)
#    palHw <- get_brewer_pal("Blues", n = 6, plot = TRUE)
#    
#    static_comparison <- tmap_mode('plot') +
#      tm_shape(dataStack) + 
#      tm_raster(palette = list(palMw, palHw)) +
#      tm_facets(ncol = 2, free.scales = TRUE) +
#      tm_layout(legend.title.color = "white", 
#                legend.title.size = .001, 
#                title = c("Temperature\n(°C)", "Temperature\n(°C)"), 
#                title.size = .9,
#                scale = 1.2,
#                panel.labels = c("MW", "HW"),
#                panel.label.size = 1.1,
#                asp = 0,
#                compass.type = "arrow", attr.outside = TRUE, legend.outside = FALSE)+
#      tm_compass(position = c("LEFT", "TOP")) 
#    ##end 4 panel
#    
#    ##start 2 panel
#    stack2 <- stack(raster1, raster3)
#    
#    static_vals <- tmap_mode('plot') +
#      tm_shape(stack2) + 
#      tm_raster(palette = get_brewer_pal("Blues", plot = TRUE))+
#       tm_facets(ncol = 2, free.scales.raster = FALSE) +
#      tm_layout(panel.labels = c("MW", "HW"), 
#                panel.label.size = 1.1,
#                legend.title.color = "white", 
#                legend.title.size = .001, 
#                scale = 1.2,
#                title = "Temperature\n(°C)", 
#                compass.type = "arrow", 
#                attr.outside = TRUE, 
#                legend.outside = FALSE,
#                asp = 0)+
#      tm_scale_bar(position = c("RIGHT", "TOP") )+
#      tm_compass(position = c("LEFT", "TOP"))
#    
#    ##end 2 panel
#  }
#  
##  else if(str_detect(myDescriptiveVariable, "ppt")){
#    
#    raster1 <- get(paste(myDescriptiveVariable, "_mw_combined", sep = ""))
#    raster2 <- get(paste(myDescriptiveVariable, "_mw_combined_cv", sep = ""))
##    raster3 <- get(paste(myDescriptiveVariable, "_hw_combined", sep = ""))
#    raster4 <- get(paste(myDescriptiveVariable, "_hw_combined_cv", sep = ""))
#    
#    dataStack <- stack(raster1, raster2, raster3, raster4)
#    
#    palMw <- get_brewer_pal("Blues", plot = TRUE)
#    palMw_cv <- get_brewer_pal("Greys", plot = TRUE)
#    palHw <- get_brewer_pal("Blues", plot = TRUE)
#    palHw_cv <- get_brewer_pal("Greys", plot = TRUE)
#    
#    
#    static_comparison <- tmap_mode('plot') +
#      tm_shape(dataStack) + 
#      tm_raster(palette = list(palMw, palMw_cv, palHw, palHw_cv)) +
#      tm_facets(ncol = 4, free.scales = TRUE) +
#      tm_layout(legend.title.color = "white", 
#                legend.title.size = .001, 
#                scale = 1.2,
#                title = c("Precipitation (mm)", "CV", "Precipitation (mm)", "CV"), 
#                title.size = .9,
#                panel.labels = c("MW", "MW CV", "HW", "HW CV"),
#                panel.label.size = 1.1,
#                compass.type = "arrow", attr.outside = TRUE, legend.outside = FALSE)+
#      tm_compass(position = c("LEFT", "TOP")) 
#    
#    #end 4 panel
#    
#    #start 2 panel
#    stack2 <- stack(
#      raster1,
#      raster3
#    )
#    stack2_cv <- stack(
#      raster2,
#      raster4
#    )
#    
#    static_vals <- tmap_mode('plot') +
#      tm_shape(stack2) + 
#      tm_raster(palette = get_brewer_pal("Blues", plot = TRUE))+
#      tm_facets(ncol = 2, free.scales.raster = FALSE) +
#      tm_layout(panel.labels = c("MW", "HW"), 
#                panel.label.size = 1.1,
#                legend.title.color = "white", 
#                legend.title.size = .001, 
#                title = "Precipitation\n(mm)", 
#                scale = 1.2,
#                compass.type = "arrow", 
#                attr.outside = TRUE, 
#                legend.outside = FALSE)+
#      tm_scale_bar(position = c("RIGHT", "TOP") )+
#      tm_compass(position = c("LEFT", "TOP"))
#    
#    static_variation <- tmap_mode('plot') +
#      tm_shape(stack2_cv) + 
#      tm_raster(palette = get_brewer_pal("Greys", plot = TRUE))+
#      tm_facets(ncol = 2, free.scales.raster = FALSE) +
#      tm_layout(panel.labels = c("MW", "HW"), 
#                panel.label.size = 1.1,
#                legend.title.color = "white",
#                scale = 1.2,
#                legend.title.size = .001, 
#                title = "CV", 
#                compass.type = "arrow", 
#                attr.outside = TRUE, 
#                legend.outside = FALSE)+
#      tm_scale_bar(position = c("RIGHT", "TOP") )+
#      tm_compass(position = c("LEFT", "TOP"))
#    
#    
#    #end 2 panel
#    
#    
#  }
#  else {
#    
#    
#    raster1 <- get(paste(myDescriptiveVariable, "_mw_combined", sep = ""))
#    raster3 <- get(paste(myDescriptiveVariable, "_hw_combined", sep = ""))
#    
#    dataStack <- stack(
#      raster1,
#      raster3
#    )
#    
#    palMw <- get_brewer_pal("Oranges", n = 6, plot = TRUE)
#    palHw <- get_brewer_pal("Oranges", n = 6, plot = TRUE)
#    
#    static_comparison <- tmap_mode('plot') +
#      tm_shape(dataStack) + 
#      tm_raster(palette = list(palMw, palHw)) +
#      tm_facets(ncol = 2, free.scales = TRUE) +
#      tm_layout(legend.title.color = "white", 
#                scale = 1.2,
#                legend.title.size = .001, 
#                title = c("GDD > 5", "GDD > 5"), 
#                title.size = .9,
#                panel.labels = c("MW", "HW"),
#                panel.label.size = 1.1,
#                compass.type = "arrow", attr.outside = TRUE, legend.outside = FALSE)+
#      tm_compass(position = c("LEFT", "TOP")) 
#    
#    #start 2 panel     
#    
#    stack2 <- stack(
#      raster1,
#      raster3
#    )
#    
#    static_vals <- tmap_mode('plot') +
#      tm_shape(stack2) + 
#      tm_raster(palette = get_brewer_pal("Oranges", plot = TRUE))+
#      tm_facets(ncol = 2, free.scales.raster = FALSE) +
#      tm_layout(panel.labels = c("MW", "HW"), 
#                panel.label.size = 1.1,
#                legend.title.color = "white", 
#                legend.title.size = .001, 
#                scale = 1.2,
#                title = "GDD > 5", 
#                compass.type = "arrow", 
#                attr.outside = TRUE, 
#                legend.outside = FALSE)+
#      tm_scale_bar(position = c("RIGHT", "TOP") )+
#      tm_compass(position = c("LEFT", "TOP"))
#    
#    #end 2 panel
#    
#  }
#  
#  assign(
#    x = paste(myDescriptiveVariable, "_static_v3", sep = ""),
#    value = static_comparison,
#    envir = .GlobalEnv
#  )
#  
#  
#  assign(#need to make an ifelse for ppt if we want to keep CV
#    x = paste(myDescriptiveVariable, "_fixed_v3", sep = ""),
#    value = tmap_arrange(static_vals, static_variation),
#    envir = .GlobalEnv
#    
#  )
#  
#  
#}
