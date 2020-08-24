library(raster)
library(sp)
library(tidyverse)

#The plan is to use these functions as a pseudo package by calling this file using source("functions.R")
#in the analysis 
#For this to work, you must have the original folders output by ClimateBC stored in a folder named "bcvin_raster" in your current wd

####   Climate Projections Summary   ####

##   GDD from May to October (GDD > 5 & 0)
##   GDD from April to September (GDD > 5 & 0)
##   precipitation in September NEED TO UPDATE SCRIPT 
##   precipitation in October 
##   max temp in June 
##   max temp in July
##   max temp in August
##   max temp in September
##   min temperature in October 
##   min temperature in September 
##   min temp in April 
##   min temp in March 
##   min temp of December to March 

####   Projection Variation   ####

##   SD is created for each variable for each month after aggregated over the 20 year period
##   CV is created for only PPT but can easily be changed to include all variables
##   Histogram function allows any variable to be plotted in three different regimes (can do this manually with ggplot2 if they aren't satisfactory)
##   For nw, mw, and hw: take SD for each MEMBER (5 counts) then average them (created SD & CV for ppt for variation between members as well)

##function that reads the output folders of ClimateBC and assigns them logical variable names 
open_MAT <- function(startYear, endYear, member_as.string){
  for (i in startYear:endYear) {
    assign(paste("mat_", i, sep = ""), 
           raster(
             paste("bcvin_raster/CanESM2_RCP85_",
                   member_as.string,
                   "_", 
                   i, 
                   "MSY/MAT.asc", 
                   sep = "")
           ), envir = parent.frame()
       )
    
    }
  
  
}

#only do one member at a time!
open_MAT(2070, 2071, "r11i1p1")
open_MAT(2070, 2071, "r21i1p1")
open_MAT(2070, 2071, "r31i1p1")
open_MAT(2070, 2071, "r41i1p1")
open_MAT(2070, 2071, "r51i1p1")

##open & assign funciton for monthly Tmax
open_Tmax_m <- function(startYear, endYear, startMonth, endMonth, member_as.string){
  
  for (i in startYear:endYear) {
  for (j in startMonth:endMonth) {
    if(j < 10){
      assign(paste("tmax_0", j, "_", i, sep = ""), 
             raster(
               paste("bcvin_raster/CanESM2_RCP85_",
                     member_as.string,
                     "_",
                     i,
                     "MSY/Tmax",
                     "0",
                     j,
                     ".asc",
                     sep = "")
               
             ),
             envir = parent.frame()
      )
    }
    else{
      assign(paste("tmax_", j, "_", i, sep = ""), 
             raster(
               paste("bcvin_raster/CanESM2_RCP85_",
                     member_as.string,
                     "_",
                     i,
                     "MSY/Tmax",
                     j,
                     ".asc",
                     sep = "")
             ),
             envir = parent.frame()
      )
      
    }
  }
}
}

open_Tmax_m(2070, 2071, 9, 10, "r11i1p1")
open_Tmax_m(2070, 2071, 9, 10, "r21i1p1")
open_Tmax_m(2070, 2071, 9, 10, "r31i1p1")
open_Tmax_m(2070, 2071, 9, 10, "r41i1p1")
open_Tmax_m(2070, 2071, 9, 10, "r51i1p1")

##open & assign funciton for monthly Tmin

open_Tmin_m <- function(startYear, endYear, startMonth, endMonth, member_as.string){
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(
          paste("tmin_0", j, "_", i, sep = ""), 
          raster(
            paste("bcvin_raster/CanESM2_RCP85_",
                  member_as.string,
                  "_",
                  i,
                  "MSY/Tmin",
                  "0",
                  j,
                  ".asc",
                  sep = "")
            
          ),
          envir = parent.frame()
        ) #end assign
        
      }
      else{
        assign(
          paste("tmin_", j, "_", i, sep = ""), 
          raster(
            paste("bcvin_raster/CanESM2_RCP85_",
                  member_as.string,
                  "_",
                  i,
                  "MSY/Tmin",
                  j,
                  ".asc",
                  sep = "")
          ),
          envir = parent.frame()
        )#end assign
        
      }
    }
  }
  
}

open_Tmin_m(2070, 2071, 9, 10, "r11i1p1")
open_Tmin_m(2070, 2071, 9, 10, "r21i1p1")
open_Tmin_m(2070, 2071, 9, 10, "r31i1p1")
open_Tmin_m(2070, 2071, 9, 10, "r41i1p1")
open_Tmin_m(2070, 2071, 9, 10, "r51i1p1")


##open & assign funciton for monthly precipitation
#####################################################################NEED TO TEST USING DIFFERENT MEMBER OR START YEAR
#####################################################################I deleted PPT data in r1i1p1 mw
open_PPT_m <- function(startYear, endYear, startMonth, endMonth, member_as.string){
  
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(
          paste("ppt_0", j, "_", i, sep = ""), 
          raster(
            paste("bcvin_raster/CanESM2_RCP85_",
                  member_as.string,
                  "_",
                  i,
                  "MSY/PPT",
                  "0",
                  j,
                  ".asc",
                  sep = "")
            
          ),
          envir = parent.frame()
        ) #end assign
        
      }
      else{
        assign(
          paste("ppt_", j, "_", i, sep = ""), 
          raster(
            paste("bcvin_raster/CanESM2_RCP85_",
                  member_as.string,
                  "_",
                  i,
                  "MSY/PPT",
                  j,
                  ".asc",
                  sep = "")
          ),
          envir = parent.frame()
        )#end assign
        
      }
    }
    
    
  }
  
}

open_PPT_m(2070, 2071, 9, 10, "r11i1p1")
open_PPT_m(2070, 2071, 9, 10, "r21i1p1")
open_PPT_m(2070, 2071, 9, 10, "r31i1p1")
open_PPT_m(2070, 2071, 9, 10, "r41i1p1")
open_PPT_m(2070, 2071, 9, 10, "r51i1p1")

#open & assign function for GDD > 5

open_GDD5_m <- function(startYear, endYear, startMonth, endMonth, member_as.string) {
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(
          paste("GDD5_0", j, "_", i, sep = ""), 
          raster(
            paste("bcvin_raster/CanESM2_RCP85_",
                  member_as.string,
                  "_",
                  i,
                  "MSY/DD5",
                  "_0",
                  j,
                  ".asc",
                  sep = "")
            
          ),
          envir = parent.frame()
        ) #end assign
        
      }
      else{
        assign(
          paste("GDD5_", j, "_", i, sep = ""), 
          raster(
            paste("bcvin_raster/CanESM2_RCP85_",
                  member_as.string,
                  "_",
                  i,
                  "MSY/DD5",
                  "_",
                  j,
                  ".asc",
                  sep = "")
          ),
          envir = parent.frame()
        )#end assign
        
      }
    }
    
    
  }
  
}

open_GDD5_m(2070, 2071, 9, 10, "r11i1p1")
open_GDD5_m(2070, 2071, 9, 10, "r21i1p1")
open_GDD5_m(2070, 2071, 9, 10, "r31i1p1")
open_GDD5_m(2070, 2071, 9, 10, "r41i1p1")
open_GDD5_m(2070, 2071, 9, 10, "r51i1p1")


#open & assign function for GDD > 0
open_GDD0_m <- function(startYear, endYear, startMonth, endMonth, member_as.string) {
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(
          paste("GDD0_0", j, "_", i, sep = ""), 
          raster(
            paste("bcvin_raster/CanESM2_RCP85_",
                  member_as.string,
                  "_",
                  i,
                  "MSY/DD_0",
                  "_0",
                  j,
                  ".asc",
                  sep = "")
            
          ),
          envir = parent.frame()
        ) #end assign
        
      }
      else{
        assign(
          paste("GDD0_", j, "_", i, sep = ""), 
          raster(
            paste("bcvin_raster/CanESM2_RCP85_",
                  member_as.string,
                  "_",
                  i,
                  "MSY/DD_0",
                  "_",
                  j,
                  ".asc",
                  sep = "")
          ),
          envir = parent.frame()
        )#end assign
        
      }
    }
    
    
  }
  
}

open_GDD0_m(2070, 2071, 9, 10, "r11i1p1")
open_GDD0_m(2070, 2071, 9, 10, "r21i1p1")
open_GDD0_m(2070, 2071, 9, 10, "r31i1p1")
open_GDD0_m(2070, 2071, 9, 10, "r41i1p1")
open_GDD0_m(2070, 2071, 9, 10, "r51i1p1")

#########################################################same functions but for the historical dataset


##function that reads the output folders of ClimateBC and assigns them logical variable names 
open_MAT_historical <- function(startYear, endYear){ #finished & works
  for (i in startYear:endYear) {
    assign(paste("mat_", i, sep = ""), 
           raster(
             paste("bcvin_raster/Year_",
                   i, 
                   "MSY/MAT.asc", 
                   sep = "")
           ), envir = parent.frame()
    )
    
  }
  
  
}

open_MAT_historical(1970, 1971)

##open & assign funciton for monthly Tmax
open_Tmax_m_historical <- function(startYear, endYear, startMonth, endMonth){ #finished & works
  
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(paste("tmax_0", j, "_", i, sep = ""), 
               raster(
                 paste("bcvin_raster/Year_",
                       i,
                       "MSY/Tmax",
                       "0",
                       j,
                       ".asc",
                       sep = "")
                 
               ),
               envir = parent.frame()
        )
      }
      else{
        assign(paste("tmax_", j, "_", i, sep = ""), 
               raster(
                 paste("bcvin_raster/Year_",
                       i,
                       "MSY/Tmax",
                       j,
                       ".asc",
                       sep = "")
               ),
               envir = parent.frame()
        )
        
      }
    }
  }
}

open_Tmax_m_historical(1970, 1971, 9, 10)

##open & assign funciton for monthly Tmin

open_Tmin_m_historical <- function(startYear, endYear, startMonth, endMonth){ #finished & works
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(
          paste("tmin_0", j, "_", i, sep = ""), 
          raster(
            paste("bcvin_raster/Year_",
                  i,
                  "MSY/Tmin",
                  "0",
                  j,
                  ".asc",
                  sep = "")
            
          ),
          envir = parent.frame()
        ) #end assign
        
      }
      else{
        assign(
          paste("tmin_", j, "_", i, sep = ""), 
          raster(
            paste("bcvin_raster/Year_",
                  i,
                  "MSY/Tmin",
                  j,
                  ".asc",
                  sep = "")
          ),
          envir = parent.frame()
        )#end assign
        
      }
    }
  }
  
}

open_Tmin_m_historical(1970, 1971, 9, 10)

##open & assign funciton for monthly precipitation
#####################################################################NEED TO TEST USING DIFFERENT MEMBER OR START YEAR
#####################################################################I deleted PPT data in r1i1p1 mw
open_PPT_m_historical <- function(startYear, endYear, startMonth, endMonth){ #finished & works
  
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(
          paste("ppt_0", j, "_", i, sep = ""), 
          raster(
            paste("bcvin_raster/Year_",
                  i,
                  "MSY/PPT",
                  "0",
                  j,
                  ".asc",
                  sep = "")
            
          ),
          envir = parent.frame()
        ) #end assign
        
      }
      else{
        assign(
          paste("ppt_", j, "_", i, sep = ""), 
          raster(
            paste("bcvin_raster/Year_",
                  i,
                  "MSY/PPT",
                  j,
                  ".asc",
                  sep = "")
          ),
          envir = parent.frame()
        )#end assign
        
      }
    }
    
    
  }
  
}

open_PPT_m_historical(1970, 1971, 9, 10)

#open & assign function for GDD > 5

open_GDD5_m_historical <- function(startYear, endYear, startMonth, endMonth) { #finished
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(
          paste("GDD5_0", j, "_", i, sep = ""), 
          raster(
            paste("bcvin_raster/Year_",
                  i,
                  "MSY/DD5",
                  "_0",
                  j,
                  ".asc",
                  sep = "")
            
          ),
          envir = parent.frame()
        ) #end assign
        
      }
      else{
        assign(
          paste("GDD5_", j, "_", i, sep = ""), 
          raster(
            paste("bcvin_raster/Year_",
                  i,
                  "MSY/DD5",
                  "_",
                  j,
                  ".asc",
                  sep = "")
          ),
          envir = parent.frame()
        )#end assign
        
      }
    }
    
    
  }
  
}

open_GDD5_m_historical(1970, 1971, 9, 10)

#open & assign function for GDD < 0
open_GDD0_m_historical <- function(startYear, endYear, startMonth, endMonth) { #finished
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(
          paste("GDD0_0", j, "_", i, sep = ""), 
          raster(
            paste("bcvin_raster/Year_",
                  i,
                  "MSY/DD_0",
                  "_0",
                  j,
                  ".asc",
                  sep = "")
            
          ),
          envir = parent.frame()
        ) #end assign
        
      }
      else{
        assign(
          paste("GDD0_", j, "_", i, sep = ""), 
          raster(
            paste("bcvin_raster/Year_",
                  i,
                  "MSY/DD_0",
                  "_",
                  j,
                  ".asc",
                  sep = "")
          ),
          envir = parent.frame()
        )#end assign
        
      }
    }
    
    
  }
  
}

open_GDD0_m_historical(1970, 1971, 9, 10)

#aggregating monthly variable over the 20 year period, averaging, and SD 
#NOTE: these are going to eventually be averaged between member_as.strings. STORE IN A DESCRIPTIVE FOLDER
#Names do not include which member_as.string the data is from to simplify code
#labels will be as follows: myVariable_as.string_month_warmingScenario_as.string
#myVariable_as.string = Tmax, Tmin, PPT, GDD0, or GDD5
#month = 1 : 12 depending on what month it is. This function should cycle through all interested months
#warmingScenario_as.string = moderate warming, mw (2040-2059); high warming, hw (2070-2089); no warming, nw (1970 - 1989)


########################################################################################
###TESTING
##CURRENTLY WORKS FOR: Tmax, Tmin
##Doesn't work for PPT (the SD is on the magnitude of 10's ) and I think GDD but I haven't tried yet
##Check with Geoff if dividing SD by 10 is a rational decision
##MUST USE MORE THAN ONE YEAR 
aggregate_warmingScenario_m <- function(startYear, endYear, startMonth, endMonth, myVariable_as.string){
  
  for(i in startMonth:endMonth) {
    
    
    if(startYear >= 2040 && startYear <= 2059){
      if(i < 10) {
        assign(
          paste(myVariable_as.string, "_0", i, "_", "mw", sep = ""),
          lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = mean, na.rm = TRUE) %>%
            {if (myVariable_as.string == "tmax" || myVariable_as.string == "tmin") 
              calc( . , fun = function(x){x/10})
              else .}
          ,
          envir = parent.frame()
        )#end assign
        assign(
          paste(myVariable_as.string, "_0", i, "_", "mw_sd", sep = ""),
          lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = sd, na.rm = TRUE) %>%
            {if (myVariable_as.string == "tmax" || myVariable_as.string == "tmin") 
              calc( . , fun = function(x){x/10})
              else .}
          ,
          envir = parent.frame()
        )#end assign
        if(str_detect(myVariable_as.string, "ppt")){
        assign(
        paste(myVariable_as.string, "_0", i, "_", "mw_cv", sep = ""),
        lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
          stack() %>%
          calc(fun = cv, na.rm = TRUE) 
        ,
        envir = parent.frame()
        )#end assign
        }
      }
      if(i >= 10) {
        assign(
          paste(myVariable_as.string, "_", i, "_", "mw", sep = ""),
          lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = mean, na.rm = TRUE) %>%
            {if (myVariable_as.string == "tmax" || myVariable_as.string == "tmin") 
              calc( . , fun = function(x){x/10})
              else .}
          ,
          envir = parent.frame()
        )#end assign
        assign(
          paste(myVariable_as.string, "_", i, "_", "mw_sd", sep = ""),
          lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = sd, na.rm = TRUE) %>%
            {if (myVariable_as.string == "tmax" || myVariable_as.string == "tmin") 
              calc( . , fun = function(x){x/10})
              else .}         
          ,
          envir = parent.frame()
        )
        if(str_detect(myVariable_as.string, "ppt")){
          assign(
            paste(myVariable_as.string, "_", i, "_", "mw_cv", sep = ""),
            lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
              stack() %>%
              calc(fun = cv, na.rm = TRUE) 
            ,
            envir = parent.frame()
          )#end assign
        }
      }
    }#end if statement that determines mw 
    
    
    if(startYear >= 2070 && startYear <= 2089){
      if(i < 10) {
        assign(
          paste(myVariable_as.string, "_0", i, "_", "hw", sep = ""),
          lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = mean, na.rm = TRUE) %>%
            {if (myVariable_as.string == "tmax" || myVariable_as.string == "tmin") 
              calc( . , fun = function(x){x/10})
              else .}
          ,
          envir = parent.frame()
        )#end assign
        assign(
          paste(myVariable_as.string, "_0", i, "_", "hw_sd", sep = ""),
          lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = sd, na.rm = TRUE) %>%
            {if (myVariable_as.string == "tmax" || myVariable_as.string == "tmin") 
              calc( . , fun = function(x){x/10})
              else .}
          ,
          envir = parent.frame()
        )#end assign
        if(str_detect(myVariable_as.string, "ppt")){
          assign(
            paste(myVariable_as.string, "_0", i, "_", "hw_cv", sep = ""),
            lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
              stack() %>%
              calc(fun = cv, na.rm = TRUE) 
            ,
            envir = parent.frame()
          )#end assign
        }
      }
      if(i >= 10) {
        assign(
          paste(myVariable_as.string, "_", i, "_", "hw", sep = ""),
          lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = mean, na.rm = TRUE) %>%
            {if (myVariable_as.string == "tmax" || myVariable_as.string == "tmin") 
              calc( . , fun = function(x){x/10})
              else .}
          ,
          envir = parent.frame()
        )#end assign
        assign(
          paste(myVariable_as.string, "_", i, "_", "hw_sd", sep = ""),
          lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = sd, na.rm = TRUE) %>%
            {if (myVariable_as.string == "tmax" || myVariable_as.string == "tmin") 
              calc( . , fun = function(x){x/10})
              else .}
          ,
          envir = parent.frame()
        )
        if(str_detect(myVariable_as.string, "ppt")){
          assign(
            paste(myVariable_as.string, "_", i, "_", "hw_cv", sep = ""),
            lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
              stack() %>%
              calc(fun = cv, na.rm = TRUE) 
            ,
            envir = parent.frame()
          )#end assign
        }
      }
    }#end if statement that determines hw 
    
    
    if(startYear >= 1970 && startYear <= 1989){
      if(i < 10) {
        assign(
          paste(myVariable_as.string, "_0", i, "_", "nw", sep = ""),
          lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = mean, na.rm = TRUE) %>%
            {if (myVariable_as.string == "tmax" || myVariable_as.string == "tmin") 
              calc( . , fun = function(x){x/10})
              else .}
          ,
          envir = parent.frame()
        )#end assign
        assign(
          paste(myVariable_as.string, "_0", i, "_", "nw_sd", sep = ""),
          lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = sd, na.rm = TRUE) %>%
            {if (myVariable_as.string == "tmax" || myVariable_as.string == "tmin") 
              calc( . , fun = function(x){x/10})
              else .}
          ,
          envir = parent.frame()
        )#end assign
        if(str_detect(myVariable_as.string, "ppt")){
          assign(
            paste(myVariable_as.string, "_0", i, "_", "nw_cv", sep = ""),
            lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
              stack() %>%
              calc(fun = cv, na.rm = TRUE) 
            ,
            envir = parent.frame()
          )#end assign
        }
      }
      if(i >= 10) {
        assign(
          paste(myVariable_as.string, "_", i, "_", "nw", sep = ""),
          lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = mean, na.rm = TRUE) %>%
            {if (myVariable_as.string == "tmax" || myVariable_as.string == "tmin") 
              calc( . , fun = function(x){x/10})
              else .}
          ,
          envir = parent.frame()
        )#end assign
        assign(
          paste(myVariable_as.string, "_", i, "_", "nw_sd", sep = ""),
          lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = sd, na.rm = TRUE) %>%
            {if (myVariable_as.string == "tmax" || myVariable_as.string == "tmin") 
              calc( . , fun = function(x){x/10})
              else .}
          ,
          envir = parent.frame()
        )
        if(str_detect(myVariable_as.string, "ppt")){
          assign(
            paste(myVariable_as.string, "_", i, "_", "nw_cv", sep = ""),
            lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
              stack() %>%
              calc(fun = cv, na.rm = TRUE) 
            ,
            envir = parent.frame()
          )#end assign
        }
      }
    }#end if statement that determines nw
  }
}
#startYear, endYear, startMonth, endMonth, myVariable_as.string
#run for every member
aggregate_warmingScenario_m(2070, 2071, 9, 10, "tmax")
aggregate_warmingScenario_m(2070, 2071, 9, 10, "tmin")
aggregate_warmingScenario_m(2070, 2071, 9, 10, "ppt")
aggregate_warmingScenario_m(2070, 2071, 9, 10, "GDD5")
aggregate_warmingScenario_m(2070, 2071, 9, 10, "GDD0")

#only run once
aggregate_warmingScenario_m(1970, 1971, 9, 10, "tmax")
aggregate_warmingScenario_m(1970, 1971, 9, 10, "tmin")
aggregate_warmingScenario_m(1970, 1971, 9, 10, "ppt")
aggregate_warmingScenario_m(1970, 1971, 9, 10, "GDD5")
aggregate_warmingScenario_m(1970, 1971, 9, 10, "GDD0")


########################################################################################

#function to aggregate MAT


aggregate_mat <- function(startYear, endYear, warmingScenario_as.string){
  assign(
    paste("mat_", warmingScenario_as.string, sep = ""), 
    lapply( paste("mat_", startYear:endYear, sep = ""), get ) %>%
      stack() %>%
      calc(fun = mean, na.rm = TRUE) %>%
      calc( . , fun = function(x){x/10}),
    envir = parent.frame()
  )
  assign(paste("mat_", warmingScenario_as.string, "_sd", sep = ""),
         lapply( paste("mat_", startYear:endYear, sep = ""), get ) %>%
           stack() %>%
           calc(fun = sd, na.rm = TRUE) %>%
           calc( . , fun = function(x){x/10}),
         envir = parent.frame()
  )
  
}

aggregate_mat(2070, 2071, "hw")
aggregate_mat(1970, 1971, "nw")

#function to write monthly for mw/hw/nw 

write_monthly_var <- function(startMonth, endMonth, myVariable_as.string, warmingScenario_as.string){ 
  for(i in startMonth:endMonth){ 
    if(i < 10){ #for aggregated mean file
      writeRaster(
        x = get(paste(myVariable_as.string, "_0", i, "_", warmingScenario_as.string, sep = "")),
        filename = paste(myVariable_as.string, "_0", i, "_", warmingScenario_as.string, ".asc", sep = "")
      )
    }
    if(i >= 10){ #for aggregated mean file
      writeRaster(
        x = get(paste(myVariable_as.string, "_", i, "_", warmingScenario_as.string, sep = "")),
        filename = paste(myVariable_as.string, "_", i, "_", warmingScenario_as.string, ".asc", sep = "")
      )
    }
    if(i < 10){ #for aggregated sd file
      writeRaster(
        x = get(paste(myVariable_as.string, "_0", i, "_", warmingScenario_as.string, "_sd", sep = "")),
        filename = paste(myVariable_as.string, "_0", i, "_", warmingScenario_as.string, "_sd", ".asc", sep = "")
      )
    }
    if(i >= 10){ #for aggregated sd file
      writeRaster(
        x = get(paste(myVariable_as.string, "_", i, "_", warmingScenario_as.string, "_sd",sep = "")),
        filename = paste(myVariable_as.string, "_", i, "_", warmingScenario_as.string, "_sd", ".asc", sep = "")
      )
    }
    if(str_detect(myVariable_as.string, "ppt")){
      if(i < 10){ #for aggregated sd file
        writeRaster(
          x = get(paste(myVariable_as.string, "_0", i, "_", warmingScenario_as.string, "_cv", sep = "")),
          filename = paste(myVariable_as.string, "_0", i, "_", warmingScenario_as.string, "_cv", ".asc", sep = "")
        )
      }
      if(i >= 10){ #for aggregated sd file
        writeRaster(
          x = get(paste(myVariable_as.string, "_", i, "_", warmingScenario_as.string, "_cv",sep = "")),
          filename = paste(myVariable_as.string, "_", i, "_", warmingScenario_as.string, "_cv", ".asc", sep = "")
        )
      }
    }
  }
}

#run for every member
write_monthly_var(9, 10, "tmax", "hw")
write_monthly_var(9, 10, "tmin", "hw")
write_monthly_var(9, 10, "GDD5", "hw")
write_monthly_var(9, 10, "GDD0", "hw")
write_monthly_var(9, 10, "ppt", "hw")

#only run once
write_monthly_var(9, 10, "tmax", "nw")
write_monthly_var(9, 10, "tmin", "nw")
write_monthly_var(9, 10, "GDD5", "nw")
write_monthly_var(9, 10, "GDD0", "nw")
write_monthly_var(9, 10, "ppt", "nw")

#function to write yearly for mw/hw/nw

write_yearly_MAT <- function(warmingScenario_as.string){
  writeRaster(
    x = get(paste("mat", warmingScenario_as.string, sep = "_")),
    filename = paste("mat_", warmingScenario_as.string, ".asc", sep = "")
    )
  writeRaster(
    x = get(paste("mat_", warmingScenario_as.string, "_sd", sep = "")),
    filename = paste("mat_", warmingScenario_as.string, "_sd.asc", sep = "")
  )
  
}

write_yearly_MAT("hw")

#only run once
write_yearly_MAT("nw")

#function to aggregate myVariable_as.string to any groups of months
#I won't be creating a custom writeRaster function for these because they will all be one-offs
#I don't think that SD should be included in this output because it would be artificially high.
#If SD is desired, calculate it with the difference between member_as.strings to get worthwhile data
#Incomplete

combine_monthly_vars <- function(startMonth, endMonth, myVariable_as.string, warmingScenario_as.string) { #finished and works for ppt & nw at least
  if(startMonth > endMonth){ #i.e. Monthly Tmin between October and March:
    for(i in c(startMonth:12, 1:endMonth)) { 
      
      if(startMonth >= 10 && endMonth < 10){ #when start month is double digit and end month is single digit
        assign(
          x = paste(myVariable_as.string, "_", startMonth, "_0", endMonth, "_", warmingScenario_as.string, sep = ""),
          
          value = lapply( #if statements are to determine which naming scheme to write
            {if(i < 10){
              paste(myVariable_as.string, "_0", i, "_", warmingScenario_as.string, sep = "")
            }
              
              else{
                paste(myVariable_as.string, "_", i, "_", warmingScenario_as.string, sep = "")
              }}
            , 
            FUN = get) %>%
            stack() %>%
            calc(. , fun = mean)
          ,
          envir = parent.frame()
          
        )
      }
      
      
      if(startMonth < 10 && endMonth < 10){ #when start and end month are both single digit
        assign(
          x = paste(myVariable_as.string, "_0", startMonth, "_0", endMonth, "_", warmingScenario_as.string, sep = ""),
          
          value = lapply(
            paste(myVariable_as.string, "_0", i, "_", warmingScenario_as.string, sep = "")
            , 
            FUN = get) %>%
            stack() %>%
            calc(. , fun = mean)
          ,
          envir = parent.frame()
          
        )
        
      }
    }
    
  }
  ######################ELSE IS NOT DONE. MAKE SURE TO CHANGE THE ASSIGNED VARIABLE NAME AND ANY PRINTING 
  else{ #three cases: single w/ double, single w/ single, double w/ double
    for(i in startMonth:endMonth) { #i.e. June, July, August Tmax 
      #passes my logic & visual check      
      if(startMonth < 10 && endMonth < 10){ #i.e. 01 - 09
        assign(
          x = paste(myVariable_as.string, "_0", startMonth, "_0", endMonth, "_", warmingScenario_as.string, sep = ""),
          
          value = lapply( 
            paste(myVariable_as.string, "_0", i, "_", warmingScenario_as.string, sep = "")
            , 
            FUN = get) %>%
            stack() %>%
            calc(. , fun = mean)
          ,
          envir = parent.frame()
          
        )
        
      }
      
      
      if(startMonth < 10 && endMonth >= 10){ #i.e. 09 - 12
        assign(                              #passes my logic & visual test
          x = paste(myVariable_as.string, "_0", startMonth, "_", endMonth, "_", warmingScenario_as.string, sep = ""),
          
          value = lapply( #if statements are to determine which naming scheme to write
            {if(i < 10){
              paste(myVariable_as.string, "_0", i, "_", warmingScenario_as.string, sep = "")
            }
              
              else{
                paste(myVariable_as.string, "_", i, "_", warmingScenario_as.string, sep = "")
              }}
            , 
            FUN = get) %>%
            stack() %>%
            calc(. , fun = mean)
          ,
          envir = parent.frame()
          
        )
        
      }
      
      if(startMonth >= 10 && endMonth >= 10){ #i.e. 10 - 12
        assign(                               #passes my logic & visual test
          x = paste(myVariable_as.string, "_", startMonth, "_", endMonth, "_", warmingScenario_as.string, sep = ""),
          
          value = lapply(
            paste(myVariable_as.string, "_", i, "_", warmingScenario_as.string, sep = "")
            , 
            FUN = get) %>%
            stack() %>%
            calc(. , fun = mean)
          ,
          envir = parent.frame()
          
        )
        
      }
      
      
    }
    
  }
  
}
#remember to manually write these with writeRaster()
combine_monthly_vars(9, 10, "tmax", "hw")
combine_monthly_vars(9, 10, "tmin", "hw")
combine_monthly_vars(9, 10, "ppt", "hw")
combine_monthly_vars(9, 10, "GDD5", "hw")
combine_monthly_vars(9, 10, "GDD0", "hw")

combine_monthly_vars(9, 10, "tmax", "nw")
combine_monthly_vars(9, 10, "tmin", "nw")
combine_monthly_vars(9, 10, "GDD5", "nw")
combine_monthly_vars(9, 10, "GDD0", "nw")
combine_monthly_vars(9, 10, "ppt", "nw")



########################################################################################################################
#open all members
#myDescriptiveVariable should be the exact filename, as a string without the file extension, that was output by any of the writeRaster functions 
#the output variable names are truncated to keep clutter down

open_members <- function(myDescriptiveVariable_as.string){ #tested and works
 
  assign(
    x = paste(myDescriptiveVariable_as.string, "_r1", sep = ""),
    value = raster(paste( "bcvin_raster/r11i1p1/", myDescriptiveVariable_as.string, ".asc", sep = "")),
    envir = parent.frame()
  )
 
  assign(
    x = paste(myDescriptiveVariable_as.string, "_r2", sep = ""),
    value = raster(paste( "bcvin_raster/r21i1p1/", myDescriptiveVariable_as.string, ".asc", sep = "")), 
    envir = parent.frame()
  )
 
  assign(
    x = paste(myDescriptiveVariable_as.string, "_r3", sep = ""),
    value = raster(paste( "bcvin_raster/r31i1p1/", myDescriptiveVariable_as.string, ".asc", sep = "")), 
    envir = parent.frame()
  )
 
  assign(
    x = paste(myDescriptiveVariable_as.string, "_r4", sep = ""),
    value = raster(paste( "bcvin_raster/r41i1p1/", myDescriptiveVariable_as.string, ".asc", sep = "")), 
    envir = parent.frame()
  )
  
  assign(
    x = paste(myDescriptiveVariable_as.string, "_r5", sep = ""),
    value = raster(paste( "bcvin_raster/r51i1p1/", myDescriptiveVariable_as.string, ".asc", sep = "")), 
    envir = parent.frame()
  )
  assign(
    x = paste(myDescriptiveVariable_as.string, "_historical", sep = ""),
    value = raster(paste( "bcvin_raster/historical/", myDescriptiveVariable_as.string, ".asc", sep = "")), 
    envir = parent.frame()
  )
  
}

open_members("tmax_09_hw")
open_members("tmax_10_hw")
open_members("tmin_09_hw")
open_members("tmin_10_hw")
open_members("GDD0_09_hw")
open_members("GDD0_10_hw")
open_members("GDD5_09_hw")
open_members("GDD5_10_hw")
open_members("ppt_09_hw")
open_members("ppt_10_hw")
open_members("mat_hw")

#in this case myDescriptiveVariable does NOT include any individual r's
combine_all_members <- function(myDescriptiveVariable){ #tested and works
  assign(
    x = paste(myDescriptiveVariable, "_combined", sep = ""),
    value = 
      lapply(paste(myDescriptiveVariable, "_r", 1:5, sep = ""), get) %>%
      stack() %>%
      calc(. , fun = mean),
    envir = parent.frame()
  )
  assign(
    x = paste(myDescriptiveVariable, "_combined_sd", sep = ""),
    value = 
      lapply(paste(myDescriptiveVariable, "_r", 1:5, sep = ""), get) %>%
      stack() %>%
      calc(. , fun = sd),
    envir = parent.frame()
  )
  
  if(str_detect(myDescriptiveVariable, "ppt") == TRUE) { #expressed as a percentage
    assign(
      x = paste(myDescriptiveVariable, "_combined_cv", sep = ""),
      value = 
        lapply(paste(myDescriptiveVariable, "_r", 1:5, sep = ""), get) %>%
        stack() %>%
        calc(. , fun = cv),
      envir = parent.frame()
    )
    
  }
  
  
}
  
combine_all_members("tmax_09_hw")
combine_all_members("tmax_10_hw")
combine_all_members("tmin_09_hw")
combine_all_members("tmin_10_hw")
combine_all_members("GDD0_09_hw")
combine_all_members("GDD0_10_hw")
combine_all_members("GDD5_09_hw")
combine_all_members("GDD5_10_hw")
combine_all_members("ppt_09_hw")
combine_all_members("ppt_10_hw")
combine_all_members("mat_hw")


#No function to open historical because there is only one final file in the historical group


#function to plot histograms
#outputs faceted histogram for all 5 members 
#myDescriptiveVariable = variable name without the *r*!!!!
#plotStyle == "stack", "facet", or "density"
plot_histogram_allmembers <- function(myDescriptiveVariable, plotStyle){ #tested and works
  #would be nice to have a subtitle that describes the month or group of months but that can be added later using str_detect
  
  for( i in 1:5 ){
  assign(
    x = paste("df_r", i, sep = ""),
    value = {
      get(paste(myDescriptiveVariable, "_r", i, sep = "")) %>%
        as.data.frame() %>%
      mutate(. , member = paste("r", i, sep = "")) %>%
        rename(., value = str2lang(paste(myDescriptiveVariable)), member = member) 
    },
    envir = parent.frame()
    #should only exist in local environment, but I don't know how to do that syntactically yet
  )
  }
  assign( 
    x = "df_combined",
    value = rbind(df_r1, df_r2, df_r3, df_r4, df_r5)      ,
    envir = parent.frame()
      )
  
  if(str_detect(myDescriptiveVariable, "ppt") == TRUE){ 
    lab = "Accumulated Precipitation (mm)"
    plotTitle = "Comparing Precipitation Projections Between CanESM2 Members"
    nbins = 38
  }
  
  else if(str_detect(myDescriptiveVariable, "tmax") == TRUE){ 
    lab = "Temperature (°C)"
    plotTitle = "Comparing Mean Maximum Temperature Projections Between CanESM2 Members"
    nbins = 14
    }
  else if(str_detect(myDescriptiveVariable, "tmin") == TRUE){
    lab = "Temperature (°C)"
    plotTitle = "Comparing Mean Minimum Temperature Projections Between CanESM2 Members"
  }
  else if(str_detect(myDescriptiveVariable, "mat")){
    lab = "Temperature (°C)"
    plotTitle = "Comparing Mean Annual Temperature Projections Between CanESM2 Members"
    
  }
  else if(str_detect(myDescriptiveVariable, "GDD5") == TRUE){
    lab = "Degree Days > 5"
    plotTitle = "Comparing Precipitation Projections Between CanESM2 Members"
  }
  
  else if(str_detect(myDescriptiveVariable, "GDD0") == TRUE){
    lab = "Degree Days < 0"
    plotTitle = "Comparing Precipitation Projections Between CanESM2 Members"
    }
  
  if(plotStyle == "facet"){
    aesthetics <-
      ggplot(df_combined, mapping = aes(x = value, fill = member)) +
      geom_histogram(bins = nbins) + 
      xlab(lab) +
      ylab("Count") +
      facet_wrap(~member) +
      theme(legend.position = "none") +
      ggtitle(plotTitle)
    
  }
  
  if(plotStyle == "stack"){
    aesthetics <- 
      ggplot(df_combined, mapping = aes(x = value, y = ..count.., fill = member)) +
      geom_histogram(bins = nbins, alpha = 0.8) + 
      xlab(lab) +
      ylab("Count") +
      ggtitle(plotTitle)
    
  }
  
  if(plotStyle == "density") {
    aesthetics <- 
    ggplot(df_combined, mapping = aes(x = value, fill = member)) +
      geom_density(alpha = 0.8) + 
      xlab(lab) +
      ylab("Density") +
      ggtitle(plotTitle)
    
  }
  
  aesthetics
        
  
  }
  


plot_histogram_allmembers("ppt_10_hw", "density")
  
test <- ppt_09_hw_combined %>%
  as.data.frame() %>%
  rename(value = layer) 

IQR(test$value, na.rm = TRUE)
