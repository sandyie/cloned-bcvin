library(raster)
library(sp)
library(tidyverse)

#This file is for sourcing in other scripts. There are only functions and no code that will create any variables without being called
##function that reads the output folders of ClimateBC and assigns them logical variable names 
##Less notes as well
##NOTD: you will most likely need to change the drive letter for your specific computer
open_MAT <- function(startYear, endYear, member_as.string){
  for (i in startYear:endYear) {
    assign(paste("mat_", i, sep = ""), 
           raster(
             paste("D:/climate_projection_data/bcvin_raster/CanESM2_RCP85_",
                   member_as.string,
                   "_", 
                   i, 
                   "MSY/MAT.asc", 
                   sep = "")
           ), envir = .GlobalEnv
    )
    
  }
  
  
}


##open & assign funciton for monthly Tmax
open_Tmax_m <- function(startYear, endYear, startMonth, endMonth, member_as.string){
  
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(paste("tmax_0", j, "_", i, sep = ""), 
               raster(
                 paste("D:/climate_projection_data/bcvin_raster/CanESM2_RCP85_",
                       member_as.string,
                       "_",
                       i,
                       "MSY/Tmax",
                       "0",
                       j,
                       ".asc",
                       sep = "")
                 
               ),
               envir = .GlobalEnv
        )
      }
      else{
        assign(paste("tmax_", j, "_", i, sep = ""), 
               raster(
                 paste("D:/climate_projection_data/bcvin_raster/CanESM2_RCP85_",
                       member_as.string,
                       "_",
                       i,
                       "MSY/Tmax",
                       j,
                       ".asc",
                       sep = "")
               ),
               envir = .GlobalEnv
        )
        
      }
    }
  }
}

##open & assign funciton for monthly Tmin
open_Tmin_m <- function(startYear, endYear, startMonth, endMonth, member_as.string){
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(
          paste("tmin_0", j, "_", i, sep = ""), 
          raster(
            paste("D:/climate_projection_data/bcvin_raster/CanESM2_RCP85_",
                  member_as.string,
                  "_",
                  i,
                  "MSY/Tmin",
                  "0",
                  j,
                  ".asc",
                  sep = "")
            
          ),
          envir = .GlobalEnv
        ) #end assign
        
      }
      else{
        assign(
          paste("tmin_", j, "_", i, sep = ""), 
          raster(
            paste("D:/climate_projection_data/bcvin_raster/CanESM2_RCP85_",
                  member_as.string,
                  "_",
                  i,
                  "MSY/Tmin",
                  j,
                  ".asc",
                  sep = "")
          ),
          envir = .GlobalEnv
        )#end assign
        
      }
    }
  }
  
}



##open & assign funciton for monthly precipitation
open_PPT_m <- function(startYear, endYear, startMonth, endMonth, member_as.string){
  
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(
          paste("ppt_0", j, "_", i, sep = ""), 
          raster(
            paste("D:/climate_projection_data/bcvin_raster/CanESM2_RCP85_",
                  member_as.string,
                  "_",
                  i,
                  "MSY/PPT",
                  "0",
                  j,
                  ".asc",
                  sep = "")
            
          ),
          envir = .GlobalEnv
        ) #end assign
        
      }
      else{
        assign(
          paste("ppt_", j, "_", i, sep = ""), 
          raster(
            paste("D:/climate_projection_data/bcvin_raster/CanESM2_RCP85_",
                  member_as.string,
                  "_",
                  i,
                  "MSY/PPT",
                  j,
                  ".asc",
                  sep = "")
          ),
          envir = .GlobalEnv
        )#end assign
        
      }
    }
    
    
  }
  
}


#open & assign function for GDD > 5
open_GDD5_m <- function(startYear, endYear, startMonth, endMonth, member_as.string) {
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(
          paste("GDD5_0", j, "_", i, sep = ""), 
          raster(
            paste("D:/climate_projection_data/bcvin_raster/CanESM2_RCP85_",
                  member_as.string,
                  "_",
                  i,
                  "MSY/DD5",
                  "_0",
                  j,
                  ".asc",
                  sep = "")
            
          ),
          envir = .GlobalEnv
        ) #end assign
        
      }
      else{
        assign(
          paste("GDD5_", j, "_", i, sep = ""), 
          raster(
            paste("D:/climate_projection_data/bcvin_raster/CanESM2_RCP85_",
                  member_as.string,
                  "_",
                  i,
                  "MSY/DD5",
                  "_",
                  j,
                  ".asc",
                  sep = "")
          ),
          envir = .GlobalEnv
        )#end assign
        
      }
    }
    
    
  }
  
}


#open & assign function for GDD > 0
open_GDD0_m <- function(startYear, endYear, startMonth, endMonth, member_as.string) {
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(
          paste("GDD0_0", j, "_", i, sep = ""), 
          raster(
            paste("D:/climate_projection_data/bcvin_raster/CanESM2_RCP85_",
                  member_as.string,
                  "_",
                  i,
                  "MSY/DD_0",
                  "_0",
                  j,
                  ".asc",
                  sep = "")
            
          ),
          envir = .GlobalEnv
        ) #end assign
        
      }
      else{
        assign(
          paste("GDD0_", j, "_", i, sep = ""), 
          raster(
            paste("D:/climate_projection_data/bcvin_raster/CanESM2_RCP85_",
                  member_as.string,
                  "_",
                  i,
                  "MSY/DD_0",
                  "_",
                  j,
                  ".asc",
                  sep = "")
          ),
          envir = .GlobalEnv
        )#end assign
        
      }
    }
    
    
  }
  
}


#########################################################same functions but for the historical dataset


##function that reads the output folders of ClimateBC and assigns them logical variable names 
open_MAT_historical <- function(startYear, endYear){ #finished & works
  for (i in startYear:endYear) {
    assign(paste("mat_", i, sep = ""), 
           raster(
             paste("D:/climate_projection_data/bcvin_raster/Year_",
                   i, 
                   "MSY/MAT.asc", 
                   sep = "")
           ), envir = .GlobalEnv
    )
    
  }
  
  
}



##open & assign funciton for monthly Tmax
open_Tmax_m_historical <- function(startYear, endYear, startMonth, endMonth){ #finished & works
  
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(paste("tmax_0", j, "_", i, sep = ""), 
               raster(
                 paste("D:/climate_projection_data/bcvin_raster/Year_",
                       i,
                       "MSY/Tmax",
                       "0",
                       j,
                       ".asc",
                       sep = "")
                 
               ),
               envir = .GlobalEnv
        )
      }
      else{
        assign(paste("tmax_", j, "_", i, sep = ""), 
               raster(
                 paste("D:/climate_projection_data/bcvin_raster/Year_",
                       i,
                       "MSY/Tmax",
                       j,
                       ".asc",
                       sep = "")
               ),
               envir = .GlobalEnv
        )
        
      }
    }
  }
}



##open & assign funciton for monthly Tmin

open_Tmin_m_historical <- function(startYear, endYear, startMonth, endMonth){ #finished & works
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(
          paste("tmin_0", j, "_", i, sep = ""), 
          raster(
            paste("D:/climate_projection_data/bcvin_raster/Year_",
                  i,
                  "MSY/Tmin",
                  "0",
                  j,
                  ".asc",
                  sep = "")
            
          ),
          envir = .GlobalEnv
        ) #end assign
        
      }
      else{
        assign(
          paste("tmin_", j, "_", i, sep = ""), 
          raster(
            paste("D:/climate_projection_data/bcvin_raster/Year_",
                  i,
                  "MSY/Tmin",
                  j,
                  ".asc",
                  sep = "")
          ),
          envir = .GlobalEnv
        )#end assign
        
      }
    }
  }
  
}



##open & assign funciton for monthly precipitation

open_PPT_m_historical <- function(startYear, endYear, startMonth, endMonth){ #finished & works
  
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(
          paste("ppt_0", j, "_", i, sep = ""), 
          raster(
            paste("D:/climate_projection_data/bcvin_raster/Year_",
                  i,
                  "MSY/PPT",
                  "0",
                  j,
                  ".asc",
                  sep = "")
            
          ),
          envir = .GlobalEnv
        ) #end assign
        
      }
      else{
        assign(
          paste("ppt_", j, "_", i, sep = ""), 
          raster(
            paste("D:/climate_projection_data/bcvin_raster/Year_",
                  i,
                  "MSY/PPT",
                  j,
                  ".asc",
                  sep = "")
          ),
          envir = .GlobalEnv
        )#end assign
        
      }
    }
    
    
  }
  
}


#open & assign function for GDD > 5

open_GDD5_m_historical <- function(startYear, endYear, startMonth, endMonth) { #finished
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(
          paste("GDD5_0", j, "_", i, sep = ""), 
          raster(
            paste("D:/climate_projection_data/bcvin_raster/Year_",
                  i,
                  "MSY/DD5",
                  "_0",
                  j,
                  ".asc",
                  sep = "")
            
          ),
          envir = .GlobalEnv
        ) #end assign
        
      }
      else{
        assign(
          paste("GDD5_", j, "_", i, sep = ""), 
          raster(
            paste("D:/climate_projection_data/bcvin_raster/Year_",
                  i,
                  "MSY/DD5",
                  "_",
                  j,
                  ".asc",
                  sep = "")
          ),
          envir = .GlobalEnv
        )#end assign
        
      }
    }
    
    
  }
  
}



#open & assign function for GDD < 0
open_GDD0_m_historical <- function(startYear, endYear, startMonth, endMonth) { #finished
  for (i in startYear:endYear) {
    for (j in startMonth:endMonth) {
      if(j < 10){
        assign(
          paste("GDD0_0", j, "_", i, sep = ""), 
          raster(
            paste("D:/climate_projection_data/bcvin_raster/Year_",
                  i,
                  "MSY/DD_0",
                  "_0",
                  j,
                  ".asc",
                  sep = "")
            
          ),
          envir = .GlobalEnv
        ) #end assign
        
      }
      else{
        assign(
          paste("GDD0_", j, "_", i, sep = ""), 
          raster(
            paste("D:/climate_projection_data/bcvin_raster/Year_",
                  i,
                  "MSY/DD_0",
                  "_",
                  j,
                  ".asc",
                  sep = "")
          ),
          envir = .GlobalEnv
        )#end assign
        
      }
    }
    
    
  }
  
}



#aggregating monthly variable over the 20 year period, averaging, and SD 
#NOTD: these are going to eventually be averaged between member_as.strings. STORE IN A DESCRIPTIVE FOLDER
#Names do not include which member_as.string the data is from to simplify code
#labels will be as follows: myVariable_as.string_month_warmingScenario_as.string
#myVariable_as.string = Tmax, Tmin, PPT, GDD0, or GDD5
#month = 1 : 12 depending on what month it is. This function should cycle through all interested months
#warmingScenario_as.string = moderate warming, mw (2040-2059); high warming, hw (2070-2089); no warming, nw (1970 - 1989)
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
          envir = .GlobalEnv
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
          envir = .GlobalEnv
        )#end assign
        if(str_detect(myVariable_as.string, "ppt")){
          assign(
            paste(myVariable_as.string, "_0", i, "_", "mw_cv", sep = ""),
            lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
              stack() %>%
              calc(fun = cv, na.rm = TRUE) 
            ,
            envir = .GlobalEnv
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
          envir = .GlobalEnv
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
          envir = .GlobalEnv
        )
        if(str_detect(myVariable_as.string, "ppt")){
          assign(
            paste(myVariable_as.string, "_", i, "_", "mw_cv", sep = ""),
            lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
              stack() %>%
              calc(fun = cv, na.rm = TRUE) 
            ,
            envir = .GlobalEnv
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
          envir = .GlobalEnv
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
          envir = .GlobalEnv
        )#end assign
        if(str_detect(myVariable_as.string, "ppt")){
          assign(
            paste(myVariable_as.string, "_0", i, "_", "hw_cv", sep = ""),
            lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
              stack() %>%
              calc(fun = cv, na.rm = TRUE) 
            ,
            envir = .GlobalEnv
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
          envir = .GlobalEnv
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
          envir = .GlobalEnv
        )
        if(str_detect(myVariable_as.string, "ppt")){
          assign(
            paste(myVariable_as.string, "_", i, "_", "hw_cv", sep = ""),
            lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
              stack() %>%
              calc(fun = cv, na.rm = TRUE) 
            ,
            envir = .GlobalEnv
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
          envir = .GlobalEnv
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
          envir = .GlobalEnv
        )#end assign
        if(str_detect(myVariable_as.string, "ppt")){
          assign(
            paste(myVariable_as.string, "_0", i, "_", "nw_cv", sep = ""),
            lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
              stack() %>%
              calc(fun = cv, na.rm = TRUE) 
            ,
            envir = .GlobalEnv
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
          envir = .GlobalEnv
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
          envir = .GlobalEnv
        )
        if(str_detect(myVariable_as.string, "ppt")){
          assign(
            paste(myVariable_as.string, "_", i, "_", "nw_cv", sep = ""),
            lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
              stack() %>%
              calc(fun = cv, na.rm = TRUE) 
            ,
            envir = .GlobalEnv
          )#end assign
        }
      }
    }#end if statement that determines nw
  }
}

########################################################################################

#function to aggregate MAT


aggregate_mat <- function(startYear, endYear, warmingScenario_as.string){
  assign(
    paste("mat_", warmingScenario_as.string, sep = ""), 
    lapply( paste("mat_", startYear:endYear, sep = ""), get ) %>%
      stack() %>%
      calc(fun = mean, na.rm = TRUE) %>%
      calc( . , fun = function(x){x/10}),
    envir = .GlobalEnv
  )
  assign(paste("mat_", warmingScenario_as.string, "_sd", sep = ""),
         lapply( paste("mat_", startYear:endYear, sep = ""), get ) %>%
           stack() %>%
           calc(fun = sd, na.rm = TRUE) %>%
           calc( . , fun = function(x){x/10}),
         envir = .GlobalEnv
  )
  
}



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



#only run once


#function to aggregate myVariable_as.string to any groups of months
#I won't be creating a custom writeRaster function for these because they will all be one-offs
#I don't think that SD should be included in this output because it would be artificially high.
#If SD is desired, calculate it with the difference between member_as.strings to get worthwhile data
#DEBUG: the output variable is just the last month in the period... 

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
          envir = .GlobalEnv
          
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
          envir = .GlobalEnv
          
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
          envir = .GlobalEnv
          
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
          envir = .GlobalEnv
          
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
          envir = .GlobalEnv
          
        )
        
      }
      
      
    }
    
  }
  
}




########################################################################################################################
#open all members
#myDescriptiveVariable should be the exact filename, as a string without the file extension, that was output by any of the writeRaster functions 
#the output variable names are truncated to keep clutter down

open_members <- function(myDescriptiveVariable_as.string){ #tested and works
 if(str_detect(myDescriptiveVariable_as.string, "mw") == TRUE || str_detect(myDescriptiveVariable_as.string, "hw") == TRUE) {
  assign(
    x = paste(myDescriptiveVariable_as.string, "_r1", sep = ""),
    value = raster(paste( "D:/climate_projection_data/bcvin_raster/r11i1p1/", myDescriptiveVariable_as.string, ".asc", sep = "")),
    envir = .GlobalEnv
  )
  
  assign(
    x = paste(myDescriptiveVariable_as.string, "_r2", sep = ""),
    value = raster(paste( "D:/climate_projection_data/bcvin_raster/r21i1p1/", myDescriptiveVariable_as.string, ".asc", sep = "")), 
    envir = .GlobalEnv
  )
  
  assign(
    x = paste(myDescriptiveVariable_as.string, "_r3", sep = ""),
    value = raster(paste( "D:/climate_projection_data/bcvin_raster/r31i1p1/", myDescriptiveVariable_as.string, ".asc", sep = "")), 
    envir = .GlobalEnv
  )
  
  assign(
    x = paste(myDescriptiveVariable_as.string, "_r4", sep = ""),
    value = raster(paste( "D:/climate_projection_data/bcvin_raster/r41i1p1/", myDescriptiveVariable_as.string, ".asc", sep = "")), 
    envir = .GlobalEnv
  )
  
  assign(
    x = paste(myDescriptiveVariable_as.string, "_r5", sep = ""),
    value = raster(paste( "D:/climate_projection_data/bcvin_raster/r51i1p1/", myDescriptiveVariable_as.string, ".asc", sep = "")), 
    envir = .GlobalEnv
  )
 }
  else{
  assign(
    x = paste(myDescriptiveVariable_as.string, "_historical", sep = ""),
    value = raster(paste( "D:/climate_projection_data/bcvin_raster/historical/", myDescriptiveVariable_as.string, ".asc", sep = "")), 
    envir = .GlobalEnv
  )
  }
}

#in this case myDescriptiveVariable does NOT include any individual r's
combine_all_members <- function(myDescriptiveVariable){ #tested and works
  assign(
    x = paste(myDescriptiveVariable, "_combined", sep = ""),
    value = 
      lapply(paste(myDescriptiveVariable, "_r", 1:5, sep = ""), get) %>%
      stack() %>%
      calc(. , fun = mean),
    envir = .GlobalEnv
  )
  assign(
    x = paste(myDescriptiveVariable, "_combined_sd", sep = ""),
    value = 
      lapply(paste(myDescriptiveVariable, "_r", 1:5, sep = ""), get) %>%
      stack() %>%
      calc(. , fun = sd),
    envir = .GlobalEnv
  )
  
  if(str_detect(myDescriptiveVariable, "ppt|GDD") == TRUE) { #expressed as a percentage
    assign(
      x = paste(myDescriptiveVariable, "_combined_cv", sep = ""),
      value = 
        lapply(paste(myDescriptiveVariable, "_r", 1:5, sep = ""), get) %>%
        stack() %>%
        calc(. , fun = cv),
      envir = .GlobalEnv
    )
    
  }
  
  
}

write_combined <- function(myDescriptiveVariable_as.string) {
  writeRaster(
    get(
      paste(myDescriptiveVariable_as.string, "_combined", sep = "")
    ),
    filename = paste(myDescriptiveVariable_as.string, "_combined.asc", sep = "")
  )
  writeRaster(
    get(
      paste(myDescriptiveVariable_as.string, "_combined_sd", sep = "")
    ),
    filename = paste(myDescriptiveVariable_as.string, "_combined_sd.asc", sep = "")
  )
  if(str_detect(myDescriptiveVariable_as.string, "ppt|GDD") == TRUE ){
  writeRaster(
    get(
      paste(myDescriptiveVariable_as.string, "_combined_cv", sep = "")
    ),
    filename = paste(myDescriptiveVariable_as.string, "_combined_cv.asc", sep = "")
  )
  }
}


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
          mutate(. , member = paste("r", i * 10 + 1, sep = "")) %>%
          rename(., value = str2lang(paste(myDescriptiveVariable)), member = member) 
      },
      envir = .GlobalEnv
      #should only exist in function environment, but I don't know how to do that syntactically yet
    )
  }
  assign( 
    x = "df_combined",
    value = rbind(df_r1, df_r2, df_r3, df_r4, df_r5)      ,
    envir = .GlobalEnv
  )
  
  if(str_detect(myDescriptiveVariable, "ppt") == TRUE){ 
    lab = "Accumulated Precipitation (mm)"
    plotTitle = ""
    nbins = 38
  }
  
  else if(str_detect(myDescriptiveVariable, "tmax") == TRUE){ 
    lab = "Temperature (°C)"
    plotTitle = ""
    nbins = 14
  }
  else if(str_detect(myDescriptiveVariable, "tmin") == TRUE){
    lab = "Temperature (°C)"
    plotTitle = ""
    nbins = 14
  }
  else if(str_detect(myDescriptiveVariable, "mat")){
    lab = "Temperature (°C)"
    plotTitle = ""
    nbins = 14
    
  }
  else if(str_detect(myDescriptiveVariable, "GDD5") == TRUE){
    lab = "Degree Days > 5"
    plotTitle = ""
    nbins = 20
  }
  
  else if(str_detect(myDescriptiveVariable, "GDD0") == TRUE){
    lab = "Degree Days < 0"
    plotTitle = ""
    nbins = 14
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
      geom_density(alpha = 0.6) + 
      xlab(lab) +
      ylab("Density") +
      ggtitle(plotTitle)
    
  }
  
  aesthetics
  
  
}

