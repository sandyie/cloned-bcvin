library(raster)
library(sp)
library(tidyverse)

################################################################################################
####                 CHANGING FILE EXTENSIONS FOR HISTORICAL NAMING SCHEME                  ####
################################################################################################

#####################################
##  Write #finished next to the script after it has been updated
#####################################

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

##   SD for temperature projections
##   Plot histograms for each 20 year period
##   Precipitation most likley non-normal, check plots (maybe use IQR or CV) (no precipitation in this file yet)
##   For nw, mw, and hw: take SD for each MEMBER (5 counts) then average them

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



#aggregating monthly variable over the 20 year period, averaging, and SD 
#NOTE: these are going to eventually be averaged between members. STORE IN A DESCRIPTIVE FOLDER
#Names do not include which member the data is from to simplify code
#labels will be as follows: myVariable_as.string_month_warmingScenario_as.string
#myVariable_as.string = Tmax, Tmin, PPT, GDD0, or GDD5
#month = 1 : 12 depending on what month it is. This function should cycle through all interested months
#warmingScenario_as.string = moderate warming, mw (2040-2059); high warming, hw (2070-2089); no warming, nw (1970 - 1989)


########################################################################################
##CURRENTLY WORKS FOR: Tmax, Tmin, GDD, ppt (not SD)
##Doesn't work for PPT (the SD is on the magnitude of 10's ) 
##Check with Geoff if dividing SD by 10 is a rational decision
aggregate_warmingScenario_m <- function(startYear, endYear, startMonth, endMonth, myVariable_as.string){ #finished and works except for SD for ppt
  
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
      }
    }#end if statement that determines nw
  }
}


########################################################################################

#function to aggregate MAT


aggregate_mat <- function(startYear, endYear, warmingScenario_as.string){ #finished and works
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


#function to write monthly for mw/hw/nw 

write_monthly_var <- function(startMonth, endMonth, myVariable_as.string, warmingScenario_as.string){ #finished and works
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
    
  }
}

#function to write yearly for mw/hw/nw

write_yearly_MAT <- function(warmingScenario_as.string){ #finished and works
  writeRaster(
    x = get(paste("mat_", warmingScenario_as.string, sep = "")),
    filename = paste("mat_", warmingScenario_as.string, ".asc", sep = "")
  )
  writeRaster(
    x = get(paste("mat_", warmingScenario_as.string, "_sd", sep = "")),
    filename = paste("mat_", warmingScenario_as.string, "_sd.asc", sep = "")
  )
  
}

#function to aggregate myVariable_as.string to any groups of months
#I won't be creating a custom writeRaster function for these because they will all be one-offs
#I don't think that SD should be included in this output because it would be artificially high.
#If SD is desired, calculate it with the difference between members to get worthwhile data
#Incomplete


combine_monthly_vars <- function(startMonth, endMonth, myVariable_as.string, warmingScenario_as.string) { #didn't compute the mean... find bug
  if(startMonth > endMonth){ #i.e. Monthly Tmin between October and March:
    
      
      if(startMonth >= 10 && endMonth < 10){ #when start month is double digit and end month is single digit
        assign(
          x = paste(myVariable_as.string, "_", startMonth, "_0", endMonth, "_", warmingScenario_as.string, sep = ""),
          
          value = lapply( #if statements are to determine which naming scheme to write
            {
              c(paste(myVariable_as.string, "_0", 1:endMonth, "_", warmingScenario_as.string, sep = ""),
              paste(myVariable_as.string, "_", startMonth:12, "_", warmingScenario_as.string, sep = ""))
            }
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
  ######################ELSE IS NOT DONE. MAKE SURE TO CHANGE THE ASSIGNED VARIABLE NAME AND ANY PRINTING 
  else{ #three cases: single w/ double, single w/ single, double w/ double
    { #i.e. June, July, August Tmax 
      #passes my logic & visual check      
      if(startMonth < 10 && endMonth < 10){ #i.e. 01 - 09
        assign(
          x = paste(myVariable_as.string, "_0", startMonth, "_0", endMonth, "_", warmingScenario_as.string, sep = ""),
          
          value = lapply( 
            paste(myVariable_as.string, "_0", startMonth:endMonth, "_", warmingScenario_as.string, sep = "")
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
            {
             c(paste(myVariable_as.string, "_0", startMonth:9, "_", warmingScenario_as.string, sep = ""),
           
              paste(myVariable_as.string, "_", 10:endMonth, "_", warmingScenario_as.string, sep = ""))
              }
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
            paste(myVariable_as.string, "_", startMonth:endMonth, "_", warmingScenario_as.string, sep = "")
            , 
            FUN = get) %>%
            stack() %>%
            calc(. , fun = mean)
          ,
          envir = parent.frame()
          
        )
        
      }
      
      
    }#end for loop
    
  }
  
}


#Testing to prove logic in dividing by 10 after SD is taken in Tmax & Tmin 
#testSd2 = testSd1

testStack <- stack(tmax_01_2040, tmax_01_2042)

testSd1 <- testStack %>%
  calc(fun = sd) %>%
  calc(fun = function(x){x / 10})

tmax_01_2040_test <- calc(tmax_01_2040, fun = function(x){x / 10})
tmax_01_2042_test <- calc(tmax_01_2042, fun = function(x){x / 10})

testStack2 <- stack(tmax_01_2040_test, tmax_01_2042_test)

testSd2 <- testStack2 %>%
  calc(fun = sd)


#######################################MAKE THIS SO I CAN ENTER myVariable_as.string & change output
#######################################This version is an archive just in case I ruin the testing function
aggregate_warmingScenario_m <- function(startYear, endYear, startMonth, endMonth, myVariable_as.string){
  
  for(i in startMonth:endMonth) {
    
    
    if(startYear >= 2040 && startYear <= 2059){
      if(i < 10) {
        assign(
          paste(myVariable_as.string, "_0", i, "_", "mw", sep = ""),
          lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = mean, na.rm = TRUE) %>%
            calc(fun = function(x){x/10}),
          envir = parent.frame()
        )#end assign
        assign(
          paste(myVariable_as.string, "_0", i, "_", "mw_sd", sep = ""),
          lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = sd, na.rm = TRUE) %>%
            calc(fun = function(x){x/10}),
          envir = parent.frame()
        )#end assign
      }
      if(i >= 10) {
        assign(
          paste(myVariable_as.string, "_", i, "_", "mw", sep = ""),
          lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = mean, na.rm = TRUE) %>%
            calc(fun = function(x){x/10}),
          envir = parent.frame()
        )#end assign
        assign(
          paste(myVariable_as.string, "_", i, "_", "mw_sd", sep = ""),
          lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = sd, na.rm = TRUE) %>%
            calc(fun = function(x){x/10}),
          envir = parent.frame()
        )
      }
    }#end if statement that determines mw 
    
    
    if(startYear >= 2070 && startYear <= 2089){
      if(i < 10) {
        assign(
          paste(myVariable_as.string, "_0", i, "_", "hw", sep = ""),
          lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = mean, na.rm = TRUE) %>%
            calc(fun = function(x){x/10}),
          envir = parent.frame()
        )#end assign
        assign(
          paste(myVariable_as.string, "_0", i, "_", "hw_sd", sep = ""),
          lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = sd, na.rm = TRUE) %>%
            calc(fun = function(x){x/10}),
          envir = parent.frame()
        )#end assign
      }
      if(i >= 10) {
        assign(
          paste(myVariable_as.string, "_", i, "_", "hw", sep = ""),
          lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = mean, na.rm = TRUE) %>%
            calc(fun = function(x){x/10}),
          envir = parent.frame()
        )#end assign
        assign(
          paste(myVariable_as.string, "_", i, "_", "hw_sd", sep = ""),
          lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = sd, na.rm = TRUE) %>%
            calc(fun = function(x){x/10}),
          envir = parent.frame()
        )
      }
    }#end if statement that determines hw 
    
    
    if(startYear >= 1970 && startYear <= 1989){
      if(i < 10) {
        assign(
          paste(myVariable_as.string, "_0", i, "_", "nw", sep = ""),
          lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = mean, na.rm = TRUE) %>%
            calc(fun = function(x){x/10}),
          envir = parent.frame()
        )#end assign
        assign(
          paste(myVariable_as.string, "_0", i, "_", "nw_sd", sep = ""),
          lapply( paste(myVariable_as.string, "_0", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = sd, na.rm = TRUE) %>%
            calc(fun = function(x){x/10}),
          envir = parent.frame()
        )#end assign
      }
      if(i >= 10) {
        assign(
          paste(myVariable_as.string, "_", i, "_", "nw", sep = ""),
          lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = mean, na.rm = TRUE) %>%
            calc(fun = function(x){x/10}),
          envir = parent.frame()
        )#end assign
        assign(
          paste(myVariable_as.string, "_", i, "_", "nw_sd", sep = ""),
          lapply( paste(myVariable_as.string, "_", i,"_", startYear:endYear, sep = ""), get) %>%
            stack() %>%
            calc(fun = sd, na.rm = TRUE) %>%
            calc(fun = function(x){x/10}),
          envir = parent.frame()
        )
      }
    }#end if statement that determines nw
  }
}
