library(raster)
library(sp)
library(tidyverse)

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

####   Periods of Years   ####
##   2040 - 2059 (moderate warming)

####   Initial Condition Member   ####
## r1i1p1


#############################################
#file upload, file names will be the same just 
#this script does not describe the member within the filename
#make sure to keep files stored in different folders for each member
#for functionality these variables can update easily:
# 1. file path in same syntax as written in code 
# 2. initial condition (note: output from ClimateBC is r11i1p1 but the "r1i1p1" seems to be the more likely name)
# 3. start and end Year


##loop to read all the files and assign to corresponding variable names

startYear <- 2040
endYear <- 2059
member <- "r11i1p1"

#outputs mean annual temperature as mat_YEAR
#YEAR <- startYear:endYear
for (i in startYear:endYear) {
 assign(paste("mat_", i, sep = ""), 
        raster(
          paste("final_climate_proj/bcvin_raster/CanESM2_RCP85_",
          member,
          "_", 
                i, 
                "MSY/MAT.asc", 
                sep = "")
    )
  )
}

#############################################
##tmax assigning variables
##outputs as tmax_XX_YEAR
##XX <- startMonth:endMonth
##YEAR <- startYear:endYear

startMonth <- 1
endMonth <- 12

for (i in startYear:endYear) {
  for (j in startMonth:endMonth) {
    if(j < 10){
    assign(paste("tmax_0", j, "_", i, sep = ""), 
           raster(
             paste("final_climate_proj/bcvin_raster/CanESM2_RCP85_",
                  member,
                  "_",
                  i,
                  "MSY/Tmax",
                  "0",
                  j,
                  ".asc",
                  sep = "")
    
      )
    )
    }
    else{
      assign(paste("tmax_", j, "_", i, sep = ""), 
             raster(
               paste("final_climate_proj/bcvin_raster/CanESM2_RCP85_",
                     member,
                     "_",
                     i,
                     "MSY/Tmax",
                     j,
                     ".asc",
                     sep = "")
        )
      )

    }
  }
}


#############################################
#tmin assigning variables
##outputs as tmax_XX_YEAR
##XX <- startMonth:endMonth
##YEAR <- startYear:endYear

startMonth <- 1
endMonth <- 12

for (i in startYear:endYear) {
  for (j in startMonth:endMonth) {
    if(j < 10){
      assign(
        paste("tmin_0", j, "_", i, sep = ""), 
             raster(
               paste("final_climate_proj/bcvin_raster/CanESM2_RCP85_",
                     member,
                     "_",
                     i,
                     "MSY/Tmin",
                     "0",
                     j,
                     ".asc",
                     sep = "")
               
       )
      ) #end assign
      
    }
    else{
      assign(
        paste("tmin_", j, "_", i, sep = ""), 
             raster(
               paste("final_climate_proj/bcvin_raster/CanESM2_RCP85_",
                     member,
                     "_",
                     i,
                     "MSY/Tmin",
                     j,
                     ".asc",
                     sep = "")
        )
      )#end assign
      
    }
  }
}
#############################################




##########################################################################################
##data manipulation and writing derived files
#converting to average between startYear and endYear
#mw == moderate warming 

##########################################################################################
#tmax mean and sd over the 20 year period, patience is key, this took me ~5 min to load

#assigning new variables
for(i in 1:12) {
  if(i < 10) {
  assign(
      paste("tmax_0", i, "_", "mw", sep = ""),
      lapply( paste("tmax_0", i,"_", startYear:endYear, sep = ""), get) %>%
      stack() %>%
      calc(fun = mean, na.rm = TRUE) %>%
      calc(fun = function(x){x/10})
    )#end assign
  assign(
      paste("tmax_0", i, "_", "mw_sd", sep = ""),
      lapply( paste("tmax_0", i,"_", startYear:endYear, sep = ""), get) %>%
        stack() %>%
        calc(fun = sd, na.rm = TRUE) %>%
        calc(fun = function(x){x/10})
    )#end assign
  }
  if(i >= 10) {
    assign(
      paste("tmax_", i, "_", "mw", sep = ""),
      lapply( paste("tmax_", i,"_", startYear:endYear, sep = ""), get) %>%
        stack() %>%
        calc(fun = mean, na.rm = TRUE) %>%
        calc(fun = function(x){x/10})
    )#end assign
    assign(
      paste("tmax_", i, "_", "mw_sd", sep = ""),
      lapply( paste("tmax_", i,"_", startYear:endYear, sep = ""), get) %>%
        stack() %>%
        calc(fun = sd, na.rm = TRUE) %>%
        calc(fun = function(x){x/10})
    )
  }
}

#writing corresponding files
for(i in 1:12) {
  if(i < 10) {
  writeRaster(
    x = get(paste("tmax_0", i, "_mw", sep = "")),
    filename = paste("tmax_0", i, "_mw.asc", sep = "")
    )
  writeRaster(
    x = get(paste("tmax_0", i, "_mw_sd", sep = "")),
    filename = paste("tmax_0", i, "_mw_sd.asc", sep = "")
    )
  }
  if(i >= 10) {
  writeRaster(
    x = get(paste("tmax_", i, "_mw", sep = "")),
    filename = paste("tmax_", i, "_mw.asc", sep = "")
    )
  writeRaster(
    x = get(paste("tmax_", i, "_mw_sd", sep = "")),
    filename = paste("tmax_", i, "_mw_sd.asc", sep = "")
    )
  }
}
#############################################
#tmin mean over the 20 year period 

for(i in 1:12) {
  if(i < 10) {
    assign(
      paste("tmin_0", i, "_", "mw", sep = ""),
      lapply( paste("tmin_0", i,"_", startYear:endYear, sep = ""), get) %>%
        stack() %>%
        calc(fun = mean, na.rm = TRUE) %>%
        calc(fun = function(x){x/10})
    )
  }
  if(i >= 10) {
    assign(
      paste("tmin_", i, "_", "mw", sep = ""),
      lapply( paste("tmin_", i,"_", startYear:endYear, sep = ""), get) %>%
        stack() %>%
        calc(fun = mean, na.rm = TRUE) %>%
        calc(fun = function(x){x/10})
    )
  }
}

#writing corresponding files
for(i in 1:12) {
  if(i < 10) {
    writeRaster(
      x = get(paste("tmin_0", i, "_mw", sep = "")),
      filename = paste("tmin_0", i, "_mw.asc", sep = "")
    )
    writeRaster(
      x = get(paste("tmin_0", i, "_mw_sd", sep = "")),
      filename = paste("tmin_0", i, "_mw_sd.asc", sep = "")
    ) 
  }
  if(i >= 10) {
    writeRaster(
      x = get(paste("tmin_", i, "_mw", sep = "")),
      filename = paste("tmin_", i, "_mw.asc", sep = "")
    )
    writeRaster(
      x = get(paste("tmin_", i, "_mw_sd", sep = "")),
      filename = paste("tmin_", i, "_mw_sd.asc", sep = "")
    )
  }
}


#############################################

mat_mw <- lapply( paste("mat_", startYear:endYear, sep = ""), get ) %>%
  stack() %>%
    calc(fun = mean, na.rm = TRUE) %>%
      calc(fun = function(x){x/10})

mat_mw_sd <- lapply( paste("mat_", startYear:endYear, sep = ""), get ) %>%
  stack() %>%
    calc(fun = sd, na.rm = TRUE) %>%
      calc(fun = function(x){x/10})

writeRaster(mat_mw, filename = "mat_mw.asc")
writeRaster(mat_mw_sd, filename = "mat_mw_sd.asc")
#############################################
  


#December through March
stack(tmin_12_mw, tmin_01_mw, tmin_02_mw, tmin_03_mw) %>%
  calc(fun = mean) %>%
    writeRaster(filename = "tmin_12_03_mw.asc")

#############################################
#Growing Degree Days
#formula is the summation of ((Tmax + Tmin)/2 - 10) * days in the month
#negative values are converted to zero. Reference: Parker et al

#September
GDD_09_mw <- overlay(tmax_09_mw, tmin_09_mw, fun = function(r1, r2){((r1 + r2) / 2 - 10) * 30})
GDD_09_mw[GDD_09_mw < 0] <- 0 
writeRaster(GDD_09_mw, "GDD_09_mw.asc")

#October
GDD_10_mw <- overlay(tmax_10_mw, tmin_10_mw, fun = function(r1, r2){((r1 + r2) / 2 - 10) * 31}) 
GDD_10_mw[GDD_10_mw < 0] <- 0
writeRaster(GDD_10_mw, "GDD_10_mw.asc")

#############################################
#May to October
#31 + 30 + 31 + 31 + 30 + 31 = 184 days in May through October

tmax_05_10_mw <- stack(tmax_05_mw, tmax_06_mw, tmax_07_mw, tmax_08_mw, tmax_09_mw, tmax_10_mw) %>%
  calc(fun = mean)
tmin_05_10_mw <- stack(tmin_05_mw, tmin_06_mw, tmin_07_mw, tmin_08_mw, tmin_09_mw, tmin_10_mw) %>%
  calc(fun = mean)

GDD_05_10_mw <- overlay(tmin_05_10_mw, tmax_05_10_mw, fun = function(r1, r2){((r1 + r2) / 2 - 10) * 184}) 
GDD_05_10_mw[GDD_05_10_mw < 0] <- 0
writeRaster(GDD_05_10_mw, "GDD_05_10_mw.asc")
#############################################
#April to September
#30 + 31 + 30 + 31 + 31 + 30 = 183 days in April through September

tmax_04_09_mw <- stack(tmax_04_mw, tmax_05_mw, tmax_06_mw, tmax_07_mw, tmax_08_mw, tmax_09_mw) %>%
  calc(fun = mean)
tmin_04_09_mw <- stack(tmin_04_mw, tmin_05_mw, tmin_06_mw, tmin_07_mw, tmin_08_mw, tmin_09_mw) %>%
  calc(fun = mean)

GDD_04_09_mw <- overlay(tmin_04_09_mw, tmax_04_09_mw, fun = function(r1, r2){((r1 + r2) / 2 - 10) * 183}) 
GDD_04_09_mw[GDD_04_09_mw < 0] <- 0
writeRaster(GDD_04_09_mw, "GDD_04_09_mw.asc")


