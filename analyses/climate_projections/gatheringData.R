#source("C:/Users/adamfong/Desktop/Ecology Lab/R/bcvin_git/analyses/climate_projections/functions_externaldrive.R")#in a different directory than the data
source("C:/Ecology Lab/R/bcvin_git/bcvin/analyses/climate_projections/functions_externaldrive.R")

combine_variable_periods <- function(warmingScenario_as.string)
{
  combine_monthly_vars(5, 10, "GDD5", warmingScenario_as.string)
  combine_monthly_vars(4, 9, "GDD5", warmingScenario_as.string)
  combine_monthly_vars(12, 3, "tmin", warmingScenario_as.string)
}


write_all_periods <- function(warmingScenario_as.string){
  writeRaster(get(paste("GDD5_05_10_", warmingScenario_as.string, sep = "")),
              filename = paste("GDD5_05_10_", warmingScenario_as.string, ".asc", sep = ""))
  writeRaster(get(paste("GDD5_04_09_", warmingScenario_as.string, sep = "")),
              filename = paste("GDD5_04_09_", warmingScenario_as.string, ".asc", sep = ""))
  writeRaster(get(paste("tmin_12_03_", warmingScenario_as.string, sep = "")),
              filename = paste("tmin_12_03_", warmingScenario_as.string, ".asc", sep = ""))
}

#this is script is going to be a function which will read what member to open then continue through the rest of the referenced functions in source
#it is very important to only run one member & one warming scenario at a time as the files will be written to the current working directory without indication of what member it belongs to

#for 2040 to 2059 aka moderate warming
open_MAT(2040, 2059, "r11i1p1")
#open_monthlyVars(2040, 2059, 1, 12, "r11i1p1")
open_Tmax_m(2040, 2059, 6, 9, "r11i1p1")
open_Tmin_m(2040, 2059, 1, 4, "r11i1p1")
open_Tmin_m(2040, 2059, 9, 12, "r11i1p1")
open_GDD5_m(2040, 2059, 4, 10, "r11i1p1")
open_GDD0_m(2040, 2059, 4, 10, "r11i1p1")
open_PPT_m(2040, 2059, 9, 10, "r11i1p1")
#aggregate_allVars_warmingScenario(2040, 2059, 1, 12) 
aggregate_mat(2040, 2059, "mw")
aggregate_warmingScenario_m(2040, 2059, 6, 9, "tmax")
aggregate_warmingScenario_m(2040, 2059, 1, 4, "tmin")
aggregate_warmingScenario_m(2040, 2059, 9, 12, "tmin")
aggregate_warmingScenario_m(2040, 2059, 4, 10, "GDD5")
aggregate_warmingScenario_m(2040, 2059, 4, 10, "GDD0")
aggregate_warmingScenario_m(2040, 2059, 9, 10, "ppt")
combine_variable_periods("mw")
write_monthly_var(6, 9, "tmax", "mw")
write_monthly_var(1, 4, "tmin", "mw")
write_monthly_var(9, 12, "tmin", "mw")
write_monthly_var(4, 10, "GDD5", "mw")
write_monthly_var(4, 10, "GDD0", "mw")
write_monthly_var(9, 10, "ppt", "mw")
write_yearly_MAT("mw") 
write_all_periods("mw")
##end 2040 to 2050

#for 2070 to 2089 aka high warming
open_MAT(2070, 2089, "r11i1p1")
#open_monthlyVars(2070, 2089, 1, 12, "r11i1p1")
open_Tmax_m(2070, 2089, 6, 9, "r11i1p1")
open_Tmin_m(2070, 2089, 1, 4, "r11i1p1")
open_Tmin_m(2070, 2089, 9, 12, "r11i1p1")
open_GDD5_m(2070, 2089, 4, 10, "r11i1p1")
open_GDD0_m(2070, 2089, 4, 10, "r11i1p1")
open_PPT_m(2070, 2089, 9, 10, "r11i1p1")
#aggregate_allVars_warmingScenario(2070, 2089, 1, 12)##here
aggregate_mat(2070, 2089, "hw")
aggregate_warmingScenario_m(2070, 2089, 6, 9, "tmax")
aggregate_warmingScenario_m(2070, 2089, 1, 4, "tmin")
aggregate_warmingScenario_m(2070, 2089, 9, 12, "tmin")
aggregate_warmingScenario_m(2070, 2089, 4, 10, "GDD5")
aggregate_warmingScenario_m(2070, 2089, 4, 10, "GDD0")
aggregate_warmingScenario_m(2070, 2089, 9, 10, "ppt")
combine_variable_periods("hw")
#write_all_monthly_vars(1, 12, "hw")
write_monthly_var(6, 9, "tmax", "hw")
write_monthly_var(1, 4, "tmin", "hw")
write_monthly_var(9, 12, "tmin", "hw")
write_monthly_var(4, 10, "GDD5", "hw")
write_monthly_var(4, 10, "GDD0", "hw")
write_monthly_var(9, 10, "ppt", "hw")
write_yearly_MAT("hw") 
write_all_periods("hw")
##end 2070 to 2089

#for 2040 to 2059 aka moderate warming
open_MAT(2040, 2059, "r21i1p1")
#open_monthlyVars(2040, 2059, 1, 12, "r21i1p1")
open_Tmax_m(2040, 2059, 6, 9, "r21i1p1")
open_Tmin_m(2040, 2059, 1, 4, "r21i1p1")
open_Tmin_m(2040, 2059, 9, 12, "r21i1p1")
open_GDD5_m(2040, 2059, 4, 10, "r21i1p1")
open_GDD0_m(2040, 2059, 4, 10, "r21i1p1")
open_PPT_m(2040, 2059, 9, 10, "r21i1p1")
#aggregate_allVars_warmingScenario(2040, 2059, 1, 12) 
aggregate_mat(2040, 2059, "mw")
aggregate_warmingScenario_m(2040, 2059, 6, 9, "tmax")
aggregate_warmingScenario_m(2040, 2059, 1, 4, "tmin")
aggregate_warmingScenario_m(2040, 2059, 9, 12, "tmin")
aggregate_warmingScenario_m(2040, 2059, 4, 10, "GDD5")
aggregate_warmingScenario_m(2040, 2059, 4, 10, "GDD0")
aggregate_warmingScenario_m(2040, 2059, 9, 10, "ppt")
combine_variable_periods("mw")
write_monthly_var(6, 9, "tmax", "mw")
write_monthly_var(1, 4, "tmin", "mw")
write_monthly_var(9, 12, "tmin", "mw")
write_monthly_var(4, 10, "GDD5", "mw")
write_monthly_var(4, 10, "GDD0", "mw")
write_monthly_var(9, 10, "ppt", "mw")
write_yearly_MAT("mw") 
write_all_periods("mw")
##end 2040 to 2050

#for 2070 to 2089 aka high warming
open_MAT(2070, 2089, "r21i1p1")
#open_monthlyVars(2070, 2089, 1, 12, "r21i1p1")
open_Tmax_m(2070, 2089, 6, 9, "r21i1p1")
open_Tmin_m(2070, 2089, 1, 4, "r21i1p1")
open_Tmin_m(2070, 2089, 9, 12, "r21i1p1")
open_GDD5_m(2070, 2089, 4, 10, "r21i1p1")
open_GDD0_m(2070, 2089, 4, 10, "r21i1p1")
open_PPT_m(2070, 2089, 9, 10, "r21i1p1")
#aggregate_allVars_warmingScenario(2070, 2089, 1, 12)##here
aggregate_mat(2070, 2089, "hw")
aggregate_warmingScenario_m(2070, 2089, 6, 9, "tmax")
aggregate_warmingScenario_m(2070, 2089, 1, 4, "tmin")
aggregate_warmingScenario_m(2070, 2089, 9, 12, "tmin")
aggregate_warmingScenario_m(2070, 2089, 4, 10, "GDD5")
aggregate_warmingScenario_m(2070, 2089, 4, 10, "GDD0")
aggregate_warmingScenario_m(2070, 2089, 9, 10, "ppt")
combine_variable_periods("hw")
#write_all_monthly_vars(1, 12, "hw")
write_monthly_var(6, 9, "tmax", "hw")
write_monthly_var(1, 4, "tmin", "hw")
write_monthly_var(9, 12, "tmin", "hw")
write_monthly_var(4, 10, "GDD5", "hw")
write_monthly_var(4, 10, "GDD0", "hw")
write_monthly_var(9, 10, "ppt", "hw")
write_yearly_MAT("hw") 
write_all_periods("hw")
##end 2070 to 2089

#for 2040 to 2059 aka moderate warming
open_MAT(2040, 2059, "r31i1p1")
#open_monthlyVars(2040, 2059, 1, 12, "r31i1p1")
open_Tmax_m(2040, 2059, 6, 9, "r31i1p1")
open_Tmin_m(2040, 2059, 1, 4, "r31i1p1")
open_Tmin_m(2040, 2059, 9, 12, "r31i1p1")
open_GDD5_m(2040, 2059, 4, 10, "r31i1p1")
open_GDD0_m(2040, 2059, 4, 10, "r31i1p1")
open_PPT_m(2040, 2059, 9, 10, "r31i1p1")
#aggregate_allVars_warmingScenario(2040, 2059, 1, 12) 
aggregate_mat(2040, 2059, "mw")
aggregate_warmingScenario_m(2040, 2059, 6, 9, "tmax")
aggregate_warmingScenario_m(2040, 2059, 1, 4, "tmin")
aggregate_warmingScenario_m(2040, 2059, 9, 12, "tmin")
aggregate_warmingScenario_m(2040, 2059, 4, 10, "GDD5")
aggregate_warmingScenario_m(2040, 2059, 4, 10, "GDD0")
aggregate_warmingScenario_m(2040, 2059, 9, 10, "ppt")
combine_variable_periods("mw")
write_monthly_var(6, 9, "tmax", "mw")
write_monthly_var(1, 4, "tmin", "mw")
write_monthly_var(9, 12, "tmin", "mw")
write_monthly_var(4, 10, "GDD5", "mw")
write_monthly_var(4, 10, "GDD0", "mw")
write_monthly_var(9, 10, "ppt", "mw")
write_yearly_MAT("mw") 
write_all_periods("mw")
##end 2040 to 2050

#for 2070 to 2089 aka high warming
open_MAT(2070, 2089, "r31i1p1")
#open_monthlyVars(2070, 2089, 1, 12, "r31i1p1")
open_Tmax_m(2070, 2089, 6, 9, "r31i1p1")
open_Tmin_m(2070, 2089, 1, 4, "r31i1p1")
open_Tmin_m(2070, 2089, 9, 12, "r31i1p1")
open_GDD5_m(2070, 2089, 4, 10, "r31i1p1")
open_GDD0_m(2070, 2089, 4, 10, "r31i1p1")
open_PPT_m(2070, 2089, 9, 10, "r31i1p1")
#aggregate_allVars_warmingScenario(2070, 2089, 1, 12)##here
aggregate_mat(2070, 2089, "hw")
aggregate_warmingScenario_m(2070, 2089, 6, 9, "tmax")
aggregate_warmingScenario_m(2070, 2089, 1, 4, "tmin")
aggregate_warmingScenario_m(2070, 2089, 9, 12, "tmin")
aggregate_warmingScenario_m(2070, 2089, 4, 10, "GDD5")
aggregate_warmingScenario_m(2070, 2089, 4, 10, "GDD0")
aggregate_warmingScenario_m(2070, 2089, 9, 10, "ppt")
combine_variable_periods("hw")
#write_all_monthly_vars(1, 12, "hw")
write_monthly_var(6, 9, "tmax", "hw")
write_monthly_var(1, 4, "tmin", "hw")
write_monthly_var(9, 12, "tmin", "hw")
write_monthly_var(4, 10, "GDD5", "hw")
write_monthly_var(4, 10, "GDD0", "hw")
write_monthly_var(9, 10, "ppt", "hw")
write_yearly_MAT("hw") 
write_all_periods("hw")
##end 2070 to 2089

#for 2040 to 2059 aka moderate warming
open_MAT(2040, 2059, "r41i1p1")
#open_monthlyVars(2040, 2059, 1, 12, "r41i1p1")
open_Tmax_m(2040, 2059, 6, 9, "r41i1p1")
open_Tmin_m(2040, 2059, 1, 4, "r41i1p1")
open_Tmin_m(2040, 2059, 9, 12, "r41i1p1")
open_GDD5_m(2040, 2059, 4, 10, "r41i1p1")
open_GDD0_m(2040, 2059, 4, 10, "r41i1p1")
open_PPT_m(2040, 2059, 9, 10, "r41i1p1")
#aggregate_allVars_warmingScenario(2040, 2059, 1, 12) 
aggregate_mat(2040, 2059, "mw")
aggregate_warmingScenario_m(2040, 2059, 6, 9, "tmax")
aggregate_warmingScenario_m(2040, 2059, 1, 4, "tmin")
aggregate_warmingScenario_m(2040, 2059, 9, 12, "tmin")
aggregate_warmingScenario_m(2040, 2059, 4, 10, "GDD5")
aggregate_warmingScenario_m(2040, 2059, 4, 10, "GDD0")
aggregate_warmingScenario_m(2040, 2059, 9, 10, "ppt")
combine_variable_periods("mw")
write_monthly_var(6, 9, "tmax", "mw")
write_monthly_var(1, 4, "tmin", "mw")
write_monthly_var(9, 12, "tmin", "mw")
write_monthly_var(4, 10, "GDD5", "mw")
write_monthly_var(4, 10, "GDD0", "mw")
write_monthly_var(9, 10, "ppt", "mw")
write_yearly_MAT("mw") 
write_all_periods("mw")
##end 2040 to 2050

#for 2070 to 2089 aka high warming
open_MAT(2070, 2089, "r41i1p1")
#open_monthlyVars(2070, 2089, 1, 12, "r41i1p1")
open_Tmax_m(2070, 2089, 6, 9, "r41i1p1")
open_Tmin_m(2070, 2089, 1, 4, "r41i1p1")
open_Tmin_m(2070, 2089, 9, 12, "r41i1p1")
open_GDD5_m(2070, 2089, 4, 10, "r41i1p1")
open_GDD0_m(2070, 2089, 4, 10, "r41i1p1")
open_PPT_m(2070, 2089, 9, 10, "r41i1p1")
#aggregate_allVars_warmingScenario(2070, 2089, 1, 12)##here
aggregate_mat(2070, 2089, "hw")
aggregate_warmingScenario_m(2070, 2089, 6, 9, "tmax")
aggregate_warmingScenario_m(2070, 2089, 1, 4, "tmin")
aggregate_warmingScenario_m(2070, 2089, 9, 12, "tmin")
aggregate_warmingScenario_m(2070, 2089, 4, 10, "GDD5")
aggregate_warmingScenario_m(2070, 2089, 4, 10, "GDD0")
aggregate_warmingScenario_m(2070, 2089, 9, 10, "ppt")
combine_variable_periods("hw")
#write_all_monthly_vars(1, 12, "hw")
write_monthly_var(6, 9, "tmax", "hw")
write_monthly_var(1, 4, "tmin", "hw")
write_monthly_var(9, 12, "tmin", "hw")
write_monthly_var(4, 10, "GDD5", "hw")
write_monthly_var(4, 10, "GDD0", "hw")
write_monthly_var(9, 10, "ppt", "hw")
write_yearly_MAT("hw") 
write_all_periods("hw")
##end 2070 to 2089

#for 2040 to 2059 aka moderate warming
open_MAT(2040, 2059, "r51i1p1")
#open_monthlyVars(2040, 2059, 1, 12, "r51i1p1")
open_Tmax_m(2040, 2059, 6, 9, "r51i1p1")
open_Tmin_m(2040, 2059, 1, 4, "r51i1p1")
open_Tmin_m(2040, 2059, 9, 12, "r51i1p1")
open_GDD5_m(2040, 2059, 4, 10, "r51i1p1")
open_GDD0_m(2040, 2059, 4, 10, "r51i1p1")
open_PPT_m(2040, 2059, 9, 10, "r51i1p1")
#aggregate_allVars_warmingScenario(2040, 2059, 1, 12) 
aggregate_mat(2040, 2059, "mw")
aggregate_warmingScenario_m(2040, 2059, 6, 9, "tmax")
aggregate_warmingScenario_m(2040, 2059, 1, 4, "tmin")
aggregate_warmingScenario_m(2040, 2059, 9, 12, "tmin")
aggregate_warmingScenario_m(2040, 2059, 4, 10, "GDD5")
aggregate_warmingScenario_m(2040, 2059, 4, 10, "GDD0")
aggregate_warmingScenario_m(2040, 2059, 9, 10, "ppt")
combine_variable_periods("mw")
write_monthly_var(6, 9, "tmax", "mw")
write_monthly_var(1, 4, "tmin", "mw")
write_monthly_var(9, 12, "tmin", "mw")
write_monthly_var(4, 10, "GDD5", "mw")
write_monthly_var(4, 10, "GDD0", "mw")
write_monthly_var(9, 10, "ppt", "mw")
write_yearly_MAT("mw") 
write_all_periods("mw")
##end 2040 to 2050

#for 2070 to 2089 aka high warming
open_MAT(2070, 2089, "r51i1p1")
#open_monthlyVars(2070, 2089, 1, 12, "r51i1p1")
open_Tmax_m(2070, 2089, 6, 9, "r51i1p1")
open_Tmin_m(2070, 2089, 1, 4, "r51i1p1")
open_Tmin_m(2070, 2089, 9, 12, "r51i1p1")
open_GDD5_m(2070, 2089, 4, 10, "r51i1p1")
open_GDD0_m(2070, 2089, 4, 10, "r51i1p1")
open_PPT_m(2070, 2089, 9, 10, "r51i1p1")
#aggregate_allVars_warmingScenario(2070, 2089, 1, 12)##here
aggregate_mat(2070, 2089, "hw")
aggregate_warmingScenario_m(2070, 2089, 6, 9, "tmax")
aggregate_warmingScenario_m(2070, 2089, 1, 4, "tmin")
aggregate_warmingScenario_m(2070, 2089, 9, 12, "tmin")
aggregate_warmingScenario_m(2070, 2089, 4, 10, "GDD5")
aggregate_warmingScenario_m(2070, 2089, 4, 10, "GDD0")
aggregate_warmingScenario_m(2070, 2089, 9, 10, "ppt")
combine_variable_periods("hw")
#write_all_monthly_vars(1, 12, "hw")
write_monthly_var(6, 9, "tmax", "hw")
write_monthly_var(1, 4, "tmin", "hw")
write_monthly_var(9, 12, "tmin", "hw")
write_monthly_var(4, 10, "GDD5", "hw")
write_monthly_var(4, 10, "GDD0", "hw")
write_monthly_var(9, 10, "ppt", "hw")
write_yearly_MAT("hw") 
write_all_periods("hw")
##end 2070 to 2089

#for Historical aka no warming (ONLY RUN ONCE) 
open_MAT_historical(1970, 1989)
#open_monthlyVars_historical(1, 12)
open_Tmax_m_historical(1970, 1989, 6, 9)
open_Tmin_m_historical(1970, 1989, 1, 4)
open_Tmin_m_historical(1970, 1989, 9, 12)
open_GDD5_m_historical(1970, 1989, 4, 10)
open_GDD0_m_historical(1970, 1989, 4, 10)
open_PPT_m_historical(1970, 1989, 9, 10)
#aggregate_historicalVars(1, 12)
aggregate_warmingScenario_m(1970, 1989, 6, 9, "tmax")
aggregate_warmingScenario_m(1970, 1989, 1, 4, "tmin")
aggregate_warmingScenario_m(1970, 1989, 9, 12, "tmin")
aggregate_warmingScenario_m(1970, 1989, 4, 10, "GDD5")
aggregate_warmingScenario_m(1970, 1989, 4, 10, "GDD0")
aggregate_warmingScenario_m(1970, 1989, 9, 10, "ppt")
aggregate_mat(1970, 1989, "nw")
combine_variable_periods("nw")
#write_all_historical_monthly_vars(1, 12)
write_monthly_var(6, 9, "tmax", "nw")
write_monthly_var(1, 4, "tmin", "nw")
write_monthly_var(9, 12, "tmin", "nw")
write_monthly_var(4, 10, "GDD5", "nw")
write_monthly_var(4, 10, "GDD0", "nw")
write_monthly_var(9, 10, "ppt", "nw")
write_yearly_MAT("nw")
write_all_periods("nw")
##end historical

open_monthlyVars <- function(startYear, endYear, startMonth, endMonth, member_as.string){
  open_Tmax_m(startYear, endYear, startMonth, endMonth, member_as.string)
  open_Tmin_m(startYear, endYear, startMonth, endMonth, member_as.string)
  open_PPT_m(startYear, endYear, startMonth, endMonth, member_as.string)
  open_GDD5_m(startYear, endYear, startMonth, endMonth, member_as.string)
  open_GDD0_m(startYear, endYear, startMonth, endMonth, member_as.string)
}

open_monthlyVars_historical <- function(startMonth, endMonth){
  startYear <- 1970
  endYear <- 1989
  
  open_Tmax_m_historical(startYear, endYear, startMonth, endMonth)
  open_Tmin_m_historical(startYear, endYear, startMonth, endMonth)
  open_PPT_m_historical(startYear, endYear, startMonth, endMonth)
  open_GDD5_m_historical(startYear, endYear, startMonth, endMonth)
  open_GDD0_m_historical(startYear, endYear, startMonth, endMonth)
}

aggregate_allVars_warmingScenario <- function(startYear, endYear, startMonth, endMonth){
  aggregate_warmingScenario_m(startYear, endYear, startMonth, endMonth, "tmax")
  aggregate_warmingScenario_m(startYear, endYear, startMonth, endMonth, "tmin")
  aggregate_warmingScenario_m(startYear, endYear, startMonth, endMonth, "ppt")
  aggregate_warmingScenario_m(startYear, endYear, startMonth, endMonth, "GDD5")
  aggregate_warmingScenario_m(startYear, endYear, startMonth, endMonth, "GDD0")
}

aggregate_historicalVars <- function(startMonth, endMonth){
  aggregate_warmingScenario_m(1970, 1989, startMonth, endMonth, "tmax")
  aggregate_warmingScenario_m(1970, 1989, startMonth, endMonth, "tmin")
  aggregate_warmingScenario_m(1970, 1989, startMonth, endMonth, "ppt")
  aggregate_warmingScenario_m(1970, 1989, startMonth, endMonth, "GDD5")
  aggregate_warmingScenario_m(1970, 1989, startMonth, endMonth, "GDD0")
  
}

write_all_monthly_vars <- function(startMonth, endMonth, warmingScenario_as.string){ #change to have two blocks one with "mw", one with "hw"
  write_monthly_var(startMonth, endMonth, "tmax", warmingScenario_as.string)
  write_monthly_var(startMonth,endMonth, "tmin", warmingScenario_as.string)
  write_monthly_var(startMonth,endMonth, "GDD5", warmingScenario_as.string)
  write_monthly_var(startMonth,endMonth, "GDD0", warmingScenario_as.string)
  write_monthly_var(startMonth,endMonth, "ppt", warmingScenario_as.string)
}

write_all_historical_monthly_vars <- function(startMonth, endMonth){
  write_monthly_var(startMonth,endMonth, "tmax", "nw")
  write_monthly_var(startMonth,endMonth, "tmin", "nw")
  write_monthly_var(startMonth,endMonth, "GDD5", "nw")
  write_monthly_var(startMonth,endMonth, "GDD0", "nw")
  write_monthly_var(startMonth,endMonth, "ppt", "nw")
}

#these are the combined variables listed on the Wiki
combine_variable_periods <- function(warmingScenario_as.string)
{
combine_monthly_vars(5, 10, "GDD5", warmingScenario_as.string)
combine_monthly_vars(4, 9, "GDD5", warmingScenario_as.string)
combine_monthly_vars(12, 3, "tmin", warmingScenario_as.string)
}


write_all_periods <- function(warmingScenario_as.string){
  writeRaster(get(paste("GDD5_05_10_", warmingScenario_as.string, sep = "")),
              filename = paste("GDD5_05_10_", warmingScenario_as.string, ".asc", sep = ""))
  writeRaster(get(paste("GDD5_04_09_", warmingScenario_as.string, sep = "")),
              filename = paste("GDD5_04_09_", warmingScenario_as.string, ".asc", sep = ""))
  writeRaster(get(paste("tmin_12_03_", warmingScenario_as.string, sep = "")),
              filename = paste("tmin_12_03_", warmingScenario_as.string, ".asc", sep = ""))
  }

#aggregate_warmingScenario_m(1970, 1989, 6, 9, "tmax")
#aggregate_warmingScenario_m(1970, 1989, 1, 4, "tmin")
#aggregate_warmingScenario_m(1970, 1989, 9, 12, "tmin")
#aggregate_warmingScenario_m(1970, 1989, 4, 10, "GDD5")
#aggregate_warmingScenario_m(1970, 1989, 4, 10, "GDD0")
#aggregate_warmingScenario_m(1970, 1989, 9, 10, "ppt")

##Final combining members for the necessary files 


for(i in 6:9){
open_members(paste("tmax_0", i, "_hw", sep = ""))
open_members(paste("tmax_0", i, "_mw", sep = ""))
open_members(paste("tmax_0", i, "_nw", sep = ""))
combine_all_members(paste("tmax_0", i, "_hw", sep = ""))
combine_all_members(paste("tmax_0", i, "_mw", sep = ""))
}

#writing combined tmax
for(i in 6:9){
  
  if(i < 10){  
    write_combined(paste("tmax_0", i, "_hw", sep = ""))
    write_combined(paste("tmax_0", i, "_mw", sep = ""))
  }
  else {
    write_combined(paste("tmax_", i, "_hw", sep = ""))
    write_combined(paste("tmax_", i, "_mw", sep = ""))
    
    
  }
}


#combining tmin
for(i in c(1:4, 9:12)){
  if(i < 10){
  open_members(paste("tmin_0", i, "_hw", sep = ""))
  open_members(paste("tmin_0", i, "_mw", sep = ""))
  open_members(paste("tmin_0", i, "_nw", sep = ""))
  combine_all_members(paste("tmin_0", i, "_hw", sep = ""))
  combine_all_members(paste("tmin_0", i, "_mw", sep = ""))
  }
  else{
    open_members(paste("tmin_", i, "_hw", sep = ""))
    open_members(paste("tmin_", i, "_mw", sep = ""))
    open_members(paste("tmin_", i, "_nw", sep = ""))
    combine_all_members(paste("tmin_", i, "_hw", sep = ""))
    combine_all_members(paste("tmin_", i, "_mw", sep = ""))
    
  }
}
#writing combined tmin
for(i in c(1:4, 9:12)){
  
if(i < 10){  
write_combined(paste("tmin_0", i, "_hw", sep = ""))
write_combined(paste("tmin_0", i, "_mw", sep = ""))
}
else {
  write_combined(paste("tmin_", i, "_hw", sep = ""))
  write_combined(paste("tmin_", i, "_mw", sep = ""))

  
}
}

for(i in 4:10){
  if(i < 10){
  open_members(paste("GDD5_0", i, "_hw", sep = ""))
  open_members(paste("GDD5_0", i, "_mw", sep = ""))
  open_members(paste("GDD5_0", i, "_nw", sep = ""))
  combine_all_members(paste("GDD5_0", i, "_hw", sep = ""))
  combine_all_members(paste("GDD5_0", i, "_mw", sep = ""))
  
  open_members(paste("GDD0_0", i, "_hw", sep = ""))
  open_members(paste("GDD0_0", i, "_mw", sep = ""))
  open_members(paste("GDD0_0", i, "_nw", sep = ""))
  combine_all_members(paste("GDD0_0", i, "_hw", sep = ""))
  combine_all_members(paste("GDD0_0", i, "_mw", sep = ""))
  }
  else{
    open_members(paste("GDD5_", i, "_hw", sep = ""))
    open_members(paste("GDD5_", i, "_mw", sep = ""))
    open_members(paste("GDD5_", i, "_nw", sep = ""))
    combine_all_members(paste("GDD5_", i, "_hw", sep = ""))
    combine_all_members(paste("GDD5_", i, "_mw", sep = ""))
    
    open_members(paste("GDD0_", i, "_hw", sep = ""))
    open_members(paste("GDD0_", i, "_mw", sep = ""))
    open_members(paste("GDD0_", i, "_nw", sep = ""))
    combine_all_members(paste("GDD0_", i, "_hw", sep = ""))
    combine_all_members(paste("GDD0_", i, "_mw", sep = ""))
    
  }
  
}

#write GDD
for(i in 4:10){
  
  if(i < 10){  
    write_combined(paste("GDD5_0", i, "_hw", sep = ""))
    write_combined(paste("GDD5_0", i, "_mw", sep = ""))
    write_combined(paste("GDD0_0", i, "_hw", sep = ""))
    write_combined(paste("GDD0_0", i, "_mw", sep = ""))
  }
  else {
    write_combined(paste("GDD5_", i, "_hw", sep = ""))
    write_combined(paste("GDD5_", i, "_mw", sep = ""))
    write_combined(paste("GDD0_", i, "_hw", sep = ""))
    write_combined(paste("GDD0_", i, "_mw", sep = ""))
    
    
  }
}


#combining PPT
{
open_members(paste("ppt_0", 9, "_hw", sep = ""))
open_members(paste("ppt_0", 9, "_mw", sep = ""))
combine_all_members(paste("ppt_0", 9, "_hw", sep = ""))
combine_all_members(paste("ppt_0", 9, "_mw", sep = ""))

open_members(paste("ppt_", 10, "_hw", sep = ""))
open_members(paste("ppt_", 10, "_mw", sep = ""))
combine_all_members(paste("ppt_", 10, "_hw", sep = ""))
combine_all_members(paste("ppt_", 10, "_mw", sep = ""))
}

#writing ppt
for(i in 9:10) {
  if(i < 10){  
    write_combined(paste("ppt_0", i, "_hw", sep = ""))
    write_combined(paste("ppt_0", i, "_mw", sep = ""))
  }
  else {
    write_combined(paste("ppt_", i, "_hw", sep = ""))
    write_combined(paste("ppt_", i, "_mw", sep = ""))

  }
  
}

periods_list <- c("tmin_12_03_hw", "tmin_12_03_mw", "GDD5_04_09_mw", "GDD5_04_09_hw", "GDD5_05_10_mw", "GDD5_05_10_hw")

lapply(periods_list, FUN = open_members) 

lapply(periods_list, combine_all_members)
  
lapply(periods_list, FUN = write_combined)

####Final product: anomalies

#opening all of the data from external drive
setwd("E:/climate_projection_data/bcvin_raster/combined")
filenames_combined <- list.files(pattern = ".asc", full.names = TRUE)
for(i in 1:length(filenames_combined)){
  assign(
    x = str_remove(paste(filenames_combined[[i]]), "./")%>%
      str_remove(. , ".asc"),
    raster(filenames_combined[[i]])
    
  )
  
}

#opening data from historical folder
setwd("E:/climate_projection_data/bcvin_raster/historical")
filenames_historical <- list.files(pattern = ".asc", full.names = TRUE)
for(i in 1:length(filenames_historical)){
  assign(
    x = str_remove(paste(filenames_historical[[i]]), "./")%>%
      str_remove(. , ".asc"),
    raster(filenames_historical[[i]])
    
  )
  
}

#myVariable_noWarmingScenario is to be entered as "ppt_09" or "tmax_06"
make_anomaly <- function(myVariable_noWarmingScenario){
  assign(
    x = "mw",
    value = get(paste(myVariable_noWarmingScenario, "_mw_combined", sep = ""))
  )
  assign(
    x = "hw",
    value = get(paste(myVariable_noWarmingScenario, "_hw_combined", sep = ""))
  )
  assign(
    x = "hist",
    value = get(paste(myVariable_noWarmingScenario, "_nw", sep = ""))
  )
  assign(
    x = paste(myVariable_noWarmingScenario, "_mw_anomaly", sep = ""),
    value = overlay(mw, hist, fun = function(x, y){ x - y }),
    envir = .GlobalEnv
  )
  assign(
    x = paste(myVariable_noWarmingScenario, "_hw_anomaly", sep = ""),
    value = overlay(hw, hist, fun = function(x, y){ x - y }),
    envir = .GlobalEnv
  )
}

#manually add period variables
variable_list <- c("ppt_09", "ppt_10", paste("tmax_0", 6:9, sep = ""), paste("tmin_0", 1:4, sep = ""), "tmin_09", paste("tmin_", 10:12, sep = ""), 
                   "GDD5_05_10", "GDD5_04_09", "tmin_12_03")
lapply(variable_list, make_anomaly)

anomaly_list <- ls(pattern = "anomaly")

setwd("E:/climate_projection_data/bcvin_raster/anomaly")
for(i in 2:length(anomaly_list)){#starts at 2 because the make_anomaly function occurs in that list
  
  writeRaster(
    
    get(anomaly_list[i]),
    filename = paste(anomaly_list[i], ".asc", sep = "")
  )
  
}


