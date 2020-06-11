source("C:/Users/adamfong/Desktop/Ecology Lab/R/bcvin_git/analyses/climate_projections/functions_externaldrive.R")#in a different directory than the data

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

##############################################################################################NEED TO COMPLETE THE ANALYSIS SCRIPT AFTER THIS LINE


for(i in 6:9){
open_members(paste("tmax_0", i, "_hw", sep = ""))
open_members(paste("tmax_0", i, "_mw", sep = ""))
open_members(paste("tmax_0", i, "_nw", sep = ""))
combine_all_members(paste("tmax_0", i, "_hw", sep = ""))
combine_all_members(paste("tmax_0", i, "_mw", sep = ""))
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





  
