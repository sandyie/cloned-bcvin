source("C:/Users/adamfong/Desktop/Ecology Lab/R/bcvin_git/analyses/climate_projections/functions.R")#in a different directory than the data

#this is script is going to be a function which will read what member to open then continue through the rest of the referenced functions in source
#it is very important to only run one member at a time as the files will be written to the current working directory without indication of what member it belongs to

choose_member_yearly <- function(startYear, endYear, member_as.string){
  open_MAT(startYear, endYear, member_as.string)
  open_MAT_historical(startYear, endYear)
}

choose_member_yearly <- function(startYear, endYear, startMonth, endMonth, member_as.string){
  open_Tmax_m(startYear, endYear, startMonth, endMonth, member_as.string)
  open_Tmin_m(startYear, endYear, startMonth, endMonth, member_as.string)
  open_PPT_m(startYear, endYear, startMonth, endMonth, member_as.string)
  open_GDD5_m(startYear, endYear, startMonth, endMonth, member_as.string)
  open_GDD0_m(startYear, endYear, startMonth, endMonth, member_as.string)
  open_Tmax_m_historical(startYear, endYear, startMonth, endMonth)
  open_Tmin_m_historical(startYear, endYear, startMonth, endMonth)
  open_PPT_m_historical(startYear, endYear, startMonth, endMonth)
  open_GDD5_m_historical(startYear, endYear, startMonth, endMonth)
  open_GDD0_m_historical(startYear, endYear, startMonth, endMonth)
}
  
aggregate_allVars_warmingScenarios <- function(startYear, endYear, startMonth, endMonth){
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
  
aggregate_mat_all <- 
  aggregate_mat(2070, 2089, "hw")
  aggregate_mat(2040, 2059, "mw")

aggregate_mat_historical <- aggregate_mat(1970, 1989, "nw")
  
write_all_monthly_vars <- function(startMonth, endMonth){
  write_monthly_var(startMonth, endMonth, "tmax", "hw")
  write_monthly_var(startMonth,endMonth, "tmin", "hw")
  write_monthly_var(startMonth,endMonth, "GDD5", "hw")
  write_monthly_var(startMonth,endMonth, "GDD0", "hw")
  write_monthly_var(startMonth,endMonth, "ppt", "hw")
  
}

write_all_historical_monthly_vars <- function(startMonth, endMonth){
  write_monthly_var(startMonth,endMonth, "tmax", "nw")
  write_monthly_var(startMonth,endMonth, "tmin", "nw")
  write_monthly_var(startMonth,endMonth, "GDD5", "nw")
  write_monthly_var(startMonth,endMonth, "GDD0", "nw")
  write_monthly_var(startMonth,endMonth, "ppt", "nw")
}

write_all_MAT <- {
  write_yearly_MAT("hw")
  write_yearly_MAT("nw")
  write_yearly_MAT("mw")
}

combine_monthly_vars(5, 10, "GDD5", "INSERT WARMING SCENARIO")
combine_monthly_vars(4, 9, "GDD5", "INSERT WARMING SCENARIO")
combine_monthly_vars(12, 3, "tmin", "INSERT WARMING SCENARIO")

#writeRaster those three combine monthly vars
  

##############################################################################################NEED TO COMPLETE THE ANALYSIS SCRIPT AFTER THIS LINE
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
  
  
