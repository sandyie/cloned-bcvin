#there's something weird going on with the climate(YearRange) assignment

library(tidyverse)
setwd("C:/Ecology Lab/R/bcvin_git/bcvin/hardiness/analyses/CarlModelMay2020")
source("helpfulFunctions.R")
source("magicNumbers.R")
source("IFstatements.R")
source("C:/Ecology Lab/R/bcvin_git/bcvin/hardiness/dashboard/moreFunctions.R")


###Loading and preparing data for the model. 
setwd("C:/Ecology Lab/R/bcvin_git/bcvin/hardiness/analyses/input")

climate1981to2010 <- read_csv("climhist_19812010.csv", col_names = FALSE)

climate1981to2010_1 <- climate1981to2010 %>%
  select(date = X1, twoDayAvg = X3) %>%
  .[-1,]

climate1981to2010_2 <- climate1981to2010_1 %>%
  mutate(day = getDay(date), month = sapply(date, getMonthNum, USE.NAMES = FALSE)) %>%
  select(day, month, twoDayAvg, date) %>%
  type_convert() 

CurrentMonth <- 3#str_extract(Sys.Date(), "-[0-9]+-") %>% #if you want to change this to a user input for a Shiny server etc. just make a reactive option instead of Sys.Date()
  str_remove_all(., "-") %>%
  as.numeric()
CurrentYear <- 2020#str_extract(Sys.Date(), "[0-9][0-9][0-9][0-9]") %>%
  as.numeric()
CurrentDay <- gsub("[0-9][0-9][0-9][0-9]-[0-9][0-9]-", "", Sys.Date()) %>%
  as.numeric()

#start scraping data
url_start <- "https://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=2012-01-01|2020-07-15&dlyRange=2012-05-10|2020-07-15&mlyRange=|&StationID=50269&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2020&selRowPerPage=50&Line=1&searchMethod=contains&Month="
url_end <- "&Day=15&timeframe=2&Year="

setwd("C:/Ecology Lab/R/bcvin_git/bcvin/hardiness/dashboard/historicLTEdata")
predLTE_combined_2013to14 <- read_csv("predLTE_combined_2013to14.csv") 
predLTE_combined_2014to15 <- read_csv("predLTE_combined_2014to15.csv")
predLTE_combined_2015to16 <- read_csv("predLTE_combined_2015to16.csv")
predLTE_combined_2016to17 <- read_csv("predLTE_combined_2016to17.csv")
predLTE_combined_2017to18 <- read_csv("predLTE_combined_2017to18.csv")
predLTE_combined_2018to19 <- read_csv("predLTE_combined_2018to19.csv")
predLTE_combined_2019to20 <- read_csv("predLTE_combined_2019to20.csv")

##Scraping Data from this and last year
scrape_mean_penticton_temp(12, CurrentYear-1) 
combine_monthly_temps(12, CurrentYear - 1)

scrape_mean_penticton_temp(CurrentMonth, CurrentYear)
combine_monthly_temps(12, CurrentYear)
combine_years_temps(CurrentYear - 1, CurrentYear, CurrentMonth)
##

#Data manipulation 
historicTemps <- read_csv("meanTemps2013toLastYear.csv")
historicTemps <- historicTemps %>%
  select(-(X1))

dates <- as.data.frame(seq(as.Date(paste0(CurrentYear - 1, "-01-01")), by = 1, len = nrow(meanTempsToDate)))
colnames(dates)[1] <- "date"
meanTempsToDate_1 <- meanTempsToDate %>%
  arrange(year) %>%
  bind_cols(., dates)
meanTempsToDate_1$month <- format(as.Date(meanTempsToDate_1$date), "%m")
meanTempsToDate_1$day <- format(as.Date(meanTempsToDate_1$date), "%d")

meanTempsToDate_1$tdiff <- rep(0, nrow(meanTempsToDate_1)) #initiated for the next for loop
meanTempsToDate_1$twoDayAvg <- rep(0, nrow(meanTempsToDate_1))
meanTempsToDate_1$day <- as.numeric(meanTempsToDate_1$day)
meanTempsToDate_1$month <- as.numeric(meanTempsToDate_1$month)

#calculating twoDayAverage
for(i in 1:nrow(meanTempsToDate_1)){
  if(i == 1){
    meanTempsToDate_1$twoDayAvg[1] <- meanTempsToDate_1$avgTemp[1]
  }else if(is.na(meanTempsToDate_1$avgTemp[i])){
    meanTempsToDate_1$twoDayAvg[i] <- NA
  }else if(is.na(meanTempsToDate_1$twoDayAvg[i-1])){
    meanTempsToDate_1$twoDayAvg[i] <- meanTempsToDate_1$avgTemp[i]
  }else{
    meanTempsToDate_1$twoDayAvg[i] <- (meanTempsToDate_1$avgTemp[i] + meanTempsToDate_1$avgTemp[i-1]) / 2
  }
  
} 

#this should save all of the historic data once the new year is reached
#if the current year is saved to this file, there will be redundant data 
allMeanTemps <- bind_rows(historicTemps, meanTempsToDate_1 %>% filter(year == CurrentYear))
if(CurrentMonth == 1){
  write.csv(allMeanTemps %>% filter(year != CurrentYear), "meanTemps2013toLastYear.csv")
}


currentTemps <- allMeanTemps %>% 
  filter(year == CurrentYear)

if(((CurrentMonth == 9 & CurrentDay > 14) & CurrentMonth <= 12) | (CurrentMonth > 9 & CurrentMonth <= 12)){
  currentTemps_dormancy <- allMeanTemps %>% filter(year == CurrentYear) %>%
    filter(month > 8) %>% 
    filter(!(month == 9 & day <= 14))
}else if((CurrentMonth >= 1 & CurrentMonth < 5)){
  currentTemps_dormancy <- allMeanTemps %>% filter(year == CurrentYear | year == (CurrentYear - 1)) %>%
    filter((year == CurrentYear & month < 5) | (year == (CurrentYear - 1) & month > 8)) %>%
    filter(!(month == 9 & day <= 14)) %>%
    filter(!(month == 4 & day >= 16))
}else{
  currentTemps_dormancy <- NA
  }

#calculating Tdiff
if(!is.na(currentTemps_dormancy)[1]){
for(i in 1:nrow(currentTemps_dormancy)){
  m <- currentTemps_dormancy$month[i]
  d <- currentTemps_dormancy$day[i] 
  
  diff <- currentTemps_dormancy$twoDayAvg[i] - climate1981to2010_2$twoDayAvg[climate1981to2010_2$month == m & climate1981to2010_2$day == d]
  
  if(is.na(diff)){
    currentTemps_dormancy$tdiff[i] <- 0
  }else{ 
    currentTemps_dormancy$tdiff[i] <- diff
    }
  }
  currentTemps_dormancy$daynum <- calculate_day_v(currentTemps_dormancy$day, currentTemps_dormancy$month)
  currentTemps_dormancy_1<- currentTemps_dormancy %>%
    filter(daynum >= 1 & daynum <= 176) %>%
    unnest(., daynum) 
  currentTemps_dormancy_daynum <- unnest(currentTemps_dormancy_1, daynum)
}else{
  currentTemps_no_dormancy <- allMeanTemps %>% filter(year == CurrentYear)
  }#can add an else{} here to do something special with the temperatures that are not in the dormancy range 

#may not need the following commented out code. I think the feb29th isn't worthwhile 
#adding Feb 29th 
#if(length(unique(currentTemps_dormancy$year)) == 2){
#if(unique(currentTemps_dormancy$year[1]) == 2019 | unique(currentTemps_dormancy$year[1]) == 2023 | unique(currentTemps_dormancy$year[1]) == 2027 | unique(currentTemps_dormancy$year[1]) == 2031 | unique(currentTemps_dormancy$year[1]) == 2035 | unique(currentTemps_dormancy$year[1]) == 2039 | unique(currentTemps_dormancy$year[1]) == 2043){
#  currentTemps_dormancy_1 <- currentTemps_dormancy
#}else{
#  feb29th <- tibble(year = unique(currentTemps_dormancy$year)[2], month = 2, day = 29, twoDayAvg = 0, tdiff = 0)#accounting for non leap years
#  currentTemps_dormancy_1 <- bind_rows(currentTemps_dormancy, feb29th)
#  }
#}else{
#  currentTemps_dormancy_1 <- currentTemps_dormancy
#  }

#meanTempsToDate_dormancy_1 <- bind_rows(meanTempsToDate_dormancy, feb29th) 
###end loading data


#Combining hardiness data and using season (season 1 = 2012-2013, season 7 = 2018-2019). With scraped data, season 1 isn't available as the earliest data from the database is 2012

#daynum == 73 is Dec 31st 
###Needs work here. 
if(CurrentMonth < 5){
  assign(
    x = paste0("climate", (CurrentYear - 1), "to", CurrentYear - 2000), 
    value = currentTemps_dormancy_daynum %>%
      filter((year == (CurrentYear - 1) & daynum <= 73)|(year == CurrentYear & daynum >= 74)) 
    )
  
}else if(CurrentMonth > 8){
  assign(
    x = paste0("climate", (CurrentYear), "to", CurrentYear - 2000 + 1), 
    value = currentTemps_dormancy_daynum %>%
      filter((year == CurrentYear & daynum <= 73))
  )
  
}else if(CurrentMonth >= 5 & CurrentMonth <= 8){
  assign(
    x = paste0("climate", (CurrentYear), "to", CurrentYear - 2000 + 1), 
    value = allMeanTemps %>%
      filter(year == CurrentYear)
  )
  
  
  }

LTE <- c(1:176) %>%
  as_tibble() %>%
  mutate(estLTE = estimateLTE(value)) %>%
  select(day = value, estLTE) 

estimateLTEperday(LTE$estLTE)
estimatedLTE <- bind_cols(LTE, LTEperday)
###end data loading / preparing 

######trying to calculate the initial values 
if(CurrentMonth == 9 & CurrentDay > 19){
  meanTempsToDate_init <- meanTempsToDate_1 %>%
    filter(month == 9 & CurrentDay > 19)
}else if(CurrentMonth == 10 | CurrentMonth < 5){
  meanTempsToDate_init <- meanTempsToDate_1 %>%
    filter((month == 9 & day > 19) | (month == 10 & day < 21))
}else if(CurrentMonth < 9 & CurrentMonth > 4){
  meanTempsToDate_init <- NA
}

if(!is.na(meanTempsToDate_init)){
for(i in 1:nrow(meanTempsToDate_init)){
  m <- meanTempsToDate_init$month[i]
  d <- meanTempsToDate_init$day[i] 
  
  
  diff <- meanTempsToDate_init$twoDayAvg[i] - climate1981to2010_2$twoDayAvg[climate1981to2010_2$month == m & climate1981to2010_2$day == d]
  if(is.na(diff)){
    meanTempsToDate_init$tdiff[i] <- 0
  }else{
    meanTempsToDate_init$tdiff[i] <- diff
    }
  }
}else{
  finalData <- climateCurrent
  }


#LTE data between Sept 21st and Oct 20th are all arbitrary. I'm not going to take the values from excel so I can at least calculate the initial Oct 20th value.
# "Sep 21 to Oct 20 - initial hardiness accumulation phase.  Bud hardiness increases steadily. Estimated LTE/day grows from -0.15 to -0.50 (this is guesswork as there is no hardiness data available)."
#taken from the excel sheet for the initial values
estLTEperday <- c(-0.13, -0.15, -0.18, -0.20, -0.23, -0.25, -0.28, -0.30, -0.33, -0.35, -0.38, -0.40, -0.43, -0.45, -0.48, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.49, -0.48, -0.46, -0.45, -0.44)
estimatedLTE1 <- c(-1.35, -1.48, -1.63, -1.80, -2.00, -2.23, -2.48, -2.75, -3.05, -3.38, -3.73, -4.10, -4.50, -4.93, -5.38, -5.85, -6.35, -6.85, -7.35, -7.85, -8.35, -8.85, -9.35, -9.85, -10.35, -10.85, -11.35, -11.84, -12.32, -12.78, -13.23) %>%
  as_tibble() %>%
  select(estLTE = value) %>%
  mutate(estLTEperday)

historicGDDsums <- data.frame(GDD = c(rep(0,length(unique(historicTemps$year)))), year = (unique(historicTemps$year)[1]):(unique(historicTemps$year)[length(unique(historicTemps$year))]))
index <- 1

for(i in (unique(historicTemps$year)[1]):(unique(historicTemps$year)[length(unique(historicTemps$year))])){
  historicGDDsums$GDD[index] <- sum(unlist(aboveThresholdTemp_v(historicTemps %>% filter(year == i) %>% select(twoDayAvg) %>% .[[1]], 10)))
  
  historicGDDsums$year[index] <- i
  index <- index + 1
}


historicAverageGDD <- mean(historicGDDsums$GDD) 

if(CurrentMonth >= 5){
  currentGDD <- sum(unlist(aboveThresholdTemp_v(currentTemps$twoDayAvg, 10)))
}else if(CurrentMonth <= 5){
  currentGDD <- sum(unlist(aboveThresholdTemp_v(allMeanTemps %>% filter(year == (CurrentYear - 1)) %>% select(twoDayAvg) %>% .[[1]], 10)))  
  }


Sep20 <- calculate_Sep20(historicAverageGDD, currentGDD)



if(!is.na(meanTempsToDate_init)[1]){
  Oct20 <- calculate_Oct20th(Sep20, Sep20, Sep20, "Current")
  finalLTEpredictions(Oct20, Oct20, Oct20, "Current")
  finalData <- predLTE_combined_Current
}else{
  finalData <- get(paste0())
  }


#####these only need to be ran once to get the and then store it into a CSV
#####stored in hardiness/dashboard/historicLTEdata
#finalLTEpredictions(Oct20s_df$MyOct20[1], Oct20s_df$MyOct20[1], Oct20s_df$MyOct20[1], "2013to14") 
#finalLTEpredictions(Oct20s_df$MyOct20[2], Oct20s_df$MyOct20[2], Oct20s_df$MyOct20[2], "2014to15")
#finalLTEpredictions(Oct20s_df$MyOct20[3], Oct20s_df$MyOct20[3], Oct20s_df$MyOct20[3], "2015to16")
#finalLTEpredictions(Oct20s_df$MyOct20[4], Oct20s_df$MyOct20[4], Oct20s_df$MyOct20[4], "2016to17")
#finalLTEpredictions(Oct20s_df$MyOct20[5], Oct20s_df$MyOct20[5], Oct20s_df$MyOct20[5], "2017to18")
#finalLTEpredictions(Oct20s_df$MyOct20[6], Oct20s_df$MyOct20[6], Oct20s_df$MyOct20[6], "2018to19")
#finalLTEpredictions(Oct20s_df$MyOct20[7], Oct20s_df$MyOct20[7], Oct20s_df$MyOct20[7], "2019to20") #this function may need to be edited to account for a non exact dataframe length of 176


addUntilZero <- function(value, initialTotal){
  while(value > 0){
  updatedValue <- value - 1
  total <- initialTotal + value
  return(addUntilZero(updatedValue, total))
  }
  
  return(initialTotal)
}
