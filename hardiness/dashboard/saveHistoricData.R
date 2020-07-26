#this script is the rendition of mainDashboard.R that will be called by midge
#the goal is to remove unnecessary variables so that things can be more automated without having to make annoying assign functions

#This script is the the bulk of the model and what will be necessary to run for a Shiny Dashboard
#Data scraping comes first. Carl's model appears after all the data is scraped from Environment Canada
#In order to make this functional in an updateable app a few things need to be done.
#   1. The script needs to made into functions that just iterate & assign values based on the current year up to the earliest year.
#   2. Make sure that when 2021 comes around (and every year after) things will still work
#         * start by targeting all of the repetitive function calls that end at 2020 or 2019. 2019 should be replaced by CurrentYear - 1
#         * maybe a recursive function would be great: enter the year and then recurse? until CurrentYear is reached which will be the last call
#         * can have 
#   3. Separate the script so that the server can update the current weather info
#         * would be best if the data from 2012 up to last year was saved in a .csv that could be called rather than scraped - this should decrease loading time

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

CurrentMonth <- str_extract(Sys.Date(), "-[0-9]+-") %>% #if you want to change this to a user input for a Shiny server etc. just make a reactive option instead of Sys.Date()
  str_remove_all(., "-") %>%
  as.numeric()
CurrentYear <- str_extract(Sys.Date(), "[0-9][0-9][0-9][0-9]") %>%
  as.numeric()

year1 <- 2013
year2 <- 2019

url_start <- "https://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=2012-01-01|2020-07-15&dlyRange=2012-05-10|2020-07-15&mlyRange=|&StationID=50269&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2020&selRowPerPage=50&Line=1&searchMethod=contains&Month="
url_end <- "&Day=15&timeframe=2&Year="

#this loop takes a little to long run. May need some tweaking with how the data is stored once implemented into the app
for(year in year1:CurrentYear){ #retrieves data from 2013 up to yesterday 
  if(year != CurrentYear){
    scrape_mean_penticton_temp(12, year)
  }else{
    scrape_mean_penticton_temp(CurrentMonth, year)
  }
  combine_monthly_temps(12, year)
}

combine_years_temps(2013, CurrentYear, CurrentMonth)

dates <- as.data.frame(seq(as.Date("2013-01-01"), by = 1, len = nrow(meanTempsToDate)))
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

feb29th <- tibble(year = c(2013:2015, 2017:(CurrentYear - 1)), month = 2, day = 29, twoDayAvg = 0, tdiff = 0)#accounting for non leap years

meanTempsToDate_write <- bind_rows(meanTempsToDate_1, feb29th)

write.csv(meanTempsToDate_write, "meanTemps2013toLastYear.csv")


meanTempsToDate_dormancy <- meanTempsToDate_1 %>% #making the data be the same range as the historical data from Carl
  filter(month != 5, month != 6, month != 7, month != 8)  %>%
  filter(!(month == 9 & day <= 14)) %>%
  filter(!(month == 4 & day >= 16)) %>%
  bind_rows(., feb29th)


for(i in 1:nrow(meanTempsToDate_dormancy)){
  m <- meanTempsToDate_dormancy$month[i]
  d <- meanTempsToDate_dormancy$day[i] 
  
  diff <- meanTempsToDate_dormancy$twoDayAvg[i] - climate1981to2010_2$twoDayAvg[climate1981to2010_2$month == m & climate1981to2010_2$day == d]
  
  if(is.na(diff)){
    meanTempsToDate_dormancy$tdiff[i] <- 0
  }else{ 
    meanTempsToDate_dormancy$tdiff[i] <- diff
  }
}

meanTempsToDate_dormancy_1 <- bind_rows(meanTempsToDate_dormancy, feb29th) 
###end loading data


#Combining hardiness data and using season (season 1 = 2012-2013, season 7 = 2018-2019). With scraped data, season 1 isn't available as the earliest data from the database is 2012

meanTempsToDate_dormancy$daynum <- calculate_day_v(meanTempsToDate_dormancy$day, meanTempsToDate_dormancy$month)
meanTempsToDate_dormancy_1<- meanTempsToDate_dormancy %>%
  filter(daynum >= 1 & daynum <= 176) %>%
  unnest(., daynum) 
meanTempsToDate_dormancy_daynum <- unnest(meanTempsToDate_dormancy_1, daynum)

#would be great to turn this into a function that auto assigns names rather than manually doing this. Would be more robust for the app 
#up until CurrentYear
climate2013to14 <- meanTempsToDate_dormancy_daynum %>% 
  filter((year == 2013 & daynum <= 73) | (year == 2014 & daynum >= 74)) %>%
  select(daynum, tdiff, twoDayAvg) %>% 
  filter(!is.na(tdiff))
climate2014to15 <- meanTempsToDate_dormancy_daynum %>%
  filter((year == 2014 & daynum <= 73) | (year == 2015 & daynum >= 74)) %>%
  select(daynum, tdiff, twoDayAvg) %>% 
  filter(!is.na(tdiff))
climate2015to16 <- meanTempsToDate_dormancy_daynum %>%
  filter((year == 2015 & daynum <= 73) | (year == 2016 & daynum >= 74)) %>%
  select(daynum, tdiff, twoDayAvg) %>% 
  filter(!is.na(tdiff))
climate2016to17 <- meanTempsToDate_dormancy_daynum %>%
  filter((year == 2016 & daynum <= 73) | (year == 2017 & daynum >= 74)) %>%
  select(daynum, tdiff, twoDayAvg) %>% 
  filter(!is.na(tdiff)) 
climate2017to18 <- meanTempsToDate_dormancy_daynum %>%
  filter((year == 2017 & daynum <= 73) | (year == 2018 & daynum >= 74)) %>%
  select(daynum, tdiff, twoDayAvg) %>% 
  filter(!is.na(tdiff))
climate2018to19 <- meanTempsToDate_dormancy_daynum %>%
  filter((year == 2018 & daynum <= 73) | (year == 2019 & daynum >= 74)) %>%
  select(daynum, tdiff, twoDayAvg) %>% 
  filter(!is.na(tdiff))
climate2019to20 <- meanTempsToDate_dormancy_daynum %>%
  filter((year == 2019 & daynum <= 73) | (year == 2020 & daynum >= 74)) %>%
  select(daynum, tdiff, twoDayAvg) %>%
  filter(!is.na(tdiff))

LTE <- c(1:176) %>%
  as_tibble() %>%
  mutate(estLTE = estimateLTE(value)) %>%
  select(day = value, estLTE) 

estimateLTEperday(LTE$estLTE)
estimatedLTE <- bind_cols(LTE, LTEperday)
###end data loading / preparing 

######trying to calculate the initial values 
meanTempsToDate_init <- meanTempsToDate_1 %>%
  filter((month == 9 & day > 19)| (month == 10 & day < 21))


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



#LTE data between Sept 21st and Oct 20th are all arbitrary. I'm not going to take the values from excel so I can at least calculate the initial Oct 20th value.
# "Sep 21 to Oct 20 - initial hardiness accumulation phase.  Bud hardiness increases steadily. Estimated LTE/day grows from -0.15 to -0.50 (this is guesswork as there is no hardiness data available)."
#can use the output of these functions as the initial values for the finalLTEpredictions if you want. 
estLTEperday <- c(-0.13, -0.15, -0.18, -0.20, -0.23, -0.25, -0.28, -0.30, -0.33, -0.35, -0.38, -0.40, -0.43, -0.45, -0.48, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.49, -0.48, -0.46, -0.45, -0.44)
estimatedLTE1 <- c(-1.35, -1.48, -1.63, -1.80, -2.00, -2.23, -2.48, -2.75, -3.05, -3.38, -3.73, -4.10, -4.50, -4.93, -5.38, -5.85, -6.35, -6.85, -7.35, -7.85, -8.35, -8.85, -9.35, -9.85, -10.35, -10.85, -11.35, -11.84, -12.32, -12.78, -13.23) %>%
  as_tibble() %>%
  select(estLTE = value) %>%
  mutate(estLTEperday)

sept20 <- c(.29, -.5, 0, -2.6, .14, -.52, .29, .21) #pulled directly from Carl's model 

GDDsums <- data.frame(GDD = c(rep(0,length(2013:(CurrentYear -1)))), year = 2013:(CurrentYear -1 ))
index <- 1

###################WHY ZERO LENGTH INPUT

for(i in 2013:(CurrentYear-1)){
  GDDsums$GDD[index] <- sum(unlist(aboveThresholdTemp_v(meanTempsToDate_1 %>% filter(year == i) %>% select(twoDayAvg) %>% .[[1]], 10)))

index <- index + 1
}

averageGDD <- mean(GDDsums$GDD) 

Sep20_yearly <- data.frame("sep20" = calculate_Sep20_v(averageGDD, GDDsums$GDD),
                           "year" = 2013:(CurrentYear - 1))

Oct20s_df <- data.frame(MyOct20 = c(rep(0, length(2013:(CurrentYear - 1)))), year = 2013:(CurrentYear-1))
index2 <- 1
for(i in 2013:(CurrentYear - 1)){
  assign(x = paste0("climate", i, "_init"),
         value = meanTempsToDate_init %>% filter(year == i)
          )
  Oct20s_df$MyOct20[index2] <- calculate_Oct20th(Sep20_yearly$sep20[index2], Sep20_yearly$sep20[index2], Sep20_yearly$sep20[index2], paste0(i, "_init"))[3]
  index2 <- index2 + 1
  }

#####these only need to be ran once to get the and then store it into a CSV
#####stored in hardiness/dashboard/historicLTEdata
finalLTEpredictions(Oct20s_df$MyOct20[1], Oct20s_df$MyOct20[1], Oct20s_df$MyOct20[1], "2013to14") 
finalLTEpredictions(Oct20s_df$MyOct20[2], Oct20s_df$MyOct20[2], Oct20s_df$MyOct20[2], "2014to15")
finalLTEpredictions(Oct20s_df$MyOct20[3], Oct20s_df$MyOct20[3], Oct20s_df$MyOct20[3], "2015to16")
finalLTEpredictions(Oct20s_df$MyOct20[4], Oct20s_df$MyOct20[4], Oct20s_df$MyOct20[4], "2016to17")
finalLTEpredictions(Oct20s_df$MyOct20[5], Oct20s_df$MyOct20[5], Oct20s_df$MyOct20[5], "2017to18")
finalLTEpredictions(Oct20s_df$MyOct20[6], Oct20s_df$MyOct20[6], Oct20s_df$MyOct20[6], "2018to19")
finalLTEpredictions(Oct20s_df$MyOct20[7], Oct20s_df$MyOct20[7], Oct20s_df$MyOct20[7], "2019to20") 

write.csv(predLTE_combined_2013to14, "predLTE_combined_2013to14.csv")
write.csv(predLTE_combined_2014to15, "predLTE_combined_2014to15.csv") 
write.csv(predLTE_combined_2015to16, "predLTE_combined_2015to16.csv") 
write.csv(predLTE_combined_2016to17, "predLTE_combined_2016to17.csv") 
write.csv(predLTE_combined_2017to18, "predLTE_combined_2017to18.csv") 
write.csv(predLTE_combined_2018to19, "predLTE_combined_2018to19.csv") 
write.csv(predLTE_combined_2019to20, "predLTE_combined_2019to20.csv")

#####


#final values. 
#predLTEfinal_2012to13
#use copyActualLTE in helpfulFunctions.R. So much better than manually entering this

#
measuredLTE_2013to14 <- tibble(date = as.Date(c("2013-10-25", "2013-11-08", "2013-11-22", "2013-12-06", "2013-12-20", "2014-01-03", "2014-01-17", "2014-01-31", "2014-02-14", "2014-02-28", "2014-03-14", "2014-03-27"), format = "%Y-%m-%d"),
                                   LTE = c(-13.9, -19.42, -22.87, -24.06, -24.09, -23.06, -23, -23.46, -22.6, -22.76, -17.62, -14.89))
measuredLTE_2014to15 <- tibble(date = as.Date(c("2014-10-28", "2014-11-11", "2014-11-24", "2014-12-08", "2014-12-22", "2015-01-05", "2015-01-19", "2015-02-02", "2015-02-16", "2015-03-04", "2015-03-18"), format = "%Y-%m-%d"), 
                                   LTE = c(-13.58, -18.26, -21.3, -22.5, -22.67, -23.6, -22.12, -22.17, -18.5, -18.82, -11.55))
measuredLTE_2015to16 <- tibble(date = as.Date(c("2015-10-27", "2015-11-10", "2015-11-24", "2015-12-08", "2015-12-22", "2016-01-05", "2016-01-19", "2016-02-02", "2016-02-16", "2016-03-01", "2016-03-15", "2016-03-29"), format = "%Y-%m-%d"), 
                                   LTE = c(-15.76, -20.52, -22.51, -22.77, -22.99, -24.21, -23.38, -22.84, -21.92, -18, -14.99, -11.16))
measuredLTE_2016to17 <- tibble(date = as.Date(c("2016-11-08", "2016-11-22", "2016-12-06", "2016-12-20", "2017-01-03", "2017-01-17", "2017-01-31", "2017-02-14", "2017-02-28", "2017-03-14", "2017-03-28", "2017-04-04"), format = "%Y-%m-%d"), 
                                   LTE = c(-17.25, -20.98, -23.49, -24.42, -24.69, -26.05, -23.58, -24.08, -22.87, -20.5, -15.33, -13.66))
measuredLTE_2017to18 <- tibble(date = as.Date(c("2017-11-07", "2017-11-21", "2017-12-05", "2017-12-19", "2018-01-02", "2018-01-16", "2018-01-30", "2018-02-13", "2018-02-27", "2018-03-13", "2018-03-28", "2018-04-10"), format = "%Y-%m-%d"), 
                                   LTE = c(-21.95, -22.2, -23.8, -23.6, -25.4, -23.6, -22.9, -23.6, -23.5, -20.8, -15.9, -11))
measuredLTE_2018to19 <- tibble(date = as.Date(c("2018-11-06", "2018-11-20", "2018-12-04", "2018-12-18", "2019-01-01", "2019-01-15", "2019-01-29", "2019-02-12", "2019-02-26", "2019-03-12", "2019-03-26", "2019-04-09"), format = "%Y-%m-%d"), 
                                   LTE = c(-17.52, -23.3, -23.78, -23.85, -24.69, -24.02, -24.79, -25.18, -25.33, -24.65, -16.97, -10.72))
measuredLTE_2019to20 <- tibble(date = as.Date(c("2019-10-24", "2019-11-07", "2019-11-21", "2019-12-05", "2019-12-19", "2020-01-07", "2020-01-17", "2020-01-30", "2020-02-13", "2020-02-27", "2020-03-12", "2020-03-26"), format = "%Y-%m-%d"),
                                   LTE = c(-17.4, -21.19, -22.2, -24.4, -24.12, -23.34, -24.31, -22.8, -22.45, -21.31, -18.16, -15.83))

