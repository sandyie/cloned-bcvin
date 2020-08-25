#This script references a few other scripts which contain functions + more to prevent the workflow from becoming too messy. 
#This script is only the replication of Carl's May 2020 model which uses his GDD assumptions from the previous growing season. 
#If you want to see the model with all the derived values in the script reference: bcvin/hardiness/dashboard/mainDashboard.R

library(tidyverse)

if(length(grep("faith", getwd())>0)) { 
  setwd("~/Documents/github/bcvin/bcvin/hardiness/analyses/CarlModelMay2020") 
} else setwd("C:/Users/adamfong/Desktop/Ecology Lab/R/bcvin_git/hardiness/analyses/CarlModelMay2020")
source("helpfulFunctions.R")
source("magicNumbers.R")
source("IFstatements.R")
list.files()

###Loading and preparing data for the model. 

if(length(grep("faith", getwd())>0)) { 
  setwd("~/Documents/github/bcvin/bcvin/hardiness/analyses/input") 
} else setwd("C:/Users/adamfong/Desktop/Ecology Lab/R/bcvin_git/hardiness/analyses/input")

data_files <- list.files(pattern = ".csv")
necessaryFiles <- grep("budhardiness", data_files)
data_files1 <- data_files[necessaryFiles]
data_file_names <- gsub(".csv|bud", '', data_files1)

data <- lapply(data_files1, read_csv, col_names = FALSE)
climate2012to18 <- read_csv("2012-2018_PENTICTON_WEATHER_EM.csv", col_names = FALSE)
climate1981to2010 <- read_csv("climhist_19812010.csv", col_names = FALSE)

#assigning data from list
for(i in 1:length(data)){
  unlist(assign(
    x = data_file_names[i],
    value = data[i],
    envir = .GlobalEnv
  ))
}

climate1981to2010_1 <- climate1981to2010%>%
  select(date = X1, twoDayAvg = X3) %>%
  .[-1,]

climate1981to2010_2 <- climate1981to2010_1 %>%
  mutate(day = getDay(date), month = sapply(date, getMonthNum, USE.NAMES = FALSE)) %>%
  select(day, month, twoDayAvg, date) %>%
  type_convert() 

climate2012to18_1 <- climate2012to18 %>%
  select(date = X1, year = X2, month = X3, day = X4, twoDayAvg = X9) %>%
  .[c(-1:-14),] %>% #gets rid of legend portion of csv 
  filter(month != 5, month != 6, month != 7, month != 8) %>% #may, june, july, and august do not appear in the historical climate data 
  type_convert()

climate2012to18_GS <- climate2012to18 %>%
  select(date = X1, year = X2, month = X3, day = X4, meanTemp = X7, twoDayAvg = X9) %>%
  .[c(-1:-14),] %>%
  type_convert() %>%
  arrange(year, month, day)

climate2012to18_2 <- climate2012to18_1 %>%
  filter(!(month == 9 & day <= 14)) %>%
  filter(!(month == 4 & day >= 16))


tdiff <- tibble(tdiff = 1:nrow(climate2012to18_2)) #initiated for the next for loop

for(i in 1:nrow(climate2012to18_2)){
  m <- climate2012to18_2$month[i]
  d <- climate2012to18_2$day[i] 
  y <- climate2012to18_2$year[i]
  
  diff <- climate2012to18_2$twoDayAvg[i] - climate1981to2010_2$twoDayAvg[climate1981to2010_2$month == m & climate1981to2010_2$day == d]
  
  tdiff$tdiff[i] <- diff
  tdiff$month[i] <- m
  tdiff$day[i] <- d
  tdiff$year[i] <- y
}

feb29th <- tibble(date = c("29/02/2013", "29/02/2014", "29/02/2015", "29/02/2017", "29/02/2018", "29/02/2019"),
                  year = c(2013:2015, 2017:2019), month = 2, day = 29, twoDayAvg = 0, tdiff = 0)

feb29th1 <- feb29th %>% select(tdiff, month, day, year)

tdiff_feb29th <- bind_rows(tdiff, feb29th1) %>% filter(!is.na(tdiff))

climate2012to18_feb29th <- bind_rows(climate2012to18_2, feb29th %>% select(date, year, month, day, twoDayAvg)) %>%
  filter(!is.na(date))

climate2012to18_tdiff <- bind_cols(climate2012to18_feb29th, tdiff_feb29th)

#Combining hardiness data and using season (season 1 = 2012-2013, season 7 = 2018-2019)

climate2012to18_tdiff$daynum <- calculate_day_v(climate2012to18_tdiff$day, climate2012to18_tdiff$month)
climate2012to18_tdiff_daynum <- climate2012to18_tdiff %>%
  filter(daynum >= 1 & daynum <= 176) %>%
  unnest(. , daynum) %>% 
  filter(!is.na(tdiff)) %>%
  arrange(daynum)

climate2012to13 <- climate2012to18_tdiff_daynum %>%
  filter((year == 2012 & daynum <= 73)| (year == 2013 & daynum >= 74)) %>%
  select(daynum, tdiff, twoDayAvg) %>% 
  filter(!is.na(tdiff))
climate2013to14 <- climate2012to18_tdiff_daynum %>%
  filter((year == 2013 & daynum <= 73) | (year == 2014 & daynum >= 74)) %>%
  select(daynum, tdiff, twoDayAvg) %>% 
  filter(!is.na(tdiff))
climate2014to15 <- climate2012to18_tdiff_daynum %>%
  filter((year == 2014 & daynum <= 73) | (year == 2015 & daynum >= 74)) %>%
  select(daynum, tdiff, twoDayAvg) %>% 
  filter(!is.na(tdiff))
climate2015to16 <- climate2012to18_tdiff_daynum %>%
  filter((year == 2015 & daynum <= 73) | (year == 2016 & daynum >= 74)) %>%
  select(daynum, tdiff, twoDayAvg) %>% 
  filter(!is.na(tdiff))
climate2016to17 <- climate2012to18_tdiff_daynum %>%
  filter((year == 2016 & daynum <= 73) | (year == 2017 & daynum >= 74)) %>%
  select(daynum, tdiff, twoDayAvg) %>% 
  filter(!is.na(tdiff)) 
climate2017to18 <- climate2012to18_tdiff_daynum %>%
  filter((year == 2017 & daynum <= 73) | (year == 2018 & daynum >= 74)) %>%
  select(daynum, tdiff, twoDayAvg) %>% 
  filter(!is.na(tdiff))
climate2018to19 <- climate2012to18_tdiff_daynum %>%
  filter((year == 2018 & daynum <= 73) | (year == 2019 & daynum >= 74)) %>%
  select(daynum, tdiff, twoDayAvg) %>% 
  filter(!is.na(tdiff))

LTE <- c(1:176) %>%
  as_tibble() %>%
  mutate(estLTE = estimateLTE(value)) %>%
  select(day = value, estLTE) 

estimateLTEperday(LTE$estLTE)
estimatedLTE <- bind_cols(LTE, LTEperday)

###end data loading / preparing 

#calculating all final & intermediate LTE's
#can look at intermediate Predicted LTE's using predLTE[1/2]_yearRange
#look at final predicted LTE's using predLTEfinal_yearRange
#the values in these functions are drawn directly from the excel sheet from the previous season GDD. I have made a function that calculates the September 20th initial hardiness values as well as October 20th
#expect slight variation in the final predicted LTE. This happens when there are temperature differences that are very near the bounds of each IF statement. There are variations on the scale of .1 because of discrepancies in the precision of data
finalLTEpredictions(-10.54, -10.59, -10.59, "2012to13")
finalLTEpredictions(-13.6, -13.6, -13.6, "2013to14")
finalLTEpredictions(-9.77, -10.18, -10.18, "2014to15")
finalLTEpredictions(-12.84, -12.84, -12.84, "2015to16")
finalLTEpredictions(-11.84, -11.99, -11.99, "2016to17")
finalLTEpredictions(-13.25, -13.25, -13.25, "2017to18")
finalLTEpredictions(-13.84, -13.84, -13.84, "2018to19")

predLTE_combined_2012to13
predLTE_combined_2013to14
predLTE_combined_2014to15
predLTE_combined_2015to16
predLTE_combined_2016to17
predLTE_combined_2017to18
predLTE_combined_2018to19


######trying to calculate the initial values 
######NOTE: below this line is the preliminary work for calculating GDD and other values from Carl's model. I'm a little scared to delete this - It should be in a legible format and the right order in mainDashboard.R
climate2012to18_init <- climate2012to18_GS %>%
  filter((month == 9 & day > 19)| (month == 10 & day < 21))


tdiff1 <- tibble(tdiff = rep(0, nrow(climate2012to18_init))) #initiated for the next for loop

for(i in 1:nrow(climate2012to18_init)){
  m <- climate2012to18_init$month[i]
  d <- climate2012to18_init$day[i] 
  y <- climate2012to18_init$year[i]

  diff <- climate2012to18_init$twoDayAvg[i] - climate1981to2010_2$twoDayAvg[climate1981to2010_2$month == m & climate1981to2010_2$day == d]
  
  tdiff1$tdiff[i] <- diff
  tdiff1$month[i] <- m
  tdiff1$day[i] <- d
  tdiff1$year[i] <- y
}

climate2012to18_init_tdiff <- bind_cols(climate2012to18_init, tdiff1)

climate2013_init <- climate2012to18_init_tdiff %>% 
  filter(year == 2013)
climate2014_init <- climate2012to18_init_tdiff %>% 
  filter(year == 2014)
climate2015_init <- climate2012to18_init_tdiff %>% 
  filter(year == 2015)
climate2016_init <- climate2012to18_init_tdiff %>% 
  filter(year == 2016)
climate2017_init <- climate2012to18_init_tdiff %>% 
  filter(year == 2017)
climate2018_init <- climate2012to18_init_tdiff %>% 
  filter(year == 2018)

#LTE data between Sept 21st and Oct 20th are all arbitrary. I'm not going to take the values from excel so I can at least calculate the initial Oct 20th value.
# "Sep 21 to Oct 20 - initial hardiness accumulation phase.  Bud hardiness increases steadily. Estimated LTE/day grows from -0.15 to -0.50 (this is guesswork as there is no hardiness data available)."
#can use the output of these functions as the initial values for the finalLTEpredictions if you want. 

estLTEperday <- c(-0.13, -0.15, -0.18, -0.20, -0.23, -0.25, -0.28, -0.30, -0.33, -0.35, -0.38, -0.40, -0.43, -0.45, -0.48, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.50, -0.49, -0.48, -0.46, -0.45, -0.44)
estimatedLTE1 <- c(-1.35, -1.48, -1.63, -1.80, -2.00, -2.23, -2.48, -2.75, -3.05, -3.38, -3.73, -4.10, -4.50, -4.93, -5.38, -5.85, -6.35, -6.85, -7.35, -7.85, -8.35, -8.85, -9.35, -9.85, -10.35, -10.85, -11.35, -11.84, -12.32, -12.78, -13.23) %>%
  as_tibble() %>%
  select(estLTE = value) %>%
  mutate(estLTEperday)

sept20 <- c(.29, -.5, 0, -2.6, .14, -.52, .29, .21) #these are calculated by using GDD and average GDD. Figure this out next 

#off by up to +/- .04 from the values listed in excel. This accurately predicts the October 20th values with the exact Sept 20th values from Excel
givenSep20th <- data.frame( "sep20" = c(
calculate_Oct20th(sept20[2], sept20[2], sept20[2], "2013_init")[3],
calculate_Oct20th(sept20[3], sept20[3], sept20[3], "2014_init")[3],
calculate_Oct20th(sept20[4], sept20[4], sept20[4], "2015_init")[3],
calculate_Oct20th(sept20[5], sept20[5], sept20[5], "2016_init")[3],
calculate_Oct20th(sept20[6], sept20[6], sept20[6], "2017_init")[3],
calculate_Oct20th(sept20[7], sept20[7], sept20[7], "2018_init")[3]
), year = c(2013:2018))

climate2013_GS <- climate2012to18_GS %>% 
  filter(year == 2013)
climate2014_GS <- climate2012to18_GS %>%
  filter(year == 2014)
climate2015_GS <- climate2012to18_GS %>%
  filter(year == 2015)
climate2016_GS <- climate2012to18_GS %>%
  filter(year == 2016)
climate2017_GS <- climate2012to18_GS %>%
  filter(year == 2017)
climate2018_GS <- climate2012to18_GS %>%
  filter(year == 2018)

aboveT_2013 <- aboveThresholdTemp_v(climate2013_GS[["meanTemp"]], 10)
aboveT_2014 <- aboveThresholdTemp_v(climate2014_GS[["meanTemp"]], 10)
aboveT_2015 <- aboveThresholdTemp_v(climate2015_GS[["meanTemp"]], 10)
aboveT_2016 <- aboveThresholdTemp_v(climate2016_GS[["meanTemp"]], 10)
aboveT_2017 <- aboveThresholdTemp_v(climate2017_GS[["meanTemp"]], 10)
aboveT_2018 <- aboveThresholdTemp_v(climate2018_GS[["meanTemp"]], 10)

GDDsums <- data.frame("GDD" = c(
  sum(aboveT_2013), #should be GDD but I need to find out what the "Growing Season" is and also what the threshold temperature is 
  sum(aboveT_2014),
  sum(aboveT_2015), 
  sum(aboveT_2016), 
  sum(aboveT_2017),
  sum(aboveT_2018)), "year" = c(2013:2018))

averageGDD <- mean(GDDsums$GDD)

Sep20_yearly <- data.frame("sep20" = calculate_Sep20_v(averageGDD, GDDsums$GDD),
      "year" = c(2013:2018))

Sep20s_df <- data.frame( #shows the difference between the given October 20th and the one I calculated. The GDD from excel has no documentation of the source or calculation
  "MySep20" = c(calculate_Oct20th(Sep20_yearly$sep20[1], Sep20_yearly$sep20[1], Sep20_yearly$sep20[1], "2013_init")[3],
calculate_Oct20th(Sep20_yearly$sep20[2], Sep20_yearly$sep20[2], Sep20_yearly$sep20[2], "2013_init")[3],
calculate_Oct20th(Sep20_yearly$sep20[3], Sep20_yearly$sep20[3], Sep20_yearly$sep20[3], "2013_init")[3],
calculate_Oct20th(Sep20_yearly$sep20[4], Sep20_yearly$sep20[4], Sep20_yearly$sep20[4], "2013_init")[3],
calculate_Oct20th(Sep20_yearly$sep20[5], Sep20_yearly$sep20[5], Sep20_yearly$sep20[5], "2013_init")[3],
calculate_Oct20th(Sep20_yearly$sep20[6], Sep20_yearly$sep20[6], Sep20_yearly$sep20[6], "2013_init")[3]),
"GivenSep20" = givenSep20th$sep20, "year" = givenSep20th$year
)
