#web API test for weather station data
#my CIMIS API key: 0032799d-060b-4859-b1d1-73dffc65f9e1
#template(ish): http://et.water.ca.gov/api/data?appKey=YOUR-APP-KEY&targets=2,8,127&startDate=2010-01-01&endDate=2010-01-05
library(httr)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(rvest)

#this works - kind of. I needed to run the GET function with the RETRY wrapper multiple times because the API is finicky 
#may be nice for wine quality. I guess we don't need refreshing data for wine quality?
url <- "https://et.water.ca.gov/api/data?appKey=0032799d-060b-4859-b1d1-73dffc65f9e1&targets=77&startDate=2010-01-01&endDate=2012-01-01&unitOfMeasure=M&dataItems=day-air-tmp-avg,day-air-tmp-min,day-air-tmp-max"

napa <- RETRY("GET", url)
napa_data <- rawToChar(napa$content)
napa_weird <- fromJSON(napa_data)
napa_less_weird <- as.data.frame(napa_weird$Data$Providers$Records)
napa_df <- data.frame(date = napa_less_weird$Date, station = napa_less_weird$Station, avgTemp = napa_less_weird$DayAirTmpAvg$Value, minTemp = napa_less_weird$DayAirTmpMin$Value, maxTemp = napa_less_weird$DayAirTmpMax$Value)

str(napa_less_weird)


#Gov Canada
#thinking about scraping data from Weather Canada for Penticton Weather station to get up to date climate information
#if I get a script running to update new info daily, the "Dashboard" for hardiness could be updated daily on our linux server which would host the shiny app
#https://towardsdatascience.com/how-to-build-r-shiny-apps-that-update-themselves-6ce2d9606e90

weather <- "https://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=2012-05-07|2020-07-15&dlyRange=2012-05-10|2020-07-15&mlyRange=|&StationID=50269&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2020&selRowPerPage=50&Line=1&searchMethod=contains&Month=7&Day=15&timeframe=2&Year=2020"
url_start <- "https://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=2012-01-01|2020-07-15&dlyRange=2012-05-10|2020-07-15&mlyRange=|&StationID=50269&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2020&selRowPerPage=50&Line=1&searchMethod=contains&Month="
url_end <- "&Day=15&timeframe=2&Year="

CurrentMonth <- str_extract(Sys.Date(), "-[0-9]+-") %>% #if you want to change this to a user input for a Shiny server etc. just make a reactive option instead of Sys.Time()
  str_remove_all(., "-") %>%
  as.numeric()
CurrentYear <- str_extract(Sys.Date(), "[0-9][0-9][0-9][0-9]") %>%
  as.numeric()

scrape_mean_penticton_temp <- function(curr_month, input_year){
  if(input_year < CurrentYear){#curr_month limits how far the data should scrape
    curr_month <- 12 
  }
  
  for(i in 1:curr_month){
    month <- i
    webpage <- read_html(paste0(url_start, month, url_end, input_year))
    nodes <- html_nodes(webpage, xpath = "//table//td")
    vals <- xml_double(nodes) 
    
    numTableCols <- 11
    summaryRows <- 3
    days <- floor((length(vals) / numTableCols) - 3) #how many days are in the table
    num <- days * 11
    tempVals <- c() 
    index <- 1
    
    for(col in seq(3, num, 11)){ #selects only mean temperature
      tempVals[index] <- vals[col]
      index <- index + 1
    }
    assign(
      x = paste0("meanTemp_", month, "_", input_year),
      value = tempVals,
      envir = .GlobalEnv
    )
    assign(
      x = "D", 
      value = days, 
      envir = .GlobalEnv
    )
  }
  
}

combine_monthly_temps <- function(endMonth, year){
  names <- c()
  for(index in 1:endMonth){
    assign(
      x = paste0(LETTERS[index]),
      value = get(paste0("meanTemp_", index, "_", year)),
      envir = environment()
    )
  }
  
  placeholder <- unlist(lapply(LETTERS[1:endMonth], get, envir = environment()))
  assign(
    x = paste0("meanTemps_upto_", endMonth, "_", year),
    value = placeholder,
    envir = .GlobalEnv
  )
}

combine_years_temps <- function(startYear, currentYear, currentMonth){
  index <- 1
  for(i in startYear:currentYear){
    if(i == currentYear){
      assign(
        x = paste(LETTERS[index]),
        value = as.data.frame(get(paste0("meanTemps_upto_", currentMonth, "_", i), envir = .GlobalEnv)) 
      )
    }else{
    assign(
      x = paste(LETTERS[index]),
      value = as.data.frame(get(paste0("meanTemps_upto_12_", i), envir = .GlobalEnv)) 
      )
    }
    temp <- get(LETTERS[index], envir = environment())
    colnames(temp)[1] <- "avgTemp"
    temp1 <- temp %>%
      mutate(year = i, day = 1:nrow(temp))
    
    assign(
      x = letters[index],
      value = temp1
    )
    
    index <- index + 1
    
  }
  years <- startYear:currentYear
  numYears <- length(years)
  
  placeholder <- as.data.frame(sapply(letters[1:numYears], get, envir = environment())) 
  for(j in 2:numYears){
    if(j == 2){
    temp1 <- bind_rows(placeholder[[j]], placeholder[[j-1]])
    }else if(j > 2){
    temp2 <- bind_rows(temp1, placeholder[[j]])
    temp1 <- temp2
    }
  }
  
  assign(
    x = paste0("meanTempsToDate"),
    value = temp2,
    envir = .GlobalEnv
  )
}

year1 <- 2012
year2 <- 2019

for(year in year1:year2){
  scrape_mean_penticton_temp(12, year)
  combine_monthly_temps(12, year)
}
scrape_mean_penticton_temp(CurrentMonth, 2020)
combine_monthly_temps(CurrentMonth, 2020)

combine_years_temps(2013, 2020, CurrentMonth)
meanTempsToDate

#run this function daily (should save all of the historical data so we don't need to scrape that every day )
#scrape_mean_penticton_temp(CurrentMonth, 2020) 
#combine_monthly_temps(CurrentMonth, 2020)
#either run the entire combine_years_temps again or somehow append only the new day onto the current up to date list 


