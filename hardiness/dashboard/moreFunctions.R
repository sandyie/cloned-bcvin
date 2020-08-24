library(httr)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(rvest)

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
  if(year == CurrentYear){
   endMonth <- CurrentMonth 
  }
  
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

combine_years_temps <- function(startYear, endYear, currentMonth){
  index <- 1
  for(i in startYear:endYear){
    if(i == CurrentYear){
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
  years <- startYear:endYear
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
    value = temp1,
    envir = .GlobalEnv
  )
}



add_date <- function(day, year){
  as.Date(day, paste0(year,"-01-01"))
}

#need to have a function where the current year that is read from the system is entered and it pretty much calls everything present in mainDashboard.R
#this would be called daily from the linux server and would have up to date climate data from enviro canada
  

