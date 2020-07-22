getMonthNum <- function(dateString){
  names <- c("Jan", "Feb", "March", "April", "May", "June", "July", "August", "Sep", "Oct", "Nov", "Dec")
  string <- gsub("[0-9]+\\-", '', dateString)
  monthNum <- grep(string, names)
  return(monthNum[[1]])
}

getDay <- function(dateString){
  string <- gsub("\\-[A-Z][a-z]+", '', dateString)
  return(string)
}

calculate_day <- function(day, month){#day 1 is October 20th, day 176 is April 12th, NOTE: the model doesn't change for a leap/notleap year. Assume that each year has Feb 29th.
  daysperseasonmonth <- c(12, 30, 31, 31, 29, 31, 12)
  season_months <- c(10:12, 1:4)
  df <- tibble(season_months, daysperseasonmonth) 
  output <- 0
  sum <- 0
  
  if(month <= 12 & month >= 10){
    monthHits <- c(10:month)
  }
  else {
    monthHits <- c(10:12, 1:month)
  }
  if(month != 10){
    for(i in 1:length(monthHits)){
      if(monthHits[i] != month){
        sum <- sum + df$daysperseasonmonth[season_months == monthHits[i]]
      }
      else{
        output <- sum + day
      }
    }
  }
  else{
    output <- day - 19 #rescales the output back to normal
  }
  return(output)
}

calculate_day_v <- Vectorize(calculate_day)


calculate_section <- function(day){ #uses day to calculate which section the day is in. This is very helpful in determining which magicNum to use
  if(day == 1){
      section <- 1
  }else if(day > 1 & day <= calculate_day(7, 12)){
    section <- 2
  }else if(day > calculate_day(7,12) & day <= calculate_day(6, 1)){
    section <- 3
  }else if(day > calculate_day(6,1) & day <= calculate_day(6, 2)){
    section <- 4
  }else if(day > calculate_day(6,2) & day < calculate_day(1, 3)){
    section <- 5
  }else if(day >= calculate_day(1, 3) & day <= calculate_day(12, 4)){
    section <- 6
  }else{
    stop("day is out of the range between October 20th and April 12th. You will probably have an easier time making a function that will predict the initial values of each predicted LTE than trying to update the existing code.")
  }
  
  return(section)
}

estimateLTEperday <- function(estLTEcol){ ##MUST ADJUST THE temp[1] <- -0.44 to other years once this works
  temp <- tibble(estLTEperday = c(1:length(estLTEcol)))
  temp[1] <- -0.44 #value taken from excel sheet...
  for(i in 2:length(estLTEcol)){
    temp$estLTEperday[i] <- estLTEcol[i] - estLTEcol[i-1]
    if(((i < calculate_day(8, 12)) & (temp$estLTEperday[i] > -0.1 & temp$estLTEperday[i] < 0.1))){
      
      temp$estLTEperday[i] <- -0.1
      
    }else if(((i >= calculate_day(8, 12)) & (temp$estLTEperday[i] > -0.1 & temp$estLTEperday[i] < 0.1))){
      
      temp$estLTEperday[i] <- 0.1
    }
  }
  LTEperday <<- temp
}

estimateLTE <- function(day){ #this just replicates the polynomial Carl had created in Excel. Probably not worth the time to recreate it in R
  x4 <- 12.296 * (day / 100) * (day / 100) * (day / 100) * (day / 100)
  x3 <- 42.657 * (day / 100) * (day / 100) * (day / 100)
  x2 <- 63.019 * (day / 100) * (day / 100) 
  x1 <- 43.525 * (day / 100)
  c <- 12.802
  
  LTE <- x4 - x3 + x2 - x1 - c
  
  return(LTE)
}

aboveThresholdTemp <- function(meanTemperature, threshold){
  if(is.na(meanTemperature)){
    value <- 0
  }else if(meanTemperature > threshold){
    value <- meanTemperature - threshold
  }else{
    value <- 0
  }
  return(value)
}

aboveThresholdTemp_v <- Vectorize(aboveThresholdTemp)

calculate_Sep20 <- function(averageGDD, yearlyGDD){
  
  if(yearlyGDD < averageGDD){
    sept20 <- (yearlyGDD - averageGDD) * -0.005
  }else if(yearlyGDD > averageGDD){
    sept20 <- (yearlyGDD - averageGDD) * -0.02
  }else{
    sept20 <- (yearlyGDD - average)
  }
  
  return(sept20)
}

calculate_Sep20_v <- Vectorize(calculate_Sep20)

#                               October 20th      October 20th        October 20th 
finalLTEpredictions <- function(initialPredLTE1, initialPredLTE2, initialPredLTEfinal, yearRange){#yearRange == "2012to13" "2013to14" etc up to "2018to19"

  
  predLTE1 <- tibble(predLTE1 = c(initialPredLTE1, rep(0, 175)))
  assign(
    x = paste0("predLTE1_", yearRange),
    value = tibble(predLTE1 = c(initialPredLTE1, rep(0, 175))),
    envir = environment() #global environment for ease of checking. I guess it doesn't necessarily need to be outside of the current scope. Just nice to reference
  )
  
  assign(
    x = paste0("predLTE2_", yearRange),
    value = tibble(predLTE2 = c(initialPredLTE2, rep(0, 175))),
    envir = environment()
  ) 
  
  assign(
    x = paste0("predLTEfinal_", yearRange),
    value = tibble(predLTEfinal = c(initialPredLTEfinal, rep(0, 175))),
    envir = environment()
  ) 
  
  assign(
    x = paste0("climatedf_", yearRange),
    value = get(paste0("climate", yearRange)),
    envir = environment()
  )
  
  temp1 <- get(paste0("predLTE1_", yearRange))
  temp2 <- get(paste0("predLTE2_", yearRange))
  temp3 <- get(paste0("predLTEfinal_", yearRange))
  temp4 <- get(paste0("climatedf_", yearRange))
  
  
  tempscale <- IF1_v(temp4$daynum, temp4$tdiff) + IF2_v(temp4$daynum, temp4$tdiff) %>%
    as_tibble(tempscale = value) 
  
  if(yearRange != "2015to16" | yearRange != "2019to20" | yearRange != "2023to2024" | yearRange != "2027to2028" | yearRange != "2031to2032"){
    tempscale$value[133] <- 0 #accounts for non leap years
  }
  
  for(index in 1:176){ #expect for the predLTE1 and predLTE2 to vary by +/- 0.1 C because of significant figures disparity. Data in csv is to 1 decimal point while excel sheet has data that is 2 decimal places. Check October 26th 2012 to 13 for a disparity in "scale"
    #IF3 <- IF3 <- function(daynum, tempscale, today_estLTEperday, yesterday_predLTE1, yesterday_predLTE2)
    #IF3Dec7th <- function(daynum, today_estLTEperday, yesterday_predLTE1, daybeforeyesterday_predLTE1)
    #IF4567(daynum, yesterday_predLTE2, estLTE, yesterday_estLTE, estLTEperday, tempdiff, today_predLTE1, yesterday_predLTE1)
    
    if(index == 1){
      next
    }else if(index == 50){
      temp1$predLTE1[index] <- IF3Dec8th(index, temp1$predLTE1[index - 1], temp1$predLTE1[index - 2])
      temp2$predLTE2[index] <- IF4567(index, temp2$predLTE2[index - 1], estimatedLTE$estLTE[index], estimatedLTE$estLTE[index - 1], estimatedLTE$estLTEperday[index], temp4$tdiff[index], temp1$predLTE1[index], temp1$predLTE1[index - 1])
    }else{
      temp1$predLTE1[index] <- IF3(index, tempscale$value[index], estimatedLTE$estLTEperday[index], temp1$predLTE1[index - 1], temp2$predLTE2[index - 1])
      temp2$predLTE2[index] <- IF4567(index, temp2$predLTE2[index - 1], estimatedLTE$estLTE[index], estimatedLTE$estLTE[index - 1], estimatedLTE$estLTEperday[index], temp4$tdiff[index], temp1$predLTE1[index], temp1$predLTE1[index - 1])
      }
    
    }
  
  for(index in 1:176){
    section <- calculate_section(index)
    
    if(index == 1){
      next
    }else if(section == 1){
      today_if8 <- IF8(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1])
      temp3$predLTEfinal[index] <- temp3$predLTEfinal[index - 1] + today_if8
    }else if(section == 2 | section == 4 | section == 5){
      today_if8 <- IF8(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1])
      today_if9 <- IF9(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1], today_if8)
      today_if10 <- IF10(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1], estimatedLTE$estLTE[index], today_if9)
      temp3$predLTEfinal[index] <- temp3$predLTEfinal[index - 1] + today_if10
    }else if(section == 3 | index == 176){
      today_if8 <- IF8(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1])
      today_if9 <- IF9(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1], today_if8)
      temp3$predLTEfinal[index] <- temp3$predLTEfinal[index - 1] + today_if9
    }else if(section == 6 & index != 176){
      today_if8 <- IF8(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1])
      today_if9 <- IF9(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1], today_if8)
      today_if10 <- IF10(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1], estimatedLTE$estLTE[index], today_if9)
      today_if11 <- IF11(index, temp4$tdiff[index], temp3$predLTEfinal[index - 1], estimatedLTE$estLTE[index], today_if10)
      today_if12 <- IF12(index, temp4$tdiff[index], temp3$predLTEfinal[index - 1], estimatedLTE$estLTE[index], today_if11)
      temp3$predLTEfinal[index] <- temp3$predLTEfinal[index - 1] + today_if12
      }
    
    }
    
    assign(
      x = paste0("predLTE1_", yearRange),
      value = temp1,
      envir = environment()
      )
    assign(
      x = paste0("predLTE2_", yearRange),
      value = temp2,
      envir = environment()
    )
    assign(
      x = paste0("predLTEfinal_", yearRange),
      value = temp3,
      envir = environment()
    )
    assign(
      x = paste0("climatedf_", yearRange),
      value = temp4,
      envir = environment()
    )
    assign(
      x = paste0("scale_", yearRange),
      value = tempscale,
      envir = environment()
    )
    
#    if(yearRange == "2015to16" | yearRange == "2019to20" | yearRange == "2023to24" | yearRange == "2027to28" | yearRange == "2031to2032"){ #leap years
        dates <- seq(as.Date(paste0(str_extract(yearRange, "20[0-9][0-9]"), "/10/20")), as.Date(paste0("20", gsub("20[0-9][0-9]to", "", yearRange), "/4/12")), "days")
#    }else{
#        dates <- c(as.character(seq(as.Date(paste0(str_extract(testRange, "20[0-9][0-9]"), "/10/20")), as.Date(paste0("20", gsub("20[0-9][0-9]to", "", testRange), "/2/28")), "days")),
#                   paste0("20", gsub("20[0-9][0-9]to", "", testRange), "/02/29"),
#                   as.character(seq(as.Date(paste0("20", gsub("20[0-9][0-9]to", "", testRange), "/3/1")), as.Date(paste0("20", gsub("20[0-9][0-9]to", "", testRange), "/4/12")), "days"))
#        )
#    }
  if(yearRange == "2015to16" | yearRange == "2019to20" | yearRange == "2023to24" | yearRange == "2027to28" | yearRange == "2031to2032"){ #leap years
    temp5 <- tibble(
      "predLTE1" = temp1$predLTE1,
      "predLTE2" = temp2$predLTE2,
      "predLTE3" = temp3$predLTEfinal,
      "daynum" = temp4$daynum,
      "twoDayAvg" = temp4$twoDayAvg,
      "tdiff" = temp4$tdiff,
      "scale" = tempscale$value,
      "date" = dates
    )
  }else{
    temp5 <- tibble(
      "predLTE1" = temp1$predLTE1,
      "predLTE2" = temp2$predLTE2,
      "predLTE3" = temp3$predLTEfinal,
      "daynum" = temp4$daynum,
      "twoDayAvg" = temp4$twoDayAvg,
      "tdiff" = temp4$tdiff,
      "scale" = tempscale$value
    ) %>%
      filter(daynum != 133) %>%
      mutate(date = dates)
  }
    
      assign(
      x = paste0("predLTE_combined_", yearRange),
      value = temp5,
      envir = .GlobalEnv
      )
    
      
}

#                                    October 20th      October 20th        October 20th 
finalLTEpredictions_test <- function(initialPredLTE1, initialPredLTE2, initialPredLTEfinal, yearRange){ #this function is identical apart from the naming scheme for the assigned variables. Main use is to have a convenient place to look at the model for the September 20th Values that Carl calculated from his GDD
  
  
  predLTE1 <- tibble(predLTE1 = c(initialPredLTE1, rep(0, 175)))
  assign(
    x = paste0("predLTE1_test_", yearRange),
    value = tibble(predLTE1 = c(initialPredLTE1, rep(0, 175))),
    envir = environment() #global environment for ease of checking. I guess it doesn't necessarily need to be outside of the current scope. Just nice to reference
  )
  
  assign(
    x = paste0("predLTE2_test_", yearRange),
    value = tibble(predLTE2 = c(initialPredLTE2, rep(0, 175))),
    envir = environment()
  ) 
  
  assign(
    x = paste0("predLTEfinal_test_", yearRange),
    value = tibble(predLTEfinal = c(initialPredLTEfinal, rep(0, 175))),
    envir = environment()
  ) 
  
  assign(
    x = paste0("climatedf_test_", yearRange),
    value = get(paste0("climate", yearRange)),
    envir = environment()
  )
  
  temp1 <- get(paste0("predLTE1_test_", yearRange))
  temp2 <- get(paste0("predLTE2_test_", yearRange))
  temp3 <- get(paste0("predLTEfinal_test_", yearRange))
  temp4 <- get(paste0("climatedf_test_", yearRange))
  
  
  tempscale <- IF1_v(temp4$daynum, temp4$tdiff) + IF2_v(temp4$daynum, temp4$tdiff) %>%
    as_tibble(tempscale = value) 
  
  if(yearRange != "2015to16"){
    tempscale$value[133] <- 0 #accounts for non leap years
  }
  
  for(index in 1:176){ #expect for the predLTE1 and predLTE2 to vary by +/- 0.1 C because of significant figures disparity. Data in csv is to 1 decimal point while excel sheet has data that is 2 decimal places. Check October 26th 2012 to 13 for a disparity in "scale"
    #IF3 <- IF3 <- function(daynum, tempscale, today_estLTEperday, yesterday_predLTE1, yesterday_predLTE2)
    #IF3Dec7th <- function(daynum, today_estLTEperday, yesterday_predLTE1, daybeforeyesterday_predLTE1)
    #IF4567(daynum, yesterday_predLTE2, estLTE, yesterday_estLTE, estLTEperday, tempdiff, today_predLTE1, yesterday_predLTE1)
    
    if(index == 1){
      next
    }else if(index == 50){
      temp1$predLTE1[index] <- IF3Dec8th(index, temp1$predLTE1[index - 1], temp1$predLTE1[index - 2])
      temp2$predLTE2[index] <- IF4567(index, temp2$predLTE2[index - 1], estimatedLTE$estLTE[index], estimatedLTE$estLTE[index - 1], estimatedLTE$estLTEperday[index], temp4$tdiff[index], temp1$predLTE1[index], temp1$predLTE1[index - 1])
    }else{
      temp1$predLTE1[index] <- IF3(index, tempscale$value[index], estimatedLTE$estLTEperday[index], temp1$predLTE1[index - 1], temp2$predLTE2[index - 1])
      temp2$predLTE2[index] <- IF4567(index, temp2$predLTE2[index - 1], estimatedLTE$estLTE[index], estimatedLTE$estLTE[index - 1], estimatedLTE$estLTEperday[index], temp4$tdiff[index], temp1$predLTE1[index], temp1$predLTE1[index - 1])
    }
    
  }
  
  for(index in 1:176){
    section <- calculate_section(index)
    
    if(index == 1){
      next
    }else if(section == 1){
      today_if8 <- IF8(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1])
      temp3$predLTEfinal[index] <- temp3$predLTEfinal[index - 1] + today_if8
    }else if(section == 2 | section == 4 | section == 5){
      today_if8 <- IF8(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1])
      today_if9 <- IF9(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1], today_if8)
      today_if10 <- IF10(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1], estimatedLTE$estLTE[index], today_if9)
      temp3$predLTEfinal[index] <- temp3$predLTEfinal[index - 1] + today_if10
    }else if(section == 3 | index == 176){
      today_if8 <- IF8(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1])
      today_if9 <- IF9(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1], today_if8)
      temp3$predLTEfinal[index] <- temp3$predLTEfinal[index - 1] + today_if9
    }else if(section == 6 & index != 176){
      today_if8 <- IF8(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1])
      today_if9 <- IF9(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1], today_if8)
      today_if10 <- IF10(index, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1], estimatedLTE$estLTE[index], today_if9)
      today_if11 <- IF11(index, temp4$tdiff[index], temp3$predLTEfinal[index - 1], estimatedLTE$estLTE[index], today_if10)
      today_if12 <- IF12(index, temp4$tdiff[index], temp3$predLTEfinal[index - 1], estimatedLTE$estLTE[index], today_if11)
      temp3$predLTEfinal[index] <- temp3$predLTEfinal[index - 1] + today_if12
    }
    
  }
  
  assign(
    x = paste0("predLTE1_test_", yearRange),
    value = temp1,
    envir = environment()
  )
  assign(
    x = paste0("predLTE2_test_", yearRange),
    value = temp2,
    envir = environment()
  )
  assign(
    x = paste0("predLTEfinal_test_", yearRange),
    value = temp3,
    envir = environment()
  )
  assign(
    x = paste0("climatedf_test_", yearRange),
    value = temp4,
    envir = environment()
  )
  assign(
    x = paste0("scale_test_", yearRange),
    value = tempscale,
    envir = environment()
  )
  assign(
    x = paste0("predLTE_test_combined_", yearRange),
    value = tibble(
      "predLTE1" = temp1$predLTE1,
      "predLTE2" = temp2$predLTE2,
      "predLTE3" = temp3$predLTEfinal,
      "daynum" = temp4$daynum,
      "twoDayAvg" = temp4$twoDayAvg,
      "tdiff" = temp4$tdiff,
      "scale" = tempscale
    ),
    envir = .GlobalEnv
  )
}

#                              September 20th   September 20th   September 20th 
calculate_Oct20th <- function(initialPredLTE1, initialPredLTE2, initialPredLTEfinal, yearRange){#yearRange == "2012to13_GS" "2013to14_GS" etc up to "2018to19_GS"
#Calculating October 20th is only necessary as I stuck to Carl's regime for counting days. 1 being (October 20th) and 176 (April 12th) being the final day. 
#the structure of this function is a bit redundant. The predLTE names originally were written into the global environment but I realized that this is futile and a bit confusing.
#they were now moved to be written to the local environment for the function.
#
  
  predLTE1 <- tibble(predLTE1 = c(initialPredLTE1, rep(0, 30)))
  
  assign(
    x = paste0("predLTE1_", yearRange),
    value = tibble(predLTE1 = c(initialPredLTE1, rep(0, 30))),
    envir = environment() #global environment for ease of checking. I guess it doesn't necessarily need to be outside of the current scope. Just nice to reference
  )
  
  assign(
    x = paste0("predLTE2_", yearRange),
    value = tibble(predLTE2 = c(initialPredLTE2, rep(0, 30))),
    envir = environment()
  ) 
  
  assign(
    x = paste0("predLTEfinal_", yearRange),
    value = tibble(predLTEfinal = c(initialPredLTEfinal, rep(0, 30))),
    envir = environment()
  ) 
  
  assign(
    x = paste0("climatedf_", yearRange),
    value = get(paste0("climate", yearRange)),
    envir = environment()
  )
  
  temp1 <- get(paste0("predLTE1_", yearRange), envir = environment())
  temp2 <- get(paste0("predLTE2_", yearRange), envir = environment())
  temp3 <- get(paste0("predLTEfinal_", yearRange), envir = environment())
  temp4 <- get(paste0("climatedf_", yearRange), envir = environment())
  
  tempscale <- IF1_v(1, temp4$tdiff) + IF2_v(1, temp4$tdiff) %>%
    as_tibble(tempscale = value) 
  
  
  for(index in 2:31){ 
      temp1$predLTE1[index] <- IF3(1, tempscale$value[index], estimatedLTE1$estLTEperday[index], temp1$predLTE1[index - 1], temp2$predLTE2[index - 1])
      temp2$predLTE2[index] <- IF4567(1, temp2$predLTE2[index - 1], estimatedLTE1$estLTE[index], estimatedLTE1$estLTE[index - 1], estimatedLTE1$estLTEperday[index], temp4$tdiff[index], temp1$predLTE1[index], temp1$predLTE1[index - 1])
    }
  
  for(index in 2:31){ #####################
    section <- 1
    if(section == 1){
      today_if8 <- IF8(1, temp4$tdiff[index], temp2$predLTE2[index], temp2$predLTE2[index - 1], temp3$predLTEfinal[index - 1])
      temp3$predLTEfinal[index] <- temp3$predLTEfinal[index - 1] + today_if8
        }
    
  }
  #assign( #commented out because I think they are unnecessary. Still working on some things that I need to keep my environment for. May need to uncomment if the functions don't work
  #  x = paste0("predLTE1_", yearRange),
  #  value = temp1,
  #  envir = .GlobalEnv #global environment for ease of checking. I guess it doesn't necessarily need to be outside of the current scope. Just nice to reference
  #)
  
  #assign(
  #  x = paste0("predLTE2_", yearRange),
  #  value = temp2,
  #  envir = .GlobalEnv 
  #) 
  
  #assign(
  #  x = paste0("predLTEfinal_", yearRange),
  #  value = temp3,
  #  envir = .GlobalEnv 
  #) 
  
  #assign(
  #  x = paste0("climatedf_", yearRange),
  #  value = temp4,
  #  envir = .GlobalEnv
  #)
  
  return(c(temp1$predLTE1[31], temp2$predLTE2[31], temp3$predLTEfinal[31]))
}

copyActualLTE <- function(yearRange, year1, year2){#have to change what you've copied each time you do this
  dateRange <- seq(as.Date(paste0(2015, "-10-20")), as.Date(paste0(2016, "-4-12")), by = "days")
  
  clipboard <- readClipboard() #make sure to copy from October 20th to April 12th on each measured LTE column and copy 
  grepHits <- grep("[0-9]+", clipboard)
  measuredDates <- dateRange[grepHits]
  

    for(i in 1:length(measuredDates)){
      if(str_detect(measuredDates[i], "2015")){
      measuredDates[i] <- gsub("2015", year1, measuredDates[i])
      }else{
        measuredDates[i] <- gsub("2016", year2, measuredDates[i])
      }
    }
  
  measuredLTE <- readClipboard()[grepHits]
  
  df <- data.frame(date = measuredDates, LTE = parse_number(measuredLTE))
  
  assign(
    x = paste0("measuredLTE_", yearRange),
    value = df,
    envir = .GlobalEnv
  )
  
}
