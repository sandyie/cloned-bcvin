IF1 <- function(day, tempdiff){
  section <- calculate_section(day)
  scale1 <- 0
  rounding <- 0.001
  
  if(section == 1){
   
     if(tempdiff < magicNum1_1$value[1] - rounding){
      scale1 <- magicNum1_1$value[2]
    }else if(tempdiff < magicNum1_1$value[3] - rounding){
      scale1 <- magicNum1_1$value[4]
    }else if(tempdiff < magicNum1_1$value[5] - rounding){
      scale1 <- magicNum1_1$value[6]
    }else if(tempdiff < magicNum1_1$value[7] - rounding){
      scale1 <- magicNum1_1$value[8]
    }else if(tempdiff < magicNum1_1$value[9] - rounding){
      scale1 <- magicNum1_1$value[10]
    }else if(tempdiff < magicNum1_1$value[11] - rounding){
      scale1 <- magicNum1_1$value[12]
    }else if(tempdiff < magicNum1_1$value[13] - rounding){
      scale1 <- magicNum1_1$value[14]
    }else if(tempdiff < magicNum1_1$value[15] - rounding){
      scale1 <- magicNum1_1$value[16]
    }else{
      scale1 <- magicNum1_1$value[17]
    }
    
  }else if(section == 2){
    
    if(tempdiff < magicNum1_2$value[1] - rounding){
      scale1 <- magicNum1_2$value[2]
    }else if(tempdiff < magicNum1_2$value[3] - rounding){
      scale1 <- magicNum1_2$value[4]
    }else if(tempdiff < magicNum1_2$value[5] - rounding){
      scale1 <- magicNum1_2$value[6]
    }else if(tempdiff < magicNum1_2$value[7] - rounding){
      scale1 <- magicNum1_2$value[8]
    }else if(tempdiff < magicNum1_2$value[9] - rounding){
      scale1 <- magicNum1_2$value[10]
    }else if(tempdiff < magicNum1_2$value[11] - rounding){
      scale1 <- magicNum1_2$value[12]
    }else if(tempdiff < magicNum1_2$value[13] - rounding){
      scale1 <- magicNum1_2$value[14]
    }else if(tempdiff < magicNum1_2$value[15] - rounding){
      scale1 <- magicNum1_2$value[16]
    }else{
      scale1 <- magicNum1_2$value[17]
    }
    
  }else if(section == 3){
    
    if(tempdiff < magicNum1_3$value[1] - rounding){
      scale1 <- magicNum1_3$value[2]
    }else if(tempdiff < magicNum1_3$value[3] - rounding){
      scale1 <- magicNum1_3$value[4]
    }else if(tempdiff < magicNum1_3$value[5] - rounding){
      scale1 <- magicNum1_3$value[6]
    }else if(tempdiff < magicNum1_3$value[7] - rounding){
      scale1 <- magicNum1_3$value[8]
    }else if(tempdiff < magicNum1_3$value[9] - rounding){
      scale1 <- magicNum1_3$value[10]
    }else{
      scale1 <- magicNum1_3$value[11]
    }
    
  }else if(section == 4){
    
    if(tempdiff < magicNum1_4$value[1] - rounding){
      scale1 <- magicNum1_4$value[2]
    }else if(tempdiff < magicNum1_4$value[3] - rounding){
      scale1 <- magicNum1_4$value[4]
    }else if(tempdiff < magicNum1_4$value[5] - rounding){
      scale1 <- magicNum1_4$value[6]
    }else if(tempdiff < magicNum1_4$value[7] - rounding){
      scale1 <- magicNum1_4$value[8]
    }else if(tempdiff < magicNum1_4$value[9] - rounding){
      scale1 <- magicNum1_4$value[10]
    }else if(tempdiff < magicNum1_4$value[11] - rounding){
      scale1 <- magicNum1_4$value[12]
    }else if(tempdiff < magicNum1_4$value[13] - rounding){
      scale1 <- magicNum1_4$value[14]
    }else {
      magicNum1_4$value[15]
    }
      
  }else if(section == 5){

    if(tempdiff < magicNum1_5$value[1] - rounding){
      scale1 <- magicNum1_5$value[2]
    }else if(tempdiff < magicNum1_5$value[3] - rounding){
      scale1 <- magicNum1_5$value[4]
    }else if(tempdiff < magicNum1_5$value[5] - rounding){
      scale1 <- magicNum1_5$value[6]
    }else if(tempdiff < magicNum1_5$value[7] - rounding){
      scale1 <- magicNum1_5$value[8]
    }else if(tempdiff < magicNum1_5$value[9] - rounding){
      scale1 <- magicNum1_5$value[10]
    }else if(tempdiff < magicNum1_5$value[11] - rounding){
      scale1 <- magicNum1_5$value[12]
    }else if(tempdiff < magicNum1_5$value[13] - rounding){
      scale1 <- magicNum1_5$value[14]
    }else {
      magicNum1_5$value[15]
    }
    
  }else if(section == 6){
    
    if(tempdiff < magicNum1_6$value[1] - rounding){
      scale1 < magicNum1_6$value[2]
    }else if(tempdiff < magicNum1_6$value[3] - rounding){
      scale1 <- magicNum1_6$value[4]
    }else if(tempdiff < magicNum1_6$value[5] - rounding){
      scale1 <- magicNum1_6$value[6]
    }else if(tempdiff < magicNum1_6$value[7] - rounding){
      scale1 <- magicNum1_6$value[8]
    }else if(tempdiff < magicNum1_6$value[9] - rounding){
      scale1 <- magicNum1_6$value[10]
    }else if(tempdiff < magicNum1_6$value[11] - rounding){
      scale1 <- magicNum1_6$value[12]
    }else if(tempdiff < magicNum1_6$value[13] - rounding){
      scale1 <- magicNum1_6$value[14]
    }else {
      magicNum1_6$value[15]
    }
    
  }
  return(scale1)
}

IF1_v <- Vectorize(IF1)

IF2 <- function(day, tempdiff){
  section <- calculate_section(day)
  scale2 <- 0
  rounding <- 0.001
  
  if(section == 1){
    
    if(tempdiff > magicNum2_1$value[1]+ rounding){
      scale2 <- magicNum2_1$value[2]
    }else if(tempdiff > magicNum2_1$value[3] + rounding){
      scale2 <- magicNum2_1$value[4]
    }else if(tempdiff > magicNum2_1$value[5] + rounding){
      scale2 <- magicNum2_1$value[6]
    }else if(tempdiff > magicNum2_1$value[7] + rounding){
      scale2 <- magicNum2_1$value[8]
    }else if(tempdiff > magicNum2_1$value[9] + rounding){
      scale2 <- magicNum2_1$value[10]
    }else if(tempdiff > magicNum2_1$value[11] + rounding){
      scale2 <- magicNum2_1$value[12]
    }else if(tempdiff > magicNum2_1$value[13] + rounding){
      scale2 <- magicNum2_1$value[14]
    }else {
      magicNum2_1$value[15]
    }
    
  }else if(section == 2){
    
    if(tempdiff > magicNum2_2$value[1] + rounding){
      scale2 <- magicNum2_2$value[2]
    }else if(tempdiff > magicNum2_2$value[3] + rounding){
      scale2 <- magicNum2_2$value[4]
    }else if(tempdiff > magicNum2_2$value[5] + rounding){
      scale2 <- magicNum2_2$value[6]
    }else if(tempdiff > magicNum2_2$value[7] + rounding){
      scale2 <- magicNum2_2$value[8]
    }else if(tempdiff > magicNum2_2$value[9] + rounding){
      scale2 <- magicNum2_2$value[10]
    }else if(tempdiff > magicNum2_2$value[11] + rounding){
      scale2 <- magicNum2_2$value[12]
    }else if(tempdiff > magicNum2_2$value[13] + rounding){
      scale2 <- magicNum2_2$value[14]
    }else {
      magicNum2_2$value[15]
    }
    
  }else if(section == 3){
    
    if(tempdiff > magicNum2_3$value[1] + rounding){
      scale2 <- magicNum2_3$value[2]
    }else if(tempdiff > magicNum2_3$value[3] + rounding){
      scale2 <- magicNum2_3$value[4]
    }else if(tempdiff > magicNum2_3$value[5] + rounding){
      scale2 <- magicNum2_3$value[6]
    }else if(tempdiff > magicNum2_3$value[7] + rounding){
      scale2 <- magicNum2_3$value[8]
    }else if(tempdiff > magicNum2_3$value[9] + rounding){
      scale2 <- magicNum2_3$value[10]
    }else if(tempdiff > magicNum2_3$value[11] + rounding){
      scale2 <- magicNum2_3$value[12]
    }else{
      scale2 <- magicNum2_3$value[13]
    }
    
  }else if(section == 4){
    
    if(tempdiff > magicNum2_4$value[1] + rounding){
      scale2 <- magicNum2_4$value[2]
    }else if(tempdiff > magicNum2_4$value[3] + rounding){
      scale2 <- magicNum2_4$value[4]
    }else if(tempdiff > magicNum2_4$value[5] + rounding){
      scale2 <- magicNum2_4$value[6]
    }else if(tempdiff > magicNum2_4$value[7] + rounding){
      scale2 <- magicNum2_4$value[8]
    }else if(tempdiff > magicNum2_4$value[9] + rounding){
      scale2 <- magicNum2_4$value[10]
    }else if(tempdiff > magicNum2_4$value[11] + rounding){
      scale2 <- magicNum2_4$value[12]
    }else{
      scale2 <- magicNum2_4$value[13]
    }
    
  }else if(section == 5){
    
    if(tempdiff > magicNum2_5$value[1] + rounding){
      scale2 <- magicNum2_5$value[2]
    }else if(tempdiff > magicNum2_5$value[3] + rounding){
      scale2 <- magicNum2_5$value[4]
    }else if(tempdiff > magicNum2_5$value[5] + rounding){
      scale2 <- magicNum2_5$value[6]
    }else if(tempdiff > magicNum2_5$value[7] + rounding){
      scale2 <- magicNum2_5$value[8]
    }else if(tempdiff > magicNum2_5$value[9] + rounding){
      scale2 <- magicNum2_5$value[10]
    }else if(tempdiff > magicNum2_5$value[11] + rounding){
      scale2 <- magicNum2_5$value[12]
    }else if(tempdiff > magicNum2_5$value[13] + rounding){
      scale2 <- magicNum2_5$value[14]
    }else {
      magicNum2_5$value[15]
    }
    
  }else if(section == 6){
    
    if(tempdiff > magicNum2_6$value[1] + rounding){
      scale2 <- magicNum2_6$value[2]
    }else if(tempdiff > magicNum2_6$value[3] + rounding){
      scale2 <- magicNum2_6$value[4]
    }else if(tempdiff > magicNum2_6$value[5] + rounding){
      scale2 <- magicNum2_6$value[6]
    }else if(tempdiff > magicNum2_6$value[7] + rounding){
      scale2 <- magicNum2_6$value[8]
    }else if(tempdiff > magicNum2_6$value[9] + rounding){
      scale2 <- magicNum2_6$value[10]
    }else if(tempdiff > magicNum2_6$value[11] + rounding){
      scale2 <- magicNum2_6$value[12]
    }else if(tempdiff > magicNum2_6$value[13] + rounding){
      scale2 <- magicNum2_6$value[14]
    }else if(tempdiff > magicNum2_6$value[15] + rounding){
      scale2 <- magicNum2_6$value[16]
    }
    else {
      magicNum2_6$value[17]
    }
    
  }
  return(scale2)
}

IF2_v <- Vectorize(IF2)


IF3 <- function(daynum, scale, today_estLTEperday, yesterday_predLTE1, yesterday_predLTE2){
  section <- calculate_section(daynum)
  
  if((section == 1 | section == 2)){
    if(yesterday_predLTE2 < -24){
    coefficient <- magicNum3_1$value[2]
    }else if(yesterday_predLTE2 >= -24){
      coefficient <- 1
    }
  }
  if((section == 3)){
    if(yesterday_predLTE1 <= -24.5 & scale < 0){
    coefficient <- magicNum3_3$value[3]
    }else{
    coefficient <- 1
    }
  }
  if((section == 4 | section == 5 | section == 6)){
    if(yesterday_predLTE1 <= -24.5 & scale < 0){
    coefficient <- magicNum3_4$value[3]
    }else{
    coefficient <- 1
    }
  }
   today_predLTE1 <- (scale * coefficient * today_estLTEperday) + yesterday_predLTE1
   
    return(today_predLTE1)
  
}

IF3Dec8th <- function(daynum, yesterday_predLTE1, daybeforeyesterday_predLTE1){
  
  if((daynum == 50) & yesterday_predLTE1 < -24.5){#december 8th
    value <- -24.5 
  }else{
    value <- yesterday_predLTE1 + (yesterday_predLTE1 - daybeforeyesterday_predLTE1)
  }
  return(value)
}

IF4567 <- function(daynum, yesterday_predLTE2, estLTE, yesterday_estLTE, estLTEperday, tempdiff, today_predLTE1, yesterday_predLTE1){ #for this to work you must make a predLTE vector of some sort (can be a dataframe) that has the initial value for day 1 calculated from before Oct 20th. I don't have a function to do this yet because chronologically it didn't make sense to create this yet but manually enter the first value
  section <- calculate_section(daynum)

  if(section < 6){ 
    for(column in c(4:7)){
      assign(
        x = "df",
        value = get(paste0("magicNum", column, "_", section))
      )
    if(column == 4){ #conditional pattern == alternate <<
      
      if((yesterday_predLTE2 < (estLTE + df[[1]][1])) & (tempdiff < df[[1]][2])){
          result <- estLTEperday * df[[1]][3]
          break
        }else if((yesterday_predLTE2 < (estLTE + df[[1]][4])) & (tempdiff < df[[1]][5])){
          result <- estLTEperday * df[[1]][6]
          break
        }else if((yesterday_predLTE2 < (estLTE + df[[1]][7])) & (tempdiff < df[[1]][8])){          
          result <- estLTEperday * df[[1]][9]
          break
        }
    }
    if(column == 5){ #conditional pattern == alternate <>
        if((yesterday_predLTE2 < (estLTE + df[[1]][1])) & (tempdiff > df[[1]][2])){
              result <- estLTEperday * df[[1]][3]
          break
        }else if((yesterday_predLTE2 < (estLTE + df[[1]][4])) & (tempdiff > df[[1]][5])){
          result <- estLTEperday * df[[1]][6]
          break
        }else if((yesterday_predLTE2 < (estLTE + df[[1]][7])) & (tempdiff > df[[1]][8])){
          result <- estLTEperday * df[[1]][9]
          break
        }
      }
        
    if(column == 6){ #conditional pattern == alternate >>
        if((yesterday_predLTE2 > (estLTE + df[[1]][1])) & (tempdiff > df[[1]][2])){
          result <- estLTEperday * df[[1]][3]
          break
        }else if((yesterday_predLTE2 > (estLTE + df[[1]][4])) & (tempdiff > df[[1]][5])){
          result <- estLTEperday * df[[1]][6]
          break
        }else if((yesterday_predLTE2 > (estLTE + df[[1]][7])) & (tempdiff > df[[1]][8])){
          result <- estLTEperday * df[[1]][9]
          break
        }
      }
  
    if(column == 7){ #conditional pattern == alternate ><
      if((yesterday_predLTE2 > (estLTE + df[[1]][1])) & (tempdiff < df[[1]][2])){
        result <- estLTEperday * df[[1]][3]
        break
      }else if((yesterday_predLTE2 > (estLTE + df[[1]][4])) & (tempdiff < df[[1]][5])){
        result <- estLTEperday * df[[1]][6]
        break
      }else if((yesterday_predLTE2 > (estLTE + df[[1]][7])) & (tempdiff < df[[1]][8])){
        result <- estLTEperday * df[[1]][9]
        break
      }else{
        result <- 0 
        break
      }
      
    }
  }
    today_predLTE2 <- yesterday_predLTE2 + (today_predLTE1 - yesterday_predLTE1) + result
  }else if(section == 6){
    today_predLTE2 <- yesterday_predLTE2 + (today_predLTE1 - yesterday_predLTE1)
  }
  
  
  return(today_predLTE2)
  
}

IF8 <- function(daynum, today_tempdiff, today_predLTE2, yesterday_predLTE2, yesterday_LTEfinal){
  section <- calculate_section(daynum)
  assign(
    x = "df",
    value = get(paste0("magicNum", 8, "_", section))
  )
  dailySum <- today_predLTE2 - yesterday_predLTE2
  if(daynum == 176){
    if(yesterday_predLTE2 < -23 & today_tempdiff < 0){
      returnValue <- (dailySum ) * 0.6 #no magic numbers dataframe because I didn't see this conditional when selecting from the excel sheet
    }else{
      returnValue <- (dailySum)
    }
  }else if(yesterday_LTEfinal < df[[1]][1] & today_tempdiff < df[[1]][2]){
    returnValue <- (dailySum) * df[[1]][3]
  }else{
    returnValue <- (dailySum)
  }
  
  return(returnValue)
}

IF9 <- function(daynum, today_tempdiff, today_predLTE2, yesterday_predLTE2, yesterday_LTEfinal, today_if8){
  section <- calculate_section(daynum) #section > 1
  assign(
    x = "df",
    value = get(paste0("magicNum", 9, "_", section))
  )
  dailySum <- today_predLTE2 - yesterday_predLTE2
  
  if(daynum == 176){
    if(yesterday_predLTE2 < -24 & today_tempdiff < 0){
      returnValue <- dailySum * 0.4 #no magic numbers dataframe because I didn't see this conditional when selecting from the excel sheet
    }else{
      returnValue <- dailySum
    }
  }else if(section == 2){
      if(yesterday_LTEfinal < df[[1]][1]){
      returnValue <- dailySum * df[[1]][2]
      }else if(yesterday_LTEfinal < df[[1]][3]){
      returnValue <- dailySum * df[[1]][[4]]
      }else{
      returnValue <- today_if8
      }
  
    }else{
    if(yesterday_LTEfinal < df[[1]][1] & today_tempdiff < df[[1]][2]){
      returnValue <- dailySum * df[[1]][3]
    }else{
      returnValue <- today_if8
    }
      
  }
  
  return(returnValue)
  
}

IF10 <- function(daynum, today_tempdiff, today_predLTE2, yesterday_predLTE2, yesterday_LTEfinal, today_estLTE, today_if9){
  section <- calculate_section(daynum) #section != 1, 3
  assign(
    x = "df",
    value = get(paste0("magicNum", 10, "_", section))
  )
  dailySum <- today_predLTE2 - yesterday_predLTE2
  
  if(section == 2){
    if((yesterday_LTEfinal > (today_estLTE + df[[1]][1])) & today_tempdiff > df[[1]][2]){
      returnValue <- today_if9 * df[[1]][3]
    }else{
      returnValue <- today_if9 * df[[1]][4]
    }
  }else{
    if(yesterday_LTEfinal < df[[1]][1] & today_tempdiff > df[[1]][2]){
      returnValue <- dailySum * df[[1]][3]
    }else{
      returnValue <- today_if9
    }
  }
  
  return(returnValue)
}

IF11 <- function(daynum, today_tempdiff, yesterday_LTEfinal, today_estLTE, today_if10){
  section <- calculate_section(daynum) #section == 6
  assign(
    x = "df",
    value = get(paste0("magicNum", 11, "_", section))
  )
  
  if((today_estLTE + df[[1]][1]) > yesterday_LTEfinal & today_tempdiff > df[[1]][2]){
    returnValue <- today_if10 * df[[1]][3]
  }else{
    returnValue <- today_if10 
  }
  
  return(returnValue)
}

IF12 <- function(daynum, today_tempdiff, yesterday_LTEfinal, today_estLTE, today_if11){
  section <- calculate_section(daynum) #section == 6
  assign(
    x = "df",
    value = get(paste0("magicNum", 12, "_", section))
  )
  
  if(today_estLTE > df[[1]][1] & yesterday_LTEfinal < df[[1]][2] & today_tempdiff > df[[1]][3]){
    returnValue <- today_if11 * df[[1]][4]
  }else{
    returnValue <- today_if11
  }
  
  return(returnValue)
}
