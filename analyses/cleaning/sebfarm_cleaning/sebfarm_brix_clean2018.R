########################## 2018_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse)
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2018_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: notes
notes <- ""
SebF2018 <- cbind(SebFarms_Brix, notes)

#Removing columns: time, Code, Owner, ID, region, product, malic, alpha.amino, ammonia, calculated, potassium, X
SebF2018 <- subset(SebF2018, select = -c(time, Code, Owner, ID, region, product, malic, alpha.amino, ammonia, calculated, potassium, X))

#Rename columns & changing capital letters
  #changing 'Grower' to 'company'
      colnames(SebF2018)[colnames(SebF2018) == 'Grower'] <- 'company'
  #renaming 'blockID' to 'block'
      colnames(SebF2018)[colnames(SebF2018) == 'blockID'] <- 'block'
   #changing capital letters
      colnames(SebF2018)[colnames(SebF2018) == 'Vineyard'] <- 'vineyard'

#Removing empty rows 
SebF2018 <- SebF2018[!(is.na(SebF2018$company) & is.na(SebF2018$block) & is.na(SebF2018$vineyard)), ] 
SebF2018 <- SebF2018[!(is.na(SebF2018$date)), ] 
      
#Reformating dates - Seperating date into three columns, Y/M/D
date <- SebF2018$date
date2 <- ymd(date) #lubridate
SebF2018 <- cbind(SebF2018, date2)

SebF2018 <- separate(SebF2018, date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2018$date <- NULL

#Creating Events and Value Column
SebF2018 <- pivot_longer(SebF2018, #tidyr
                     cols = c(brix, ta, ph),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : #unique columns (time, code ID, product, calculated/potassium?) ask EW
#"company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2018 <- select(SebF2018, -notes, notes)

#Isolating block ID
SebF2018$block <- gsub("[0-9]+", "", SebF2018$block) #removing vineyard number

  #removing variety (2 digit)
    for(i in 1:nrow(SebF2018)){
      if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2018[i, "block"]))){
        SebF2018$block[i] <- gsub("^.{0,2}", "", SebF2018$block[i])
      } 
    }

  #removing vareity (3 digit)
    for(i in 1:nrow(SebF2018)){
      if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{4}([^A-Z]|$)", x = SebF2018[i, "block"]))){
        SebF2018$block[i] <- gsub("^.{0,3}", "", SebF2018$block[i])
      } 
    }
  
  #removing vareity (5 digit)
    for(i in 1:nrow(SebF2018)){
      if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{5}([^A-Z]|$)", x = SebF2018[i, "block"]))){ 
        SebF2018$block[i] <- gsub("^.{0,4}", "", SebF2018$block[i])
      } 
    }

  #removing vareity (6 digit)
    for(i in 1:nrow(SebF2018)){
      if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{6}([^A-Z]|$)", x = SebF2018[i, "block"]))){ #isolating blocks with 6 letters
        SebF2018$block[i] <- gsub("^.{0,5}", "", SebF2018$block[i])
      } 
    }

#isolating vineyard numbers
SebF2018$vineyard <- gsub("SF", "", SebF2018$vineyard)

#Removing empty rows (this should be looked into)
SebF2018 <- SebF2018[!(is.na(SebF2018$year)), ]
SebF2018 <- SebF2018[!(SebF2018$company=="" & SebF2018$vineyard==""), ] #84 empty rows, likely an error in reading in the CSV
