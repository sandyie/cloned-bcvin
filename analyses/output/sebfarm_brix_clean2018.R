########################## 2018_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
rm(list=ls())
options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse) 
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2018_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: notes 
notes <- ""
SebF <- cbind(SebFarms_Brix, notes)

#Rename columns & changing capital letters
  #changing 'Grower' to 'company'
      colnames(SebF)[colnames(SebF) == 'Grower'] <- 'company'
  #renaming 'blockID' to 'block'
      colnames(SebF)[colnames(SebF) == 'blockID'] <- 'block'
   #changing capital letters
      colnames(SebF)[colnames(SebF) == 'Code'] <- 'code'
      colnames(SebF)[colnames(SebF) == 'Owner'] <- 'owner'
      colnames(SebF)[colnames(SebF) == 'Vineyard'] <- 'vineyard'

#Deleting empty column, 'X'
SebF <- SebF[, -20]

#Reformating dates - Seperating date into three columns, Y/M/D
date <- SebF$date
date2 <- ymd(date) #lubridate
SebF <- cbind(SebF, date2)

SebF <- separate(SebF, date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF <- SebF[, -1]

#Creating Events and Value Column
SebF <- pivot_longer(SebF, #tidyr
                     cols = c(brix, ta, ph, malic, alpha.amino, ammonia, calculated, potassium),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : #unique columns (time, code ID, product, calculated/potassium?) as EW
#"company", "vineyard", "sampler", block", "variety", "year", "month", "day", "event", "value", "notes"
SebF <- select(SebF, block, everything())
SebF <- select(SebF, variety, everything())
SebF <- select(SebF, vineyard, everything())
SebF <- select(SebF, company, everything())
SebF <- select(SebF, -time, time)
SebF.clean <- select(SebF, -notes, notes)
