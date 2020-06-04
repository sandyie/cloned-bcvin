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

#Removed columns: time, Code, Owner, ID, region, product, malic, alpha.amino, ammonia, calculated, potassium, X
SebF <- subset(SebF, select = -c(time, Code, Owner, ID, region, product, malic, alpha.amino, ammonia, calculated, potassium, X))

#Rename columns & changing capital letters
  #changing 'Grower' to 'company'
      colnames(SebF)[colnames(SebF) == 'Grower'] <- 'company'
  #renaming 'blockID' to 'block'
      colnames(SebF)[colnames(SebF) == 'blockID'] <- 'block'
   #changing capital letters
      colnames(SebF)[colnames(SebF) == 'Vineyard'] <- 'vineyard'

#Reformating dates - Seperating date into three columns, Y/M/D
date <- SebF$date
date2 <- ymd(date) #lubridate
SebF <- cbind(SebF, date2)

SebF <- separate(SebF, date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF <- SebF[, -1]

#Creating Events and Value Column
SebF <- pivot_longer(SebF, #tidyr
                     cols = c(brix, ta, ph),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : #unique columns (time, code ID, product, calculated/potassium?) ask EW
#"company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"
SebF.clean <- select(SebF, -notes, notes)

#Export Final Output
setwd("/Users/phoebeautio/desktop/bcvin/analyses/output/sebfarm_clean")
write.csv(SebF.clean, "sebfarm_brix_clean2018.csv", row.names = F)

