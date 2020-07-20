########################## 2015_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse)
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2015_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes
company <- "SebastianFarms"
vineyard <- ""
SebF2015 <- cbind(SebFarms_Brix, company, vineyard)

#Removing columns: tag.no, appell, tanks, deputy, jtemp, location, bins
SebF2015 <- subset(SebF2015, select = -c(tag.no, appell, tanks, deputy, jtemp, location, bins))

#Rename block column
colnames(SebF2015)[colnames(SebF2015) == 'growblk'] <- 'block'
colnames(SebF2015)[colnames(SebF2015) == 'comments'] <- 'notes'

#Reformating dates - Seperating date into three columns, Y/M/D
date <- SebF2015$date
date2 <- ymd(date) #lubridate
SebF2015 <- cbind(SebF2015, date2)

SebF2015 <- separate(SebF2015, date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2015$date <- NULL

#Creating Events and Value Column
SebF2015 <- pivot_longer(SebF2015, #tidyr
                     cols = c(brix, ta, ph, lbs),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : 
  #"company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2015 <- select(SebF2015, vineyard, everything())
SebF2015 <- select(SebF2015, company, everything())
SebF2015 <- select(SebF2015, -notes, notes)

#Export Final Output
#setwd("/Users/phoebeautio/desktop/bcvin/analyses/output/sebfarm_clean")
#write.csv(SebF2015, "sebfarm_brix_clean2015.csv", row.names = F)

