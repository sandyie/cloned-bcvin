########################## 2015_SebFarms_Brix.csv Cleaning (PA) ##########################

  # Ask EW about different columns #

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse)
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2015_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes
company <- "SebastianFarms"
vineyard <- ""
SebF <- cbind(SebFarms_Brix, company, vineyard)

#Rename block column
colnames(SebF)[colnames(SebF) == 'growblk'] <- 'block'
colnames(SebF)[colnames(SebF) == 'comments'] <- 'notes'

#Reformating dates - Seperating date into three columns, Y/M/D
date <- SebF$date
date2 <- ymd(date) #lubridate
SebF <- cbind(SebF, date2)

SebF <- separate(SebF, date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF <- SebF[, -2]

#Creating Events and Value Column
SebF <- pivot_longer(SebF, #tidyr
                     cols = c(brix, ta, ph),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : #unique columns (tag.no, appell, lbs, tanks, deputy, jtemp, location, bins) as EW
#"company", "vineyard", "sampler", block", "variety", "year", "month", "day", "event", "value", "notes"
SebF <- select(SebF, block, everything())
SebF <- select(SebF, variety, everything())
SebF <- select(SebF, vineyard, everything())
SebF <- select(SebF, company, everything())
SebF.clean <- select(SebF, -notes, notes)
