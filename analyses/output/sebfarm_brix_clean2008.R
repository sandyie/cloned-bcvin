########################## 2008_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
rm(list=ls())
options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse) 
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2008_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes 
company <- "SebastianFarms"
notes <- ""
SebF <- cbind(SebFarms_Brix, company, notes)

#Rename block column 
colnames(SebF)[colnames(SebF) == 'growblk'] <- 'block'

#Reformating dates - Seperating date into three columns, Y/M/D
crush.tag.date <- SebF$crush.tag.date
crush.tag.date2 <- ymd(crush.tag.date) #lubridate
SebF <- cbind(SebF, crush.tag.date2)

SebF <- separate(SebF, crush.tag.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF <- SebF[, -4]

#Creating Events and Value Column
SebF <- pivot_longer(SebF, #tidyr
                     cols = c(brix, ta, ph),
                     names_to = "event",
                     values_to = "value")

#Reordering column names
  #"company", "vineyard", "sampler", block", "variety", "year", "month", "day", "event", "value", "notes"
SebF <- select(SebF, company, everything())
SebF.clean <- select(SebF, -notes, notes)
