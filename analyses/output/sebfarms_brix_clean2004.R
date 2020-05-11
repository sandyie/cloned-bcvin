########################## 2004_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse)
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2004_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes (if these are empty do I add them?)
company <- "SebastianFarms"
vineyard <- ""
notes <- ""
SebF <- cbind(SebFarms_Brix, company, vineyard, notes)

#Rename block column
colnames(SebF)[colnames(SebF) == 'growblk'] <- 'block'
colnames(SebF)[colnames(SebF) == 'avg..berry.wt.'] <- 'avg.berry.wt'

#Reformating dates - Seperating date into three columns, Y/M/D
sample.date <- SebF$sample.date
sample.date2 <- ymd(sample.date) #lubridate
SebF <- cbind(SebF, sample.date2)

SebF <- separate(SebF, sample.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF <- SebF[, -1]

#Creating Events and Value Column
SebF <- pivot_longer(SebF, #tidyr
                      cols = c(brix, ta, ph, avg.berry.wt),
                      names_to = "event",
                      values_to = "value")

#Reordering column names :
 #"company", "vineyard", "sampler", block", "variety", "year", "month", "day", "event", "value", "notes"
SebF <- select(SebF, vineyard, everything())
SebF <- select(SebF, company, everything())
SebF.clean <- select(SebF, -notes, notes)
