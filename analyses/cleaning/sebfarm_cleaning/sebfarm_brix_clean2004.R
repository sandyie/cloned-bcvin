########################## 2004_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse) 
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2004_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes 
company <- "SebastianFarms"
vineyard <- ""
notes <- ""
SebF2004 <- cbind(SebFarms_Brix, company, vineyard, notes)

#Remove column 'Sampler'
SebF2004$sampler <- NULL

#Rename block and berry wt. columns
colnames(SebF2004)[colnames(SebF2004) == 'growblk'] <- 'block'
colnames(SebF2004)[colnames(SebF2004) == 'avg..berry.wt.'] <- 'avg.berry.wt'

#Reformating dates - Seperating date into three columns, Y/M/D
sample.date <- SebF2004$sample.date
sample.date2 <- ymd(sample.date) #lubridate
SebF2004 <- cbind(SebF2004, sample.date2)

SebF2004 <- separate(SebF2004, sample.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2004$sample.date <- NULL

#Creating Events and Value Column
SebF2004 <- pivot_longer(SebF2004, #tidyr
                      cols = c(brix, ta, ph, avg.berry.wt),
                      names_to = "event",
                      values_to = "value")

#Reordering column names : 
 #"company", "vineyard", block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2004 <- select(SebF2004, vineyard, everything())
SebF2004 <- select(SebF2004, company, everything())
SebF2004 <- select(SebF2004, -notes, notes)

#Export Final Output
#setwd("/Users/phoebeautio/desktop/bcvin/analyses/output/sebfarm_clean")
#write.csv(SebF2004, "sebfarm_brix_clean2004.csv", row.names = F)
