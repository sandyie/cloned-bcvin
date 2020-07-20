########################## 2013_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse) 
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2013_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes 
company <- "SebastianFarms"
vineyard <- ""
notes <- ""
SebF2013 <- cbind(SebFarms_Brix, company, vineyard, notes)

#Remove col "sampler"
SebF2013$sampler <- NULL

#Rename block column 
colnames(SebF2013)[colnames(SebF2013) == 'growblk'] <- 'block'

#Reformating dates - Seperating date into three columns, Y/M/D
sample.date <- SebF2013$sample.date
sample.date2 <- ymd(sample.date) #lubridate
SebF2013 <- cbind(SebF2013, sample.date2)

SebF2013 <- separate(SebF2013, sample.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2013$sample.date <- NULL

#Creating Events and Value Column
SebF2013 <- pivot_longer(SebF2013, #tidyr
                     cols = c(brix, ta, ph),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : 
#"company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2013 <- select(SebF2013, vineyard, everything())
SebF2013 <- select(SebF2013, company, everything())
SebF2013 <- select(SebF2013, -notes, notes)

#Export Final Output
#setwd("/Users/phoebeautio/desktop/bcvin/analyses/output/sebfarm_clean")
#write.csv(SebF2013, "sebfarm_brix_clean2013.csv", row.names = F)
