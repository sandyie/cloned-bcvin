########################## 2014_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse) 
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2014_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes 
company <- "SebastianFarms"
vineyard <- ""
notes <- ""
SebF2014 <- cbind(SebFarms_Brix, company, vineyard, notes)

#Remove columns: tag.no, grower
SebF2014 <- subset(SebF2014, select = -c(tag.no, grower))

#Rename block column 
colnames(SebF2014)[colnames(SebF2014) == 'grow.blk'] <- 'block'

#Reformating dates - Seperating date into three columns, Y/M/D
date <- SebF2014$date
date2 <- ymd(date) #lubridate
SebF2014 <- cbind(SebF2014, date2)

SebF2014 <- separate(SebF2014, date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2014$date <- NULL

#Creating Events and Value Column
SebF2014 <- pivot_longer(SebF2014, #tidyr
                     cols = c(brix, ta, ph, lbs),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : 
#"company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2014 <- select(SebF2014, block, everything())
SebF2014 <- select(SebF2014, vineyard, everything())
SebF2014 <- select(SebF2014, company, everything())
SebF2014 <- select(SebF2014, -notes, notes)

#Export Final Output
#setwd("/Users/phoebeautio/desktop/bcvin/analyses/output/sebfarm_clean")
#write.csv(SebF2014, "sebfarm_brix_clean2014.csv", row.names = F)

