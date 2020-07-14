########################## 2012_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse)
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2012_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard
company <- "SebastianFarms"
vineyard <- ""
SebF2012 <- cbind(SebFarms_Brix, company, vineyard)

#Remove columns: tag.no, appell, grapecost, tanks, deputy, bins
SebF2012 <- subset(SebF2012, select = -c(tag.no, appell, grapecost, tanks, deputy, bins))

#Rename block, notes, column
colnames(SebF2012)[colnames(SebF2012) == 'growblk'] <- 'block'
colnames(SebF2012)[colnames(SebF2012) == 'comments'] <- 'notes'

#Reformating dates - Seperating date into three columns, Y/M/D
crush.tag.date <- SebF2012$crush.tag.date
crush.tag.date2 <- ymd(crush.tag.date) #lubridate
SebF2012 <- cbind(SebF2012, crush.tag.date2)

SebF2012 <- separate(SebF2012, crush.tag.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2012$crush.tag.date <- NULL

#Creating Events and Value Column
SebF2012 <- pivot_longer(SebF2012, #tidyr
                     cols = c(brix, ta, ph, lbs),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : #a lot of new columns (tag.no, appell, lbs, grapecost, tanks, deputy, bins)
#"company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2012 <- select(SebF2012, vineyard, everything())
SebF2012 <- select(SebF2012, company, everything())
SebF2012 <- select(SebF2012, -notes, notes)

#Export Final Output
#setwd("/Users/phoebeautio/desktop/bcvin/analyses/output/sebfarm_clean")
#write.csv(SebF2012, "sebfarm_brix_clean2012.csv", row.names = F)

