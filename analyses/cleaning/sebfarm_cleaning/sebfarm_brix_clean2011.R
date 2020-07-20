########################## 2011_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse) 
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2011_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard 
company <- "SebastianFarms"
vineyard <- ""
SebF2011 <- cbind(SebFarms_Brix, company, vineyard)

#Remove columns: tag.no, appell, unalloc, tanks, deputy, location, bins, gcexclude
SebF2011 <- subset(SebF2011, select = -c(tag.no, appell, unalloc, tanks, deputy, location, bins, gcexclude))

#Rename block, notes, column 
colnames(SebF2011)[colnames(SebF2011) == 'growblk'] <- 'block'
colnames(SebF2011)[colnames(SebF2011) == 'comments'] <- 'notes'

#Reformating dates - Seperating date into three columns, Y/M/D
crush.tag.date <- SebF2011$crush.tag.date
crush.tag.date2 <- ymd(crush.tag.date) #lubridate
SebF2011 <- cbind(SebF2011, crush.tag.date2)

SebF2011 <- separate(SebF2011, crush.tag.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2011$crush.tag.date <- NULL

#Creating Events and Value Column
SebF2011 <- pivot_longer(SebF2011, #tidyr
                     cols = c(brix, ta, ph, lbs),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : 
#"company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2011 <- select(SebF2011, vineyard, everything())
SebF2011 <- select(SebF2011, company, everything())
SebF2011 <- select(SebF2011, -notes, notes)

#Export Final Output
#setwd("/Users/phoebeautio/desktop/bcvin/analyses/output/sebfarm_clean")
#write.csv(SebF2011, "sebfarm_brix_clean2011.csv", row.names = F)
