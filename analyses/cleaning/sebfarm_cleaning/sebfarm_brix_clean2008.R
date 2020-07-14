########################## 2008_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse) 
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2008_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes 
company <- "SebastianFarms"
notes <- ""
SebF2008 <- cbind(SebFarms_Brix, company, notes)

#Delete columns tag.no and sampler
SebF2008$sampler <- NULL
SebF2008$tag.no <- NULL

#Rename block column 
colnames(SebF2008)[colnames(SebF2008) == 'growblk'] <- 'block'

#Reformating dates - Seperating date into three columns, Y/M/D
crush.tag.date <- SebF2008$crush.tag.date
crush.tag.date2 <- ymd(crush.tag.date) #lubridate
SebF2008 <- cbind(SebF2008, crush.tag.date2)

SebF2008 <- separate(SebF2008, crush.tag.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2008$crush.tag.date <- NULL

#Creating Events and Value Column
SebF2008 <- pivot_longer(SebF2008, #tidyr
                     cols = c(brix, ta, ph, lbs),
                     names_to = "event",
                     values_to = "value")

#Reordering column names
  #"company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2008 <- select(SebF2008, block, everything())
SebF2008 <- select(SebF2008, vineyard, everything())
SebF2008 <- select(SebF2008, company, everything())
SebF2008 <- select(SebF2008, -notes, notes)

#Export Final Output
#setwd("/Users/phoebeautio/desktop/bcvin/analyses/output/sebfarm_clean")
#write.csv(SebF2008, "sebfarm_brix_clean2008.csv", row.names = F)
