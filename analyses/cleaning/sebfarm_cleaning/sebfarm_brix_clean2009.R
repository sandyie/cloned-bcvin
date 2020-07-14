########################## 2009_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse) 
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2009_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes 
company <- "SebastianFarms"
vineyard <- ""
notes <- ""
SebF2009 <- cbind(SebFarms_Brix, company, vineyard, notes)

#Remove col "sampler"
SebF2009$sampler <- NULL

#Rename block column 
colnames(SebF2009)[colnames(SebF2009) == 'growblk'] <- 'block'

#Reformating dates - Seperating date into three columns, Y/M/D
sample.date <- SebF2009$sample.date
sample.date2 <- ymd(sample.date) #lubridate
SebF2009 <- cbind(SebF2009, sample.date2)

SebF2009 <- separate(SebF2009, sample.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2009$sample.date <- NULL

#Creating Events and Value Column
SebF2009 <- pivot_longer(SebF2009, #tidyr
                     cols = c(brix, ta, ph),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : 
#"company", "vineyard", block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2009 <- select(SebF2009, vineyard, everything())
SebF2009 <- select(SebF2009, company, everything())
SebF2009 <- select(SebF2009, -notes, notes)

#Export Final Output
#setwd("/Users/phoebeautio/desktop/bcvin/analyses/output/sebfarm_clean")
#write.csv(SebF2009, "sebfarm_brix_clean2009.csv", row.names = F)
