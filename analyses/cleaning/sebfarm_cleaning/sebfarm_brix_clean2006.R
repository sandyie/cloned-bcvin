########################## 2006_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse) 
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2006_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes 
company <- "SebastianFarms"
vineyard <- ""
notes <- ""
SebF2006 <- cbind(SebFarms_Brix, company, vineyard, notes)

#Remove column "sampler"
SebF2006$sampler <- NULL

#Rename block column 
colnames(SebF2006)[colnames(SebF2006) == 'growblk'] <- 'block'

#Reformating dates - Seperating date into three columns, Y/M/D
sample.date <- SebF2006$sample.date
sample.date2 <- ymd(sample.date) #lubridate
SebF2006 <- cbind(SebF2006, sample.date2)

SebF2006 <- separate(SebF2006, sample.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2006$sample.date <- NULL

#Creating Events and Value Column
SebF2006 <- pivot_longer(SebF2006, #tidyr
                     cols = c(brix, ta, ph),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : 
#"company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2006 <- select(SebF2006, vineyard, everything())
SebF2006 <- select(SebF2006, company, everything())
SebF2006 <- select(SebF2006, -notes, notes)

#Export Final Output
#setwd("/Users/phoebeautio/desktop/bcvin/analyses/output/sebfarm_clean")
#write.csv(SebF2006, "sebfarm_brix_clean2006.csv", row.names = F)
