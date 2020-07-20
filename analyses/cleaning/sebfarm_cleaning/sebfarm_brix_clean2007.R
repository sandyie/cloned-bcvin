########################## 2007_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse) 
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2007_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes 
company <- "SebastianFarms"
vineyard <- ""
notes <- ""
SebF2007 <- cbind(SebFarms_Brix, company, vineyard, notes)

#Remove column "sampler"
SebF2007$sampler <- NULL

#Rename block column 
colnames(SebF2007)[colnames(SebF2007) == 'growblk'] <- 'block'

#Reformating dates - Seperating date into three columns, Y/M/D
sample.date <- SebF2007$sample.date
sample.date2 <- ymd(sample.date) #lubridate
SebF2007 <- cbind(SebF2007, sample.date2)

SebF2007 <- separate(SebF2007, sample.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2007$sample.date <- NULL

#Creating Events and Value Column
SebF2007 <- pivot_longer(SebF2007, #tidyr
                     cols = c(brix, ta, ph),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : 
#"company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2007 <- select(SebF2007, vineyard, everything())
SebF2007 <- select(SebF2007, company, everything())
SebF2007 <- select(SebF2007, -notes, notes)

#Export Final Output
#setwd("/Users/phoebeautio/desktop/bcvin/analyses/output/sebfarm_clean")
#write.csv(SebF2007, "sebfarm_brix_clean2007.csv", row.names = F)
