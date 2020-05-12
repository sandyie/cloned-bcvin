########################## 2005_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
rm(list=ls())
options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse) 
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2005_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes 
company <- "SebastianFarms"
vineyard <- ""
notes <- ""
SebF <- cbind(SebFarms_Brix, company, vineyard, notes)

#Rename block and berry wt. columns
colnames(SebF)[colnames(SebF) == 'growblk'] <- 'block'
colnames(SebF)[colnames(SebF) == 'avg..berry.wt.'] <- 'avg.berry.wt'

#Reformating dates - Seperating date into three columns, Y/M/D
sample.date <- SebF$samdate
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
#"company", "vineyard", "sampler", block", "variety", "year", "month", "day", "event", "value", "dMACH", "notes"
## what is dMACH column?? If an event, add to line 35.
SebF <- select(SebF, vineyard, everything())
SebF <- select(SebF, company, everything())
SebF <- select(SebF, -dMACH, dMACH)
SebF.clean <- select(SebF, -notes, notes)
