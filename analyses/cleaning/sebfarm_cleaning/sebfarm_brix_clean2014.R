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

#Deriving the vineyard from the code entered in block, and isolating the block
SebF2014$block <- gsub("^\\*", "", SebF2014$block) #removing asterix
SebF2014$vineyard <- paste(SebF2014$vineyard, SebF2014$block, sep = "") #pasting block value to vineyard
SebF2014$block <- gsub("[0-9]+", "", SebF2014$block) #removing vineyard digits to isolate block

#isolating vineyard numbers
for(i in 1:nrow(SebF2014)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2014[i, "block"]))){
    SebF2014$vineyard[i] <- gsub("[a-zA-Z]", "", SebF2014$vineyard[i])
  } 
}

#isolating block IDs
for(i in 1:nrow(SebF2014)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2014[i, "block"]))){
    SebF2014$block[i] <- gsub("^.{0,2}", "", SebF2014$block[i])
  } 
}