########################## 2017_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse) 
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2017_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes 
company <- "SebastianFarms"
vineyard <- ""
notes <- ""
SebF2017 <- cbind(SebFarms_Brix, company, vineyard, notes)

#Remove col "sampler"
SebF2017$sampler <- NULL

#Rename block column 
colnames(SebF2017)[colnames(SebF2017) == 'growblk'] <- 'block'

#Reformating dates - Seperating date into three columns, Y/M/D
sample.date <- SebF2017$sample.date
sample.date2 <- ymd(sample.date) #lubridate
SebF2017 <- cbind(SebF2017, sample.date2)

SebF2017 <- separate(SebF2017, sample.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2017$sample.date <- NULL

#Creating Events and Value Column
SebF2017 <- pivot_longer(SebF2017, #tidyr
                     cols = c(brix, ta, ph),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : 
#"company", "vineyard", block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2017 <- select(SebF2017, vineyard, everything())
SebF2017 <- select(SebF2017, company, everything())
SebF2017 <- select(SebF2017, -notes, notes)

#Deriving the vineyard from the code entered in block, and isolating the block
SebF2017$block <- gsub("^\\*", "", SebF2017$block) #removing asterix
SebF2017$vineyard <- paste(SebF2017$vineyard, SebF2017$block, sep = "") #pasting block value to vineyard
SebF2017$block <- gsub("[0-9]+", "", SebF2017$block) #removing vineyard digits to isolate block

#isolating vineyard numbers
for(i in 1:nrow(SebF2017)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2017[i, "block"]))){
    SebF2017$vineyard[i] <- gsub("[a-zA-Z]", "", SebF2017$vineyard[i])
  } 
}

#isolating block IDs
for(i in 1:nrow(SebF2017)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2017[i, "block"]))){
    SebF2017$block[i] <- gsub("^.{0,2}", "", SebF2017$block[i])
  } 
}