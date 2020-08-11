########################## 2005_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
#rm(list=ls())
#options(stringsAsFactors = FALSE)

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
SebF2005 <- cbind(SebFarms_Brix, company, vineyard, notes)

#Delete columns, dMACH and sampler
SebF2005$dMACH <- NULL
SebF2005$sampler <- NULL

#Rename block and berry wt. columns
colnames(SebF2005)[colnames(SebF2005) == 'growblk'] <- 'block'
colnames(SebF2005)[colnames(SebF2005) == 'avg..berry.wt.'] <- 'avg.berry.wt'

#Reformating dates - Seperating date into three columns, Y/M/D
sample.date <- SebF2005$samdate
sample.date2 <- ymd(sample.date) #lubridate
SebF2005 <- cbind(SebF2005, sample.date2)

SebF2005 <- separate(SebF2005, sample.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2005$samdate <- NULL

#Creating Events and Value Column
SebF2005 <- pivot_longer(SebF2005, #tidyr
                     cols = c(brix, ta, ph, avg.berry.wt),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : 
#"company", "vineyard", block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2005 <- select(SebF2005, vineyard, everything())
SebF2005 <- select(SebF2005, company, everything())
SebF2005 <- select(SebF2005, -notes, notes)

#Deriving the vineyard from the code entered in block, and isolating the block
SebF2005$block <- gsub("^\\*", "", SebF2005$block) #removing asterix
SebF2005$vineyard <- paste(SebF2005$vineyard, SebF2005$block, sep = "") #pasting block value to vineyard
SebF2005$block <- gsub("[0-9]+", "", SebF2005$block) #removing vineyard digits to isolate block

#isolating vineyard numbers
for(i in 1:nrow(SebF2005)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2005[i, "block"]))){
    SebF2005$vineyard[i] <- gsub("[a-zA-Z]", "", SebF2005$vineyard[i])
  } 
}

#isolating block IDs
for(i in 1:nrow(SebF2005)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2005[i, "block"]))){
    SebF2005$block[i] <- gsub("^.{0,2}", "", SebF2005$block[i])
  } 
}
