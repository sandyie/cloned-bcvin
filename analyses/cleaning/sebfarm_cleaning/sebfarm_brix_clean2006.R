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

#Deriving the vineyard from the code entered in block, and isolating the block
SebF2006$block <- gsub("^\\*", "", SebF2006$block) #removing asterix
SebF2006$vineyard <- paste(SebF2006$vineyard, SebF2006$block, sep = "") #pasting block value to vineyard
SebF2006$block <- gsub("[0-9]+", "", SebF2006$block) #removing vineyard digits to isolate block

#isolating vineyard numbers
for(i in 1:nrow(SebF2006)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2006[i, "block"]))){
    SebF2006$vineyard[i] <- gsub("[a-zA-Z]", "", SebF2006$vineyard[i])
  } 
}

#isolating block IDs
for(i in 1:nrow(SebF2006)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2006[i, "block"]))){
    SebF2006$block[i] <- gsub("^.{0,2}", "", SebF2006$block[i])
  } 
}