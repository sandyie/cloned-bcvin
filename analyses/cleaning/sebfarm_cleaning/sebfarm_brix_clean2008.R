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

#Deriving the vineyard from the code entered in block, and isolating the block
SebF2008$block <- gsub("^\\*", "", SebF2008$block) #removing asterix
SebF2008$vineyard <- gsub("^\\*", "", SebF2008$block) #removing asterix
SebF2008$block <- gsub("[0-9]+", "", SebF2008$block) #removing vineyard digits to isolate block

#isolating vineyard numbers
for(i in 1:nrow(SebF2008)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2008[i, "vineyard"]))){
    SebF2008$vineyard[i] <- gsub("[a-zA-Z]", "", SebF2008$vineyard[i])
  } 
}

#isolating block IDs
for(i in 1:nrow(SebF2008)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2008[i, "block"]))){
    SebF2008$block[i] <- gsub("^.{0,2}", "", SebF2008$block[i])
  } 
}
