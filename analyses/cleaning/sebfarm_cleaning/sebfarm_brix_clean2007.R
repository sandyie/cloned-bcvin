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

#Deriving the vineyard from the code entered in block, and isolating the block
SebF2007$block <- gsub("^\\*", "", SebF2007$block) #removing asterix
SebF2007$vineyard <- paste(SebF2007$vineyard, SebF2007$block, sep = "") #pasting block value to vineyard
SebF2007$block <- gsub("[0-9]+", "", SebF2007$block) #removing vineyard digits to isolate block

#isolating vineyard numbers
for(i in 1:nrow(SebF2007)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2007[i, "block"]))){
    SebF2007$vineyard[i] <- gsub("[a-zA-Z]", "", SebF2007$vineyard[i])
  } 
}

#isolating block IDs
for(i in 1:nrow(SebF2007)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2007[i, "block"]))){
    SebF2007$block[i] <- gsub("^.{0,2}", "", SebF2007$block[i])
  } 
}
