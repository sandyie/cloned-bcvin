########################## 2013_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse) 
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2013_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes 
company <- "SebastianFarms"
vineyard <- ""
notes <- ""
SebF2013 <- cbind(SebFarms_Brix, company, vineyard, notes)

#Remove col "sampler"
SebF2013$sampler <- NULL

#Rename block column 
colnames(SebF2013)[colnames(SebF2013) == 'growblk'] <- 'block'

#Reformating dates - Seperating date into three columns, Y/M/D
sample.date <- SebF2013$sample.date
sample.date2 <- ymd(sample.date) #lubridate
SebF2013 <- cbind(SebF2013, sample.date2)

SebF2013 <- separate(SebF2013, sample.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2013$sample.date <- NULL

#Creating Events and Value Column
SebF2013 <- pivot_longer(SebF2013, #tidyr
                     cols = c(brix, ta, ph),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : 
#"company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2013 <- select(SebF2013, vineyard, everything())
SebF2013 <- select(SebF2013, company, everything())
SebF2013 <- select(SebF2013, -notes, notes)

#Addressing vineyard and block codes
SebF2013$block <- gsub("^\\*", "", SebF2013$block) #removing asterix
SebF2013$vineyard <- paste(SebF2013$vineyard, SebF2013$block, sep = "") #pasting block value to empty vineyard cell

#vineyard
for(i in 1:nrow(SebF2013)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2013[i, "block"]))){ #isolating vineyard numbers
    SebF2013$vineyard[i] <- gsub("[A-Z]", "", SebF2013$vineyard[i])
  } 
}

for(i in 1:nrow(SebF2013)){
  if(isTRUE(grepl(pattern = "[A-Z]+", x = SebF2013[i, "vineyard"]))){ #isolating remaining vineyard character codes
    SebF2013$vineyard[i] <- substr(SebF2013$vineyard[i], 1:6, 3:6)
  } 
}

#block (blocks to look into: 16MUA for MSC; 18MEG for RSL)
SebF2013$block <- gsub("[0-9]+", "", SebF2013$block) #removing vineyard digits to isolate block

for(i in 1:nrow(SebF2013)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2013[i, "block"]))){ #isolating blocks
    SebF2013$block[i] <- gsub("^.{0,2}", "", SebF2013$block[i])
  } 
}

for(i in 1:nrow(SebF2013)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{6}([^A-Z]|$)", x = SebF2013[i, "block"]))){ #isolating blocks with 6 letters
    SebF2013$block[i] <- gsub("^.{0,5}", "", SebF2013$block[i])
  } 
}

for(i in 1:nrow(SebF2013)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{5}([^A-Z]|$)", x = SebF2013[i, "block"]))){ #isolating blocks with 5 letters
    SebF2013$block[i] <- gsub("^.{0,4}", "", SebF2013$block[i])
  } 
}
