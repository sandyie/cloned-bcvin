########################## 2015_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse)
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2015_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes
company <- "SebastianFarms"
vineyard <- ""
SebF2015 <- cbind(SebFarms_Brix, company, vineyard)

#Removing columns: tag.no, appell, tanks, deputy, jtemp, location, bins
SebF2015 <- subset(SebF2015, select = -c(tag.no, appell, tanks, deputy, jtemp, location, bins))

#Rename block column
colnames(SebF2015)[colnames(SebF2015) == 'growblk'] <- 'block'
colnames(SebF2015)[colnames(SebF2015) == 'comments'] <- 'notes'

#Reformating dates - Seperating date into three columns, Y/M/D
date <- SebF2015$date
date2 <- ymd(date) #lubridate
SebF2015 <- cbind(SebF2015, date2)

SebF2015 <- separate(SebF2015, date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2015$date <- NULL

#Creating Events and Value Column
SebF2015 <- pivot_longer(SebF2015, #tidyr
                     cols = c(brix, ta, ph, lbs),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : 
  #"company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2015 <- select(SebF2015, vineyard, everything())
SebF2015 <- select(SebF2015, company, everything())
SebF2015 <- select(SebF2015, -notes, notes)

#Addressing vineyard and block codes
SebF2015$block <- gsub("^\\*", "", SebF2015$block) #removing asterix
SebF2015$vineyard <- paste(SebF2015$vineyard, SebF2015$block, sep = "") #pasting block value to empty vineyard cell

#vineyard 
for(i in 1:nrow(SebF2015)){
  if(SebF2015[i, "vineyard"] == "CCPN8" | SebF2015[i, "vineyard"] == "R13GWA" | SebF2015[i, "vineyard"] == "R13PGB" | SebF2015[i, "vineyard"] == "SDHP107") next
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3,4}([^A-Z]|$)", x = SebF2015[i, "block"]))){ #isolating vineyard numbers
    SebF2015$vineyard[i] <- gsub("[A-Z]", "", SebF2015$vineyard[i])
  } 
}

for(i in 1:nrow(SebF2015)){
  if(isTRUE(grepl(pattern = "[A-Z]+", x = SebF2015[i, "vineyard"]))){ #isolating remaining vineyard character codes
    SebF2015$vineyard[i] <- substr(SebF2015$vineyard[i], 1:6, 3:6)
  } 
}

#block
SebF2015$block[which(SebF2015$block=="SDHPG97")] <- "97"
SebF2015$block[which(SebF2015$block=="SDHP107")] <- "107"
SebF2015$block[which(SebF2015$block=="CCPN8")] <- "8"
SebF2015$block[which(SebF2015$block=="R13PGB")] <- "B"
SebF2015$block[which(SebF2015$block=="PHTCHA2")] <- "A2"
SebF2015$block[which(SebF2015$block=="PHTCHB3")] <- "B3"
SebF2015$block[which(SebF2015$block=="R13GWA")] <- "A"

for(i in 1:nrow(SebF2015)){
  if(SebF2015[i, "block"] == "8" | SebF2015[i, "block"] == "97" | SebF2015[i, "block"] == "107" | 
     SebF2015[i, "block"] == "B" | SebF2015[i, "block"] == "A2" | SebF2015[i, "block"] == "B3") next
  if(isTRUE(grepl(pattern = "[0-9]+", "", x = SebF2015[i, "block"]))){ #isolating remaining vineyard character codes
    SebF2015$block[i] <- gsub("[0-9]+", "", SebF2015$block[i])
  } 
}

for(i in 1:nrow(SebF2015)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2015[i, "block"]))){ #isolating 1 digit blocks
    SebF2015$block[i] <- gsub("^.{0,2}", "", SebF2015$block[i])
  } 
}

for(i in 1:nrow(SebF2015)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{4}([^A-Z]|$)", x = SebF2015[i, "block"]))){ #isolating 2 digit blocks
    SebF2015$block[i] <- gsub("^.{0,2}", "", SebF2015$block[i])
  } 
}

for(i in 1:nrow(SebF2015)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{6}([^A-Z]|$)", x = SebF2015[i, "block"]))){ #isolating blocks with 6 letters
    SebF2015$block[i] <- gsub("^.{0,5}", "", SebF2015$block[i])
  } 
}
