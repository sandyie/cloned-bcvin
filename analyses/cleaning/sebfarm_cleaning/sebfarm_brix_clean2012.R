########################## 2012_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse)
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2012_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard
company <- "SebastianFarms"
vineyard <- ""
SebF2012 <- cbind(SebFarms_Brix, company, vineyard)

#Remove columns: tag.no, appell, grapecost, tanks, deputy, bins
SebF2012 <- subset(SebF2012, select = -c(tag.no, appell, grapecost, tanks, deputy, bins))

#Rename block, notes, column
colnames(SebF2012)[colnames(SebF2012) == 'growblk'] <- 'block'
colnames(SebF2012)[colnames(SebF2012) == 'comments'] <- 'notes'

#Reformating dates - Seperating date into three columns, Y/M/D
crush.tag.date <- SebF2012$crush.tag.date
crush.tag.date2 <- ymd(crush.tag.date) #lubridate
SebF2012 <- cbind(SebF2012, crush.tag.date2)

SebF2012 <- separate(SebF2012, crush.tag.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2012$crush.tag.date <- NULL

#Creating Events and Value Column
SebF2012 <- pivot_longer(SebF2012, #tidyr
                     cols = c(brix, ta, ph, lbs),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : #a lot of new columns (tag.no, appell, lbs, grapecost, tanks, deputy, bins)
#"company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2012 <- select(SebF2012, vineyard, everything())
SebF2012 <- select(SebF2012, company, everything())
SebF2012 <- select(SebF2012, -notes, notes)

#Removing empty rows
SebF2012 <- SebF2012[!(is.na(SebF2012$year)), ]

#Addressing vineyard and block codes
SebF2012$block <- gsub("^\\*", "", SebF2012$block) #removing asterix
SebF2012$vineyard <- paste(SebF2012$vineyard, SebF2012$block, sep = "") #pasting block value to empty vineyard cell

#vineyard
for(i in 1:nrow(SebF2012)){
  if(SebF2012[i, "vineyard"] == "R13GWA" | SebF2012[i, "vineyard"] == "BR1PGB" | SebF2012[i, "vineyard"] == "R13PGB" | SebF2012[i, "vineyard"] == "BR1GWA") next
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2012[i, "block"]))){ #isolating vineyard numbers
    SebF2012$vineyard[i] <- gsub("[A-Z]", "", SebF2012$vineyard[i])
  } 
}

for(i in 1:nrow(SebF2012)){
  if(isTRUE(grepl(pattern = "[A-Z]+", x = SebF2012[i, "vineyard"]))){ #isolating remaining vineyard character codes
    SebF2012$vineyard[i] <- substr(SebF2012$vineyard[i], 1:6, 3:6)
  } 
}

SebF2012$vineyard[which(SebF2012$vineyard=="CVM" | SebF2012$vineyard=="CVP")] <- "CV" #correcting vin code
SebF2012$vineyard[which(SebF2012$vineyard=="GSF" | SebF2012$vineyard=="GSP")] <- "GS" #correcting vin code

#block
SebF2012$block <- gsub("[0-9]+", "", SebF2012$block) #removing vineyard digits to isolate block

for(i in 1:nrow(SebF2012)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2012[i, "block"]))){ #isolating blocks
    SebF2012$block[i] <- gsub("^.{0,2}", "", SebF2012$block[i])
  } 
}

for(i in 1:nrow(SebF2012)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{6}([^A-Z]|$)", x = SebF2012[i, "block"]))){ #isolating blocks with 6 letters
    SebF2012$block[i] <- gsub("^.{0,5}", "", SebF2012$block[i])
  } 
}

for(i in 1:nrow(SebF2012)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{5}([^A-Z]|$)", x = SebF2012[i, "block"]))){ #isolating blocks with 5 letters
    SebF2012$block[i] <- gsub("^.{0,4}", "", SebF2012$block[i])
  } 
}
