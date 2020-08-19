########################## 2011_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse) 
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2011_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard 
company <- "SebastianFarms"
vineyard <- ""
SebF2011 <- cbind(SebFarms_Brix, company, vineyard)

#Remove columns: tag.no, appell, unalloc, tanks, deputy, location, bins, gcexclude
SebF2011 <- subset(SebF2011, select = -c(tag.no, appell, unalloc, tanks, deputy, location, bins, gcexclude))

#Rename block, notes, column 
colnames(SebF2011)[colnames(SebF2011) == 'growblk'] <- 'block'
colnames(SebF2011)[colnames(SebF2011) == 'comments'] <- 'notes'

#Reformating dates - Seperating date into three columns, Y/M/D
crush.tag.date <- SebF2011$crush.tag.date
crush.tag.date2 <- ymd(crush.tag.date) #lubridate
SebF2011 <- cbind(SebF2011, crush.tag.date2)

SebF2011 <- separate(SebF2011, crush.tag.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2011$crush.tag.date <- NULL

#Creating Events and Value Column
SebF2011 <- pivot_longer(SebF2011, #tidyr
                     cols = c(brix, ta, ph, lbs),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : 
#"company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2011 <- select(SebF2011, vineyard, everything())
SebF2011 <- select(SebF2011, company, everything())
SebF2011 <- select(SebF2011, -notes, notes)

#Addressing vineyard and block codes
SebF2011$block <- gsub("^\\*", "", SebF2011$block) #removing asterix
SebF2011$vineyard <- paste(SebF2011$vineyard, SebF2011$block, sep = "") #pasting block value to empty vineyard cell

#vineyard
for(i in 1:nrow(SebF2011)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2011[i, "block"]))){ #isolating vineyard numbers
    SebF2011$vineyard[i] <- gsub("[A-Z]", "", SebF2011$vineyard[i])
  } 
}

for(i in 1:nrow(SebF2011)){
  if(isTRUE(grepl(pattern = "[A-Z]+", x = SebF2011[i, "vineyard"]))){ #isolating remaining vineyard character codes
    SebF2011$vineyard[i] <- substr(SebF2011$vineyard[i], 1:6, 3:6)
  } 
}

SebF2011$vineyard[which(SebF2011$vineyard=="CVM" | SebF2011$vineyard=="CVP")] <- "CV" #correcting vin code
SebF2011$vineyard[which(SebF2011$vineyard=="GSF")] <- "GS" #correcting vin code

#block
SebF2011$block <- gsub("[0-9]+", "", SebF2011$block) #removing vineyard digits to isolate block

for(i in 1:nrow(SebF2011)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2011[i, "block"]))){ #isolating blocks
    SebF2011$block[i] <- gsub("^.{0,2}", "", SebF2011$block[i])
  } 
}

for(i in 1:nrow(SebF2011)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{6}([^A-Z]|$)", x = SebF2011[i, "block"]))){ #isolating blocks with 6 letters
    SebF2011$block[i] <- gsub("^.{0,5}", "", SebF2011$block[i])
  } 
}

for(i in 1:nrow(SebF2011)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{5}([^A-Z]|$)", x = SebF2011[i, "block"]))){ #isolating blocks with 5 letters
    SebF2011$block[i] <- gsub("^.{0,4}", "", SebF2011$block[i])
  } 
}
