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

#Addressing vineyard and block codes
SebF2017$block <- gsub("^\\*", "", SebF2017$block) #removing asterix
SebF2017$vineyard <- paste(SebF2017$vineyard, SebF2017$block, sep = "") #pasting block value to empty vineyard cell

#vineyard(ADDexcept for: BR1GWA; BR1PGB; BR1RSC; PHTA10; PHTA11; PHTA12; PHTA2; PHTA3; RG1CHB; RG1MEC; RG1RSA; RG2CFA; RG2PGB; RG2RSC; SD1CAA; SD3GWA; 
#SD3PGC; SD4RSA/SD4RSE)----

for(i in 1:nrow(SebF2017)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3,4}([^A-Z]|$)", x = SebF2017[i, "block"]))){ #isolating vineyard numbers
    SebF2017$vineyard[i] <- gsub("[A-Z]", "", SebF2017$vineyard[i])
  } 
}

for(i in 1:nrow(SebF2017)){
  if(isTRUE(grepl(pattern = "[A-Z]+", x = SebF2017[i, "vineyard"]))){ #isolating remaining vineyard character codes
    SebF2017$vineyard[i] <- substr(SebF2017$vineyard[i], 1:6, 3:6)
  } 
}

#block 
SebF2017$block <- gsub("[0-9]+", "", SebF2017$block) #removing vineyard digits to isolate block

for(i in 1:nrow(SebF2017)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2017[i, "block"]))){ #isolating 1 digit blocks
    SebF2017$block[i] <- gsub("^.{0,2}", "", SebF2017$block[i])
  } 
}

for(i in 1:nrow(SebF2017)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{4}([^A-Z]|$)", x = SebF2017[i, "block"]))){ #isolating 2 digit blocks
    SebF2017$block[i] <- gsub("^.{0,2}", "", SebF2017$block[i])
  } 
}

for(i in 1:nrow(SebF2017)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{6}([^A-Z]|$)", x = SebF2017[i, "block"]))){ #isolating blocks with 6 letters
    SebF2017$block[i] <- gsub("^.{0,5}", "", SebF2017$block[i])
  } 
}

for(i in 1:nrow(SebF2017)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{5}([^A-Z]|$)", x = SebF2017[i, "block"]))){ #isolating blocks with 5 letters
    SebF2017$block[i] <- gsub("^.{0,4}", "", SebF2017$block[i])
  } 
}

for(i in 1:nrow(SebF2017)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{7}([^A-Z]|$)", x = SebF2017[i, "block"]))){ #isolating blocks with 7 letters
    SebF2017$block[i] <- gsub("^.{0,5}", "", SebF2017$block[i])
  } 
}

for(i in 1:nrow(SebF2017)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{7}([^A-Z]|$)", x = SebF2017[i, "block"]))){ #isolating blocks with 7 letters
    SebF2017$block[i] <- gsub("^.{0,5}", "", SebF2017$block[i])
  } 
}

#Removing empty rows
SebF2017 <- SebF2017[!(is.na(SebF2017$year)), ]

