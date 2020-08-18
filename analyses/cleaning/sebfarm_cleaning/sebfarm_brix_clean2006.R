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

#Removing incorrect code
SebF2006 <- SebF2006[!(SebF2006$block=="SPEPCF" & SebF2006$variety=="CSA"), ]

#Addressing vineyard and block codes
SebF2006$block <- gsub("^\\*", "", SebF2006$block) #removing asterix
SebF2006$vineyard <- paste(SebF2006$vineyard, SebF2006$block, sep = "") #pasting block value to empty vineyard cell

#Removing blocks that do not exist 
#GAB***may not have blocks and it might just be the full variety code... but not sure.
#DEK***

#vineyard (codes to look into: )
for(i in 1:nrow(SebF2006)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2006[i, "block"]))){ #isolating vineyard numbers
    SebF2006$vineyard[i] <- gsub("[A-Z]", "", SebF2006$vineyard[i])
  } 
}

for(i in 1:nrow(SebF2006)){
  if(isTRUE(grepl(pattern = "[A-Z]+", x = SebF2006[i, "vineyard"]))){ #isolating remaining vineyard character codes
    SebF2006$vineyard[i] <- substr(SebF2006$vineyard[i], 1:6, 3:6)
  } 
}

#block (blocks to look into: )
SebF2006$block <- gsub("[0-9]+", "", SebF2006$block) #removing vineyard digits to isolate block

for(i in 1:nrow(SebF2006)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2006[i, "block"]))){ #isolating blocks
    SebF2006$block[i] <- gsub("^.{0,2}", "", SebF2006$block[i])
  } 
}

for(i in 1:nrow(SebF2006)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{6}([^A-Z]|$)", x = SebF2006[i, "block"]))){ #isolating blocks with 6 letters
    SebF2006$block[i] <- gsub("^.{0,5}", "", SebF2006$block[i])
  } 
}

for(i in 1:nrow(SebF2006)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{5}([^A-Z]|$)", x = SebF2006[i, "block"]))){ #isolating blocks with 5 letters
    SebF2006$block[i] <- gsub("^.{0,4}", "", SebF2006$block[i])
  } 
}

