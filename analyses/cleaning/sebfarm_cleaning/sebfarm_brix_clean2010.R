########################## 2010_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse) 
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2010_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes 
company <- "SebastianFarms"
vineyard <- ""
notes <- ""
SebF2010 <- cbind(SebFarms_Brix, company, vineyard, notes)

#Remove col "sampler"
SebF2010$sampler <- NULL

#Rename block column 
colnames(SebF2010)[colnames(SebF2010) == 'growblk'] <- 'block'

#Reformating dates - Seperating date into three columns, Y/M/D
sample.date <- SebF2010$sample.date
sample.date2 <- ymd(sample.date) #lubridate
SebF2010 <- cbind(SebF2010, sample.date2)

SebF2010 <- separate(SebF2010, sample.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2010$sample.date <- NULL

#Creating Events and Value Column
SebF2010 <- pivot_longer(SebF2010, #tidyr
                     cols = c(brix, ta, ph),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : 
#"company", "vineyard", block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2010 <- select(SebF2010, vineyard, everything())
SebF2010 <- select(SebF2010, company, everything())
SebF2010 <- select(SebF2010, -notes, notes)

#Addressing vineyard and block codes
SebF2010$block <- gsub("^\\*", "", SebF2010$block) #removing asterix
SebF2010$vineyard <- paste(SebF2010$vineyard, SebF2010$block, sep = "") #pasting block value to empty vineyard cell

#vineyard
for(i in 1:nrow(SebF2010)){
  if (SebF2010[i, "vineyard"] == "RG1PGR") next
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2010[i, "block"]))){ #isolating vineyard numbers
    SebF2010$vineyard[i] <- gsub("[A-Z]", "", SebF2010$vineyard[i])
  } 
}

for(i in 1:nrow(SebF2010)){
  if(isTRUE(grepl(pattern = "[A-Z]+", x = SebF2010[i, "vineyard"]))){ #isolating remaining vineyard character codes
    SebF2010$vineyard[i] <- substr(SebF2010$vineyard[i], 1:6, 3:6)
  } 
}

#block
SebF2010$block <- gsub("[0-9]+", "", SebF2010$block) #removing vineyard digits to isolate block

for(i in 1:nrow(SebF2010)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2010[i, "block"]))){ #isolating blocks
    SebF2010$block[i] <- gsub("^.{0,2}", "", SebF2010$block[i])
  } 
}

for(i in 1:nrow(SebF2010)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{6}([^A-Z]|$)", x = SebF2010[i, "block"]))){ #isolating blocks with 6 letters
    SebF2010$block[i] <- gsub("^.{0,5}", "", SebF2010$block[i])
  } 
}

for(i in 1:nrow(SebF2010)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{5}([^A-Z]|$)", x = SebF2010[i, "block"]))){ #isolating blocks with 5 letters
    SebF2010$block[i] <- gsub("^.{0,4}", "", SebF2010$block[i])
  } 
}
