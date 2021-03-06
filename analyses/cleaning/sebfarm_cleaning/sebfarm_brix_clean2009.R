########################## 2009_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping 
#rm(list=ls())
#options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse) 
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2009_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard, notes 
company <- "SebastianFarms"
vineyard <- ""
notes <- ""
SebF2009 <- cbind(SebFarms_Brix, company, vineyard, notes)

#Remove col "sampler"
SebF2009$sampler <- NULL

#Rename block column 
colnames(SebF2009)[colnames(SebF2009) == 'growblk'] <- 'block'

#Reformating dates - Seperating date into three columns, Y/M/D
sample.date <- SebF2009$sample.date
sample.date2 <- ymd(sample.date) #lubridate
SebF2009 <- cbind(SebF2009, sample.date2)

SebF2009 <- separate(SebF2009, sample.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF2009$sample.date <- NULL

#Creating Events and Value Column
SebF2009 <- pivot_longer(SebF2009, #tidyr
                     cols = c(brix, ta, ph),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : 
#"company", "vineyard", block", "variety", "year", "month", "day", "event", "value", "notes"
SebF2009 <- select(SebF2009, vineyard, everything())
SebF2009 <- select(SebF2009, company, everything())
SebF2009 <- select(SebF2009, -notes, notes)

#Addressing vineyard and block codes
SebF2009$block <- gsub("^\\*", "", SebF2009$block) #removing asterix
SebF2009$vineyard <- paste(SebF2009$vineyard, SebF2009$block, sep = "") #pasting block value to empty vineyard cell

#vineyard
for(i in 1:nrow(SebF2009)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2009[i, "block"]))){ #isolating vineyard numbers
    SebF2009$vineyard[i] <- gsub("[A-Z]", "", SebF2009$vineyard[i])
  } 
}

for(i in 1:nrow(SebF2009)){
  if(isTRUE(grepl(pattern = "[A-Z]+", x = SebF2009[i, "vineyard"]))){ #isolating remaining vineyard character codes
    SebF2009$vineyard[i] <- substr(SebF2009$vineyard[i], 1:6, 3:6)
  } 
}

#block 
SebF2009$block <- gsub("[0-9]+", "", SebF2009$block) #removing vineyard digits to isolate block

for(i in 1:nrow(SebF2009)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{3}([^A-Z]|$)", x = SebF2009[i, "block"]))){ #isolating blocks
    SebF2009$block[i] <- gsub("^.{0,2}", "", SebF2009$block[i])
  } 
}

for(i in 1:nrow(SebF2009)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{6}([^A-Z]|$)", x = SebF2009[i, "block"]))){ #isolating blocks with 6 letters
    SebF2009$block[i] <- gsub("^.{0,5}", "", SebF2009$block[i])
  } 
}

for(i in 1:nrow(SebF2009)){
  if(isTRUE(grepl(pattern = "(^|[^A-Z])[A-Z]{5}([^A-Z]|$)", x = SebF2009[i, "block"]))){ #isolating blocks with 5 letters
    SebF2009$block[i] <- gsub("^.{0,4}", "", SebF2009$block[i])
  } 
}
