########################## 2012_SebFarms_Brix.csv Cleaning (PA) ##########################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/input/sebastianfarms/brix/")
library(tidyverse)
library(lubridate)

#Reading in csv files
SebFarms_Brix <- read.csv("2012_SebFarms_Brix.csv", header=TRUE)
head(SebFarms_Brix)

#Add columns: company, vineyard
company <- "SebastianFarms"
vineyard <- ""
SebF <- cbind(SebFarms_Brix, company, vineyard)

#Remove columns: tag.no, appell, grapecost, tanks, deputy, bins
SebF <- subset(SebF, select = -c(tag.no, appell, grapecost, tanks, deputy, bins))

#Rename block, notes, column
colnames(SebF)[colnames(SebF) == 'growblk'] <- 'block'
colnames(SebF)[colnames(SebF) == 'comments'] <- 'notes'

#Reformating dates - Seperating date into three columns, Y/M/D
crush.tag.date <- SebF$crush.tag.date
crush.tag.date2 <- ymd(crush.tag.date) #lubridate
SebF <- cbind(SebF, crush.tag.date2)

SebF <- separate(SebF, crush.tag.date2, into = c("year", "month", "day"), sep = "-") #tidyr
SebF <- SebF[, -1]

#Creating Events and Value Column
SebF <- pivot_longer(SebF, #tidyr
                     cols = c(brix, ta, ph, lbs),
                     names_to = "event",
                     values_to = "value")

#Reordering column names : #a lot of new columns (tag.no, appell, lbs, grapecost, tanks, deputy, bins)
#"company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"
SebF <- select(SebF, vineyard, everything())
SebF <- select(SebF, company, everything())
SebF.clean <- select(SebF, -notes, notes)

#Export Final Output
setwd("/Users/phoebeautio/desktop/bcvin/analyses/output/sebfarm_clean")
write.csv(SebF.clean, "sebfarm_brix_clean2012.csv", row.names = F)

