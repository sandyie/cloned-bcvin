## Sebastian Farms cleaning script (November 2019, MG)

rm(list = ls())
options(stringsAsFactors = FALSE)

library(lubridate)
library(dplyr)
library(stringr)

setwd("~/Documents/git/bcvin/analyses/input/sebastianfarms")
seb1 <- read.csv("SebFarms_PhenologyData.csv")

head(seb1)

## add company column
company <- "SebastianFarms"
seb2 <- cbind(seb1, company)

## separate date into year, month and day columns
seb2$year <- year(ymd(seb2$Date))
seb2$month <- month(ymd(seb2$Date))
seb2$day <- day(ymd(seb2$Date))

## Move "retired" comments from block column to notes column, add "block" to message so we know what is the retired thing

sideA<-as.data.frame(str_match(seb2$Block, "^(.*)\\s(\\d\\d\\w{3})\\s(\\(.*\\))$")[,-1])
colnames(sideA)[colnames(sideA) == 'V3'] <- 'notes'
head(sideA)

sideB<-as.data.frame(str_match(seb2$Block, "^(\\w{1,2})\\s(.*)$")[,-1])
colnames(sideB) <- c("block", "blockCode")
head(sideB)

seb3 <- cbind(seb2, sideB$block, sideA$notes)
head(seb3)

## rename vineyard, variety, and block columns for lower case
colnames(seb3)[colnames(seb3) == 'Vineyard'] <- 'vineyard'
colnames(seb3)[colnames(seb3) == 'Variety'] <- 'variety'
colnames(seb3)[colnames(seb3) == 'sideB$block'] <- 'block'
colnames(seb3)[colnames(seb3) == 'sideA$notes'] <- 'notes'

## Where to put Business Unit - merge into vineyard column?? nope keep them separate. Just leave business unit out of final dataset - vineyard should give appropriate location information.

## Change Pheno.Stage column name to "event" events column
colnames(seb3)[colnames(seb3) == 'Pheno.Stage'] <- 'event'

head(seb3)

# reorder columns
clean.seb <- select(seb3, c("company", "vineyard", "block", "variety", "event", "year", "month", "day", "notes"))

head(clean.seb)

write.csv(clean.seb, "../cleaning/sebfarm_cleaning/seb_pheno_clean.csv")
