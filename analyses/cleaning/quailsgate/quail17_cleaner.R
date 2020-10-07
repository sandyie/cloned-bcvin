#cleaning script for Quails Gate Phenology data 2017-2019 (September 2019, Mira Garner)

rm(list = ls())
options(stringsAsFactors = FALSE)

library(lubridate)
library(dplyr)

setwd("~/Documents/git/bcvin/analyses/input/quailsgate/")
q <- read.csv("qg_phenology2017_19COPY.csv")
mp <- read.csv("VineyardMapsDataCompiled.csv")

head(q)
head(mp)

# remove lines that are not observations - look at again, some pheno info in Other.obs?
notphen  <- q[which(q$Asset.Class == "Admin / Staff"),] #putting them in df to look at
q <- q[which(q$Asset.Class != "Admin / Staff"),] #removing them from dataset

# asplit Asset.Indentifier column into block and vneyard columns - rought split since the format isn't the same for all entries. Will need to clean up some vineyard names and block names. Maybe Faith or Geoff has more elegant method?
assetsplit <- strsplit(q$Asset.Identifier, " ", fixed = TRUE, useBytes = TRUE)
q$block <- unlist(lapply(assetsplit, function(x) x[1]))
q$vineyard <- unlist(lapply(assetsplit, function(x) x[2]))

# fix vineyard names
q$vineyard[which(q$vineyard == "Estate")] <- "QuailsGateEstate"
q$vineyard[which(q$vineyard == "Blue")] <- "BlueFox"
q$vineyard[which(q$vineyard == "Stewart")] <- "StewartFamilyEstates"
q$vineyard[which(q$vineyard == "West")] <- "WestPoint"

q$vineyard[which(q$vineyard == "\x95" & q$block == "Beaumont")] <- "Beaumont"
q$block[which(q$vineyard == "Beaumont" & q$block == "Beaumont")] <- "1-0"
q$vineyard[which(q$vineyard == "\x95" & q$block == "Banducci")] <- "Banducci"
q$block[which(q$vineyard == "Banducci" & q$block == "Banducci")] <- "0"
q$block[which(q$vineyard == "")] <- "0"
q$vineyard[which(q$vineyard == "")] <- "Illig"


#q[which(q$vineyard == "&"),] <- "WestPoint" #fix block numbers first


q <- q[which(!is.na(q$vineyard)),]


#<<DATA>>$Asset.Identifier <- sub(" ", "_", <<DATA>>$Asset.Identifier) #to replace space

# add column for variety

# won't have variety match spreadsheet for vineyards: Patricia, WestPoint, Illig, StewartFamilyEstates, Beaumont, Banducci,

# split date into columns: year, month, day
###Question: which date is right?? Diff btwn TaskDate and DateOfObs
breakdate <- strsplit(as.character(q$Date.of.Observation), "-", fixed=TRUE)
q$day <- unlist(lapply(breakdate, function(x) x[1]))
q$month <- unlist(lapply(breakdate, function(x) x[2]))
q$year <- unlist(lapply(breakdate, function(x) x[3]))

# add column for event - what do stages mean?

# add column for value (percent flowering - again, stages?)
# for "flowering" = 80%

# add column for company: Quails Gate
company <- "QuailsGate"
q2 <- cbind(q2, company)

# add notes column
colnames(q2)[colnames(q2) == "Other.observations"] <- "notes"

# reorder columns
clean.quail17 <- select(q2, c("company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"))
