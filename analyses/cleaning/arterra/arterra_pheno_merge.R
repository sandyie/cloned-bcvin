# combining arterra phenology data into one final cleaned file. MG 24 Sep 2020

# housekeeping
rm(list = ls())
options(stringsAsFactors = FALSE)

if(length(grep("miragarner", getwd()))>0) { 
  setwd("~/Documents/git/bcvin/analyses/cleaning/arterra/")
} else setwd("~/Documents/git/bcvin/analyses/cleaning/arterra/")

library(reshape2)
library(dplyr)

# ------ Arterra_Budbreak phenological data cleaning script (15 Nov 2019, Mira Garner)

art_bud <- read.csv("~/Documents/git/bcvin/analyses/input/arterra/Arterra_Budbreak.csv")

#correct column names
bud1 <- art_bud
colnames(bud1) <- bud1[1,] #replace column names with row1
bud1 <- bud1[-1,] #delete row1 (is now column names)

# remove "Average Bud Break column" and last NA column
bud1 <- bud1[, -c(22, 23)]

# Need to get year column from separate year columns
bud2 <- melt(bud1, id.var=c("Variety", "Block", "Vineyard")) 

# separate event and year columns from 'variable' column
breakvar <- strsplit(as.character(bud2$variable), " ", fixed=TRUE)
bud2$year <- unlist(lapply(breakvar, function(x) x[3]))

# add event column
event <- "budburst"
bud2 <- cbind(bud2, event)


# ----- Arterra_Bloom phenological data cleaning script (15 Nov 2019, Mira Garner)
art_bloom <- read.csv("~/Documents/git/bcvin/analyses/input/arterra/Arterra_Bloom.csv")

#correct column names
bloom1 <- art_bloom
colnames(bloom1) <- bloom1[1,] #replace column names with row1
bloom1 <- bloom1[-1,] #delete row1 (is now column names)

#remove average bloom and column of NAs
bloom1 <- bloom1[,-c(22,23)]

# make 2001 column name consistent with other columns
colnames(bloom1)[colnames(bloom1) == "2001"] <- "2001 Bloom"

# Need to get year column from separate year columns
bloom2 <- melt(bloom1, id.var=c("Variety", "Block", "Vineyard"))

# separate event and year columns from 'variable' column
breakvar <- strsplit(as.character(bloom2$variable), "    ", fixed=TRUE)
bloom2$event <- unlist(lapply(breakvar, function(x) x[1]))
bloom2$year <- unlist(lapply(breakvar, function(x) x[2]))


# ------ Arterra_Veraison phenological data cleaning script (Sept. 2019, Mira Garner)

art_veraison <- read.csv("~/Documents/git/bcvin/analyses/input/arterra/Arterra_Veraison.csv")

#correct column names
veraison1 <- art_veraison
colnames(veraison1) <- veraison1[1,] #replace column names with row1
veraison1 <- veraison1[-1,] #delete row1 (is now column names)

#remove average veraison and column of NAs
veraison1 <- veraison1[,-c(22,23)]

# Need to get year column from separate year columns
veraison2 <- melt(veraison1, id.var=c("Variety", "Block", "Vineyard"))

# separate event and year columns from 'variable' column
breakvar <- strsplit(as.character(veraison2$variable), " ", fixed=TRUE)
veraison2$event <- unlist(lapply(breakvar, function(x) x[1]))
veraison2$year <- unlist(lapply(breakvar, function(x) x[2]))



# -------- Arterra_Harvest phenological data cleaning script (15 Nov 2019, Mira Garner)

art_harvest <- read.csv("~/Documents/git/bcvin/analyses/input/arterra/Arterra_Harvest.csv")

#correct column names
harvest1 <- art_harvest
colnames(harvest1) <- harvest1[1,] #replace column names with row1
harvest1 <- harvest1[-1,] #delete row1 (is now column names)

#remove average harvest and column of NAs
harvest1 <- harvest1[,-c(22,23)]

# Need to get year column from separate year columns
harvest2 <- melt(harvest1, id.var=c("Variety", "Block", "Vineyard"))

# separate event and year columns from 'variable' column
breakvar <- strsplit(as.character(harvest2$variable), " ", fixed=TRUE)
harvest2$event <- unlist(lapply(breakvar, function(x) x[1]))
harvest2$year <- unlist(lapply(breakvar, function(x) x[2]))

## combine datasets now that they are the same format

ph <- rbind(bud2, bloom2, veraison2, harvest2)

# vineyard, block, and variety columns - have but rename without cap letters
colnames(ph)[colnames(ph) == 'Vineyard'] <- 'vineyard'
colnames(ph)[colnames(ph) == 'Variety'] <- 'variety'
colnames(ph)[colnames(ph) == 'Block'] <- 'block'

# Separate month and day columns from 'values' column
breakdate <- strsplit(as.character(ph$value), "-", fixed=TRUE)
ph$day <- unlist(lapply(breakdate, function(x) x[1]))
ph$month <- unlist(lapply(breakdate, function(x) x[2]))

# remove capitalization
ph$event[grep("Harvest", ph$event)] <- "harvest"

# remove capitalization
ph$event[grep("Veraison", ph$event)] <- "veraison"

# remove capitalization
ph$event[grep("Bloom", ph$event)] <- "bloom"

# Add company column
company <- "Arterra"
ph <- cbind(ph, company)

# remove NA
ph2 <- ph[!is.na(ph$day),]

# reorder and select only the correct columns
cleanph <- select(ph2, c("company", "vineyard", "block", "variety", "year", "month", "day", "event"))


write.csv(cleanph, "arterra_pheno_clean.csv", row.names = FALSE)

######
###### WARNING: Code below is for preliminary analyses (Geoff)
###### 

# remove entries with no data for day
#clean_art_bud <- subset(clean_art_bud, is.na(clean_art_bud$day) == FALSE)

# replace month names with numbers (names must match format of month.abb constant)
#clean_art_bud$month <- match(clean_art_bud$month, month.abb)

# create a date column
#clean_art_bud$dates <- paste(clean_art_bud$year, clean_art_bud$month, clean_art_bud$day, sep="-")

# extract day of year
#clean_art_bud$dayofyear <- strftime(strptime(clean_art_bud$dates,format="%Y-%m-%d"), format = "%j")

# save working copy to temporary folder
#write.csv(file = "../output/temporary/arterra_bud_clean.csv", x = clean_art_bud, row.names = FALSE)

######
###### WARNING: Code below is for preliminary analyses (Geoff)
###### 

# remove entries with no data for day
#clean_art_veraison <- subset(clean_art_veraison, is.na(clean_art_veraison$day) == FALSE)

# replace month names with numbers (names must match format of month.abb constant)
#clean_art_veraison$month <- match(clean_art_veraison$month, month.abb)

# create a date column
#clean_art_veraison$dates <- paste(clean_art_veraison$year, clean_art_veraison$month, clean_art_veraison$day, sep="-")

# extract day of year
#clean_art_veraison$dayofyear <- strftime(strptime(clean_art_veraison$dates,format="%Y-%m-%d"), format = "%j")

# save working copy to temporary folder
#write.csv(file = "../../output/temporary/arterra_veraison_clean.csv", x = clean_art_veraison, row.names = FALSE)
