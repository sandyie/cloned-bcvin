# Arterra_Veraison phenological data cleaning script (Sept. 2019, Mira Garner)

rm(list = ls())
options(stringsAsFactors = FALSE)
setwd("~/Documents/git/bcvin/analyses/cleaning")
art_veraison <- read.csv("~/Documents/git/bcvin/analyses/input/arterra/Arterra_Veraison.csv")

library(reshape2)
library(dplyr)

#correct column names
veraison1 <- art_veraison
colnames(veraison1) <- veraison1[1,] #replace column names with row1
veraison1 <- veraison1[-1,] #delete row1 (is now column names)

# Need to get year column from separate year columns
veraison2 <- melt(veraison1, id.var=c("Variety", "Block", "Vineyard")) #Regular expressions

# vineyard, block, and variety columns - have but rename without cap letters
colnames(veraison2)[colnames(veraison2) == 'Vineyard'] <- 'vineyard'
colnames(veraison2)[colnames(veraison2) == 'Variety'] <- 'variety'
colnames(veraison2)[colnames(veraison2) == 'Block'] <- 'block'

# Separate month and day columns from 'values' column
breakdate <- strsplit(as.character(veraison2$value), "-", fixed=TRUE)
veraison2$day <- unlist(lapply(breakdate, function(x) x[1]))
veraison2$month <- unlist(lapply(breakdate, function(x) x[2]))

# separate event and year columns from 'variable' column
breakvar <- strsplit(as.character(veraison2$variable), " ", fixed=TRUE)
veraison2$event <- unlist(lapply(breakvar, function(x) x[1]))
veraison2$year <- unlist(lapply(breakvar, function(x) x[2]))

# remove capitalization
veraison2$event[grep("Veraison", veraison2$event)] <- "veraison"

# Add company column
company <- "Arterra"
veraison3 <- cbind(veraison2, company)

# reorder and select only the correct columns
clean_art_veraison <- select(veraison3, c("company", "vineyard", "block", "variety", "year", "month", "day", "event"))

######
###### WARNING: Code below is for preliminary analyses (Geoff)
###### 

# remove entries with no data for day
clean_art_veraison <- subset(clean_art_veraison, is.na(clean_art_veraison$day) == FALSE)

# replace month names with numbers (names must match format of month.abb constant)
clean_art_veraison$month <- match(clean_art_veraison$month, month.abb)

# create a date column
clean_art_veraison$dates <- paste(clean_art_veraison$year, clean_art_veraison$month, clean_art_veraison$day, sep="-")

# extract day of year
clean_art_veraison$dayofyear <- strftime(strptime(clean_art_veraison$dates,format="%Y-%m-%d"), format = "%j")

# save working copy to temporary folder
write.csv(file = "../../output/temporary/arterra_veraison_clean.csv", x = clean_art_veraison, row.names = FALSE)
