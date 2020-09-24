# Arterra_Harvest phenological data cleaning script (15 Nov 2019, Mira Garner)

rm(list = ls())
options(stringsAsFactors = FALSE)
setwd("~/Documents/git/bcvin/analyses/cleaning")
art_harvest <- read.csv("~/Documents/git/bcvin/analyses/input/arterra/Arterra_Harvest.csv")

library(reshape2)
library(dplyr)

#correct column names
harvest1 <- art_harvest
colnames(harvest1) <- harvest1[1,] #replace column names with row1
harvest1 <- harvest1[-1,] #delete row1 (is now column names)

# Need to get year column from separate year columns
harvest2 <- melt(harvest1, id.var=c("Variety", "Block", "Vineyard")) #Regular expressions

# vineyard, block, and variety columns - have but rename without cap letters
colnames(harvest2)[colnames(harvest2) == 'Vineyard'] <- 'vineyard'
colnames(harvest2)[colnames(harvest2) == 'Variety'] <- 'variety'
colnames(harvest2)[colnames(harvest2) == 'Block'] <- 'block'

# Separate month and day columns from 'values' column
breakdate <- strsplit(as.character(harvest2$value), "-", fixed=TRUE)
harvest2$day <- unlist(lapply(breakdate, function(x) x[1]))
harvest2$month <- unlist(lapply(breakdate, function(x) x[2]))

# separate event and year columns from 'variable' column
breakvar <- strsplit(as.character(harvest2$variable), " ", fixed=TRUE)
harvest2$event <- unlist(lapply(breakvar, function(x) x[1]))
harvest2$year <- unlist(lapply(breakvar, function(x) x[2]))

# remove capitalization
harvest2$event[grep("Harvest", harvest2$event)] <- "harvest"

# Add company column
company <- "Arterra"
harvest3 <- cbind(harvest2, company)

# reorder and select only the correct columns
clean_art_harvest <- select(harvest3, c("company", "vineyard", "block", "variety", "year", "month", "day", "event"))
