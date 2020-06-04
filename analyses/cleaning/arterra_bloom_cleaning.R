# Arterra_Bloom phenological data cleaning script (15 Nov 2019, Mira Garner)

rm(list = ls())
options(stringsAsFactors = FALSE)
setwd("~/Documents/git/bcvin/analyses/cleaning")
#setwd("C:\\Users\\Faith Jones\\Documents\\ubc\\github\\bcvin\\analyses")# Faiths working directory
art_bloom <- read.csv("~/Documents/git/bcvin/analyses/input/Arterra_Bloom.csv")
#art_bloom <- read.csv("C:\\Users\\Faith Jones\\Documents\\ubc\\github\\bcvin\\analyses\\input\\arterra\\Arterra_Bloom.csv")
#FJ note - I dont think Mira's filepath will work now because teh CVS files have been moved in to an artera "folder"


library(reshape2)
library(dplyr)

#correct column names
bloom1 <- art_bloom
colnames(bloom1) <- bloom1[1,] #replace column names with row1
bloom1 <- bloom1[-1,] #delete row1 (is now column names)

# Need to get year column from separate year columns
bloom2 <- melt(bloom1, id.var=c("Variety", "Block", "Vineyard")) #Regular expressions

# vineyard, block, and variety columns - have but rename without cap letters
colnames(bloom2)[colnames(bloom2) == 'Vineyard'] <- 'vineyard'
colnames(bloom2)[colnames(bloom2) == 'Variety'] <- 'variety'
colnames(bloom2)[colnames(bloom2) == 'Block'] <- 'block'

# Separate month and day columns from 'values' column
breakdate <- strsplit(as.character(bloom2$value), "-", fixed=TRUE)
bloom2$day <- unlist(lapply(breakdate, function(x) x[1]))
bloom2$month <- unlist(lapply(breakdate, function(x) x[2]))

# separate event and year columns from 'variable' column
breakvar <- strsplit(as.character(bloom2$variable), "    ", fixed=TRUE)
bloom2$event <- unlist(lapply(breakvar, function(x) x[1]))
bloom2$year <- unlist(lapply(breakvar, function(x) x[2]))

# remove capitalization
bloom2$event[grep("Bloom", bloom2$event)] <- "bloom"

# Add company column
company <- "Arterra"
bloom3 <- cbind(bloom2, company)

# reorder and select only the correct columns
clean_art_bloom <- select(bloom3, c("company", "vineyard", "block", "variety", "year", "month", "day", "event"))
