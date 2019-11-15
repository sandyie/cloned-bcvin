# Arterra_Veraison phenological data cleaning script (Sept. 2019, Mira Garner)

rm(list = ls())
options(stringsAsFactors = FALSE)
setwd("/Users/miragarner/Documents/git/bcvin/analyses/cleaning")
art_veraison <- read.csv("/Users/miragarner/Documents/git/bcvin/analyses/input/Arterra_Veraison.csv")

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
clean_arterra <- select(veraison3, c("company", "vineyard", "block", "variety", "year", "month", "day", "event"))
