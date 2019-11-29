# Arterra_Budbreak phenological data cleaning script (15 Nov 2019, Mira Garner)

rm(list = ls())
options(stringsAsFactors = FALSE)
setwd("~/Documents/git/bcvin/analyses/cleaning")
art_bud <- read.csv("~/Documents/git/bcvin/analyses/input/Arterra_Budbreak.csv")

library(reshape2)
library(dplyr)

#correct column names
bud1 <- art_bud
colnames(bud1) <- bud1[1,] #replace column names with row1
bud1 <- bud1[-1,] #delete row1 (is now column names)

# Need to get year column from separate year columns
bud2 <- melt(bud1, id.var=c("Variety", "Block", "Vineyard")) #Regular expressions

# vineyard, block, and variety columns - have but rename without cap letters
colnames(bud2)[colnames(bud2) == 'Vineyard'] <- 'vineyard'
colnames(bud2)[colnames(bud2) == 'Variety'] <- 'variety'
colnames(bud2)[colnames(bud2) == 'Block'] <- 'block'

# Separate month and day columns from 'values' column
breakdate <- strsplit(as.character(bud2$value), "-", fixed=TRUE)
bud2$day <- unlist(lapply(breakdate, function(x) x[1]))
bud2$month <- unlist(lapply(breakdate, function(x) x[2]))

# separate event and year columns from 'variable' column
breakvar <- strsplit(as.character(bud2$variable), " ", fixed=TRUE)
bud2$year <- unlist(lapply(breakvar, function(x) x[3]))

# add event column
event <- "budburst"
bud2 <- cbind(bud2, event)

# Add company column
company <- "Arterra"
bud3 <- cbind(bud2, company)

# reorder and select only the correct columns
clean_art_bud <- select(bud3, c("company", "vineyard", "block", "variety", "year", "month", "day", "event"))
