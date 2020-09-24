# Arterra_Budbreak phenological data cleaning script (15 Nov 2019, Mira Garner)

rm(list = ls())
options(stringsAsFactors = FALSE)
setwd("~/Documents/git/bcvin/analyses/cleaning")
art_bud <- read.csv("~/Documents/git/bcvin/analyses/input/arterra/Arterra_Budbreak.csv")

library(reshape2)
library(dplyr)

#correct column names
bud1 <- art_bud
colnames(bud1) <- bud1[1,] #replace column names with row1
bud1 <- bud1[-1,] #delete row1 (is now column names)

# remove "Average Bud Break column" and last NA column
bud1 <- bud1[, -c(22, 23)]
    
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

######
###### WARNING: Code below is for preliminary analyses (Geoff)
###### 

# remove entries with no data for day
clean_art_bud <- subset(clean_art_bud, is.na(clean_art_bud$day) == FALSE)

# replace month names with numbers (names must match format of month.abb constant)
clean_art_bud$month <- match(clean_art_bud$month, month.abb)

# create a date column
clean_art_bud$dates <- paste(clean_art_bud$year, clean_art_bud$month, clean_art_bud$day, sep="-")

# extract day of year
clean_art_bud$dayofyear <- strftime(strptime(clean_art_bud$dates,format="%Y-%m-%d"), format = "%j")

# save working copy to temporary folder
write.csv(file = "../output/temporary/arterra_bud_clean.csv", x = clean_art_bud, row.names = FALSE)
