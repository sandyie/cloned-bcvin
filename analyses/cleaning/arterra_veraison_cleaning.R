# Arterra_Veraison phenological data cleaning
setwd("/Users/miragarner/Documents/git/bcvin/analyses/cleaning")
art_veraison <- read.csv("/Users/miragarner/Documents/git/bcvin/analyses/input/Arterra_Veraison.csv")

library(reshape2)

# Add company column
company <- "Arterra"
veraison1 <- cbind(art_veraison, company)

# vineyard, block, and variety columns - have but rename without cap letters
colnames(veraison1)[colnames(veraison1) == 'Vineyard'] <- 'vineyard'
colnames(veraison1)[colnames(veraison1) == 'Variety'] <- 'variety'
colnames(veraison1)[colnames(veraison1) == 'Blocks'] <- 'blocks'

# Need to separate month and day columns

# Need to get year column from separate year columns
veraison2 <- melt(veraison1, id.var=c("variety", "block", "vineyard", "company")) #Regular expressions

# event column - should just be one event per spreadsheet
event <- "veraison"
veraison3 <- cbind(veraison2, event)

# value column - no DATA?

# notes spreadsheet

# reorder columns
clean_arterra <- select(<<DATA>>, c("company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"))
