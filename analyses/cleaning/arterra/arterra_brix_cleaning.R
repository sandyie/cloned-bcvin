### Cleaning script for Arterra Brix data
### Started 20 July 2020 by Mira Garner

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(dplyr)
library(reshape2)

if(length(grep("miragarner", getwd()))>0) { 
  setwd("~/Documents/git/bcvin/analyses/input/arterra/")
} else setwd("~/Documents/git/bcvin/analyses/input/arterra/")

bnp <- read.csv("2001-2019 Vintage Brix Data.csv")
head(bnp) # this dataset contains some phenology data along with the brix data

# separate phenology data and brix data
ph <- bnp[,c("VYears", "Source", "Vineyard", "Block", "Variety", "Budbreak", "Bloom", "Veraison", "HarvestDate")]
head(ph)

br <- bnp[, c("VYears", "Source", "Vineyard", "Block", "Variety", "Acres", "Tier", "BunchesPerVine", "BunchWt.kg.", "BerryMass.g.", "Brix", "pH", "TA", "GDDtoHarvest")]
head(br)

### ----------------- CLEAN PHENO DATA -----------------------###

# make pheno dataframe long format
ph2 <- melt(ph, id.vars=c("VYears", "Source", "Vineyard", "Block", "Variety"))

# separate dates for
breakdate <- strsplit(as.character(ph2$value), "-", fixed=TRUE)
ph2$day <- unlist(lapply(breakdate, function(x) x[1]))
ph2$month <- unlist(lapply(breakdate, function(x) x[2]))
ph2$year <- unlist(lapply(breakdate, function(x) x[3]))

# remove NAs
ph2 <- ph2[complete.cases(ph2),]

# years listed as 10 for 2010 etc. Change to full year. # VYear not same as year for 37 entries
ph2$year <- as.numeric(ph2$year)+2000

# change column names
colnames(ph2)[colnames(ph2) == 'Vineyard'] <- 'vineyard'
colnames(ph2)[colnames(ph2) == 'Variety'] <- 'variety'
colnames(ph2)[colnames(ph2) == 'Block'] <- 'block'
colnames(ph2)[colnames(ph2) == 'variable'] <- 'event'
colnames(ph2)[colnames(ph2) == 'Source'] <- 'notes'

# add company column
company <- "Arterra"
ph2 <- cbind(ph2, company)

ph.fin <- ph2[,c("company", "vineyard", "block", "variety", "year", "month", "day", "event", "notes")]

write.csv(ph.fin, "~/Documents/git/bcvin/analyses/output/phenoFromBrix_arterra.csv")

### ------------------------ CLEAN BRIX DATA ----------------------- ###

# do we want brix in long format?????
br2 <- melt(br, id.vars=c("VYears", "Source", "Vineyard", "Block", "Variety"))
head(br2)

br3 <- br2[which(br2$value != ""),]

#VYear = vintage year? Should brix data have this year or the harvest dat year?

#end