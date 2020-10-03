### Cleaning script for Arterra Brix data plus phenology data from the dataset
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

br <- bnp[, c("VYears", "Source", "Vineyard", "Block", "Variety", "Acres", "Tier", "BunchesPerVine", "BunchWt.kg.", "BerryMass.g.", "Brix", "pH", "TA", "GDDtoHarvest", "HarvestDate")]
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

# fix data entry errors
offyear <- ph2[which(ph2$VYears != ph2$year),]

# these have the same date for budburst for two Vyears in a row. The second Vyear, the other dates have event dates with the correct year, just budburst is a year early.
#ph2[which(ph2$vineyard == "McIntyre" & ph2$value == "23-Apr-03"),]
#ph2[which(ph2$value == "28-Apr-05" & ph2$variety == "Zinfandel"),]
#ph2[which(ph2$value == "8-May-08" & ph2$variety == "Petit Verdot"),]

# bloom: same problem
#ph2[which(ph2$value == "19-Jun-03" & ph2$variety == "Pinot Noir"),] #two blocks
#ph2[which(ph2$value == "16-Jun-03" & ph2$variety == "Semillon"),] #two blocks
#ph2[which(ph2$value == "15-Jun-08" & ph2$variety == "Cabernet Sauvignon"),]

#veraison same problem
#ph2[which(ph2$value == "17-Aug-03" & ph2$variety == "Pinot Noir"),] #two blocks
#ph2[which(ph2$value == "11-Aug-08" & ph2$variety == "Gewurztraminer"),]

# has only one entry
ph2$year[which(ph2$value == "12-May-08" & ph2$variety == "Cabernet Franc")] <- 2009
ph2$year[which(ph2$value == "18-Jun-08" & ph2$variety == "Malbec")] <- 2009
ph2$year[which(ph2$value == "17-Jun-08" & ph2$variety == "Shiraz")] <- 2009
ph2$year[which(ph2$value == "24-Aug-08" & ph2$variety == "Petit Verdot")] <- 2009
ph2$year[which(ph2$value == "21-Oct-11" & ph2$variety == "Tempranillo")] <- 2010
ph2$year[which(ph2$value == "21-Oct-11" & ph2$variety == "Riesling")] <- 2010
ph2$year[which(ph2$value == "28-Sep-11" & ph2$variety == "Gewurztraminer")] <- 2010
ph2$year[which(ph2$value == "6-Oct-03" & ph2$variety == "Chardonnay")] <- 2004
ph2$year[which(ph2$value == "19-Oct-03" & ph2$variety == "Ehrenfelser")] <- 2002 # two blocks
ph2$year[which(ph2$value == "7-Oct-03" & ph2$variety == "Pinot Blanc")] <- 2002
ph2$year[which(ph2$value == "7-Oct-03" & ph2$variety == "Chardonnay")] <- 2002 #two blocks
ph2$year[which(ph2$value == "30-Sep-03" & ph2$variety == "Gewurztraminer")] <- 2002
ph2$year[which(ph2$value == "8-Oct-03" & ph2$variety == "Chardonnay")] <- 2002 #two blocks
ph2$year[which(ph2$value == "8-Oct-03" & ph2$variety == "Gewurztraminer")] <- 2002 #two blocks
ph2$year[which(ph2$value == "19-Oct-03" & ph2$variety == "Gamay Noir")] <- 2002 # two blocks
ph2$year[which(ph2$value == "28-Sep-03" & ph2$vineyard == "HMV")] <- 2002 # two blocks
ph2$year[which(ph2$value == "27-Sep-03" & ph2$variety == "Gewurztraminer")] <- 2002
ph2$year[which(ph2$value == "26-Sep-03" & ph2$variety == "Gewurztraminer")] <- 2002
ph2$year[which(ph2$value == "23-Sep-03" & ph2$vineyard == "HMV")] <- 2002
ph2$year[which(ph2$value == "12-Oct-03" & ph2$variety == "Gewurztraminer")] <- 2002



ph.fin <- ph2[,c("company", "vineyard", "block", "variety", "year", "month", "day", "event", "notes")]

write.csv(ph.fin, "~/Documents/git/bcvin/analyses/output/phenoFromBrix_arterra.csv")

### ------------------------ CLEAN BRIX DATA ----------------------- ###

# do we want brix in long format?????
br2 <- melt(br, id.vars=c("VYears", "Source", "Vineyard", "Block", "Variety", "HarvestDate"))
head(br2)

br3 <- br2[which(br2$value != ""),]

# break up harvest date to day, month, year columns
breakdate <- strsplit(as.character(br2$HarvestDate), "-", fixed=TRUE)
br2$day.harv <- unlist(lapply(breakdate, function(x) x[1]))
br2$month.harv <- unlist(lapply(breakdate, function(x) x[2]))
br2$year.harv <- unlist(lapply(breakdate, function(x) x[3]))

br2$year <- as.numeric(br2$year)+2000

#check years match
broff <- br2[which(br2$VYears != br2$year),]

#end