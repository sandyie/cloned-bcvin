#cleaning script for Quails Gate Phenology data 2017-2019 (September 2019, Mira Garner)
rm(list = ls())
options(stringsAsFactors = FALSE)

setwd("~/Documents/git/bcvin/analyses/cleaning")
quail17 <- read.csv("~/Documents/git/bcvin/analyses/input/qg_phenology2017_19COPY.csv")

library(lubridate)
library(dplyr)

# add column for variety - NEED TO DO BEFORE SPLIT
#### need to look at maps and Asset Identifier, create another spreadsheet of the asset.id with variety then match with asset.id in pheno spreadsheet?


# add column for vineyard - split from block
# add column for block - weird block/vineyard names??)
#<<DATA>>$Asset.Identifier <- sub(" ", "_", <<DATA>>$Asset.Identifier) #to replace space

# split date into columns: year, month, day
###Question: which date is right?? Diff btwn TaskDate and DateOfObs
<<DF>>$year <- year(mdy(<<DF>>$<DATE>))
<<DF>>$month <- month(mdy(<<DF>>$<<DATE>>))
<<DF>>$day <- day(mdy(<<DF>>$<<DATE>>))

# add column for event - what do stages mean?

# add column for value (percent flowering - again, stages?)
# for "flowering" = 80%

# add column for company: Quails Gate
company <- "QuailsGate"
NEXTDATA <- cbind(<<DATA>>, company)

# add notes column
#Other.observations <- notes
#library(data.table)
#setnames(data, old=c("old_name","another_old_name"), new=c("new_name", "another_new_name"))
# OR colnames(data)[colnames(data)=="old_name"] <- "new_name" for single columns

# reorder columns
clean.quail17 <- select(<<DATA>>, c("company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"))
