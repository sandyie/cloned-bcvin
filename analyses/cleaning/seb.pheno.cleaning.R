## Sebastian Farms cleaning script (November 2019, MG)

rm(list = ls())
options(stringsAsFactors = FALSE)

setwd("~/Documents/git")
seb1 <- read.csv("~/Documents/git/bcvin/analyses/input/SebFarms_PhenologyData.csv")

library(lubridate)
library(dplyr)

## add company column
company <- "SebastianFarms"
seb2 <- cbind(seb1, company)

## rename vineyard, variety, and block columns for lower case
colnames(seb2)[colnames(seb2) == 'Vineyard'] <- 'vineyard'
colnames(seb2)[colnames(seb2) == 'Variety'] <- 'variety'
colnames(seb2)[colnames(seb2) == 'Block'] <- 'block'

## Where to put Business Unit - merge into vineyard column??

## Check date order - y/m/d makes most sense
<<DF>>$year <- year(ymd(<<DF>>$<DATE>))
<<DF>>$month <- month(ymd(<<DF>>$<<DATE>>))
<<DF>>$day <- day(ymd(<<DF>>$<<DATE>>))

## Make events column and switch pheno stage column to value?? - ask LW

## Move "retired" comments from block column to notes column OR do we want retiredDate column?


# reorder columns
clean.seb <- select(<<DATA>>, c("company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"))
