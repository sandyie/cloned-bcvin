## Started 28 May 2019 ##
## En route to Knoxville ##
## It's dark outside this plane people, it's dark ##


##edited by Faith Jones 27th Nov 2019

#######################################
## Notes ...
# 'col' means column
#######################################

###########################################################
## To do list (see also hardiness_questionsanswers.txt) ##
## (1) Need to try to make the acclimation/max hardiness/de-acclimation eqns myself
## (2) How did Carl impute missing temp data from Penticton stn?
###########################################################

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/vinmisc/bcvin/hardiness") 
} else setwd("C:/Users/Faith Jones/Documents/ubc/github/bcvin/hardiness")

# libraries
library(reshape)

#source r skripts
source("analyses/ColumnCC_function.R")#Faith's function for making column cc

## Da data
# 2X/month bud hardiness data
# 2-day average of daily mean temperature (Environment Canada Penticton Stn) and its deviation from the 2-day average historical mean temperature

clim <- read.delim("analyses/input/envcanada_penticton.csv", skip=25, sep=",", header=TRUE)
clim$date <- as.Date(clim$Date.Time, format="%m/%d/%y")
clim$month <- format(clim$date, "%b")
clim$day<- format(clim$date,"%d")

#extra climate data taken from the 2012-2018_PENTICTON_WEATHER_EM.xlxs file sent by Carl in 
#an email. this tenperature is used ti till in missing temperatures in clim
#climExtra <- read.delim("analyses/input/2012-2018_PENTICTON_WEATHER_EM.csv", skip=2,sep = ",", header = TRUE)
#names(climExtra)[7] <- "meanTempExtra"
#names(climExtra)[9] <- "meanTempExtra2Day"

# Historical climate (pulled from Carl's Excel file)
histclim <- read.csv("analyses/input/climhist_19812010.csv", header=TRUE)
names(histclim) <- c("Date", "meanC", "meanC2day", "meanC3day")


# faking a year, otherwise it assumes 2019 and the leap day=NA (and breaks my GDD loop)
histclim$doy <- format(as.Date(paste("2016-", histclim$Date), format="%Y-%d-%b"), "%j")
histclim$month <- format(as.Date(paste("2016-", histclim$Date), format="%Y-%d-%b"), "%b")
histclim$day<- format(as.Date(paste("2016-", histclim$Date), format="%Y-%d-%b"), "%d")


# hardiness data
budhardiness2012to13 <- read.csv("analyses/input/budhardiness2012to13.csv", header=TRUE)
budhardiness2013to14 <- read.csv("analyses/input/budhardiness2013to14.csv", header=TRUE)
budhardiness2014to15 <- read.csv("analyses/input/budhardiness2014to15.csv", header=TRUE)
budhardiness2015to16 <- read.csv("analyses/input/budhardiness2015to16.csv", header=TRUE)
budhardiness2016to17 <- read.csv("analyses/input/budhardiness2016to17.csv", header=TRUE)
budhardiness2017to18 <- read.csv("analyses/input/budhardiness2017to18.csv", header=TRUE) 
budhardiness2018to19 <- read.csv("analyses/input/budhardiness2018to19.csv", header=TRUE) 

bh12 <- melt(budhardiness2012to13, id.var=c("X2012...2013", "Variety"))
bh13 <- melt(budhardiness2013to14, id.var=c("X2013...2014", "Variety"))
bh14 <- melt(budhardiness2014to15, id.var=c("X2014...2015", "Variety"))
bh15 <- melt(budhardiness2015to16, id.var=c("X2015...2016", "Variety"))
bh16 <- melt(budhardiness2016to17, id.var=c("site", "Variety")) 
bh17 <- melt(budhardiness2017to18, id.var=c("site", "X2017...2018")) 
bh18 <- melt(budhardiness2018to19, id.var=c("site", "Variety")) 

nameshere <- c("site", "variety", "Date", "lte")
names(bh12) <- nameshere
names(bh13) <- nameshere
names(bh14) <- nameshere
names(bh15) <- nameshere
names(bh16) <- nameshere
names(bh17) <- nameshere
names(bh18) <- nameshere
bh12$years <- "2012to2013"
bh13$years <- "2013to2014"
bh14$years <- "2014to2015"
bh15$years <- "2015to2016"
bh16$years <- "2016to2017"
bh17$years <- "2017to2018"
bh18$years <- "2018to2019"

bhall.rbind <- rbind(bh12, bh13, bh14, bh15, bh16, bh17, bh18)

# remove the averages....
bhall <- subset(bhall.rbind, site!="Average Bud Hardiness (all sites, all varieties)")

# cleaning names
sort(unique(bhall$site))
bhall$site[bhall$site=="Naramata bench"] <- "Naramata Bench"
sort(unique(bhall$variety))
sort(unique(bhall$Date))
# cleaning dates
breakbyperiod <- strsplit(as.character(bhall$Date), ".", fixed=TRUE) 
bhall$Day <- unlist(lapply(breakbyperiod, function(x) x[1]))
bhall$month <- unlist(lapply(breakbyperiod, function(x) x[2]))
bhall$day <- unlist(lapply(strsplit(as.character(bhall$Day), "X", fixed=TRUE), function(x) x[2]))

# right, so now, we need to fix year!
bhall$year <- NA
bhall$year[bhall$years=="2012to2013" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2012
bhall$year[bhall$years=="2012to2013" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2013
bhall$year[bhall$years=="2013to2014" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2013
bhall$year[bhall$years=="2013to2014" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2014
bhall$year[bhall$years=="2014to2015" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2014
bhall$year[bhall$years=="2014to2015" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2015
bhall$year[bhall$years=="2015to2016" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2015
bhall$year[bhall$years=="2015to2016" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2016
bhall$year[bhall$years=="2016to2017" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2016
bhall$year[bhall$years=="2016to2017" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2017
bhall$year[bhall$years=="2017to2018" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2017
bhall$year[bhall$years=="2017to2018" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2018
bhall$year[bhall$years=="2018to2019" & (bhall$month=="Oct"|bhall$month=="Nov"|bhall$month=="Dec")] <- 2018
bhall$year[bhall$years=="2018to2019" & (bhall$month=="Jan"|bhall$month=="Feb"|bhall$month=="Mar"|bhall$month=="Apr")] <- 2019

# and make a useful df
bh <- subset(bhall, select=c("year", "month", "day", "variety", "lte", "site"))
head(bh)

##
## some f(x)s to help calculate GDD
##

## this f(x) makes a column which zeroes out all data
# below your threshold temperature

makethreshold.data <- function(dater, temp.col, thresh){ 
    ifelse(dater[[temp.col]]>thresh,
       (dater[[temp.col]]-thresh), 0)
  }

makestartdatecounter <- function(dater, monthcol, daycol, countercol, whatmon, whatday){
    dater[[countercol]][which(dater[[monthcol]]==whatmon & dater[[daycol]]==whatday)] <- 0
    for(j in c(2:nrow(dater))){
        if(is.na(dater[[countercol]][j])){
        dater[[countercol]][j] <- dater[[countercol]][j-1]+1
        }
    }
    return(dater)
}


clim$seasonday <- NA
clim <- makestartdatecounter(clim, "Month", "Day", "seasonday", 9, 1)
head(clim)
## this f(x) adds up gdd
## requires data ordered by doy (I do this in the loop below)
## this f(x) returns the value while treating NA as zeroes
# needstartdate is when you require data to start that year, otherwise it returns NA
# for example, 5 means you need data that starts before 5 January to actually count
if(FALSE){ # for testing below function
dater <- histclim
gdd.col <-  "gddtemp"
doy.col <- "doynum"
startdate <-  1
needstartdate <- 366
    }

makegdd.data.skipNA <- function(dater, gdd.col, doy.col, startdate, needstartdate){
     saveme <- c()
     for(i in 1:nrow(dater)){
     # start the counter with the starting doy of the data ...
     # j <- dater[[doy.col]][1]
     # deal with cases where the data start after Jan 1
     if (dater[[doy.col]][1]>needstartdate) saveme[i] <- NA
     else
     # deal with cases where the entire column is NA
     if (sum(is.na(dater[[gdd.col]]))==length(dater[[gdd.col]])) saveme[i] <- NA
     else
     # deal with cases before startdate
     if (dater[[doy.col]][i]<startdate) saveme[i] <- NA
     else
     # okay, finally calculate the GDD
     if (dater[[doy.col]][i]==startdate) saveme[i] <- (dater[[gdd.col]][i])
     else
     # if a cell is NA, just add 0 instead of the cell
     if (is.na(dater[[gdd.col]][i])) saveme[i] <- (0+saveme[i-1])
     else
     saveme[i] <- (dater[[gdd.col]][i]+saveme[i-1])
 }
 return(saveme)
}

## And some code to make 2-day rolling average climate data
n <- 2
cx <- c(0,cumsum(histclim$meanC)) # https://stackoverflow.com/questions/743812/calculating-moving-average
rsum <- (cx[(n+1):length(cx)] - cx[1:(length(cx) - n)]) / n

# Add in GDD and rolling averages of meanC
histclim$doynum <- as.numeric(histclim$doy)
histclim$gddtemp <- makethreshold.data(histclim, "meanC", 5)
histclim$gdd <- makegdd.data.skipNA(histclim, "gddtemp", "doynum", 1, 366) # or 288, 366 if you want it to do first part of data

histclim$meanC2day <- c(NA, rsum)

climsm <- subset(clim, select=c("Year", "month","day", "Mean.Temp..C.", "Mean.Temp.Flag", "date", "seasonday"))
names(climsm) <- c("Year", "month","day", "meanC", "meanC.flag", "date", "seasonday")

climsm$doy <- format(climsm$date, "%j")
climsm$doynum <- as.numeric(climsm$doy)
climsm[climsm$month == "Mar" & climsm$day == "01",]

#remove the feb 29th day rows from the database now we have days of the year sorted
climsm[!climsm$Year == 2016 & climsm$doy == ]


# impute missing meanC data! (not using MICE since it overwrites rbind) and our imputation is easy
meanimpute <- function(df, colname){
for (i in 1:nrow(df)){
   if (is.na(df[[colname]][i])==TRUE) { # impute given data in immediate surrounding cells
       df[[colname]][i] <- sum(df[[colname]][i-1]+ df[[colname]][i+1])/2
        }
   if (is.na(df[[colname]][i])==TRUE) { # impute given data in surrounding cells further away
       df[[colname]][i] <- sum(df[[colname]][i-2]+ df[[colname]][i+2])/2
        }
    if (is.na(df[[colname]][i])==TRUE) { # impute given data in surrounding cells even further away
       df[[colname]][i] <- sum(df[[colname]][i-3]+ df[[colname]][i+3])/2
        }
       }
   return(df)
}
climsm$meanC.imp <- climsm$meanC

climsm <- meanimpute(climsm, "meanC.imp")
climsm <- meanimpute(climsm, "meanC.imp")

plot(meanC.imp~meanC, data=climsm)


climsm$gddtemp <- makethreshold.data(climsm, "meanC.imp", 10)
climsm$gdd <- makegdd.data.skipNA(climsm, "gddtemp", "doynum", 1, 366) # or 1, 366

cx <- c(0, cumsum(climsm$meanC.imp)) # https://stackoverflow.com/questions/743812/calculating-moving-average
rsum <- (cx[(n+1):length(cx)] - cx[1:(length(cx) - n)]) / n
climsm$meanC2day <- c(NA, rsum)


# START here ... fix so it does all years and dates possible. Then go down to next START here ....

## Merge the climate datasets ##

#There are leap years in some of teh data and also the historical data, so we cant include doy 
#as a merge column because after feb teh doy are different for leap years and non leap years
#now the only NAs for historical data are the non winter dates 

climall <- merge(climsm, histclim, by=c("month", "day"), all.x=TRUE, suffixes=c("", ".hist"))
climall <- climall[order(climall$date),]
climall$avgTdiff <- climall$meanC2day-climall$meanC2day.hist # column CC (looks good, except where I imputed) -- created by Q-M (note that there are lots of NA since we only have historical data for mid Sept to Apr, but we only show it through end of February ... WHY?


#############################################################
## Acclimation, max hardiness and de-acclimation equations ##
#############################################################

## Here's the spot in the code where I wondered about how to generate the acclimation, max hardiness and de-acclimatoon curves
# see my notes on this in hardiness_questionsnotes ... we'll just type them in for now ...
chard <- subset(bh, variety=="Chardonnay")

eqacc <- function(x){
    0.121*x^2 + 0.4898*x -23.175
} 
eqmaxh <- function(x){
    0.5371*x - 23.229
}
eqdeacc <- function(x){
    0.232*x^ + 0.0314*x - 23.336
}

# If acclimation change in one day is smaller than -0.5 (e.g., if it's -2 or such) then make it max out at -0.5 ...
# Actually the f(x) is more flexible than that but that's the current usage
makediffs.lte <- function(df, colname, newcolname, maxdiffcol, maxdiff){
    # First find the day to start accumulating on ...
    whichrows <- which(df[["seasonday"]]==1)
    for (i in c(whichrows[1]:nrow(df))){
        df[[newcolname]][i] <- df[[colname]][i]-df[[colname]][i-1]
    }
    df[[maxdiffcol]][which(df[[newcolname]]<maxdiff)] <- maxdiff
    df[[maxdiffcol]][which(df[[newcolname]]>maxdiff)] <- df[[newcolname]][which(df[[newcolname]]>maxdiff)]  
    return(df)
}


# Okay, now let's try to calculate! No accumulation before 21 September

# Working on CD-CO columns in Carl's xls .... if I can get through these than I should understand the process better ... 
# have done column CC (diff from historical), which is used for CD and CE (have not done)

## START HERE! (once the above start here is done) # below looks pretty good, start trying to create CD onward (CC is okay, but off around imputed dates) ... currently on CD -- have done early part of year need to do new eqn on 8 Dec, and then ANOTHER new eqn on 7 Jan

climall$acc <- eqacc(climall$meanC2day.hist) # col CA is using the equations and the 2-day historical climate (BX)
climall$accdiff <-NA # change in estimated LTE each day
climall$accdiffmax <-NA # should be same as col CB

climall <- makediffs.lte(climall, "acc", "accdiff", "accdiffmax", -0.5)
climall[58:77,] # looking good...
#values for LTE/day (accdiffmax/CB) < 0.1 or > -0.1 are converted to 0.1 and -0.1 (assumption 5)   
climall$accdiffmax[climall$accdiffmax < 0.1 & climall$accdiffmax > 0] <- 0.1
climall$accdiffmax[climall$accdiffmax > -0.1 & climall$accdiffmax < 0] <- -0.1

#climall$adjustcd <- NA

# Column CD tdiff less than zero
# need to add while loop for dates ...
# https://stackoverflow.com/questions/32610271/looping-over-dates-with-r
adjustcd <- function(df){
    adjustcd <- c()
    for(i in c(1:nrow(df))){
    if(is.na(df[["avgTdiff"]][i])==TRUE) {
        adjustcd[i] <- NA} else {
    if(df[["avgTdiff"]][i]< -7) {
        adjustcd[i] <- 1.8} else {
    if(df[["avgTdiff"]][i]< -5) {
        adjustcd[i] <- 1.4} else {
    if(df[["avgTdiff"]][i]< -4) {
        adjustcd[i] <- 1.3} else {
    if(df[["avgTdiff"]][i]< -3) {
        adjustcd[i] <- 1.2} else {
    if(df[["avgTdiff"]][i]< -2) {
        adjustcd[i] <- 1.1} else {
        if(df[["avgTdiff"]][i]< -0) {
            adjustcd[i] <- 1} else {
                adjustcd[i] <- 0} # need to check this very last statement is correct
               }
              }
            }
          }
        }
     }
    }
    return(adjustcd)
}

#climall$adjustcd <- adjustcd(climall) # quick glance: looks fine...

#-------------------------------------------------------
#Faith's attempts at the columns CD - CJ
#---------------------------------------------------

#colum CD has different if/and statements different periods of time
#for Sep to Dec 7. (acclimation?)
#=IF(CC27<-8,1.8,IF(CC27<-5,1.6,IF(CC27<-4,1.4,IF(CC27<-3,1.2,IF(CC27<-2,1.1,IF(CC27<0,1,0))))))

#for Dec 8 to Jan 6 (Max hardiness?)
#=IF(CC104<-4,-1.4,IF(CC104<-3,-1.3,IF(CC104<-2,-1.25,IF(CC104<-1,-1.2,IF(CC104<0,-1.15,1)))))

#Jan 7 - Feb 6 (Max hardiness also?)
#=IF(CC165<-9,CB165*-2,IF(CC165<-7,CB165*-1.5,IF(CC165<-5,CB165*-1.1,IF(CC165<-4,CB165*-1,
#	IF(CC165<-3,CB165*-0.5,IF(CC165<-2,CB165*0.2,IF(CC165<-1,CB165*1.1,1))

#Feb 7 - Apr 15 (Deacc?)
#=IF(CC228<-9,CB228*-2,IF(CC228<-7,CB228*-1.5,IF(CC228<-5,CB228*-1.1,
3	IF(CC228<-4,CB228*-1,IF(CC228<-3,CB228*-0.5,IF(CC228<-2,CB228*0.2,IF(CC228<-1,CB228*1.1,1)))))))

#Making a column for the different periods of time using a ridiculous amount of steps 
#--------------------------------------------------------------------------------------

#select days of teh year that correspond to the different periods of time
#this is different for leap and non leap years after Febuary 

nYear <- length(unique(climall$Year))
doyEachPeriod <- data.frame(matrix(NA,nYear , 11))
names(doyEachPeriod ) <- c("Year", "accStart","accEnd", "maxStart", "maxEndDec", "maxStartJan", "maxEndJan", "maxStart2Jan", "maxEnd2Jan", "deaccStart", "deaccEnd")
doyEachPeriod$Year <- unique(climall$Year)

doyEachPeriod$accStart <- climall$doynum[climall$month == "Sep" & climall$day == "21"]
doyEachPeriod$accEnd <- climall$doynum[climall$month == "Dec" & climall$day == "07"]

doyEachPeriod$maxStart <- climall$doynum[climall$month == "Dec" & climall$day == "08"]
doyEachPeriod$maxStart <- as.numeric(doyEachPeriod$maxStart )

doyEachPeriod$maxEndDec[!doyEachPeriod$Year == 2016] <- 365
doyEachPeriod$maxEndDec[doyEachPeriod$Year == 2016] <- 366 #leap year

doyEachPeriod$maxStartJan[!doyEachPeriod$Year == 2012] <- 1 #always start first of january. no data for 2012
doyEachPeriod$maxStartJan<- as.numeric(doyEachPeriod$maxStartJan)

doyEachPeriod$maxEndJan[!doyEachPeriod$Year == 2012] <- 6 #always six of january . no data for 2012
doyEachPeriod$maxEndJan<- as.numeric(doyEachPeriod$maxEndJan)

doyEachPeriod$maxStart2Jan[!doyEachPeriod$Year == 2012] <- 7 # no data for 2012
doyEachPeriod$maxStart2Jan<- as.numeric(doyEachPeriod$maxStart2Jan)

doyEachPeriod$maxEnd2Jan[!doyEachPeriod$Year == 2012] <- climall$doynum[climall$month == "Feb" & climall$day == "06"] #always six of january . no data for 2012
doyEachPeriod$maxEnd2Jan<- as.numeric(doyEachPeriod$maxEnd2Jan)

doyEachPeriod$deaccStart[!doyEachPeriod$Year == 2012] <- climall$doynum[climall$month == "Feb" & climall$day == "07"]#no jan 2012 data
doyEachPeriod$deaccStart<- as.numeric(doyEachPeriod$deaccStart)

doyEachPeriod$deaccEnd[!doyEachPeriod$Year == 2012]<- climall$doynum[climall$month == "Apr" & climall$day == "15"]#no jan 2012 data
doyEachPeriod$deaccEnd<- as.numeric(doyEachPeriod$deaccEnd)

#make a column in climall to put the period data in 
climall$HardinessPeriod <- NA

#2012 - do first because only acclimation data 
#take the row for 2012 only 

periods2012 <- doyEachPeriod[doyEachPeriod$Year == 2012, ]
climall$HardinessPeriod[climall$doynum %in% periods2012 $accStart:periods2012 $accEnd & climall$Year == 2012] <- "Acc"
climall$HardinessPeriod[climall$doynum %in% periods2012 $maxStart:periods2012 $maxEndDec& climall$Year == 2012] <- "Max"


#2013 onwards 

str(climall)
str(doyEachPeriod)

for (year in unique(climall$Year[!climall$Year == 2012])){

	periodsYear <- doyEachPeriod[doyEachPeriod$Year == year , ]
	climall$HardinessPeriod[climall$doynum %in% periodsYear$accStart:periodsYear$accEnd & climall$Year == year] <- "Acc"
	climall$HardinessPeriod[climall$doynum %in% periodsYear$maxStart:periodsYear$maxEndDec & climall$Year == year] <- "Max"
	climall$HardinessPeriod[climall$doynum %in% periodsYear$maxStartJan:periodsYear$maxEndJan & climall$Year == year] <- "Max"
	climall$HardinessPeriod[climall$doynum %in% periodsYear$maxStart2Jan:periodsYear$maxEnd2Jan & climall$Year == year] <- "Max2"
	climall$HardinessPeriod[climall$doynum %in% periodsYear$deaccStart:periodsYear$deaccEnd & climall$Year == year] <- "Deacc"
}

#Add column CC to the climall dataset
#note - at teh momment it doesnt perfectly match the original spreadsheet because of teh different ways we have filled in missing temp data

climall$CD <- adjustcd(climall$HardinessPeriod, climall$avgTdiff, climall$accdiffmax)
View(climall)








