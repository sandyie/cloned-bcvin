#Simulating_hardiness from Ferguson et al 2011, but using my own temperature data

rm(list = ls())
setwd("/home/faith/Documents/github/bcvin/hardiness/analyses")

#tempData <- read.csv("input/climhist_19812010.csv")#Carls historical temp data
tempData <- read.csv("input/DummyTempChard.csv")#temp data scraped from Fergusun 2011 figure of chardonay grape temps


#getting temperature data for each day 
fit1 <- lm(temperature ~ poly(Month, 4, raw = TRUE), data = tempData)# get a model that fits the data (mostly)
plot(tempData$temperature ~ tempData$Month)
lines(tempData$Month, predict(fit1, data.frame( x = tempData$Month)), col="red")
day <- 7/221 # number of months/number of dates 
simDays <- data.frame(Month = seq(1,8, day))
simDays$simTemps <- predict(fit1, newdata = simDays)


# a list of parameters needed 
#-------------------------------

i1 <- 2 # dont start on 1 becuase then the follwoing asymptotic (clogs) bounds wont work 
tempValues <- tempData$simTemps # a set temperature values for the winter. I used data from the original manuscript  

#other parameters setimated by the model
hcInit <- -10.55 # start hardiness value. mean of earliest LTE50 in the fall 
hcMax <- -25.24 # maximum hardiness
hcMin <- -3 # min hardiness. Hardiness of green tissues 
tThreshEnd <- 11.75 # threshold temperature for accumilating cold during endodormancy 
tThreshEct <- 3.75 # threshold for accumilating warming during ectodormancy
kaEnd <- 0.14 # constant accumilation rate during endodormancy
kaEct <- 0.07 # constant accumilation rate during ectodormancy
kdEnd <- 0.01 # constant deaccumilation rate during endodormancy
kdEct <- 0.13 # constant deaccumilation during ectodormancy 
edb <- -750 # amount of chilling required to switch from endo to ecto dormancy 

hcChange <- vector()
hcChange[i1-1] <- NA
hc <- vector()
hc[i1-1] <- hcInit

#initiating secondary parameters
cloga <- vector() #the correction putting absolute bounds on change in hc. Equations 5 and 6 of Fergusonetal2011
clogd <- vector()

#tempeperatures below the mean threshold values are chilling days, and above are heating
ddEnd <- tempValues - tThreshEnd 
ddEct <- tempValues - tThreshEct

#make a vector of chilling days only 
ddEndc <- ddEnd
ddEndc[ddEndc > 0 ] <- 0

#ddEndc <- ddEndc * -1 # i think these should be negative values?

ddEctc <- ddEct
ddEctc[ddEctc > 0 ] <- 0
#ddEctc <- ddEctc * -1

#vector of heating days only 
ddEndh <- ddEnd
ddEndh[ddEndh < 0] <- 0

ddEcth <- ddEct
ddEcth[ddEcth < 0] <- 0

#get accumilating degree growing days
accumilatedDD <- cumsum(ddEndc)

#main model for Endodormancy for teh first value i = 2
#------------------------------------
#cloga[i] <- 1 - ((hcMin - hc[i-1])/(hcMin-hcMax))
#clogd[i] <- 1 - ((hc[i-1]-hcMax)/(hcMin - hcMax))
#hcChange[i] <- (ddEndc[i] * kaEnd * cloga[i])+(ddEndh[i] * kdEnd* clogd[i])

#build a loop for all data 

for (i in (2:length(tempValues))){

	#i <- 150

	#calculate the asymptotic bound logistic function 
		cloga[i] <- 1 - ((hcMin - hc[i-1])/(hcMin-hcMax))
		clogd[i] <- 1 - ((hc[i-1]-hcMax)/(hcMin - hcMax))

	#track accumilated degree days to decide if we are in Endo or Ectodormancy
	
	if (accumilatedDD[i] > edb) {

		hcChange[i] <- (ddEndc[i] * kaEnd * cloga[i]) + (ddEndh[i] * kdEnd * clogd[i])

	} else

		hcChange[i] <- (ddEctc[i] * kaEct * cloga[i]) + (ddEcth[i] * kdEct * clogd[i])	

	#get the actual hardiness value 
	hc[i] <- hc[i-1] + hcChange[i]
}

plot(hcChange)
plot(hc)



