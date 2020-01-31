rm(list = ls())
setwd("/home/faith/Documents/github/bcvin/hardiness/analyses")


#trialing my hardiness data on the data used by Ferguson 
# the data came from their excel spredasheet http://wine.wsu.edu/extension/weather/cold-hardiness/

library(rstan)
library(rethinking)
library(truncnorm)

#read in temp data from ferguson et al 2011/2014. It is Chardony data 
chardDataf <- read.csv("input/WashingtonStatecsFergusonData.csv")

head(chardDataf)

#select only the observed data 
chardObserved <- chardDataf[!is.na(chardDataf$Observed_Hc), ]

#get change in hardiness
chardDataf$HardinessChange <- c(0, diff(chardDataf$Predicted_Hc))

#parameter values
tempValues <- chardDataf$T_mean

hcInit <- -10.5 # start hardiness value. mean of earliest LTE50 in the fall 
hcMax <- -25.1 # maximum hardiness
hcMin <- -1.2 # min hardiness. Hardiness of green tissues 
tThreshEnd <- 13 # threshold temperature for accumilating cold during endodormancy 
tThreshEct <- 5 # threshold for accumilating warming during ectodormancy
kaEnd <- 0.142# constant accumilation rate during endodormancy
kaEct <- 0.20 # constant accumilation rate during ectodormancy
kdEnd <- 0.08 # constant deaccumilation rate during endodormancy
kdEct <- 0.10 # constant deaccumilation during ectodormancy 
edb <- -700 # amount of chilling required to switch from endo to ecto dormancy  
theta <- 7 # this is an exponentyial added in teh 2014 paper to help the clogs 

#simulate data based on these parameter values and the temperatures for 2009/2010
#----------------------------------------------------------------------------------

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

	#i <- 2

	#calculate the asymptotic bound logistic function 
		cloga[i] <- 1 - ((hcMin - hc[i-1])/(hcMin-hcMax))^ theta
		clogd[i] <- 1 - ((hc[i-1]-hcMax)/(hcMin - hcMax))^ theta

	#track accumilated degree days to decide if we are in Endo or Ectodormancy
	
	if (accumilatedDD[i] > edb) { # is teh value larger than -700. 

		hcChange[i] <- (ddEndc[i] * kaEnd * cloga[i]) + (ddEndh[i] * kdEnd * clogd[i])

	} else

	hcChange[i] <- (ddEctc[i] * kaEct * cloga[i]) + (ddEcth[i] * kdEct * clogd[i])

	#get the actual hardiness value 
	hc[i] <- hc[i-1] + hcChange[i]
}

plot(hcChange)
plot(hc)	
plot(hc ~ tempValues) # In this plot the data looks more spreadout than i would expect 

# how do my simulated data compare to their predicted values?

plot(hc ~ chardDataf$Predicted_Hc)

plot(chardDataf$Predicted_Hc ~ tempValues)


# the simulated data generally predicts the model well, but there is an issue with early autumn 
# temperatures. In the predicted data from the excel file, bud hardiness starts at -10.3, and then 
# remains at this temperature until day 272. I can't find an explination for this. Accodring to their 
#parameters as I understand them, the hardiness should increase before decreasing because temperatures
# are above the threshold of 13 degrees C for warming. I am going to start teh model at day 272 as see 
# how this affects things

#model from day 272
#--------------------------

#remove dates before 272
chardData272 <- chardDataf[chardDataf$jday > 271, ]

#parameter values
tempValues <- chardData272$T_mean

hcInit <- -10.5 # start hardiness value. mean of earliest LTE50 in the fall 
hcMax <- -25.1 # maximum hardiness
hcMin <- -1.2 # min hardiness. Hardiness of green tissues 
tThreshEnd <- 10 # threshold temperature for accumilating cold during endodormancy 
tThreshEct <- 5 # threshold for accumilating warming during ectodormancy
kaEnd <- 0.142# constant accumilation rate during endodormancy
kaEct <- 0.20 # constant accumilation rate during ectodormancy
kdEnd <- 0.08 # constant deaccumilation rate during endodormancy
kdEct <- 0.10 # constant deaccumilation during ectodormancy 
edb <- -700 # amount of chilling required to switch from endo to ecto dormancy  
theta <- 7 # this is an exponentyial added in teh 2014 paper to help the clogs 

#simulate data based on these parameter values and the temperatures for 2009/2010
#----------------------------------------------------------------------------------

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

	#i <- 2

	#calculate the asymptotic bound logistic function 
		cloga[i] <- 1 - ((hcMin - hc[i-1])/(hcMin-hcMax))^ theta
		clogd[i] <- 1 - ((hc[i-1]-hcMax)/(hcMin - hcMax))^ theta

	#track accumilated degree days to decide if we are in Endo or Ectodormancy
	
	if (accumilatedDD[i] > edb) { # is teh value larger than -700. 

		hcChange[i] <- (ddEndc[i] * kaEnd * cloga[i]) + (ddEndh[i] * kdEnd * clogd[i])

	} else

	hcChange[i] <- (ddEctc[i] * kaEct * cloga[i]) + (ddEcth[i] * kdEct * clogd[i])

	#get the actual hardiness value 
	hc[i] <- hc[i-1] + hcChange[i]
}

plot(hcChange)
plot(hc)	
plot(hc ~ tempValues) # In this plot the data looks more spreadout than i would expect 

# how do my simulated data compare to their predicted values?

plot(hc ~ chardData272$Predicted_Hc)

plot(chardData272$Predicted_Hc ~ tempValues)
plot(chardData272$HardinessChange ~ hcChange)


