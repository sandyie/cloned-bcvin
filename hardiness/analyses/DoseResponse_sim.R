rm(list = ls())

#Simulating a new winter hardiness model, thi sone based on a dose response curve. 
#Model is based on teh one in Ritz et al, 2015. PLOS one. 

#script started by Faith Jones on June 26th, 2020
#First focus on vust varieties rather than site and variety 


library(rstan)
library(truncnorm)
library(reshape2)
library(ggplot2)
library(tidyr)
library(dplyr)
library(drc)
library(bayesplot)# nice posterior check plots 
library(rethinking) # for functiosn to get hpdi


library(foreach)
library(doParallel)
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

set.seed(16)


if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/vinmisc/bcvin/hardiness/analyses/") 
} else
setwd("/home/faith/Documents/github/bcvin/bcvin/hardiness/analyses/")

#Utilities writen by Michael Betancourt
util <- new.env()
source('fromCourseMB/stan_utility.R', local=util)


#climate data
clim <- read.delim("input/envcanada_penticton.csv", skip=25, sep=",", header=TRUE)
clim$date <- as.Date(clim$Date.Time, format="%m/%d/%y")
clim$month <- format(clim$date, "%b")
clim$day<- format(clim$date,"%d")
head(clim)

#Replace NA temps with mean from day before and after for min and max temps 
for(i in 1:nrow(clim)){
	if(is.na(clim$Max.Temp..C.[i])){
		clim$Max.Temp..C.[i] <- mean(clim$Max.Temp..C.[i-1] + clim$Max.Temp..C.[i+1])
	}
	if(is.na(clim$Min.Temp..C.[i])){
		clim$Min.Temp..C.[i] <- mean(clim$Min.Temp..C.[i-1] + clim$Min.Temp..C.[i+1])
	}
}



#Make mean temp for last 24 hours by sing yesterdays max and today's min. the logic behind thsi is that Carl takes measurements mid morning. 
for(i in 1:nrow(clim)){
	if(i == 1){#If toay is first temp listed then ise max from today as well as min from today
		clim$mean1DayC[i] <- clim$Max.Temp..C. [i] + clim$Min.Temp..C.[i])/2
	#}else if (is.na(clim$Max.Temp..C. [i])){
#		clim$mean1DayC[i] <- clim$Mean.Temp..C.[i]
#			} else if (is.na(clim$Max.Temp..C. [i-1])){ # if there is no max temp for yesterday then I will use toaday's max temp
#		clim$mean1DayC[i] <- (clim$Max.Temp..C. [i] + clim$Min.Temp..C.[i])/2
	} else clim$mean1DayC[i] <-  (clim$Max.Temp..C. [i-1] + clim$Min.Temp..C.[i])/2
}

is.na(clim$Max.Temp..C.)
is.na(clim$Mean.Temp..C.)
plot(clim$Max.Temp..C. ~ clim$Mean.Temp..C.)


climsm <- subset(clim, select=c("Year", "month","day", "Mean.Temp..C.", "Mean.Temp.Flag", "date", "Date.Time", "mean1DayC"))
names(climsm) <- c("Year", "month","day", "meanC", "meanC.flag", "date", "Date.Time", "mean1DayC")

#Washington state hardiness data
otherCabSovData <- read.csv("input/WashingtonHardinessData_Ferguson.csv")



# hardiness data
#--------------------------
budhardiness2012to13 <- read.csv("input/budhardiness2012to13.csv", header=TRUE)
budhardiness2013to14 <- read.csv("input/budhardiness2013to14.csv", header=TRUE)
budhardiness2014to15 <- read.csv("input/budhardiness2014to15.csv", header=TRUE)
budhardiness2015to16 <- read.csv("input/budhardiness2015to16.csv", header=TRUE)
budhardiness2016to17 <- read.csv("input/budhardiness2016to17.csv", header=TRUE)
budhardiness2017to18 <- read.csv("input/budhardiness2017to18.csv", header=TRUE) 
budhardiness2018to19 <- read.csv("input/budhardiness2018to19.csv", header=TRUE) 

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

#make a date column 
bh$Date <- paste(bh$month, bh$day, bh$year, sep = "/")
bh$Datestrptime <- as.POSIXct(strptime(bh$Date ,format="%b/%d/%Y"))
climsm$Datestrptime <- as.POSIXct(strptime(climsm$Date.Time,format="%m/%d/%y"))
#note, dates that are in strptime format rather than as,POSIXct cannot be used to merge 

#combine datasets
bhclim <- merge(bh, climsm, by.x = "Datestrptime", by.y = "Datestrptime") 
bhclim$Month_num <- format(as.Date(bhclim$Datestrptime), "%m")
bhclim$month_day <- as.numeric(paste(bhclim$Month_num, bhclim$day.x, sep = "."))

#set columns as factors
bhclim$year <- as.factor(bhclim$year)
bhclim$variety <- as.factor(bhclim$variety)

plot(bhclim$lte ~bhclim $Datestrptime)
plot(bhclim$lte ~bhclim $month_day )
plot(bhclim$lte ~bhclim $mean1DayC)

tail(bhclim)



##simulate temperature data 
#-------------------------------------------------

#inputs
nrep <- 500 # number of reps of each variety 
meanTemp <- 4
sigmaTemp <- 8
simTemps <- rnorm(nrep, meanTemp,sigmaTemp)


#make temperatures positive because teh x value cant be negative. I do this by adding 20 to the data
# I will also model teh hardiness resonse as inverted sp plants get a higher number as they get more cold hardy rather than a more negative number
simTempsPos <- simTemps + 30

#Set model parameters - I have no idea what they should be so I will run a similar model in R (below)

b <- 11 #this is the rate paramater, like a slope in a linear regession 
d <- 24 # maximum hardiness (inverted from -24)
c <- 3 # minimum hardiness (inverted from -10)
e <- 37 # Effective dose ED50. x value where y value is halfway bewteen max(d) and min (c)
sigma_g <- 2

x <- simTempsPos

hardinessPos <- c + ( (d-c) / (1 + exp(b*(log(x)-log(e)))))
eps <- rnorm(n = length(hardinessPos), mean = 0, sd = sigma_g)
simLTEPos <- hardinessPos + eps

hardiness <- (-1 * hardinessPos)
simLTE <- simLTEPos * -1

plot(simLTEPos ~ simTempsPos, pch = 16, col = 2, xlab = "Simulated temperatures plus 30", ylab = "winter hardiness * -1")#modefied positive  data
plot(simLTE ~ simTemps, pch = 16, col = 3, xlab = "Simulated temperatures", ylab = "winter hardiness")#changed back to negative values




#Get sensible parameters by running a real dose response curve model on my data
#--------------------------------------------------------------------------
bhclim$ltePositive <- bhclim$lte * -1 # I want positive values?
bhclim$meanC20 <- bhclim$meanC + 30 # make suer all temperatures are positive 

drmHardiness <- drm(ltePositive  ~ meanC20, data = bhclim, fct = LL.4())
summary(drmHardiness)
plot(drmHardiness)

predictedHardiness <- data.frame(predict(drmHardiness, newdata=expand.grid(simTempsPos), interval="confidence"))
names(predictedHardiness)

#Plot model results
ggplot(bhclim, aes(x = meanC20, y = ltePositive)) +geom_point() +
	geom_ribbon(data=predictedHardiness, aes(x=simTempsPos, y=Prediction, ymin=Lower, ymax=Upper), alpha=0.2) +
	geom_line(data=predictedHardiness, aes(x=simTempsPos, y=Prediction))+
	xlab("Air temp plus 30 degrees") + 
	ylab("inverted hardiness (-LTE50)")

predictedHardiness$PredictedNegative <- -1*predictedHardiness$Prediction
predictedHardiness$LowerNegative <- -1*predictedHardiness$Lower
predictedHardiness$UpperNegative <- -1*predictedHardiness$Upper

#Plot model results with hardiness and air temp back how they were originally 
ggplot(bhclim, aes(x = meanC, y = lte)) +geom_point() +
	geom_ribbon(data=predictedHardiness, aes(x=simTemps, y=PredictedNegative, ymin=LowerNegative, ymax=UpperNegative), alpha=0.2) +
	geom_line(data=predictedHardiness, aes(x=simTemps, y=PredictedNegative))+
	xlab("Air temp (degrees C)") + 
	ylab("cold hardiness (LTE50)") +
	theme_classic()+
    theme(text = element_text(size=20))

bhclim$meanC30 <-  bhclim$meanC + 30
bhclim$ltePos <-  -bhclim$lte 

#Plot model results with hardiness and air temp back how they were originally 
ggplot(bhclim, aes(x = meanC30, y = ltePos)) +geom_point() +
	xlab("Air temp (degrees C) + 30") + 
	ylab("-cold hardiness (LTE50)") +
	theme_classic()+
    theme(text = element_text(size=20))



#
#https://discourse.mc-stan.org/t/dose-response-model-with-partial-pooling/13823

#Try worksflow from stan course


############################################################
# 1. Conceptual Analysis
############################################################

#what are we measuring? Describe system. 

#Winter hardiness - minimum LTE50 - as a response to air temperature
# both hardiness (y) and air temp (x) can be negative, and are real numbers 
# The relationship between y xan x will not be negative
# I expect a non-linera respons,e in that hardiness will have an upper and lower asumptote. 
# The lower asymptote (mimimum hardiness) will not be above 0. It might be -3 (the hardiness 
# of green tissue) Or I could use 0.
# My measurements of cold hardiness do not cover asumytoete around minimum hardiness because 
# they dont gather hardiness data during teh summer when hardiness will be at its lowest - so 
# maybe I just chose a lower asymptote instead of uing minimum value from data? 

############################################################
# 2. Define Observational Space
############################################################

#what mathematical form do our observations take?  
#how many observations?
# for example, what is N, x and y? 
# what distribution should I use for y against likelyhood/x? 
# I assume a gaussian observational model because there is nothing special 
#about how the data is formed. 
	#For teh likelyhood I am going use use a dose response curve model, although 
	#this will mean changing the data so it is all positive 

############################################################
# 3. Construct Summary Statistics
############################################################

# Here we use our domain knowledge to decide what would be unreasonable values for the 
# different summary statistcs

# histogram of each parameter 
# Plot x againt real and predicted y values
# histogram of predicted y values



#######################################################################################
#
# Post-Model, Pre-Data
#
#######################################################################################


############################################################
# 4. Model Development
############################################################

#Chose an observation model. This is the y ~ model(likelyhood_x) bit. Chose one that 
#make sense based on the data type (integer? real? negative values? count? ordered data?)

# Chose priors based on the bit before when we were using our domian knowledge to 
# decide what values are ok vs really off. Stan has a function to solve algebra that 
# can be used to find prior values that give the range wanted for the prior. 

#below parematers are teh values before changing to positives! Remember to add 100 x to model values, and tiomes y values with -1!
#Parameters:
	# d = the higher asymptote. Should be between -10 and -50 (10 and 50). Mean 30, sd 10. 
	# c = the lower asymptote. should be between 0 and -5. 0 and 5. Mean 0, half normal, sd of 2.  
	# b = teh response intensity. A scaler. SHoudl be positive, but not sure of values. From playing with values om going to say 10 plus/minus sd 5  
	# e = x where y is half way between c and d. Should be centred around mean of x, plus/minus 10. But goes in as log e, so need to consider that
	# sigma_g = observation error from the gaussian process model. Not sure, but probably plus/minus 10 degrees?

#Maybe I shoudl have a lognormal prior for c because I know that a minimum hardiness of exactly 0 is unlikely.
# physiologically, green tissue is somewhat hardy to cold air temperatures
logc_Prior<- rlnorm(1000, meanlog = 3, sdlog = 1)
hist(log(logc_Prior)	)
gammaCPrior <- rgamma(1000, shape = 3, rate = 1)
hist(gammaCPrior)
gammabPrior <- rgamma(1000, shape = 7, rate = 1)

hist(gammabPrior, main = "b Prior",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

#Plot showing how gamma changes
gammaCPrior1 <- rgamma(1000, shape = 1, rate = 1)
gammaCPrior2 <- rgamma(1000, shape = 2, rate = 1)
gammaCPrior3 <- rgamma(1000, shape = 3, rate = 1)

gammaCPrior4 <- rgamma(1000, shape = 2, rate = 1)
gammaCPrior5 <- rgamma(1000, shape = 2, rate = 2)
gammaCPrior6 <- rgamma(1000, shape = 2, rate = 3)

par(mfrow=c(2,3))

plot(density(gammaCPrior1), main = "shape = 1, rate = 1",  xlim = c(-0.5, 12), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
plot(density(gammaCPrior2), main = "shape = 2, rate = 1",  xlim = c(-0.5, 12), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
plot(density(gammaCPrior3), main = "shape = 3, rate = 1",  xlim = c(-0.5, 12), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

plot(density(gammaCPrior4), main = "shape = 2, rate = 1",  xlim = c(-0.5, 12), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
plot(density(gammaCPrior5), main = "shape = 2, rate = 2",  xlim = c(-0.5, 12), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
plot(density(gammaCPrior6), main = "shape = 2, rate = 3",  xlim = c(-0.5, 12), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

par(mfrow=c(1,1))

plot(density(gammaCPrior), main = "c Prior (gamma) shape = 3, rate = 1",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
plot(density(gammaCPrior), main = "c Prior (gamma)",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
plot(density(gammabPrior), main = "beta Prior (gamma) shape = 7, scale = 0.75",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

#Run a model with priors and simulate y values from it, so try and chose sensible priors

#Data
x <- I(simTempsPos)
N <- length(simTempsPos)

stanData_priot_drs <- list(N = N, x = x)

mean(x)

#Plot each prior
d_Prior <- rtruncnorm(n = 1000 ,a=0, mean = 25, sd = 10)
c_Prior <- rtruncnorm(n = 1000 ,a=0, mean = 2, sd = 0.5)# I should as Cat and Carl and growers what they think this is  
ehat_Prior <- rtruncnorm(n = 1000 ,a=0, mean = log(30), sd = 0.15)#centred around mean temperature, se 20
sigma_g_Prior <- rtruncnorm(n = 1000,a=0, mean = 0, sd = 2)
b_Prior <- gamma(n = 1000, shape = 7, rate = 1)

e_Prior <- exp(ehat_Prior)

 
d_var_sigma <- rgamma(1000, shape = 2.5, rate = 1.75)
d_var_raw <- rnorm(1000, mean = 0, sd = 1)
 
b_var_sigma <- rnorm(1000, mean = 0, sd = 3)
b_var_raw <- rnorm(1000, mean = 0, sd = 1)
  
d_site_sigma <- rgamma(1000, shape = 2.5, rate = 1.75)
d_site_raw <- rnorm(1000, mean = 0, sd = 1)

log(20)

hist(d_Prior, main = "d Prior",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(c_Prior, main = "c Prior",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(ehat_Prior, main = "ehat Prior (sigma 0.15)",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(e_Prior, main = "e Prior (sigma 0.15)",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(sigma_g_Prior, main = "sigma_g Prior",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
#plot(density(b_Prior), main = "beta Prior",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(gammabPrior, main = "b Prior",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)


hist(c_Prior)
hist(ehat_Prior)#-60 is pretty unlikley, but not I think impossibel enouph. Maybe i coudl tighten this prior
hist(e_Prior)
hist(b_Prior)
hist(sigma_g_Prior)
hist(exp(ehat_Prior))
hist(log(x))
mu_y_Prior <- x * 0




#Run the model just once to see if I am in the right ball park 
for(i in 1:N){

	mu_y_Prior[i] <- c_Prior + ((d_Prior - c_Prior)/(1+exp(b_Prior * (log(x[i]) - loge_Prior))))
}

hist(mu_y_Prior*-1)
plot( (x-30),(mu_y_Prior*-1))

#Check model to run prior check in stan using generated quantities 
priorModel <- writeLines(readLines("stan/doseResponse_priorCheck.stan"))

R <- 1000
#thsi model uses the positive transfomed data!
drc_prior <- stan(file = "stan/doseResponse_priorCheck.stan", data = stanData_priot_drs, 
	iter=R, warmup=0, chains=1, refresh=R,
    seed=4838282, algorithm="Fixed_param")
priorCheck <- rstan::extract(drc_prior)

hist(priorCheck$mu_y)
hist(priorCheck$y_sim) # very few values above 0. That is good. 
hist(priorCheck$ehat)
hist(priorCheck$e)

str(priorCheck$y_sim)
meanSimY <- colMeans(priorCheck$y_sim) 
quantsSimY <- apply( priorCheck$y_sim , 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

extremes <- quantsSimY[c(2, 5),]
quaters <- quantsSimY[c(3, 4),]

plottingDataPrior <- data.frame(t(quantsSimY) * -1)
plottingDataPrior$MeanY <- meanSimY * -1
plottingDataPrior$x <- plotingTempsCold


priorTempsPlot <- ggplot(data = plottingDataPrior, aes(x = x, y = MeanY ))
priorTempsPlot + 
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()

hist(plottingDataPrior$MeanY)
hist(plottingDataPrior$X95.)
hist(plottingDataPrior$X5.)


#Fit simulated data to my model using priors I checked above
#---------------------------------------


x <- I(simTempsPos)
y <- simLTEPos # this data is simulated without variety variation  
N <- length(simTempsPos)

stan_data_drs <- list(N = N, x = x, y = y)



#data passed to STan needs to be a list of named objects. names here need to match names in model code
#i make a LIST of the different varables, NOT data frame


writeLines(readLines("stan/doseResponseSimple2.stan"))

#thsi used the positive transfomed data!
drc_simple <- stan(file = "stan/doseResponseSimple2.stan", data = stan_data_drs, warmup = 4000, 
	iter = 7000, chains = 4, cores = 4, thin = 1)

drcPost <- rstan::extract(drc_simple)

#how do the posteriors look?
pairs(drc_simple, pars = c("b", "ehat","d", "c", "sigma_g", "lp__")) 


#How does teh predicted data look?
str(drcPost)
mu_post <- drcPost$mu_y * -1
hist(mu_post)
plot(colMeans(mu_post) ~ simTemps)

y_sim <- drcPost$y_sim * -1
hist(y_sim)
meanySimPost <- apply(y_sim, 2, mean)
quantsSimYPost <- apply( y_sim, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

extremesP <- quantsSimYPost[c(2, 5),]
quatersP <- quantsSimYPost[c(3, 4),]

plottingDataPost <- data.frame(t(quantsSimYPost))
plottingDataPost$MeanY <- meanySimPost 
plottingDataPost$x <- simTemps


postTempsPlot <- ggplot(data = plottingDataPost, aes(x = x, y = MeanY ))
postTempsPlot + 
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()+
	geom_point(aes(x = simTemps, y = simLTE))




plot(colMeans(y_sim) ~ simTemps)


#How do the predicted values compare to real parameter values? (plot hist with real parameter value)

e_post <- exp(drcPost$ehat)
hist(e_post)
abline(v=e,col="red", lty = 2, lwd = 2)

b_post <- drcPost$b
hist(b_post)
abline(v=b,col="red", lty = 2, lwd = 2)

c_post <- drcPost$c
hist(c_post)
abline(v=c,col="red", lty = 2, lwd = 2)

d_post <- drcPost$d
hist(d_post)
abline(v=d ,col="red", lty = 2, lwd = 2)

sigma_post <- drcPost$sigma_g
hist(sigma_post)
abline(v=sigma_g,col="red", lty = 2, lwd = 2)

#how does predicted data compare to real data (plot z scores) 

# ------  get the z score for each value 

meanMuPostY <- apply(y_sim, 2, mean)
sdMuPostY <- apply(y_sim, 2, sd)

zScoreY <-  (simLTE - meanMuPostY) / sdMuPostY

plot(zScoreY ~ simLTE)
plot(meanMuPostY ~ simLTE)


#Try with real data
#---------------------------------
#remove na rows
bhclimClean2 <- bhclim[!is.na(bhclim$lte),]

realy <- bhclimClean2$lte * -1
realx <- I(bhclimClean2 $meanC + 30)
N <- length(realx)

stan_data_drs_real <- list(N = N, x = realx, y = realy)

#thsi used the positive transfomed data!
drc_simple_real <- stan(file = "stan/doseResponseSimple2.stan", data = stan_data_drs_real, warmup = 1000, 
	iter = 2000, chains = 4, cores = 4, thin = 1)

pairs(drc_simple_real, pars = c("b", "ehat","d", "c", "sigma_g", "lp__")) 



drcPost <- rstan::extract(drc_simple_real)

#how do the posteriors look?
#pairs(drc_simple)

#How does teh predicted data look?
str(drcPost)
mu_post <- drcPost$mu_y * -1
hist(mu_post)
plot(colMeans(mu_post) ~ bhclimClean2 $meanC )

y_sim <- drcPost$y_sim * -1
hist(y_sim)
meanySimPost <- apply(y_sim, 2, mean)
quantsSimYPost <- apply( y_sim, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

extremesP <- quantsSimYPost[c(2, 5),]
quatersP <- quantsSimYPost[c(3, 4),]

plottingDataPost <- data.frame(t(quantsSimYPost))
plottingDataPost$MeanY <- meanySimPost 
plottingDataPost$x <- bhclimClean2 $meanC 


postTempsPlot <- ggplot(data = plottingDataPost, aes(x = x, y = MeanY ))
postTempsPlot + 
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()+
	geom_point(aes(x = bhclimClean2 $meanC , y = bhclimClean2$lte ))



plot(colMeans(y_sim) ~ simTemps)

# ------  get the z score for each value 

meanMuPostY <- apply(y_sim, 2, mean)
sdMuPostY <- apply(y_sim, 2, sd)

zScoreY <-  (bhclimClean2$lte  - meanMuPostY) / sdMuPostY

plot(zScoreY ~ bhclimClean2$lte )
plot(meanMuPostY ~ bhclimClean2$lte )



#How do the predicted values compare to real parameter values? (plot hist with real parameter value)

e_post <- exp(drcPost$ehat)
hist(e_post - 30)
abline(v=e,col="red", lty = 2, lwd = 2)

b_post <- drcPost$b
hist(b_post)
abline(v=b,col="red", lty = 2, lwd = 2)

c_post <- drcPost$c
hist(c_post)
abline(v=c,col="red", lty = 2, lwd = 2)

d_post <- drcPost$d
hist(d_post)
abline(v=d ,col="red", lty = 2, lwd = 2)

sigma_post <- drcPost$sigma_g
hist(sigma_post)
abline(v=sigma_g,col="red", lty = 2, lwd = 2)








#--------------------------------------------------------
#Try eith some varietiy level differences on Maximum hardiness
#---------------------------------------------------


#inputs
nrep <- 50 # number of reps of each variety 
meanTemp <- 4
sigmaTemp <- 8
simTemps <- rnorm(nrep, meanTemp,sigmaTemp)


#make temperatures positive because teh x value cant be negative. I do this by adding 20 to the data
# I will also model teh hardiness resonse as inverted sp plants get a higher number as they get more cold hardy rather than a more negative number
simTempsPos <- simTemps + 30

#Set model parameters - I have no idea what they should be so I will run a similar model in R (below)

b <- 11 #this is the rate paramater, like a slope in a linear regession 
d <- 24 # maximum hardiness (inverted from -24)
c <- 2 # minimum hardiness (inverted from -10)
e <- 37 # Effective dose ED50. x value where y value is halfway bewteen max(d) and min (c)
sigma_g <- 2


# here I am focusing on maximum hardiness first because this value is recorded to vary between varieties

#simulate data
nvariety <- 20
varNames <- as.factor(c(1:nvariety)) # make 20 "varieties" named "1" to "20"

bvarsigma <- 0
bvars <- rtruncnorm(n = nvariety , mean = b, sd = bvarsigma)

dvarsigma <- 5
dvars <- rnorm(nvariety , d, dvarsigma)
dvarsi <- rep(dvars, each = nrep)

cvarsigma <- 0
cvars <- rnorm(nvariety , c, cvarsigma)

evarsigma <- 0
evars <- rnorm(nvariety , e, evarsigma)

#make a database to hold results
varieties <- rep(varNames, each = nrep)
airtemp <- rep(simTempsPos, times = nvariety)

doseSimData <- data.frame(cbind(varieties, airtemp))

doseSimData$ltePositive <- NA

#loop through each variety
for (i in 1:length(doseSimData$ltePositive)){
	doseSimData$ltePositive[i] <- c + ( (dvarsi[i]-c) / (1 + exp(b*(log(airtemp[i])-log(e)))))
}

#add some variation 
doseSimData$eps <- rnorm(n = nrow(doseSimData), mean = 0, sd = sigma_g)

doseSimData$finalLTEPos <- doseSimData$ltePositive + doseSimData$eps 

#add columns where data is not transformed to eb positive
doseSimData$negLTE <- doseSimData$finalLTEPos *-1
doseSimData$airtempCold <- doseSimData$airtemp - 30

hist(dvarsi)

#make some data to plot the line mean model
plotingTemps <- rnorm(100, meanTemp,sigmaTemp) + 30
plotingTemps <- sort(plotingTemps)
plotingTempsCold <- plotingTemps - 30

plottingLTE <- (c + ( (d-c) / (1 + exp(b*(log(plotingTemps)-log(e)))))) * -1

head(doseSimData)


plot(doseSimData$negLTE ~ doseSimData$airtempCold, 
	xlab = "air temp (degrees)", 
	ylab = "LTE50 (degrees C)", 
	pch = 16, 
	col = 4,
	main = "bsigma = 1, dsig = 5, csig = 0, esig = 0, gsig= 5")
lines(plottingLTE ~ plotingTempsCold)


str(doseSimData)

#Prior checks
#---------------------------

gammadVarPrior <- rgamma(1000, shape = 2.5, rate = 1.75)
hist(gammadVarPrior)

doseSimData$varieties
R <- 1000

x <- I(doseSimData$airtemp )
N <- length(x)

variety <- doseSimData$varieties
n_vars <- length(variety)


stanData_priot_drs_hd <- list(N = N, x = x, variety = variety, n_vars = n_vars)

drc_prior_hd <- stan(file = "stan/doseResponse_priorCheck_vars.stan", data = stanData_priot_drs_hd, 
	iter=R, warmup=0, chains=1, refresh=R,
    seed=4838282, algorithm="Fixed_param")



priorCheckhd <- rstan::extract(drc_prior_hd)

hist(priorCheckhd$mu_y)
hist(priorCheckhd$y_sim) # very few values above 0. That is good. 
hist(priorCheckhd$ehat)
hist(priorCheckhd$e)
hist(priorCheckhd$d_var_sigma)

meanSimYhd <- colMeans(priorCheckhd$y_sim) 
quantsSimYhd <- apply( priorCheckhd$y_sim , 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

extremeshd <- quantsSimYhd[c(2, 5),]
quatershd <- quantsSimYdh[c(3, 4),]

plottingDataPriorhd <- data.frame(t(quantsSimYhd) * -1)
plottingDataPriorhd$MeanY <- meanSimYhd * -1
plottingDataPriorhd$x <- plotingTempsColdhd


priorTempsPlothd <- ggplot(data = plottingDataPriorhd, aes(x = x, y = MeanY ))
priorTempsPlothd + 
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()

hist(plottingDataPriorhd$MeanY)
hist(plottingDataPriorhd$X95.)
hist(plottingDataPriorhd$X5.)




#Run with simulated data - ncp
#------------------------------------------------

head(doseSimData)
x <- I(doseSimData$airtemp )
N <- length(x)
y <- doseSimData$finalLTEPos 

variety <- doseSimData$varieties
n_vars <- length(unique(variety))

stanData_drs_hd <- list(N = N, x = x, y = y, variety = variety, n_vars = n_vars)

#thsi used the positive transfomed data!
drc_hd <- stan(file = "stan/doseResponsedVar_ncp.stan", data = stanData_drs_hd, warmup = 1000, 
	iter = 2000, chains = 4, cores = 4, thin = 1)


#plot(drc_hd)
drc_hd_fit <-  rstan::extract(drc_hd)
str(drc_hd_fit)

plot(drc_hd_fit$b)
hist(drc_hd_fit$b)
abline(v=b ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$c )
hist(drc_hd_fit$c )
abline(v=c,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$ehat)
hist(exp(drc_hd_fit$ehat))
abline(v=e ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$d )
hist(drc_hd_fit$d )
abline(v=d ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$d_var_sigma)
hist(drc_hd_fit$d_var_sigma )
abline(v=dvarsigma ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$mu_y)
hist(drc_hd_fit$mu_y )

plot(drc_hd_fit$b ~ drc_hd_fit$c)
plot(drc_hd_fit$b ~ drc_hd_fit$ehat )
plot(drc_hd_fit$b ~ drc_hd_fit$d )
plot(drc_hd_fit$b ~ drc_hd_fit$sigma_g)
plot(drc_hd_fit$b ~ drc_hd_fit$d_var_sigma)

plot(drc_hd_fit$ehat ~ drc_hd_fit$c )
plot(drc_hd_fit$ehat ~ drc_hd_fit$d )
plot(drc_hd_fit$ehat ~ drc_hd_fit$sigma_g)
plot(drc_hd_fit$ehat ~ drc_hd_fit$d_var_sigma)

plot(drc_hd_fit$d ~ drc_hd_fit$b )
plot(drc_hd_fit$d  ~ drc_hd_fit$sigma_g)
plot(drc_hd_fit$d ~ drc_hd_fit$d_var_sigma)

pairs(drc_hd_fit)


str(drc_hd_fit)
pairs(drc_hd, pars = c("b", "c","d","ehat","sigma_g", "d_var_sigma", "lp__")) 

str(drc_hd)

#explore post retrodictive values
meanYs <- colMeans(drc_hd_fit$y_sim) * -1
quantsYs <- apply( drc_hd_fit$y_sim, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

extremesYs <- data.frame(t(quantsYs[c(2, 5),]* -1)) # chaneg back to negative values 
quatersPYs <- quantsYs[c(3, 4),]* -1 

plottingDataPost2 <- data.frame(t(quatersPYs))
plottingDataPost2$MeanY <- meanYs 
plottingDataPost2$x <- doseSimData$airtempCold 

plottingDataPost2$X5. <- extremesYs$X5.
plottingDataPost2$X95. <- extremesYs$X95.


postTempsPlot2 <- ggplot(data = plottingDataPost2, aes(x = x, y = MeanY ))
postTempsPlot2 + 
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()+
	geom_point(aes(x = doseSimData$airtempCold  , y = doseSimData$negLTE))


plot(meanYs ~ doseSimData$negLTE)#teh model cuts off at 0, so there is clustering around these values 

color_scheme_set("darkgray")
posterior_cp <- as.array(drc_hd)
#mcmc_parcoord(posterior_cp) thsi crashes 
#plot variety level differences 

drc_hd_fit$d_var_sigma # one each itteration (4000)

rawVarDs <- data.frame(drc_hd_fit$d_var_raw)# each row is an itteration and each column a variety
varD <- rawVarDs

for (i in 1:n_vars){ # get variety level differences from ncp
	varD[i,] <- rawVarDs[i,] * drc_hd_fit$d_var_sigma[i]
}

meanVarEffect <- colMeans(varD)
quantsVarEffects <- apply( varD, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

meanDneg<- mean(drc_hd_fit$d)*-1

dvarsG <- meanVarEffect + mean(drc_hd_fit$d)

plot(dvarsG ~ dvars) # plot mean predicted variety effect against simulated 

color_scheme_set("viridis")
mcmc_intervals(varD + -drc_hd_fit$d) + geom_vline(xintercept = meanDneg, linetype="dotted", color = "grey")  #intercepts 

#It looks like a tight prior on c costrains the posterior of b and e? Because they are somewhat colinear. 
#Maybe that is to be expected, though, with thsi sort of shape? 

predictedVarietyDs <- varD + drc_hd_fit$d
simulatedVarietyDs <- dvars

#How well do posteriors compare to orginal simulated variety effects?
longPredDs <- gather(predictedVarietyDs, key = "variety", value = "prediction")
unique(as.character(longPredDs$variety))
longPredDs$variety <- as.factor(longPredDs$variety)

levelsVariety <- paste("X", c(1:20),sep = "")
longPredDs$variety <- factor(longPredDs$variety ,
    levels = levelsVariety,ordered = TRUE)
nRepVar <- nrow(longPredDs)/length(levels(longPredDs$variety))#how many predicted values per variety 
longPredDs$SimulatedD <- rep(dvars, each = nRepVar) # repeat original simulated values that many times 


varBoxesD <- ggplot(data = longPredDs, aes(x = variety, y = prediction))
varBoxesD + geom_boxplot()+
	geom_point(aes(x = variety, y = SimulatedD), colour = "Red")+ # pretty good.
	theme_classic()












#try non centred parameterisation
#-----------------------------------------

#thsi used the positive transfomed data!
drc_hd <- stan(file = "stan/doseResponsedVar.stan", data = stanData_drs_hd, warmup = 1000, 
	iter = 2000, chains = 4, cores = 4, thin = 1)


#plot(drc_hd)
drc_hd_fit <-  rstan::extract(drc_hd)
str(drc_hd_fit)

plot(drc_hd_fit$b)
hist(drc_hd_fit$b)
abline(v=b ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$c )
hist(drc_hd_fit$c )
abline(v=c,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$ehat)
hist(exp(drc_hd_fit$ehat))
abline(v=e ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$d )
hist(drc_hd_fit$d )
abline(v=d ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$d_var_sigma)
hist(drc_hd_fit$d_var_sigma )
abline(v=dvarsigma ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$mu_y)
hist(drc_hd_fit$mu_y )

plot(drc_hd_fit$b ~ drc_hd_fit$c)
plot(drc_hd_fit$b ~ drc_hd_fit$ehat )
plot(drc_hd_fit$b ~ drc_hd_fit$d )
plot(drc_hd_fit$b ~ drc_hd_fit$sigma_g)
plot(drc_hd_fit$b ~ drc_hd_fit$d_var_sigma)

plot(drc_hd_fit$ehat ~ drc_hd_fit$c )
plot(drc_hd_fit$ehat ~ drc_hd_fit$d )
plot(drc_hd_fit$ehat ~ drc_hd_fit$sigma_g)
plot(drc_hd_fit$ehat ~ drc_hd_fit$d_var_sigma)

plot(drc_hd_fit$d ~ drc_hd_fit$b )
plot(drc_hd_fit$d  ~ drc_hd_fit$sigma_g)
plot(drc_hd_fit$d ~ drc_hd_fit$d_var_sigma)

pairs(drc_hd_fit)


str(drc_hd_fit)
pairs(drc_hd, pars = c("b", "c","d","ehat","sigma_g", "d_var_sigma", "lp__")) 

str(drc_hd)

#explore post retrodictive values
meanYs <- colMeans(drc_hd_fit$y_sim) * -1
quantsYs <- apply( drc_hd_fit$y_sim, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

extremesYs <- data.frame(t(quantsYs[c(2, 5),]* -1)) # chaneg back to negative values 
quatersPYs <- quantsYs[c(3, 4),]* -1 

plottingDataPost2 <- data.frame(t(quatersPYs))
plottingDataPost2$MeanY <- meanYs 
plottingDataPost2$x <- doseSimData$airtempCold 

plottingDataPost2$X5. <- extremesYs$X5.
plottingDataPost2$X95. <- extremesYs$X95.


postTempsPlot2 <- ggplot(data = plottingDataPost2, aes(x = x, y = MeanY ))
postTempsPlot2 + 
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()+
	geom_point(aes(x = doseSimData$airtempCold  , y = doseSimData$negLTE))


plot(meanYs ~ doseSimData$negLTE)

color_scheme_set("darkgray")
posterior_cp <- as.array(drc_hd)

meanAlphaVarEndo <- data.frame(drc_hd_fit$za_variety * mean(drc_hd_fit$var_sigma[,1]))
mcmc_intervals(meanAlphaSiteEndo) + geom_vline(xintercept = mean(drc_hd_fit$alpha_g), linetype="dotted", color = "grey")  #intercepts 
















#Try with real data - ncp
#---------------------------------
#remove na rows
bhclimClean3 <- bhclim[!is.na(bhclim$lte),]
bhclimClean2 <- bhclimClean3[!bhclimClean3$variety == "",]

#Try to standardize x variable instead of adding 30 - no because of ehat transformation 


realy <- bhclimClean2$lte * -1
realx <- I(bhclimClean2 $meanC + 30)
N <- length(realx)


realvariety <- as.integer(as.factor(as.character(bhclimClean2$variety)))
realn_vars <- length(unique(realvariety))

stan_data_drs_real_var <- list(N = N, x = realx, y = realy, variety = realvariety, n_vars = realn_vars)

#thsi used the positive transfomed data!
drc_simple_real <- stan(file = "stan/doseResponsedVar_ncp.stan", data = stan_data_drs_real_var, warmup = 2000, 
	iter = 3000, chains = 4, cores = 4, thin = 1)


pairs(drc_simple_real, pars = c("b", "c","d","ehat","sigma_g", "d_var_sigma", "lp__")) 

drcPost <- rstan::extract(drc_simple_real)

#how do the posteriors look?
#pairs(drc_simple)

#How does teh predicted data look?
str(drcPost)
mu_post <- drcPost$mu_y * -1
hist(mu_post)
plot(colMeans(mu_post) ~ simTemps)

y_sim <- drcPost$y_sim * -1
hist(y_sim)
meanySimPost <- apply(y_sim, 2, mean)
quantsSimYPost <- apply( y_sim, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

extremesP <- quantsSimYPost[c(2, 5),]
quatersP <- quantsSimYPost[c(3, 4),]

plottingDataPost <- data.frame(t(quantsSimYPost))
plottingDataPost$MeanY <- meanySimPost 
plottingDataPost$x <- bhclimClean2 $meanC 


postTempsPlot <- ggplot(data = plottingDataPost, aes(x = x, y = MeanY ))
postTempsPlot + 
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()+
	geom_point(aes(x = bhclimClean2 $meanC , y = bhclimClean2$lte ))



plot(colMeans(y_sim) ~ simTemps)

# ------  get the z score for each value 

meanMuPostY <- apply(y_sim, 2, mean)
sdMuPostY <- apply(y_sim, 2, sd)

zScoreY <-  (bhclimClean2$lte  - meanMuPostY) / sdMuPostY

plot(zScoreY ~ bhclimClean2$lte )
plot(meanMuPostY ~ bhclimClean2$lte )



#How do the predicted values compare to real parameter values? (plot hist with real parameter value)

e_post <- exp(drcPost$ehat)
hist(e_post-30)

b_post <- drcPost$b
hist(b_post)

c_post <- drcPost$c
hist(c_post)

d_post <- drcPost$d
hist(d_post)

sigma_post <- drcPost$sigma_g
hist(sigma_post)


d_var_sigma <- drcPost$d_var_sigma # one each itteration (4000)
hist(d_var_sigma)

rawVarDs <- data.frame(drcPost$d_var_raw)# each row is an itteration and each column a variety
varD <- rawVarDs

for (i in 1:n_vars){ # get variety level differences from ncp
	varD[i,] <- rawVarDs[i,] * drcPost$d_var_sigma[i]
}

meanVarEffect <- colMeans(varD)
quantsVarEffects <- apply( varD, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

meanDneg<- mean(drcPost$d)

dvarsG <- meanVarEffect + mean(drcPost$d)

plot(dvarsG ~ dvars) # plot mean predicted variety effect against simulated 

colnames(varD) <- levels(as.factor(as.character(bhclimClean2$variety)))

color_scheme_set("viridis")
mcmc_intervals(-varD + -drcPost$d) + geom_vline(xintercept = -meanDneg, linetype="dotted", color = "grey")  #intercepts 





# Add site level variation as well
#---------------------------------------------------------------------



#simulate data 

#inputs
nrep <- 20 # number of reps of each variety 
meanTemp <- 4
sigmaTemp <- 8
simTemps <- rnorm(nrep, meanTemp,sigmaTemp)


#make temperatures positive because teh x value cant be negative. I do this by adding 20 to the data
# I will also model teh hardiness resonse as inverted sp plants get a higher number as they get more cold hardy rather than a more negative number
simTempsPos <- simTemps + 30

#Set model parameters - I have no idea what they should be so I will run a similar model in R (below)

b <- 11 #this is the rate paramater, like a slope in a linear regession 
d <- 24 # maximum hardiness (inverted from -24)
c <- 2 # minimum hardiness (inverted from -10)
e <- 37 # Effective dose ED50. x value where y value is halfway bewteen max(d) and min (c)
sigma_g <- 2


# here I am focusing on maximum hardiness first because this value is recorded to vary between varieties

#simulate data
nvariety <- 10
varNames <- as.factor(c(1:nvariety)) # make 20 "varieties" named "1" to "20"

nsite <- 10
siteNames <- as.factor(c(1:nsite))

bvarsigma <- 0
bvars <- rtruncnorm(n = nvariety , mean = b, sd = bvarsigma)

dvarsigma <- 5
dvars <- rnorm(nvariety , 0, dvarsigma)
dvarsi <- rep(dvars, each = nrep* nsite)

dsitesigma <- 2
dsites <- rnorm(nsite, 0, dsitesigma)
dsitesi <- rep(rep(dsites, each = nrep), each = n_vars)

cvarsigma <- 0
cvars <- rnorm(nvariety , c, cvarsigma)

evarsigma <- 0
evars <- rnorm(nvariety , e, evarsigma)

#make a database to hold results
varieties <- rep(varNames, each = nsite)
sites <- rep(siteNames, by = nvariety)
varietiesN <- rep(varieties, each = nrep)
sitesN <- rep(sites, each = nrep)

airtemp <- rep(simTempsPos, times = nvariety*nsite)

doseSimData <- data.frame(cbind(varietiesN, airtemp))
doseSimData$sitesN <- sitesN
doseSimData$varietyEffectD <- dvarsi
doseSimData$siteEffectD<- dsitesi

doseSimData$ltePositive <- NA

head(doseSimData)

#loop through each row
for (i in 1:length(doseSimData$ltePositive)){
	doseSimData$ltePositive[i] <- c + ( (d + dvarsi[i] + dsitesi[i]-c) / (1 + exp(b*(log(airtemp[i])-log(e)))))
}

#add some variation 
doseSimData$eps <- rnorm(n = nrow(doseSimData), mean = 0, sd = sigma_g)

doseSimData$finalLTEPos <- doseSimData$ltePositive + doseSimData$eps 

#add columns where data is not transformed to eb positive
doseSimData$negLTE <- doseSimData$finalLTEPos *-1
doseSimData$airtempCold <- doseSimData$airtemp - 30

hist(dvarsi)

#make some data to plot the line mean model
plotingTemps <- rnorm(100, meanTemp,sigmaTemp) + 30
plotingTemps <- sort(plotingTemps)
plotingTempsCold <- plotingTemps - 30

plottingLTE <- (c + ( (d-c) / (1 + exp(b*(log(plotingTemps)-log(e)))))) * -1

head(doseSimData)


plot(doseSimData$negLTE ~ doseSimData$airtempCold, 
	xlab = "air temp (degrees)", 
	ylab = "LTE50 (degrees C)", 
	pch = 16, 
	col = 4,
	main = "bsigma = 1, dsig = 5, csig = 0, esig = 0, gsig= 5")
lines(plottingLTE ~ plotingTempsCold)


#Prior predictive check
#-------------------------------------------


doseSimData$varieties
R <- 1000

x <- I(doseSimData$airtemp )
N <- length(x)

variety <- doseSimData$varietiesN
n_vars <- length(unique(variety))

site <- as.numeric(doseSimData$sitesN)
n_sites<- length(unique(sites))

stanData_priot_drs_hd <- list(N = N, x = x, variety = variety, n_vars = n_vars, site = site, n_sites = n_sites)

drc_prior_hd <- stan(file = "stan/doseResponse_priorCheck_varsSites.stan", data = stanData_priot_drs_hd, 
	iter=R, warmup=0, chains=1, refresh=R,
    seed=4838282, algorithm="Fixed_param")


priorCheckhd <- rstan::extract(drc_prior_hd)

hist(priorCheckhd$mu_y)
hist(priorCheckhd$y_sim) # very few values above 0. That is good. 
hist(priorCheckhd$ehat)
hist(priorCheckhd$e)
hist(priorCheckhd$d_var_sigma)
hist(priorCheckhd$d_site_sigma)

meanSimYhd <- colMeans(priorCheckhd$y_sim) 
quantsSimYhd <- apply( priorCheckhd$y_sim , 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

extremeshd <- quantsSimYhd[c(2, 5),]
quatershd <- quantsSimYdh[c(3, 4),]

plottingDataPriorhd <- data.frame(t(quantsSimYhd) * -1)
plottingDataPriorhd$MeanY <- meanSimYhd * -1
plottingDataPriorhd$x <- plotingTempsColdhd


priorTempsPlothd <- ggplot(data = plottingDataPriorhd, aes(x = x, y = MeanY ))
priorTempsPlothd + 
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()


#Run simyulated data though model
#--------------------------


head(doseSimData)
x <- I(doseSimData$airtemp )
N <- length(x)
y <- doseSimData$finalLTEPos 

variety <- doseSimData$varieties
n_vars <- length(unique(variety))

site <- as.numeric(doseSimData$sitesN)
n_sites<- length(unique(sites))


stanData_drs_hd <- list(N = N, x = x, y = y, variety = variety, n_vars = n_vars, sites = site, n_sites = n_sites)

#thsi used the positive transfomed data!
drc_hd <- stan(file = "stan/doseResponsedVarSite_ncp.stan", data = stanData_drs_hd, warmup = 1000, 
	iter = 2000, chains = 4, cores = 4, thin = 1)


#plot(drc_hd)
drc_hd_fit <-  rstan::extract(drc_hd)
str(drc_hd_fit)

plot(drc_hd_fit$b)
hist(drc_hd_fit$b)
abline(v=b ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$c )
hist(drc_hd_fit$c )
abline(v=c,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$ehat)
hist(exp(drc_hd_fit$ehat))
abline(v=e ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$d )
hist(drc_hd_fit$d )
abline(v=d ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$d_var_sigma)
hist(drc_hd_fit$d_var_sigma )
abline(v=dvarsigma ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$mu_y)
hist(drc_hd_fit$mu_y )

pairs(drc_hd, pars = c("b", "c","d","ehat","sigma_g", "d_var_sigma", "d_site_sigma", "lp__")) 



#explore post retrodictive values
meanYs <- colMeans(drc_hd_fit$y_sim) * -1
quantsYs <- apply( drc_hd_fit$y_sim, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

extremesYs <- data.frame(t(quantsYs[c(2, 5),]* -1)) # chaneg back to negative values 
quatersPYs <- quantsYs[c(3, 4),]* -1 

plottingDataPost2 <- data.frame(t(quatersPYs))
plottingDataPost2$MeanY <- meanYs 
plottingDataPost2$x <- doseSimData$airtempCold 

plottingDataPost2$X5. <- extremesYs$X5.
plottingDataPost2$X95. <- extremesYs$X95.


postTempsPlot2 <- ggplot(data = plottingDataPost2, aes(x = x, y = MeanY ))
postTempsPlot2 + 
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()+
	geom_point(aes(x = doseSimData$airtempCold  , y = doseSimData$negLTE))

#Try with real data 
#-------------------------


bhclimClean3 <- bhclim[!is.na(bhclim$lte),]
bhclimClean2 <- bhclimClean3[!bhclimClean3$variety == "",]

#Clean siet column 

bhclimClean2$site2 <- as.character(bhclimClean2$site)
unique(bhclimClean2$site2 )

bhclimClean2$site2[bhclimClean2$site2 == "Osoyoos northeast"] <- "Osoyoos, northeast"
bhclimClean2$site2[bhclimClean2$site2 == "Oliver east"] <- "Oliver, east"
bhclimClean2$site2[bhclimClean2$site2 == "Osoyoos west"] <- "Osoyoos, west"


#Try to standardize x variable instead of adding 30 - no because of ehat transformation 


realy <- bhclimClean2$lte * -1
realx <- I(bhclimClean2 $meanC + 30)
N <- length(realx)

head(bhclimClean2)

realvariety <- as.integer(as.factor(as.character(bhclimClean2$variety)))
realn_vars <- length(unique(realvariety))

realsite <- as.integer(as.factor(as.character(bhclimClean2$site2)))
realn_site <- length(unique(realsite))

stan_data_drs_real_var <- list(N = N, x = realx, y = realy, variety = realvariety, 
	n_vars = realn_vars, sites = realsite, n_sites = realn_site)

#thsi used the positive transfomed data!
drc_simple_real <- stan(file = "stan/doseResponsedVarSite_ncp.stan", data = stan_data_drs_real_var, warmup = 2000, 
	iter = 3000, chains = 4, cores = 4, thin = 1)

pairs(drc_simple_real, pars = c("b", "c","d","ehat","sigma_g", "d_var_sigma", "lp__")) 

drcPost <- rstan::extract(drc_simple_real)

#how do the posteriors look?
#pairs(drc_simple)

#How does teh predicted data look?
str(drcPost)
mu_post <- drcPost$mu_y * -1
hist(mu_post)
plot(colMeans(mu_post) ~ bhclimClean2 $meanC)


y_sim <- drcPost$y_sim * -1

hist(y_sim)
meanySimPost <- apply(y_sim, 2, mean)
quantsSimYPost <- apply( y_sim, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

extremesP <- quantsSimYPost[c(2, 5),]
quatersP <- quantsSimYPost[c(3, 4),]

plottingDataPost <- data.frame(t(quantsSimYPost))
plottingDataPost$MeanY <- meanySimPost 
plottingDataPost$x <- bhclimClean2 $meanC 


postTempsPlot <- ggplot(data = plottingDataPost, aes(x = x, y = MeanY ))
postTempsPlot + 
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()+
	geom_point(aes(x = bhclimClean2 $meanC , y = bhclimClean2$lte ))



plot(colMeans(y_sim) ~ bhclimClean2 $meanC)


#What are the predicted parameter values?

e_post <- exp(drcPost$ehat)
hist(e_post-30)

b_post <- drcPost$b
hist(b_post)

c_post <- drcPost$c
hist(c_post)

d_post <- drcPost$d
hist(d_post)

sigma_post <- drcPost$sigma_g
hist(sigma_post)


d_var_sigma <- drcPost$d_var_sigma # one each itteration (4000)
hist(d_var_sigma)

d_site_sigma <- drcPost$d_site_sigma # one each itteration (4000)
hist(d_site_sigma)

#compare varietyt and site effect 
plot(density(d_var_sigma), col = 2)
lines(density(d_site_sigma), col = 4)
text(1.5, 2, "Blue = site \nRed = variety")

#Variety plot 
rawVarDs <- data.frame(drcPost$d_var_raw)# each row is an itteration and each column a variety
varD <- rawVarDs

for (i in 1:n_vars){ # get variety level differences from ncp
	varD[i,] <- rawVarDs[i,] * drcPost$d_var_sigma[i]
}

meanVarEffect <- colMeans(varD)
quantsVarEffects <- apply( varD, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

meanDneg<- mean(drcPost$d)

dvarsG <- meanVarEffect + mean(drcPost$d)

colnames(varD) <- levels(as.factor(as.character(bhclimClean2$variety)))

color_scheme_set("viridis")
mcmc_intervals(-varD + -drcPost$d) + geom_vline(xintercept = -meanDneg, linetype="dotted", color = "grey")  #intercepts 

#site Plot 

rawsiteDs <- data.frame(drcPost$d_site_raw)# each row is an itteration and each column a variety
siteD <- rawsiteDs

for (i in 1:n_sites){ # get variety level differences from ncp
	siteD[i,] <- rawsiteDs[i,] * drcPost$d_site_sigma[i]
}

meanSiteEffect <- colMeans(siteD)
quantsSiteEffects <- apply( siteD, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

meanDneg <- mean(drcPost$d)

dsiteG <- meanSiteEffect + mean(drcPost$d)


colnames(siteD) <- levels(as.factor(as.character(bhclimClean2$site2)))

color_scheme_set("viridis")
mcmc_intervals(-siteD + -drcPost$d) + geom_vline(xintercept = -meanDneg, linetype="dotted", color = "grey")  #intercepts 














#----------------------------------------------------------------
#Add a slope effect for variety 
#-----------------------------------------------------------





#simulate data 

#inputs
nrep <- 20 # number of reps of each variety 
meanTemp <- 4
sigmaTemp <- 8
simTemps <- rnorm(nrep, meanTemp,sigmaTemp)


#make temperatures positive because teh x value cant be negative. I do this by adding 20 to the data
# I will also model teh hardiness resonse as inverted sp plants get a higher number as they get more cold hardy rather than a more negative number
simTempsPos <- simTemps + 30

#Set model parameters - I have no idea what they should be so I will run a similar model in R (below)

b <- 11 #this is the rate paramater, like a slope in a linear regession 
d <- 24 # maximum hardiness (inverted from -24)
c <- 2 # minimum hardiness (inverted from -10)
e <- 37 # Effective dose ED50. x value where y value is halfway bewteen max(d) and min (c)
sigma_g <- 2


# here I am focusing on maximum hardiness first because this value is recorded to vary between varieties

#simulate data
nvariety <- 10
varNames <- as.factor(c(1:nvariety)) # make 20 "varieties" named "1" to "20"

nsite <- 10
siteNames <- as.factor(c(1:nsite))

bvarsigma <- 5
bvars <- rtruncnorm(n = nvariety , mean = b, sd = bvarsigma)
bvarsi <- rep(dvars, each = nrep* nsite)


dvarsigma <- 5
dvars <- rnorm(nvariety , 0, dvarsigma)
dvarsi <- rep(dvars, each = nrep* nsite)

dsitesigma <- 5
dsites <- rnorm(nsite, 0, dsitesigma)
dsitesi <- rep(rep(dsites, each = nrep), times = n_vars)

cvarsigma <- 0
cvars <- rnorm(nvariety , c, cvarsigma)

evarsigma <- 0
evars <- rnorm(nvariety , e, evarsigma)

#make a database to hold results
varieties <- rep(varNames, each = nsite)
sites <- rep(siteNames, by = nvariety)
varietiesN <- rep(varieties, each = nrep)
sitesN <- rep(sites, each = nrep)

airtemp <- rep(simTempsPos, times = nvariety*nsite)

doseSimData <- data.frame(cbind(varietiesN, airtemp))
doseSimData$sitesN <- sitesN
doseSimData$varietyEffectD <- dvarsi
doseSimData$siteEffectD<- dsitesi

doseSimData$ltePositive <- NA

head(doseSimData)

#loop through each row
for (i in 1:length(doseSimData$ltePositive)){
	doseSimData$ltePositive[i] <- c + ( (d + dvarsi[i] + dsitesi[i]-c) / (1 + exp((b + bvarsi[i])*(log(airtemp[i])-log(e)))))
}

#add some variation 
doseSimData$eps <- rnorm(n = nrow(doseSimData), mean = 0, sd = sigma_g)

doseSimData$finalLTEPos <- doseSimData$ltePositive + doseSimData$eps 

#add columns where data is not transformed to eb positive
doseSimData$negLTE <- doseSimData$finalLTEPos *-1
doseSimData$airtempCold <- doseSimData$airtemp - 30

hist(dvarsi)

#make some data to plot the line mean model
plotingTemps <- rnorm(100, meanTemp,sigmaTemp) + 30
plotingTemps <- sort(plotingTemps)
plotingTempsCold <- plotingTemps - 30

plottingLTE <- (c + ( (d-c) / (1 + exp(b*(log(plotingTemps)-log(e)))))) * -1

head(doseSimData)


plot(doseSimData$negLTE ~ doseSimData$airtempCold, 
	xlab = "air temp (degrees)", 
	ylab = "LTE50 (degrees C)", 
	pch = 16, 
	col = 4,
	main = "bsigma = 1, dsig = 5, csig = 0, esig = 0, gsig= 5")
lines(plottingLTE ~ plotingTempsCold)




#Prior predictive checks 
#-----------------------

#Chose a priro for effect of variety in slope
#I'm pretty sure that variety shoudl have some effect, so gamma rather than normal
#Max slope - maybe try sd 10? But that might be a bit high! Try 4? becayse sd not max value



hist(rgamma(1000, shape = 3, rate = 2))
hist(rnorm(1000, 0, 3))


doseSimData$varieties
R <- 1000

x <- I(doseSimData$airtemp )
N <- length(x)

variety <- doseSimData$varietiesN
n_vars <- length(unique(variety))

site <- as.numeric(doseSimData$sitesN)
n_sites<- length(unique(sites))

stanData_priot_drs_hd <- list(N = N, x = x, variety = variety, n_vars = n_vars, site = site, n_sites = n_sites)

drc_prior_hd <- stan(file = "stan/doseResponse_priorCheck_varsSitesDB.stan", data = stanData_priot_drs_hd, 
	iter=R, warmup=0, chains=1, refresh=R,
    seed=4838282, algorithm="Fixed_param")


priorCheckhd <- rstan::extract(drc_prior_hd)

hist(priorCheckhd$mu_y)
hist(priorCheckhd$y_sim) # very few values above 0. That is good. 
hist(priorCheckhd$ehat)
hist(priorCheckhd$e)
hist(priorCheckhd$d_var_sigma)
hist(priorCheckhd$b_var_sigma)

meanSimYhd <- colMeans(priorCheckhd$y_sim) 
quantsSimYhd <- apply( priorCheckhd$y_sim , 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

extremeshd <- quantsSimYhd[c(2, 5),]
quatershd <- quantsSimYdh[c(1, 6),]

plottingDataPriorhd <- data.frame(t(quantsSimYhd) * -1)
plottingDataPriorhd$MeanY <- meanSimYhd * -1
plottingDataPriorhd$x <- plotingTempsColdhd


priorTempsPlothd <- ggplot(data = plottingDataPriorhd, aes(x = x, y = MeanY ))
priorTempsPlothd + 
	geom_ribbon(aes(ymin= X0., ymax=  X100.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()


#try with simulated data
#------------------------------------



head(doseSimData)
x <- I(doseSimData$airtemp )
N <- length(x)
y <- doseSimData$finalLTEPos 

variety <- doseSimData$varieties
n_vars <- length(unique(variety))

site <- as.numeric(doseSimData$sitesN)
n_sites<- length(unique(sites))


stanData_drs_hd <- list(N = N, x = x, y = y, variety = variety, n_vars = n_vars, sites = site, n_sites = n_sites)

#thsi used the positive transfomed data!
drc_hd <- stan(file = "stan/doseResponseDBVarSite_ncp.stan", data = stanData_drs_hd, warmup = 1000, 
	iter = 2000, chains = 4, cores = 4, thin = 1)


#plot(drc_hd)
drc_hd_fit <-  rstan::extract(drc_hd)
str(drc_hd_fit)

plot(drc_hd_fit$b)
hist(drc_hd_fit$b)
abline(v=b ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$c )
hist(drc_hd_fit$c )
abline(v=c,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$ehat)
hist(exp(drc_hd_fit$ehat))
abline(v=e ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$d )
hist(drc_hd_fit$d )
abline(v=d ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$d_var_sigma)
hist(drc_hd_fit$d_var_sigma )
abline(v=dvarsigma ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$b_var_sigma)
hist(drc_hd_fit$b_var_sigma )
abline(v=bvarsigma ,col="red", lty = 2, lwd = 2)

plot(drc_hd_fit$mu_y)
hist(drc_hd_fit$mu_y )

pairs(drc_hd, pars = c("b", "c","d","ehat","sigma_g", "d_var_sigma", "d_site_sigma", "b_var_sigma", "lp__")) 



#explore post retrodictive values
meanYs <- colMeans(drc_hd_fit$y_sim) * -1
quantsYs <- apply( drc_hd_fit$y_sim, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

extremesYs <- data.frame(t(quantsYs[c(2, 5),]* -1)) # chaneg back to negative values 
quatersPYs <- quantsYs[c(3, 4),]* -1 

plottingDataPost2 <- data.frame(t(quatersPYs))
plottingDataPost2$MeanY <- meanYs 
plottingDataPost2$x <- doseSimData$airtempCold 

plottingDataPost2$X5. <- extremesYs$X5.
plottingDataPost2$X95. <- extremesYs$X95.


postTempsPlot2 <- ggplot(data = plottingDataPost2, aes(x = x, y = MeanY ))
postTempsPlot2 + 
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()+
	geom_point(aes(x = doseSimData$airtempCold  , y = doseSimData$negLTE))



#Try with real data 
#---------------------------------




bhclimClean3 <- bhclim[!is.na(bhclim$lte),]
bhclimClean2 <- bhclimClean3[!bhclimClean3$variety == "",]

#Clean siet column 

bhclimClean2$site2 <- as.character(bhclimClean2$site)
unique(bhclimClean2$site2 )

bhclimClean2$site2[bhclimClean2$site2 == "Osoyoos northeast"] <- "Osoyoos, northeast"
bhclimClean2$site2[bhclimClean2$site2 == "Oliver east"] <- "Oliver, east"
bhclimClean2$site2[bhclimClean2$site2 == "Osoyoos west"] <- "Osoyoos, west"


#Try to standardize x variable instead of adding 30 - no because of ehat transformation 


realy <- bhclimClean2$lte * -1
realx <- I(bhclimClean2 $meanC + 30)
N <- length(realx)

head(bhclimClean2)

realvariety <- as.integer(as.factor(as.character(bhclimClean2$variety)))
realn_vars <- length(unique(realvariety))

realsite <- as.integer(as.factor(as.character(bhclimClean2$site2)))
realn_site <- length(unique(realsite))

stan_data_drs_real_var <- list(N = N, x = realx, y = realy, variety = realvariety, 
	n_vars = realn_vars, sites = realsite, n_sites = realn_site)

#thsi used the positive transfomed data!
drc_simple_real <- stan(file = "stan/doseResponseDBVarSite_ncp.stan", data = stan_data_drs_real_var, warmup = 2000, 
	iter = 3000, chains = 4, cores = 4, thin = 1)

pairs(drc_simple_real, pars = c("b", "c","d","ehat","sigma_g", "d_var_sigma", "d_site_sigma", "b_var_sigma", "lp__")) 

drcPost <- rstan::extract(drc_simple_real)

#how do the posteriors look?
#pairs(drc_simple)

#How does teh predicted data look?
str(drcPost)
mu_post <- drcPost$mu_y * -1
hist(mu_post)
plot(colMeans(mu_post) ~ bhclimClean2 $meanC)


y_sim <- drcPost$y_sim * -1

hist(y_sim)
meanySimPost <- apply(y_sim, 2, mean)
quantsSimYPost <- apply( y_sim, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

#get mean values without effect of variety and site 
cPost <- median(drcPost$c) 
dPost <- mean(drcPost$d)
bPost <- median(drcPost$b)
eHatPost <- mean(drcPost$ehat) 

uniqueXsRaw <- c(-15:25)
uniqueXs <- uniqueXsRaw+30

#Middle values
meanYs <- uniqueXs
for (i in 1:length( uniqueXs)){
	meanYs[i] <- cPost + (( dPost - cPost) / (1 + exp(bPost*(log(uniqueXs[i])-eHatPost))))
}


#Upper and lower hpdi of parameters and predicted mean values 
SDRealY2<- apply(data.frame(drcPost$mu_y ), 2, HPDI) 

realYs2 <- data.frame(colMeans(drcPost$mu_y))
head(realYs2)
realYs2$upperHPDI <- SDRealY2[1,]
realYs2$lowerHPDI <- SDRealY2[2,]

lowerd <- HPDI(data.frame(drcPost$d))[1]
upperd <- HPDI(data.frame(drcPost$d))[2]

lowerc <- HPDI(data.frame(drcPost$c))[1]
upperc <- HPDI(data.frame(drcPost$c))[2]

lowerb <- HPDI(data.frame(drcPost$b))[1]
upperb <- HPDI(data.frame(drcPost$b))[2]

lowerehat <- HPDI(data.frame(drcPost$ehat))[1]
upperehat <- HPDI(data.frame(drcPost$ehat))[2]

#Lower mean values
lowerMeanYs <- uniqueXs
for (i in 1:length( uniqueXs)){
	lowerMeanYs[i] <- lowerc + (( lowerd - lowerc) / (1 + exp(lowerb*(log(uniqueXs[i])-lowerehat))))
}

#Upper mean values
upperMeanYs <- uniqueXs
for (i in 1:length( uniqueXs)){
	upperMeanYs[i] <- upperc + (( upperd - upperc) / (1 + exp(upperb*(log(uniqueXs[i])-upperehat))))
}

#Getting the maximum and minimum effect of variety (not bothering with b because no real effect)
VarietyEffectsMean <- colMeans(data.frame(drcPost$var_d ))
min_d_var <- min(VarietyEffectsMean)
max_d_var <- max(VarietyEffectsMean)

#Getting the maximum and minimum effect of site
siteEffectsMean <- colMeans(data.frame(drcPost$site_d ))
min_d_site <- min(siteEffectsMean)
max_d_site <- max(siteEffectsMean)

#Lower mean values with min site and variety 
lowerMeanYsVarSite <- uniqueXs
for (i in 1:length( uniqueXs)){
	lowerMeanYsVarSite[i] <- (lowerc + (( lowerd + min_d_var + min_d_site  - lowerc) / (1 + exp(lowerb*(log(uniqueXs[i])-lowerehat)))))* -1
}

#Upper mean values with max site and variety 
upperMeanYsVarSite <- uniqueXs
for (i in 1:length( uniqueXs)){
	upperMeanYsVarSite[i] <- (upperc + (( upperd + max_d_var + max_d_site - upperc) / (1 + exp(upperb*(log(uniqueXs[i])-upperehat)))))*-1
}

#adding obervational model sigma_g
meanSigmaG <- mean(drcPost$sigma_g)
upperMeanYsSigma <- upperMeanYsVarSite - meanSigmaG
lowerMeanYsSigma <- lowerMeanYsVarSite +  meanSigmaG


plottingDataPost2 <- data.frame(cbind(uniqueXsRaw, meanYs))
plottingDataPost2$negMeanYs <- -meanYs
plottingDataPost2$upperMeanYs <- -upperMeanYs
plottingDataPost2$lowerMeanYs <- -lowerMeanYs
plottingDataPost2$upperMeanYsVarSite <- upperMeanYsVarSite
plottingDataPost2$lowerMeanYsVarSite <- lowerMeanYsVarSite
plottingDataPost2$upperMeanYsSigma <- upperMeanYsSigma
plottingDataPost2$lowerMeanYsSigma <- lowerMeanYsSigma


postTempsPlot <- ggplot(data = plottingDataPost2, aes(x = uniqueXsRaw, y = negMeanYs ))
postTempsPlot + 
	geom_line(colour = "palevioletred4") + 
	theme_classic() +
	geom_ribbon(aes(ymin= lowerMeanYsSigma, ymax=  upperMeanYsSigma),fill = "palevioletred", alpha = 0.3) +
	geom_ribbon(aes(ymin= lowerMeanYsVarSite, ymax=  upperMeanYsVarSite),fill = "palevioletred2", alpha = 0.3) +
	geom_ribbon(aes(ymin= lowerMeanYs, ymax=  upperMeanYs),fill = "palevioletred3", alpha = 0.3) +
	geom_point(aes(x = meanC , y = lte ), data = bhclimClean2)

#Whats going on with the less cold hardy values around -2?
bhclimClean2[bhclimClean2$lte > -15 & bhclimClean2$meanC < 0,]
data2014 <- bhclimClean2[bhclimClean2$ Year == 2014]
problemPoints <- bhclimClean2[bhclimClean2$date == "2014-11-11",]
par(mfrow=c(2,1)) 
#Hardiness
plot(data2014$date, data2014$lte  , pch = 16, cex.lab = 1.5, cex.axis = 1.5, xlab = "", ylab = "hardiness")
points(problemPoints$date, problemPoints$lte, pch = 16, col = 2)
#airtemp
plot(data2014$date, data2014$meanC  , pch = 16, cex.lab = 1.5, cex.axis = 1.5, xlab = "date", ylab= "air temperature")
points(problemPoints$date, problemPoints$meanC, pch = 16, col = 2)
par(mfrow=c(1,1)) 



extremesP <- quantsSimYPost[c(2, 5),]
quatersP <- quantsSimYPost[c(3, 4),]

plottingDataPost <- data.frame(t(quantsSimYPost))
plottingDataPost$MeanY <- meanySimPost 
plottingDataPost$x <- bhclimClean2 $meanC 

#plot of predicted mean values and 89% highest probability density index 
postTempsPlot <- ggplot(data = plottingDataPost, aes(x = x, y = MeanY ))
postTempsPlot + 
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()+
	geom_point(aes(x = bhclimClean2 $meanC , y = bhclimClean2$lte ))



plot(colMeans(y_sim) ~ bhclimClean2 $meanC)

plot(colMeans(y_mu) ~ bhclimClean2 $lte)

plot(colMeans(y_sim) ~ bhclimClean2 $lte)



#What are the predicted parameter values?

e_post <- exp(drcPost$ehat)
hist(e_post-30)

b_post <- drcPost$b
hist(b_post)

c_post <- drcPost$c
hist(c_post)

d_post <- drcPost$d
hist(d_post)

sigma_post <- drcPost$sigma_g
hist(sigma_post)


d_var_sigma <- drcPost$d_var_sigma # one each itteration (4000)
hist(d_var_sigma)

d_site_sigma <- drcPost$d_site_sigma # one each itteration (4000)
hist(d_site_sigma)

#compare varietyt and site effect 
plot(density(d_var_sigma), col = 2)
lines(density(d_site_sigma), col = 4)
text(1.5, 2, "Blue = site \nRed = variety")

#Variety plot - max hardiness 
rawVarDs <- data.frame(drcPost$d_var_raw)# each row is an itteration and each column a variety
varD <- rawVarDs

for (i in 1:n_vars){ # get variety level differences from ncp
	varD[i,] <- rawVarDs[i,] * drcPost$d_var_sigma[i]
}

meanVarEffect <- colMeans(varD)+ mean(drcPost$d)
quantsVarEffects <- apply( varD, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

meanDneg<- mean(drcPost$d)

dvarsG <- meanVarEffect + mean(drcPost$d)

colnames(varD) <- levels(as.factor(as.character(bhclimClean2$variety)))

color_scheme_set("viridis")
mcmc_intervals(-varD + -drcPost$d) + geom_vline(xintercept = -meanDneg, linetype="dotted", color = "grey")  #intercepts 

quantsVarEffectsHPDI <- apply( varD, 2 , HPDI)
quantsVarEffectsHPDI + mean(drcPost$d)

#Hardiness taken from fergusonetal2014
fergusonHardinessNames <- c( "Cabernet Franc", "Cabernet Sauvignon", "Chardonnay", "Gewurztraminer", 
	"Merlot",  "Pinot gris",  "Riesling","Sauvignon blanc","Shiraz", "Viognier")

Values <- c(-23.5, -25.4,-25.7,-24.9 ,-25.0,-24.0,-26.1,-24.9,-24.2,-24.0)
ferValues <- data.frame(cbind(fergusonHardinessNames, Values))
ferValues$Values <- as.numeric(as.character(ferValues$Values))

comparisonVarieties <- -1*(varD[,colnames(varD)%in%fergusonHardinessNames] +  mean(drcPost$d))

names(comparisonVarieties)

hist(comparisonVarieties[,1], main = "Cabernet Franc")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Cabernet Franc"], , col="red", lwd=3, lty=2)

hist(comparisonVarieties[,2], main = "Cabernet Sauvignon", xlim = c(-26, -20))
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Cabernet Sauvignon"], , col="red", lwd=3, lty=2)

hist(comparisonVarieties$Chardonnay, main = "Chardonnay")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Chardonnay"], , col="red", lwd=3, lty=2)

hist(comparisonVarieties$Gewurztraminer, main = "Gewurztraminer")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Gewurztraminer"], , col="red", lwd=3, lty=2)

hist(comparisonVarieties$Merlot, main = "Merlot")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Merlot"], , col="red", lwd=3, lty=2)

hist(comparisonVarieties[,6], main = "Pinot gris")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Pinot gris"], , col="red", lwd=3, lty=2)

hist(comparisonVarieties$Riesling, main = "Riesling")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Riesling"], , col="red", lwd=3, lty=2)

hist(comparisonVarieties[,8], main = "Sauvignon blanc")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Sauvignon blanc"], , col="red", lwd=3, lty=2)

hist(comparisonVarieties$Viognier, main = "Viognier")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Viognier"], , col="red", lwd=3, lty=2)


#Variety plot - slope
rawVarBs <- data.frame(drcPost$b_var_raw)# each row is an itteration and each column a variety
varB <- rawVarBs

for (i in 1:n_vars){ # get variety level differences from ncp
	varB[i,] <- rawVarBs[i,] * drcPost$b_var_sigma[i]
}

meanVarEffectB <- colMeans(varB)
quantsVarEffectsB <- apply( varB, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

meanB<- mean(drcPost$b)

bvarsG <- meanVarEffectB + mean(drcPost$b)

colnames(varB) <- levels(as.factor(as.character(bhclimClean2$variety)))

color_scheme_set("viridis")
mcmc_intervals(varB + -drcPost$b) + geom_vline(xintercept = meanB, linetype="dotted", color = "grey")  #intercepts 



#site Plot 

rawsiteDs <- data.frame(drcPost$d_site_raw)# each row is an itteration and each column a variety
siteD <- rawsiteDs

for (i in 1:n_sites){ # get variety level differences from ncp
	siteD[i,] <- rawsiteDs[i,] * drcPost$d_site_sigma[i]
}

meanSiteEffect <- colMeans(siteD)
quantsSiteEffects <- apply( siteD, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

meanDneg <- mean(drcPost$d)

dsiteG <- meanSiteEffect + mean(drcPost$d)


colnames(siteD) <- levels(as.factor(as.character(bhclimClean2$site2)))

color_scheme_set("viridis")
mcmc_intervals(-siteD + -drcPost$d) + geom_vline(xintercept = -meanDneg, linetype="dotted", color = "grey")  #intercepts 






#--------------------------------------------------------------------------------------------------------
#Try seeing if my model can predict hardiness from a different data set - in this case Cab Sauv from Washinton State, 
#taken from teh Fergoson et al website http://wine.wsu.edu/extension/weather/cold-hardiness/model/
#-----------------------------------------------------------------------------------------------------


otherCabSovData$Variety <- "CabernetSauvingion"
newX <- otherCabSovData$T_mean + 30
N_p <- length(newX)


realy <- bhclimClean2$lte * -1
realx <- I(bhclimClean2 $meanC + 30)
N <- length(realx)

head(bhclimClean2)

realvariety <- as.integer(as.factor(as.character(bhclimClean2$variety)))
realn_vars <- length(unique(realvariety))

realsite <- as.integer(as.factor(as.character(bhclimClean2$site2)))
realn_site <- length(unique(realsite))

stan_data_drs_real_var <- list(N = N, x = realx, newX = newX, N_p = N_p, y = realy, variety = realvariety, 
	n_vars = realn_vars, sites = realsite, n_sites = realn_site)

#thsi used the positive transfomed data!
drc_simple_predict <- stan(file = "stan/doseResponseDBVarSite_ncp_predict.stan", data = stan_data_drs_real_var, warmup = 2000, 
	iter = 3000, chains = 4, cores = 4, thin = 1)



drcPost_Predict <- rstan::extract(drc_simple_predict)


predicted_mu <- drcPost_Predict$mu_y_p
predicted_y <- drcPost_Predict$y_sim_p
otherCabSovData$pred_mu <- -colMeans(drcPost_Predict$mu_y_p)

plot(colMeans(-predicted_mu) ~ otherCabSovData$T_mean )
plot(colMeans(-predicted_y) ~ otherCabSovData$Observed_Hc)

postTempsPlot <- ggplot(data = otherCabSovData, aes(x = Observed_Hc, y = pred_mu ))
postTempsPlot + geom_point() + 
	theme_classic()+
	labs(y= "Predicted Winter hadiness (LTE50 C)", x = "Observed Winter Hardiness (LTE50 C)")+
	theme(text = element_text(size=20))


y_sim <- drcPost_Predict$y_sim_p * -1

hist(y_sim)
meanySimPost <- apply(y_sim, 2, mean)
quantsSimYPost <- apply( y_sim, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

extremesP <- quantsSimYPost[c(2, 5),]
quatersP <- quantsSimYPost[c(3, 4),]

plottingDataPost <- data.frame(t(quantsSimYPost))
plottingDataPost$MeanY <- meanySimPost 
plottingDataPost$x <- otherCabSovData$T_mean


postTempsPlot <- ggplot(data = plottingDataPost, aes(x = x, y = MeanY ))
postTempsPlot + 
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()+
	geom_point(aes(x = otherCabSovData$T_mean , y = otherCabSovData$Observed_Hc ))

#Use hpdi instead
postTrmpsHPDI <- apply( y_sim, 2 , HPDI, prob=89) 
plottingDataPost$upperHPDI89 <- postTrmpsHPDI[2,]
plottingDataPost$lowerHPDI89 <- postTrmpsHPDI[1,]


plottingDataPost$upperHPDI97 <- postTrmpsHPDI[2,]#soem empty columsn for teh loop 
plottingDataPost$lowerHPDI97 <- postTrmpsHPDI[1,]

for(i in 1:ncol(y_sim)){
	hpdii <- HPDI(y_sim[i,], prob = 97)
	plottingDataPost$upperHPDI97[i] <- hpdii[2]
	plottingDataPost$lowerHPDI97[i] <- hpdii[1]
}

postTempsPlot <- ggplot(data = plottingDataPost, aes(x = x, y = MeanY ))
postTempsPlot + 
	geom_ribbon(aes(ymin= lowerHPDI89, ymax=  upperHPDI89), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= lowerHPDI97, ymax=  upperHPDI97), , fill = "palevioletred2", alpha = 0.5) + 
	geom_line() + theme_classic()+
	geom_point(aes(x = otherCabSovData$T_mean , y = otherCabSovData$Observed_Hc ))





#Plots for the writeup document 
#-----------------------------------------

#Prior histograms	
png("DoseResponse_WriteupImages/Priors.png")
par(mfrow=c(3,4)) 
hist(d_Prior, main = "d Prior",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(c_Prior, main = "c Prior",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(ehat_Prior, main = "ehat Prior (sigma 0.15)",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(e_Prior, main = "e Prior (sigma 0.15)",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(gammabPrior, main = "b Prior",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(sigma_g_Prior, main = "sigma_g Prior",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(d_var_sigma, main = "d_var_sigma Prior",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(d_var_raw, main = "d_var_raw Prior",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(b_var_sigma, main = "b_var_sigma Prior",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(b_var_raw, main = "b_var_raw Prior",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(d_site_sigma , main = "d_site_sigma Prior",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(d_site_raw , main = "d_site_raw Prior",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
dev.off()

par(mfrow=c(1,1)) 

#Prior predictive check 
#Bands are the 25/75% preditions and the extreme values (0/100% predictions)
png("DoseResponse_WriteupImages/PriorPrediction.png")
priorTempsPlothd <- ggplot(data = plottingDataPriorhd, aes(x = x, y = MeanY ))
priorTempsPlothd + 
	geom_ribbon(aes(ymin= X0., ymax=  X100.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	labs(y= "Predicted Winter hadiness (LTE50 Decgrees C)", x = "Mean 2 Day air temperature (degrees C)")+
	geom_line() + theme_classic()+
	theme(text = element_text(size=20))

dev.off()

#Pairs plot from model with real data 
png("DoseResponse_WriteupImages/PairsPlot.png")
pairs(drc_simple_real, pars = c("b", "c","d","ehat","sigma_g", "d_var_sigma", "d_site_sigma", "b_var_sigma", "lp__")) 
dev.off()

#Plot of predicted values against real data
#plot of predicted mean values and 89% hihest probability density index 
# bands are (from middle outwards) mean estimate without hierarchy, upper and lower hpdi of estimate without hierarchy, 
#   mean prediction with largest effects of site and variety combined, effect of variety and site plus/minus sigma g 
png("DoseResponse_WriteupImages/PredWithReal.png")

postTempsPlot <- ggplot(data = plottingDataPost2, aes(x = uniqueXsRaw, y = negMeanYs ))
postTempsPlot + 
	geom_line(colour = "palevioletred4") + 
	theme_classic() +
	geom_ribbon(aes(ymin= lowerMeanYsSigma, ymax=  upperMeanYsSigma),fill = "palevioletred", alpha = 0.3) +
	geom_ribbon(aes(ymin= lowerMeanYsVarSite, ymax=  upperMeanYsVarSite),fill = "palevioletred2", alpha = 0.3) +
	geom_ribbon(aes(ymin= lowerMeanYs, ymax=  upperMeanYs),fill = "palevioletred3", alpha = 0.3) +
	geom_point(aes(x = meanC , y = lte ), data = bhclimClean2)+
	labs(y= "Winter hadiness (LTE50 Decgrees C)", x = "Mean 2 Day air temperature (degrees C)")+
	theme(text = element_text(size=20))
dev.off()


#plot of predicted data against real data
bhclimClean2$PostMuLTE <- colMeans(mu_post)

png("DoseResponse_WriteupImages/PredAgainstReal.png")

postTempsPlot <- ggplot(data = bhclimClean2, aes(x = lte, y = PostMuLTE ))
postTempsPlot + 
	geom_point(colour = "palevioletred4") + 
	theme_classic() +
	labs(x= "Observed winter hadiness (LTE50 Decgrees C)", y = "Predicted winter hadiness (LTE50 Decgrees C)")+
	theme(text = element_text(size=20))

dev.off()

#Panel of plots for each parameter 
png("DoseResponse_WriteupImages/Parameters.png")
par(mfrow=c(3,4)) 
hist(drcPost$d, main = "d",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(drcPost$c, main = "c",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(drcPost$ehat, main = "ehat",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(drcPost$e, main = "e",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(drcPost$b, main = "b",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(drcPost$sigma_g, main = "sigma_g",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(drcPost$d_var_sigma, main = "d_var_sigma",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(drcPost$d_var_raw, main = "d_var_raw",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(drcPost$b_var_sigma, main = "b_var_sigma",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(drcPost$b_var_raw, main = "b_var_raw",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(drcPost$d_site_sigma , main = "d_site_sigma",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(drcPost$d_site_raw , main = "d_site_raw",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
dev.off() 

#Plot of effect of variety and site on d

png("DoseResponse_WriteupImages/VarSiteD.png")
plot(density(d_var_sigma), col = 2, main = "",  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
	xlab = "probability", ylab = "density")
lines(density(d_site_sigma), col = 4)
text(1.5, 2, "Blue = site \nRed = variety")
dev.off() 

#Plot of effect of varieties on d
png("DoseResponse_WriteupImages/varDs.png")
color_scheme_set("viridis")
mcmc_intervals(-varD + -drcPost$d) + geom_vline(xintercept = -meanDneg, linetype="dotted", color = "grey")  +
	theme(text = element_text(size=20))
dev.off()

#Plot of effect of sites on d
png("DoseResponse_WriteupImages/siteDs.png")
color_scheme_set("viridis")
mcmc_intervals(-siteD + -drcPost$d) + geom_vline(xintercept = -meanDneg, linetype="dotted", color = "grey") +
	theme(text = element_text(size=20))
dev.off()

#Plot of effects of varieties on b
png("DoseResponse_WriteupImages/varBs.png")
color_scheme_set("viridis")
mcmc_intervals(varB + -drcPost$b) + geom_vline(xintercept = meanB, linetype="dotted", color = "grey") +
	theme(text = element_text(size=20))
dev.off()


#Plot of model prediction x ~ y (Washington Data)

png("DoseResponse_WriteupImages/WashingtonPrediction.png")
postTempsPlot <- ggplot(data = plottingDataPost, aes(x = x, y = MeanY ))
postTempsPlot + 
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()+
	geom_point(aes(x = otherCabSovData$T_mean , y = otherCabSovData$Observed_Hc ))+
	labs(y= "Winter hadiness (LTE50 Decgrees C)", x = "Mean 2 Day air temperature (degrees C)")+
	theme(text = element_text(size=20))
dev.off()

#Plot of the model predictions for washington data againts observed data

png("DoseResponse_WriteupImages/WashingtonPrediction2.png")
postTempsPlot <- ggplot(data = otherCabSovData, aes(x = Observed_Hc, y = pred_mu ))
postTempsPlot + geom_point() + 
	theme_classic()+
	labs(y= "Predicted Winter hadiness (LTE50 C)", x = "Observed Winter Hardiness (LTE50 C)")+
	theme(text = element_text(size=20))
dev.off()

#Whats going on with the less cold hardy values around -2?
png("DoseResponse_WriteupImages/2014Data.png")
par(mfrow=c(2,1)) 
#Hardiness
plot(data2014$date, data2014$lte  , pch = 16, cex.lab = 1.5, cex.axis = 1.5, xlab = "", ylab = "hardiness")
points(problemPoints$date, problemPoints$lte, pch = 16, col = 2)
#airtemp
plot(data2014$date, data2014$meanC  , pch = 16, cex.lab = 1.5, cex.axis = 1.5, xlab = "date", ylab= "air temperature")
points(problemPoints$date, problemPoints$meanC, pch = 16, col = 2)
par(mfrow=c(1,1)) 
dev.off()

#Cpompare to Ferguson

png("DoseResponse_WriteupImages/compareFerg.png")
par(mfrow=c(3,4))

hist(comparisonVarieties[,1], main = "Cabernet Franc")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Cabernet Franc"], , col="red", lwd=3, lty=2,cex.lab = 1.5, cex.axis = 1.5)

hist(comparisonVarieties[,2], main = "Cabernet Sauvignon", xlim = c(-26, -20))
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Cabernet Sauvignon"], , col="red", lwd=3, lty=2 , cex.lab = 1.5, cex.axis = 1.5)

hist(comparisonVarieties$Chardonnay, main = "Chardonnay")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Chardonnay"], , col="red", lwd=3, lty=2, cex.lab = 1.5, cex.axis = 1.5)

hist(comparisonVarieties$Gewurztraminer, main = "Gewurztraminer")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Gewurztraminer"], , col="red", lwd=3, lty=2, cex.lab = 1.5, cex.axis = 1.5)

hist(comparisonVarieties$Merlot, main = "Merlot")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Merlot"], , col="red", lwd=3, lty=2, cex.lab = 1.5, cex.axis = 1.5)

hist(comparisonVarieties[,6], main = "Pinot gris")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Pinot gris"], , col="red", lwd=3, lty=2, cex.lab = 1.5, cex.axis = 1.5)

hist(comparisonVarieties$Riesling, main = "Riesling")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Riesling"], , col="red", lwd=3, lty=2, cex.lab = 1.5, cex.axis = 1.5)

hist(comparisonVarieties[,8], main = "Sauvignon blanc")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Sauvignon blanc"], , col="red", lwd=3, lty=2, cex.lab = 1.5, cex.axis = 1.5)

hist(comparisonVarieties$Viognier, main = "Viognier")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Viognier"], , col="red", lwd=3, lty=2, cex.lab = 1.5, cex.axis = 1.5)

par(mfrow=c(1,1)) 
dev.off()













#--------------------------------------------------------------
#Try with 1 day temperature 
#-----------------------------------------------------------------


bhclimClean3 <- bhclim[!is.na(bhclim$lte),]
bhclimClean2 <- bhclimClean3[!bhclimClean3$variety == "",]

#Clean siet column 

bhclimClean2$site2 <- as.character(bhclimClean2$site)
unique(bhclimClean2$site2 )

bhclimClean2$site2[bhclimClean2$site2 == "Osoyoos northeast"] <- "Osoyoos, northeast"
bhclimClean2$site2[bhclimClean2$site2 == "Oliver east"] <- "Oliver, east"
bhclimClean2$site2[bhclimClean2$site2 == "Osoyoos west"] <- "Osoyoos, west"

realy <- bhclimClean2$lte * -1
realx <- I(bhclimClean2 $mean1DayC + 30)
N <- length(bhclimClean2 $mean1DayC)
plot(bhclimClean2$lte ~ bhclimClean2 $mean1DayC )

head(bhclimClean2)

realvariety <- as.integer(as.factor(as.character(bhclimClean2$variety)))
realn_vars <- length(unique(realvariety))

realsite <- as.integer(as.factor(as.character(bhclimClean2$site2)))
realn_site <- length(unique(realsite))

stan_data_drs_real_var <- list(N = N, x = realx, y = realy, variety = realvariety, 
	n_vars = realn_vars, sites = realsite, n_sites = realn_site)

#thsi used the positive transfomed data!
drc_simple_real2 <- stan(file = "stan/doseResponseDBVarSite_ncp.stan", data = stan_data_drs_real_var, warmup = 2000, 
	iter = 3000, chains = 4, cores = 4, thin = 1)

pairs(drc_simple_real2, pars = c("b", "c","d","ehat","sigma_g", "d_var_sigma", "d_site_sigma", "b_var_sigma", "lp__")) 

drcPost2 <- rstan::extract(drc_simple_real2)

#how do the posteriors look?
#pairs(drc_simple)

#How does teh predicted data look?
str(drcPost2)
mu_post <- drcPost2$mu_y * -1
hist(mu_post)
plot(colMeans(mu_post) ~ bhclimClean2 $mean1DayC)


y_sim <- drcPost2$y_sim * -1

hist(y_sim)
meanySimPost <- apply(y_sim, 2, mean)
quantsSimYPost <- apply( y_sim, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

#get mean values without effect of variety and site 
cPost <- median(drcPost2$c) 
dPost <- mean(drcPost2$d)
bPost <- median(drcPost2$b)
eHatPost <- mean(drcPost2$ehat) 

uniqueXsRaw <- c(-15:25)
uniqueXs <- uniqueXsRaw+30

#Middle values
meanYs <- uniqueXs
for (i in 1:length( uniqueXs)){
	meanYs[i] <- cPost + (( dPost - cPost) / (1 + exp(bPost*(log(uniqueXs[i])-eHatPost))))
}


#Upper and lower hpdi of parameters and predicted mean values 
SDRealY2<- apply(data.frame(drcPost2$mu_y ), 2, HPDI) 

realYs2 <- data.frame(colMeans(drcPost2$mu_y))
head(realYs2)
realYs2$upperHPDI <- SDRealY2[1,]
realYs2$lowerHPDI <- SDRealY2[2,]

lowerd <- HPDI(data.frame(drcPost2$d))[1]
upperd <- HPDI(data.frame(drcPost2$d))[2]

lowerc <- HPDI(data.frame(drcPost2$c))[1]
upperc <- HPDI(data.frame(drcPost2$c))[2]

lowerb <- HPDI(data.frame(drcPost2$b))[1]
upperb <- HPDI(data.frame(drcPost2$b))[2]

lowerehat <- HPDI(data.frame(drcPost2$ehat))[1]
upperehat <- HPDI(data.frame(drcPost2$ehat))[2]

#Lower mean values
lowerMeanYs <- uniqueXs
for (i in 1:length( uniqueXs)){
	lowerMeanYs[i] <- lowerc + (( lowerd - lowerc) / (1 + exp(lowerb*(log(uniqueXs[i])-lowerehat))))
}

#Upper mean values
upperMeanYs <- uniqueXs
for (i in 1:length( uniqueXs)){
	upperMeanYs[i] <- upperc + (( upperd - upperc) / (1 + exp(upperb*(log(uniqueXs[i])-upperehat))))
}

#Getting the maximum and minimum effect of variety (not bothering with b because no real effect)
VarietyEffectsMean <- colMeans(data.frame(drcPost2$var_d ))
min_d_var <- min(VarietyEffectsMean)
max_d_var <- max(VarietyEffectsMean)

#Getting the maximum and minimum effect of site
siteEffectsMean <- colMeans(data.frame(drcPost2$site_d ))
min_d_site <- min(siteEffectsMean)
max_d_site <- max(siteEffectsMean)

#Lower mean values with min site and variety 
lowerMeanYsVarSite <- uniqueXs
for (i in 1:length( uniqueXs)){
	lowerMeanYsVarSite[i] <- (lowerc + (( lowerd + min_d_var + min_d_site  - lowerc) / (1 + exp(lowerb*(log(uniqueXs[i])-lowerehat)))))* -1
}

#Upper mean values with max site and variety 
upperMeanYsVarSite <- uniqueXs
for (i in 1:length( uniqueXs)){
	upperMeanYsVarSite[i] <- (upperc + (( upperd + max_d_var + max_d_site - upperc) / (1 + exp(upperb*(log(uniqueXs[i])-upperehat)))))*-1
}

#adding obervational model sigma_g
meanSigmaG <- mean(drcPost2$sigma_g)
upperMeanYsSigma <- upperMeanYsVarSite - meanSigmaG
lowerMeanYsSigma <- lowerMeanYsVarSite +  meanSigmaG


plottingDataPost2 <- data.frame(cbind(uniqueXsRaw, meanYs))
plottingDataPost2$negMeanYs <- -meanYs
plottingDataPost2$upperMeanYs <- -upperMeanYs
plottingDataPost2$lowerMeanYs <- -lowerMeanYs
plottingDataPost2$upperMeanYsVarSite <- upperMeanYsVarSite
plottingDataPost2$lowerMeanYsVarSite <- lowerMeanYsVarSite
plottingDataPost2$upperMeanYsSigma <- upperMeanYsSigma
plottingDataPost2$lowerMeanYsSigma <- lowerMeanYsSigma


postTempsPlot <- ggplot(data = plottingDataPost2, aes(x = uniqueXsRaw, y = negMeanYs ))
postTempsPlot + 
	geom_line(colour = "palevioletred4") + 
	theme_classic() +
	geom_ribbon(aes(ymin= lowerMeanYsSigma, ymax=  upperMeanYsSigma),fill = "palevioletred", alpha = 0.3) +
	geom_ribbon(aes(ymin= lowerMeanYsVarSite, ymax=  upperMeanYsVarSite),fill = "palevioletred2", alpha = 0.3) +
	geom_ribbon(aes(ymin= lowerMeanYs, ymax=  upperMeanYs),fill = "palevioletred3", alpha = 0.3) +
	geom_point(aes(x = mean1DayC , y = lte ), data = bhclimClean2)

#Whats going on with the less cold hardy values around -2?
bhclimClean2[bhclimClean2$lte > -15 & bhclimClean2$mean1DayC < 0,]
data2014 <- bhclimClean2[bhclimClean2$Year == 2014,]
problemPoints <- bhclimClean2[bhclimClean2$date == "2014-11-11",]
par(mfrow=c(2,1)) 
#Hardiness
plot(data2014$date, data2014$lte  , pch = 16, cex.lab = 1.5, cex.axis = 1.5, xlab = "", ylab = "hardiness")
points(problemPoints$date, problemPoints$lte, pch = 16, col = 2)
#airtemp
plot(data2014$date, data2014$mean1DayC  , pch = 16, cex.lab = 1.5, cex.axis = 1.5, xlab = "date", ylab= "air temperature")
points(problemPoints$date, problemPoints$mean1DayC, pch = 16, col = 2)
par(mfrow=c(1,1)) 



extremesP <- quantsSimYPost[c(2, 5),]
quatersP <- quantsSimYPost[c(3, 4),]

plottingDataPost <- data.frame(t(quantsSimYPost))
plottingDataPost$MeanY <- meanySimPost 
plottingDataPost$x <- bhclimClean2 $mean1DayC 

#plot of predicted mean values and 89% highest probability density index 
postTempsPlot <- ggplot(data = plottingDataPost, aes(x = x, y = MeanY ))
postTempsPlot + 
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()+
	geom_point(aes(x = bhclimClean2 $mean1DayC , y = bhclimClean2$lte ))



plot(colMeans(y_sim) ~ bhclimClean2 $mean1DayC)

plot(colMeans(y_mu) ~ bhclimClean2 $lte)

plot(colMeans(y_sim) ~ bhclimClean2 $lte)



#What are the predicted parameter values?

e_post <- exp(drcPost$ehat)
hist(e_post-30)

b_post <- drcPost$b
hist(b_post)

c_post <- drcPost$c
hist(c_post)

d_post <- drcPost$d
hist(d_post)

sigma_post <- drcPost$sigma_g
hist(sigma_post)


d_var_sigma <- drcPost$d_var_sigma # one each itteration (4000)
hist(d_var_sigma)

d_site_sigma <- drcPost$d_site_sigma # one each itteration (4000)
hist(d_site_sigma)

#compare varietyt and site effect 
plot(density(d_var_sigma), col = 2)
lines(density(d_site_sigma), col = 4)
text(1.5, 2, "Blue = site \nRed = variety")

#Variety plot - max hardiness 
rawVarDs <- data.frame(drcPost$d_var_raw)# each row is an itteration and each column a variety
varD <- rawVarDs

for (i in 1:n_vars){ # get variety level differences from ncp
	varD[i,] <- rawVarDs[i,] * drcPost$d_var_sigma[i]
}

meanVarEffect <- colMeans(varD)+ mean(drcPost$d)
quantsVarEffects <- apply( varD, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

meanDneg<- mean(drcPost$d)

dvarsG <- meanVarEffect + mean(drcPost$d)

colnames(varD) <- levels(as.factor(as.character(bhclimClean2$variety)))

color_scheme_set("viridis")
mcmc_intervals(-varD + -drcPost$d) + geom_vline(xintercept = -meanDneg, linetype="dotted", color = "grey")  #intercepts 

quantsVarEffectsHPDI <- apply( varD, 2 , HPDI)
quantsVarEffectsHPDI + mean(drcPost$d)

#Hardiness taken from fergusonetal2014
fergusonHardinessNames <- c( "Cabernet Franc", "Cabernet Sauvignon", "Chardonnay", "Gewurztraminer", 
	"Merlot",  "Pinot gris",  "Riesling","Sauvignon blanc","Shiraz", "Viognier")

Values <- c(-23.5, -25.4,-25.7,-24.9 ,-25.0,-24.0,-26.1,-24.9,-24.2,-24.0)
ferValues <- data.frame(cbind(fergusonHardinessNames, Values))
ferValues$Values <- as.numeric(as.character(ferValues$Values))

comparisonVarieties <- -1*(varD[,colnames(varD)%in%fergusonHardinessNames] +  mean(drcPost$d))

names(comparisonVarieties)

hist(comparisonVarieties[,1], main = "Cabernet Franc")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Cabernet Franc"], , col="red", lwd=3, lty=2)

hist(comparisonVarieties[,2], main = "Cabernet Sauvignon", xlim = c(-26, -20))
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Cabernet Sauvignon"], , col="red", lwd=3, lty=2)

hist(comparisonVarieties$Chardonnay, main = "Chardonnay")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Chardonnay"], , col="red", lwd=3, lty=2)

hist(comparisonVarieties$Gewurztraminer, main = "Gewurztraminer")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Gewurztraminer"], , col="red", lwd=3, lty=2)

hist(comparisonVarieties$Merlot, main = "Merlot")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Merlot"], , col="red", lwd=3, lty=2)

hist(comparisonVarieties[,6], main = "Pinot gris")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Pinot gris"], , col="red", lwd=3, lty=2)

hist(comparisonVarieties$Riesling, main = "Riesling")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Riesling"], , col="red", lwd=3, lty=2)

hist(comparisonVarieties[,8], main = "Sauvignon blanc")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Sauvignon blanc"], , col="red", lwd=3, lty=2)

hist(comparisonVarieties$Viognier, main = "Viognier")
abline(v = ferValues$Values[ferValues$fergusonHardinessNames == "Viognier"], , col="red", lwd=3, lty=2)


#Variety plot - slope
rawVarBs <- data.frame(drcPost$b_var_raw)# each row is an itteration and each column a variety
varB <- rawVarBs

for (i in 1:n_vars){ # get variety level differences from ncp
	varB[i,] <- rawVarBs[i,] * drcPost$b_var_sigma[i]
}

meanVarEffectB <- colMeans(varB)
quantsVarEffectsB <- apply( varB, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

meanB<- mean(drcPost$b)

bvarsG <- meanVarEffectB + mean(drcPost$b)

colnames(varB) <- levels(as.factor(as.character(bhclimClean2$variety)))

color_scheme_set("viridis")
mcmc_intervals(varB + -drcPost$b) + geom_vline(xintercept = meanB, linetype="dotted", color = "grey")  #intercepts 



#site Plot 

rawsiteDs <- data.frame(drcPost$d_site_raw)# each row is an itteration and each column a variety
siteD <- rawsiteDs

for (i in 1:n_sites){ # get variety level differences from ncp
	siteD[i,] <- rawsiteDs[i,] * drcPost$d_site_sigma[i]
}

meanSiteEffect <- colMeans(siteD)
quantsSiteEffects <- apply( siteD, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

meanDneg <- mean(drcPost$d)

dsiteG <- meanSiteEffect + mean(drcPost$d)


colnames(siteD) <- levels(as.factor(as.character(bhclimClean2$site2)))

color_scheme_set("viridis")
mcmc_intervals(-siteD + -drcPost$d) + geom_vline(xintercept = -meanDneg, linetype="dotted", color = "grey")  #intercepts 





