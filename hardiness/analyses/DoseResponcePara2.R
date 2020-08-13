# Dose response rurve model reparameterised to be more easily fit as suggested on discourse
# https://www.biorxiv.org/content/10.1101/2020.03.16.993162v1

rm(list = ls())


library(rstan)
library(truncnorm)
library(reshape2)
library(ggplot2)
library(tidyr)
library(dplyr)
library(drc)
library(bayesplot)


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
climsm <- subset(clim, select=c("Year", "month","day", "Mean.Temp..C.", "Mean.Temp.Flag", "date", "Date.Time"))
names(climsm) <- c("Year", "month","day", "meanC", "meanC.flag", "date", "Date.Time")

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
plot(bhclim$lte ~bhclim $meanC)

hist(bhclim$lte)

##simulate temperature data 
#-------------------------------------------------

#inputs
nrep <- 500 # number of reps of each variety 
meanTemp <- 4
sigmaTemp <- 8
simTemps <- rnorm(nrep, meanTemp,sigmaTemp)

#Unlike the first parameterization of the sigmoidal curve I ttried, I dont think x should be changed to eb positve. 
#Instead y0 (y value when x is 0) is teh intercept and is biologically meaningfu; because thsi is teh hardiness when 
#temperatures drop to freezing. 


#lower plateu of the function (c in other model) was set to 0 

#Set model parameters - I have no idea what they should be so I will run a similar model in R (below)

beta <- 10 #this is the rate paramater, like a slope in a linear regession 
y0 <-  15 # This is hardiness  when x = 0. intercept. 
eta <- 0 # This is teh middle point of teh x dataset. inflection point? Must be close to 0 or estimates are way off. Because of expenential?

sigma_g <- 2

#Try centering x - thsi definitly helps the simulating make sense   
xCentre <- (simTemps - mean(simTemps))/sd(simTemps)

plot(xCentre ~ simTemps)
x <- xCentre
#x <- simTemps

hardiness <-  (y0*(1 + exp(beta*eta))/ (1 + exp(beta*(eta-x))))
eps <- rnorm(n = length(hardiness), mean = 0, sd = sigma_g)
simLTEPos <- hardiness + eps
simLTE <- simLTEPos * -1

plot(simLTE ~ x , pch = 16, col = 2, xlab = "Simulated temperatures", ylab = "winter hardiness")#modefied positive  data

#What do I knwo abut my data and priors? Domain knowledge for building the model
#--------------------------------------------------------------------------------------

# Data
# x will be mean air temps, ranging from around -15 to 25 degrees c. I will standardize values using (x - mean(x))/sd(x) so that y0 is
#  right in the middle of the x range

# y - will be lte50 at a particular air temp. Values will all be negative

# Parameters
# beta - This is the slope. I know it should be negative, plants wont lose hardiness with declining air temp. By playing with 
#  the simulated data I think a range of 0-20 is fairly sensible. So a half normal of mean 0 sd 10 to start? Later I can try a gamma 
#  because I think it is fair to expect 0 to be very unlikely. We are very sure that there should be SOME relationship in grapevines

# eta - the x value in the middle of the sigmoidal curve. This is the infexion point, and must be constrained between min and max x.

#y0 - The intercept on the x axis. This is y when x is at its midpoint. I dont think this needs a prior because it can be taken 
#  directly from the data? Or if I do need a prior, then it shoudl constrain to the y values 

#Try some prior distributions
#----------------------------

betaPrior <-  rgamma(n = 1000, shape = 5, rate = 1)
hist(betaPrior)

etaPrior <- rnorm(n = 1000, mean = 0, sd = 0.15) # more than 0.35 makes the model go funny for simuated data, more than 0.15 for real data. This is mean plus/minus 10 degrees. 
etaTprior <- etaPrior*sd(simTemps) + mean(simTemps)
hist(simTemps)
hist(etaPrior)
hist(etaTprior)

y0Prior <- rnorm(n = 1000, mean = 10, sd = 5)
hist(y0Prior)

#Stan prior model 
#------------------------------------

# when doing teh prior checks I decided that it is better to feed in y data as positive, and then constrain 
#beta to also be positive. 

#Data
x <- I(xCentre)
N <- length(xCentre)

stanData_prior_drs2 <- list(N = N, x = x)

drc_prior2 <- stan(file = "stan/doseResponse2_priorCheck.stan", data = stanData_prior_drs2, 
	iter=1000, warmup=0, chains=1, refresh=1000,
   seed=4838282, algorithm="Fixed_param")


priorCheck <- rstan::extract(drc_prior2)

hist(priorCheck$mu_y)
hist(priorCheck$y_sim) # very few values above 0. That is good. 
hist(priorCheck$beta)
hist(priorCheck$y0)

str(priorCheck$y_sim)
meanSimY <- colMeans(priorCheck$y_sim) 
quantsSimY <- apply( priorCheck$y_sim , 2 , quantile , probs = c(0.05, 0.25, 0.5, 0.75, 0.95) , na.rm = TRUE )

extremes <- quantsSimY[c(1, 5),]
hist(extremes)
quaters <- quantsSimY[c(2, 4),]
hist(quaters)

half <- quantsSimY[3,]

plottingDataPrior <- data.frame(t(quantsSimY))
plottingDataPrior$MeanY <- meanSimY 
plottingDataPrior$x <- xCentre
plottingDataPrior$half <- half


priorTempsPlot <- ggplot(data = plottingDataPrior, aes(x = x, y = half ))
priorTempsPlot + geom_line() +
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	theme_classic()

hist(plottingDataPrior$MeanY)
hist(plottingDataPrior$X95.)
hist(plottingDataPrior$X5.)


#Try with simulated data
#----------------------------


#Fit simulated data to my model using priors I checked above
#---------------------------------------


x <- I(xCentre)
y <- simLTEPos # this data is simulated without variety variation  
N <- length(xCentre)

stan_data_drs <- list(N = N, x = x, y = y)



#data passed to STan needs to be a list of named objects. names here need to match names in model code
#i make a LIST of the different varables, NOT data frame

#try the most similar model I can manage to teh one on https://discourse.mc-stan.org/t/dose-response-model-with-partial-pooling/13823


#thsi used the positive transfomed data!
drc_simple <- stan(file = "stan/doseResponsedPar2.stan", data = stan_data_drs, warmup = 1000, 
	iter = 2000, chains = 4, cores = 4, thin = 1)

drcPost <- rstan::extract(drc_simple)

#how do the posteriors look?
pairs(drc_simple, pars = c("beta", "eta","y0","sigma_g", "lp__")) 

#How does teh predicted data look?
str(drcPost)
mu_post <- drcPost$mu_y * -1
hist(mu_post)

y_sim <- drcPost$y_sim * -1
hist(y_sim)
meanySimPost <- apply(y_sim, 2, mean)
quantsSimYPost <- apply( y_sim, 2 , quantile , probs = c(0, 0.05, 0.25, 0.75, 0.95, 1) , na.rm = TRUE )

extremesP <- quantsSimYPost[c(2, 5),]
quatersP <- quantsSimYPost[c(3, 4),]

plottingDataPost <- data.frame(t(quantsSimYPost))
plottingDataPost$MeanY <- meanySimPost 
plottingDataPost$x <- xCentre


postTempsPlot <- ggplot(data = plottingDataPost, aes(x = x, y = MeanY ))
postTempsPlot + 
	geom_ribbon(aes(ymin= X5., ymax=  X95.), , fill = "palevioletred", alpha = 0.5) +
	geom_ribbon(aes(ymin= X25., ymax=  X75.), , fill = "palevioletred", alpha = 0.5) + 
	geom_line() + theme_classic()+
	geom_point(aes(x = xCentre, y = simLTE))


hist(drcPost$mu_y)
hist(drcPost$y_sim) # very few values above 0. That is good. 
hist(drcPost$beta)
hist(drcPost$y0)


#How do the predicted values compare to real parameter values? (plot hist with real parameter value)

eta_post <- drcPost$eta
hist(eta_post)
abline(v=eta,col="red", lty = 2, lwd = 2)

beta <- drcPost$beta
hist(beta_post)
abline(v=beta,col="red", lty = 2, lwd = 2)

y0_post <- drcPost$y0
hist(y0_post)
abline(v=y0,col="red", lty = 2, lwd = 2)

sigma_post <- drcPost$sigma_g
hist(sigma_post)
abline(v=sigma_g,col="red", lty = 2, lwd = 2)


#Try with real data
#---------------------------------
#remove na rows
bhclimClean2 <- bhclim[!is.na(bhclim$lte),]
xCentreReal <- (bhclimClean2 $meanC  - mean(bhclimClean2 $meanC ))/sd(bhclimClean2 $meanC )

realy <- bhclimClean2$lte * -1
realx <- I(xCentreReal)
N <- length(realx)

stan_data_drs_real <- list(N = N, x = realx, y = realy)

#thsi used the positive transfomed data!
drc_simple_real <- stan(file = "stan/doseResponsedPar2.stan", data = stan_data_drs_real, warmup = 3000, 
	iter = 4000, chains = 4, cores = 4, thin = 1)




drcPost <- rstan::extract(drc_simple_real)
#how do the posteriors look?
pairs(drc_simple_real, pars = c("beta", "eta","y0","sigma_g", "lp__")) 


#how do the posteriors look?
#pairs(drc_simple)

#How does teh predicted data look?
str(drc_simple_real)
mu_post <- drc_simple_real$mu_y * -1
hist(mu_post)
plot(colMeans(mu_post) ~ bhclimClean2 $meanC)

y_sim <- drc_simple_real$y_sim * -1
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



plot(colMeans(y_sim) ~ bhclimClean2 $meanC )



















#Trying multi level model 
#-----------------------------------

#alow the slope only to vary (eta and y0 are basicly colinear, so I cant change one without the other?)
