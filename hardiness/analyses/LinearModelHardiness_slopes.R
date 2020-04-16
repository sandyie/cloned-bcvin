rm(list = ls())
#script started by Faith Jones on the 5th March 2020, with year removed April 8th 

#running a mixed linear model of bud winter hardiness regressed against air temperature. There is grouping 
#on the intercept and slope for variety  

#the best model at this time is slopeVarietyCov.stan (fit 6), which has a covarience structure and
#no non centred parameterisation 

#there are three more models in thsi script as well with either no covarience structure or non-centred parameterisation (or both)

if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/vinmisc/bcvin/hardiness/analyses/") 
} else
setwd("/home/faith/Documents/github/bcvin/hardiness/analyses/")


#libraries
#install.packages("reshape2")
library(reshape2)
library(ggplot2)
library(rstan)
library(lme4)
library(rstanarm)
library(truncnorm) # truncated normal distribution 
library(fitdistrplus) # fitting a gamm adsitribution 
library(brms)
library(rethinking) # for HPDI function 
library(scales) # alpha making plotting points translucent 
library(bayesplot)# nice posterior check plots 
library(tidyr)
library(dplyr)
library(shinystan)
library(MASS)

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

#explore a bit 
head(bhclim)
str(bhclim)
plot(bhclim$lte ~ bhclim$meanC)
climatehardPlot <- ggplot(aes(x = meanC, y = lte), data = bhclim)
climatehardPlot + geom_point(aes(colour = factor(Year))) +	
	theme_classic() + ylab("LTE50")

climatePlot <- ggplot(aes(x = Datestrptime, y = lte), data = bhclim)
climatePlot + geom_point() +
  xlab("Date") + ylab("LTE50")	+
	theme_classic()


plot(bhclim$lte ~bhclim $Datestrptime)
plot(bhclim$lte ~bhclim $month_day )
plot(bhclim$lte ~bhclim $meanC)


plot(bhclim$lte ~ bhclim$meanC)
abline(lmFit, col = "red")

yearPlot <- ggplot(aes(x = year, y = lte), data = bhclim)
yearPlot + geom_boxplot()+theme_classic()

varietyPlot <- ggplot(aes(x = variety, y = lte), data = bhclim)
varietyPlot + geom_boxplot()+
	theme_classic()+ ylab("LTE50")+
	theme(axis.text.x = element_text(angle = 90, hjust = 1))

sitePlot <- ggplot(aes(x = site, y = lte), data = bhclim)
sitePlot + geom_boxplot()+
	theme_classic()+ ylab("LTE50")+
	theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Try with the real Data
#----------------------------
#----------------------------------------

#using the model with a covarience structure and no non-centred parameterisation 
#------------------------------------------------------------------------------

#this model now doesnt converge. There is a problem with teh partial pooling on slopes for varieties. (banana plot)
head(bhclim)

#remove na rows
bhclimClean2 <- bhclim[!is.na(bhclim$lte),]

#remove rows where no variety data given 
bhclimClean <- bhclimClean2[!bhclimClean2$variety == "",]

x2 <- I(bhclimClean$meanC)
y2 <- bhclimClean$lte
N2 <- length(bhclimClean$meanC)
variety2 <- as.integer(as.factor(as.character(bhclimClean$variety)))
n_vars2 <- length(unique(bhclimClean$variety))


#data passed to STan needs to be a list of named objects. names here need to match names in model code
#i make a LIST of the different varables, NOT data frame

stan_data_real <- list(N = N2, x = x2, y = y2, n_vars = n_vars2,  variety = variety2 )
str(stan_data_real)

fitReal_cov <- stan(file = "stan/slopeVarietyCov.stan", data = stan_data_real, warmup = 4000, 
	iter = 6000, chains = 4, cores = 4, thin = 1)

launch_shinystan(fitReal_cov)

#try with covariance and ncp
#------------------------------------

fitRal_covncp <- stan(file = "stan/nonCentre_slopeVarietyCov.stan", data = stan_data_real, warmup = 4000, 
	iter = 6000, chains = 4, cores = 4, thin = 1, control= list(adapt_delta = 0.95))

postRealCov <- extract.samples(fitRal_covncp)


plot(density(postRealCov$alpha_g))#

plot(density(postRealCov$beta_g))#

plot(density(postRealCov$var_sigma[,1] ) )#

plot(density(postRealCov$var_sigma[,2] ) )#P. 

plot(density(postRealCov$sigma_y))#

#make an easier to suse variable for effect on alpha and beta

postRealCov$sigma_alpha_v <- postRealCov$var_sigma[,1]
postRealCov$sigma_beta_v <- postRealCov$var_sigma[,2]

#Predicted values
meanRealY2 <- colMeans(data.frame(postRealCov$ymu))
SDRealY2<- apply(data.frame(postRealCov$ymu), 2, HPDI) 
str(postRealCov)

realYs2 <- data.frame(meanRealY2)
head(realYs2)
realYs2$upperHPDI <- SDRealY2[1,]
realYs2$lowerHPDI <- SDRealY2[2,]

#plot predicted valuea against temp
#black lines are the mean slopes, coloured ones are the HPDI varience 

plot(realYs2$lowerHPDI ~ bhclimClean$meanC,col="green")
points(realYs2$upperHPDI ~ bhclimClean$meanC,col="green")
points(realYs2$meanRealY ~ bhclimClean$meanC,col="black")

#plot predicted values against empirical ones (not including sigma_y)
#black points are teh mean values, coloured ones are the HPDI variance  

plot(realYs2$lowerHPDI, bhclimClean$lte,col="purple", type = "p", pch = 16)
points(realYs2$upperHPDI, bhclimClean$lte,col="purple", pch = 16)
points(realYs2$meanRealY, bhclimClean$lte,col="black", pch = 16)

#variety effects
varietyAlphas <- postRealCov$alpha_g + postRealCov$za_variety * postRealCov$var_alpha # i dont know how to include uncertainty in var_beta here
varietyBetas <- postRealCov$beta_g + postRealCov$zb_variety * postRealCov$var_beta # i dont know how to include uncertainty in var_beta here


meanVarietyAlpha2 <- colMeans(postRealCov$var_alpha)
meanVarietyBetaZ <- colMeans(postRealCov$var_beta)
str(meanVarietyBetaZ)
plot(meanVarietyAlpha2 ~ meanVarietyBetaZ)

#predictions for each variety 
SpeciesAlphas <- data.frame(postRealCov$var_alpha)
str(SpeciesAlphas)
nrow(SpeciesAlphas)
speciesBeta <- data.frame(postRealCov$var_beta)
names(speciesBeta) <- levels(as.factor(as.character(bhclimClean$variety)))
names(SpeciesAlphas) <- levels(as.factor(as.character(bhclimClean$variety)))
mcmc_intervals(SpeciesAlphas) + geom_vline(xintercept = mean(postRealCov$alpha_g), linetype="dotted", color = "grey")  #intercepts 
mcmc_intervals(speciesBeta ) + geom_vline(xintercept = mean(postRealCov$beta_g), linetype="dotted", color = "grey") #intercepts 




#try without the covariance structure 
#------------------------------------------
#more divergent transitions, again with sigma around partial pooling of variety around slope

realFit_nocov <- stan(file = "stan/slope_varietySimple.stan", data = stan_data_real, warmup = 4000, 
	iter = 6000, chains = 4, cores = 4, thin = 1)

launch_shinystan(realFit_nocov)

#try with partial pooling, no covariance, and non-centred parameterisation for sigma_beta_var 
#--------------------------------------------------------------------------------------------

#this one fitted ok
#I think teh probelm is that there is no real variation between varieties, which is making it 
#really hard for the model

#teh estimated different alphas for varieties have no changed with the inclusion of patial pooling on slope 
realFit_nocov_bncp <- stan(file = "stan/nonCentre_slopeVariety_beta.stan", data = stan_data_real, warmup = 2000, 
	iter = 8000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15, adapt_delta = 0.95))

launch_shinystan(realFit_nocov_bncp)



postReal <- extract.samples(realFit_nocov_bncp)

str(postReal)
names(postReal)

plot(density(postReal$alpha_g))#

plot(density(postReal$beta_g))#

plot(density(postReal$sigma_alpha_v)) #

plot(density(postReal$sigma_beta_v)) #
plot(density(postReal$sigma_y))#

#Predicted values
meanRealY2 <- colMeans(data.frame(postReal$realY))
SDRealY2<- apply(data.frame(postReal$realY), 2, HPDI) 
str(postReal)

realYs2 <- data.frame(meanRealY2)
head(realYs)
realYs2$upperHPDI <- SDRealY2[1,]
realYs2$lowerHPDI <- SDRealY2[2,]

#plot predicted valuea against temp
#black lines are the mean slopes, coloured ones are the HPDI varience 

plot(realYs2$lowerHPDI ~ bhclimClean$meanC,col="green")
points(realYs2$upperHPDI ~ bhclimClean$meanC,col="green")
points(realYs2$meanRealY ~ bhclimClean$meanC,col="black")

#plot predicted values against empirical ones (not including sigma_y)
#black points are teh mean values, coloured ones are the HPDI variance  

plot(realYs2$lowerHPDI, bhclimClean$lte,col="purple", type = "p", pch = 16)
points(realYs2$upperHPDI, bhclimClean$lte,col="purple", pch = 16)
points(realYs2$meanRealY, bhclimClean$lte,col="black", pch = 16)

#variety effects
meanVarietyAlpha2 <- colMeans(postReal$alpha_var)
meanBetaVar <- data.frame(postReal$zb_variety * mean(postReal$sigma_beta_v))
meanVarietyBetaZ <- colMeans(meanBetaVar)
str(meanVarietyBetaZ)
plot(meanVarietyAlpha2 ~ meanVarietyBetaZ)

#predictions for each variety 
SpeciesAlphas <- data.frame(postReal$alpha_var)
names(SpeciesAlphas) <- levels(as.factor(as.character(bhclimClean$variety)))
mcmc_intervals(SpeciesAlphas) + geom_vline(xintercept = mean(postReal$alpha_g), linetype="dotted", color = "grey")  #intercepts 
mcmc_intervals(meanBetaVar ) + geom_vline(xintercept = mean(postReal$alpha_b), linetype="dotted", color = "grey") #intercepts 

