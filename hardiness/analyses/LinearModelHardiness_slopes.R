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

#fitReal_cov <- stan(file = "stan/slopeVarietyCov.stan", data = stan_data_real, warmup = 4000, 
#	iter = 6000, chains = 4, cores = 4, thin = 1)

#launch_shinystan(fitReal_cov)

#try with covariance and ncp
#------------------------------------

#fitRal_covncp <- stan(file = "stan/nonCentre_slopeVarietyCov.stan", data = stan_data_real, warmup = 4000, 
#	iter = 6000, chains = 4, cores = 4, thin = 1, control= list(adapt_delta = 0.95))

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

#realFit_nocov <- stan(file = "stan/slope_varietySimple.stan", data = stan_data_real, warmup = 4000, 
#	iter = 6000, chains = 4, cores = 4, thin = 1)

#launch_shinystan(realFit_nocov)

#Try splitting into two datasets, one for endodormancy and one for ectodormance (using 1st of Jan as the breakpoint)
#--------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------

#clean site colum 
bhclimClean$site2 <- bhclimClean$site # make a new column to simplify site names. This column is too simplified according to mira 

levels(bhclimClean$site2) 
table(bhclimClean$site, bhclimClean$variety)

bhclimClean[bhclimClean$site == "Average Bud Hardiness (all sites, all varieties)",] # these must have been removed


bhclimClean$site2 <- as.character(bhclimClean$site2)
bhclimClean$site2 <- gsub("Oliver east", "Oliver, east", bhclimClean$site2 )
bhclimClean$site2 <- gsub("Osoyoos northeast", "Osoyoos, northeast", bhclimClean$site2 )


head(bhclimClean)

unique(bhclimClean$month.x)
 "Feb" "Mar" "Apr" 

monthsEndo <- c("Oct", "Nov", "Dec", "Jan")
monthsEcto <-  c( "Feb", "Mar", "Apr")

bhclimClean$timePeriod <- "Endodormancy"
bhclimClean$timePeriod [bhclimClean$month.x %in% monthsEcto] <- "Ectodormancy"

endoClim <- bhclimClean[bhclimClean$month.x %in% monthsEndo,]
ectoClim <- bhclimClean[bhclimClean$month.x %in% monthsEcto,]


#run model for endodormancy (autumn)
#----------------------------------------

x2 <- I(endoClim$meanC)
y2 <- endoClim$lte
N2 <- length(endoClim$meanC)
variety2 <- as.integer(as.factor(as.character(endoClim$variety)))
n_vars2 <- length(unique(endoClim$variety))
site <- as.integer(as.factor(as.character(endoClim$site2)))
n_site <-  length(unique(endoClim$site2))


#data passed to STan needs to be a list of named objects. names here need to match names in model code
#i make a LIST of the different varables, NOT data frame

stan_data_Endo <- list(N = N2, x = x2, y = y2, n_vars = n_vars2,  variety = variety2 , n_site = n_site, site = site)


endo_Fit_nocov_bncp <- stan(file = "stan/nonCentre_slopeSiteVarietyCov.stan", data = stan_data_Endo, warmup = 4000, 
	iter = 6000, chains = 4, cores = 4, thin = 1, control = list(max_treedepth = 15, adapt_delta = 0.99))

postEndo <- extract.samples(endo_Fit_nocov_bncp)

str(postEndo)

plot(density(postEndo$alpha_g))#
mean(postEndo$alpha_g)#pretty much the same as in ectodormancy 

plot(density(postEndo$beta_g))#
mean(postEndo$beta_g)#0.42 (as opossed to 0.7 in the ectodormancy period)

plot(density(postEndo$var_sigma[,1] ) )#
mean(postEndo$var_sigma[,1] )#1.15 vs 0.15 in ectodormancy 

plot(density(postEndo$var_sigma[,2] ) )#. 0.03 vs 0.04 in ectodormancy  
mean(postEndo$var_sigma[,2] )

plot(density(postEndo$sigma_y))#

plot(density(postEndo$Rho[,2,1]))#0.67 vs 0.48 in Ectodormancy 
mean(postEndo$Rho[,2,1])

plot(density(postEndo$site_sigma))


#Predicted values
meanRealY2 <- colMeans(data.frame(postEndo$ymu))
SDRealY2<- apply(data.frame(postEndo$ymu), 2, HPDI) 
str(postEndo)

realYs2 <- data.frame(meanRealY2)
head(realYs2)
realYs2$upperHPDI <- SDRealY2[1,]
realYs2$lowerHPDI <- SDRealY2[2,]

#plot predicted valuea against temp
#black lines are the mean slopes, coloured ones are the HPDI varience 

plot(realYs2$lowerHPDI ~ endoClim$meanC,col="green")
points(realYs2$upperHPDI ~ endoClim$meanC,col="green")
points(realYs2$meanRealY2 ~ endoClim$meanC,col="black")

#plot predicted values against empirical ones (not including sigma_y)
#black points are teh mean values, coloured ones are the HPDI variance  

plot(realYs2$lowerHPDI, endoClim$lte,col="purple", type = "p", pch = 16)
points(realYs2$upperHPDI, endoClim$lte,col="purple", pch = 16)
points(realYs2$meanRealY2, endoClim$lte,col="black", pch = 16)

#variety effects
#-----
meanAlphaVarEndo <- data.frame(postEndo$za_variety * mean(postEndo$var_sigma[,1]))
meanBetaVarEndo <- data.frame(postEndo$zb_variety * mean(postEndo$var_sigma[,2]))

meanVarietyAlpha2Endo <- colMeans(meanAlphaVarEndo)
meanVarietyBetaZEndo <- colMeans(meanBetaVarEndo)

plot(meanVarietyAlpha2Endo ~ meanVarietyBetaZEndo)

#predictions for each variety 
color_scheme_set("blue")
names(meanAlphaVarEndo) <- levels(as.factor(as.character(endoClim$variety)))
names(meanBetaVarEndo) <- levels(as.factor(as.character(endoClim$variety)))

mcmc_intervals(meanAlphaVarEndo) + geom_vline(xintercept = mean(postEndo$alpha_g), linetype="dotted", color = "grey")  #intercepts 
mcmc_intervals(meanBetaVarEndo ) + geom_vline(xintercept = mean(postEndo$alpha_b), linetype="dotted", color = "grey") #intercepts 

#site effects 
#-----
meanAlphaSiteEndo <- data.frame(postEndo$alpha_site)
names(meanAlphaSiteEndo) <- levels(as.factor(as.character(endoClim$site2)))
mcmc_intervals(meanAlphaSiteEndo) + geom_vline(xintercept = mean(postEndo$alpha_g), linetype="dotted", color = "grey")  #intercepts 


#plot slopes for each variety
plottingDataEndo <- data.frame(cbind(endoClim,realYs2))

linePlotEnd <- ggplot(data = plottingDataEndo, aes(x = meanC, y = lte))
linePlotEnd + geom_point() + 
geom_smooth(method="lm", se = FALSE, aes(x = meanC, y = meanRealY2, group = variety)) +
	theme_classic()



#run model for endodormancy (autumn)
#----------------------------------------

x3 <- I(ectoClim$meanC)
y3 <- ectoClim$lte
N3 <- length(ectoClim$meanC)
variety3 <- as.integer(as.factor(as.character(ectoClim$variety)))
n_vars3 <- length(unique(ectoClim$variety))
site3 <- as.integer(as.factor(as.character(ectoClim$site2)))
n_site3 <-  length(unique(ectoClim$site2))


#data passed to STan needs to be a list of named objects. names here need to match names in model code
#i make a LIST of the different varables, NOT data frame

stan_data_Ecto <- list(N = N3, x = x3, y = y3, n_vars = n_vars3,  variety = variety3  , n_site = n_site3, site = site3)


ecto_Fit_nocov_bncp <- stan(file = "stan/nonCentre_slopeSiteVarietyCov.stan", data = stan_data_Ecto, warmup = 4000, 
	iter = 6000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15, adapt_delta = 0.95))

postEcto <- extract.samples(ecto_Fit_nocov_bncp)

str(postEcto)

plot(density(postEcto$alpha_g))#
mean(postEcto$alpha_g)

plot(density(postEcto$beta_g))#
mean(postEcto$beta_g)

plot(density(postEcto$var_sigma[,1] ) )#
mean(postEcto$var_sigma[,1] )

plot(density(postEcto$var_sigma[,2] ) )#
mean(postEcto$var_sigma[,2] )

plot(density(postEcto$sigma_y))#

plot(density(postEcto$Rho[,2,1]))
mean(postEcto$Rho[,2,1])
str(postEcto$Rho)

plot(density(postEcto$site_sigma))

#Predicted values
meanRealY3 <- colMeans(data.frame(postEcto$ymu))
SDRealY3<- apply(data.frame(postEcto$ymu), 2, HPDI) 
str(postEcto)

realYs3 <- data.frame(meanRealY2)
head(realYs)
realYs3$upperHPDI <- SDRealY3[1,]
realYs3$lowerHPDI <- SDRealY3[2,]

#plot predicted valuea against temp
#black lines are the mean slopes, coloured ones are the HPDI varience 

plot(realYs3$lowerHPDI ~ ectoClim$meanCZ,col="green")
points(realYs3$upperHPDI ~ ectoClim$meanCZ,col="green")
points(realYs3$meanRealY ~ ectoClim$meanCZ,col="black")

#plot predicted values against empirical ones (not including sigma_y)
#black points are teh mean values, coloured ones are the HPDI variance  

plot(realYs3$lowerHPDI, ectoClim$lte,col="purple", type = "p", pch = 16)
points(realYs3$upperHPDI, ectoClim$lte,col="purple", pch = 16)
points(realYs3$meanRealY, ectoClim$lte,col="black", pch = 16)

#variety effects
meanAlphaVarEcto <- data.frame(postEcto$za_variety * mean(postEcto$var_sigma[,1]))
meanBetaVarEcto <- data.frame(postEcto$zb_variety * mean(postEcto$var_sigma[,2]))

meanVarietyAlpha2Endo <- colMeans(meanAlphaVarEcto)
meanBetaVarietyEcto <- colMeans(meanBetaVarEcto)

plot(meanVarietyAlpha2Endo ~ meanBetaVarietyEcto)

#predictions for each variety 
color_scheme_set("red")

names(meanAlphaVarEcto) <- levels(as.factor(as.character(ectoClim$variety)))
names(meanBetaVarEcto) <- levels(as.factor(as.character(ectoClim$variety)))

mcmc_intervals(meanAlphaVarEcto) + geom_vline(xintercept = mean(postEcto$alpha_g), linetype="dotted", color = "grey")  #intercepts 
mcmc_intervals(meanBetaVarEcto ) + geom_vline(xintercept = mean(postEcto$beta_g), linetype="dotted", color = "grey") #intercepts 

#site effects 
#-----
meanAlphaSiteEcto <- data.frame(postEcto$alpha_site)
names(meanAlphaSiteEcto) <- levels(as.factor(as.character(ectoClim$site2)))
mcmc_intervals(meanAlphaSiteEcto) + geom_vline(xintercept = mean(postEcto$alpha_g), linetype="dotted", color = "grey")  #intercepts 


plottingDataEcto <- data.frame(cbind(ectoClim,realYs3))

linePlotEcto <- ggplot(data = plottingDataEcto, aes(x = meanC, y = lte))
linePlotEcto + geom_point() + 
geom_smooth(method="lm", se = FALSE, aes(x = meanC, y = meanRealY2, group = variety)) +
	theme_classic()

#Compare teh two subsets of data
	#---------------------------------------
	#----------------------------------------

#General overview - how do the two grand slopes compare?
#-------------------------------------------------------

#get predicted grand mean values
#predy = alpha_g + beta_g*x - code calculating this from rethinking 
mu.linkmuEnd <- function(tempC) postEndo$alpha_g + postEndo$beta_g * tempC
tempC.seq <- seq(-12, 12 , by = 1)
muEnd <- sapply(tempC.seq, mu.linkmuEnd)
muEnd.mean <- apply( muEnd , 2 , mean )
muEnd.HPDI <- apply( muEnd , 2 , HPDI , prob=0.89 )

SplitMuEnd <- data.frame(cbind(tempC.seq, muEnd.mean, t(data.frame(muEnd.HPDI))))
names(SplitMuEnd)[c(2,3,4)] <- c("mu", "Lowerhpdi_89", "Upperhpdi_89")
SplitMuEnd$timePeriod <- "Endodormancy"

mu.linkmuEct<- function(tempC) postEcto$alpha_g + postEcto$beta_g * tempC
muEct <- sapply(tempC.seq, mu.linkmuEct)
muEct.meanEct <- apply( muEct , 2 , mean )
muEct.HPDI <- apply( muEct , 2 , HPDI , prob=0.89 )

SplitMuEct <- data.frame(cbind(tempC.seq, muEct.meanEct, t(data.frame(muEct.HPDI))))
names(SplitMuEct)[c(2,3,4)] <- c("mu", "Lowerhpdi_89", "Upperhpdi_89")
SplitMuEct$timePeriod <- "Ectodormancy"

SplitMu <- rbind(SplitMuEnd, SplitMuEct)

bothPlot <- ggplot()#data = bhclimClean, aes( x = meanC, y = lte))
bothPlot + geom_point(data= bhclimClean, aes(x = meanC, y = lte, colour = timePeriod)) +
	geom_ribbon(data= SplitMu, aes(ymin = Lowerhpdi_89, ymax = Upperhpdi_89, x = tempC.seq , fill = timePeriod), alpha = 0.3)+
	geom_smooth(method = "lm", data = SplitMu, aes(x = tempC.seq, y = mu, col = timePeriod), se = FALSE) + 
	theme_classic() +
	theme(text = element_text(size=20))
#it makes biological sense that the endodormancy slope is less steep, because i think that physiologically 
#gaining hardiness rather than lossing it is more enegetic and so difficult. 
#But maybe teh cutoff date isnt the best necause there are not any temps below -5 in ectodormancy 
#but there are for endodormancy. Might that affect the slopes, due to nonlinearity?

#grand parameters compared density plots 

plot(density(postEcto$alpha_g), col = "indianred2", main = "", xlab = "Estimated grand alpha", lwd = 2)#
lines(density(postEndo$alpha_g), col = 5, lwd = 2)

plot(density(postEcto$beta_g), col = "indianred2", main = "", xlab = "Estimated grand beta", lwd = 2, xlim = c(0.25, 0.9), ylim = c(0, 28))#
lines(density(postEndo$beta_g), col = 5, lwd = 2)

plot(density(postEcto$var_sigma[,1] ) , col = "indianred2", main = "", xlab = "Estimated alpha variety effect", lwd = 2, ylim = c(0, 1.7))#
lines(density(postEndo$var_sigma[,1] ) , col = 5, lwd = 2)#

plot(density(postEcto$var_sigma[,2] ) , col = "indianred2", main = "", xlab = "Estimated beta variety effect", lwd = 2, ylim = c(0, 20))#
lines(density(postEndo$var_sigma[,2] ) , col = 5, lwd = 2)#

plot(density(postEcto$sigma_y), col = "indianred2", main = "", xlab = "Estimated effect of site (sigma_alpha_site)", lwd = 2, xlim = c(2.1, 3.3), ylim = c(0, 9))#
lines(density(postEndo$sigma_y), col = 5, lwd = 2)#


plot(density(postEcto$Rho[,2,1]), col = "indianred2", main = "", xlab = "Estimated variety rho", lwd = 2, ylim = c(0, 1))
lines(density(postEndo$Rho[,2,1]), col = 5, lwd = 2)

plot(density(postEcto$site_sigma))

head(bhclimClean)

#compare slopes of varieties in ecto and endo dormancy
#---------------------------------------------------------

plot(colMeans(meanBetaVarEcto) ~ colMeans(meanAlphaVarEcto))

plot(colMeans(meanBetaVarEndo) ~ colMeans(meanBetaVarEcto))#I dont see a relationship between 
#the effect of a slope on a variety in endo and ecto-dormancy. But then the slopes are so similar that
#maybe it is just random variation?






#run model with site and variety and all data 
#------------------------------------------
bhclimClean$site2 <- bhclimClean$site # make a new column to simplify site names. This column is too simplified according to mira 

levels(bhclimClean$site2) 
table(bhclimClean$site, bhclimClean$variety)

bhclimClean[bhclimClean$site == "Average Bud Hardiness (all sites, all varieties)",] # these must have been removed


bhclimClean$site2 <- as.character(bhclimClean$site2)
bhclimClean$site2 <- gsub("Oliver east", "Oliver, east", bhclimClean$site2 )
bhclimClean$site2 <- gsub("Osoyoos northeast", "Osoyoos, northeast", bhclimClean$site2 )

unique(bhclimClean$site2)

x <- I(bhclimClean$meanC)
y <- bhclimClean$lte
N <- length(bhclimClean$meanC)
variety<- as.integer(as.factor(as.character(bhclimClean$variety)))
n_vars <- length(unique(bhclimClean$variety))
site <- as.integer(as.factor(as.character(bhclimClean$site2)))
n_site <-  length(unique(bhclimClean$site2))

stan_data_siteVar <- list(N = N, x = x, y = y, n_vars = n_vars, variety = variety, site = site, n_site = n_site )

stan_siteVar <- stan(file = "stan/nonCentre_slopeSiteVarietyCov.stan", data = stan_data_siteVar, warmup = 4000, 
	iter = 8000, chains = 4, cores = 4, thin = 1, control = list(max_treedepth = 15)) #treedepth needed here 

postSite <- extract.samples(stan_siteVar)


plot(density(postSite$alpha_g))#
mean(postSite$alpha_g)

plot(density(postSite$beta_g))#
mean(postSite$beta_g)

plot(density(postSite$var_sigma[,1] ) )#alpha
mean(postSite$var_sigma[,1] )

plot(density(postSite$var_sigma[,2] ) )#
mean(postSite$var_sigma[,2] )

plot(density(postSite$sigma_y))#

plot(density(postSite$Rho[,2,1]))
mean(postSite$Rho[,2,1])
str(postSite$Rho)

plot(density(postSite$site_sigma))

plot(density(data.frame(postSite$Rho)[,2]), main = "", xlab = "estimated rho variety" )#-0.00183172
mean(data.frame(postSite$Rho)[,2])

plot(density(data.frame(postSite$var_sigma)[,1]), ylim=c(0,10), col = 2, main = "", 
	xlab = "Estimated sigma value") #Partial pooling on intercept. 
lines(density(postSite$site_sigma))
lines(density(postSite$sigma_y), col = 5)# 

plot(density(data.frame(postSite$var_sigma)[,1]), ylim=c(0,3), col = 2, main = "", 
	xlab = "Estimated sigma value") #
lines(density(postSite$site_sigma))

plot(density(data.frame(postSite$var_sigma)[,2]), main = "", xlab = "estimated sigma_beta variety" ) #Partial pooling on slope. 


#plot data and slopes for variety
str(postSite)

meanPredy <- colMeans(data.frame(postSite$ymu))
SDRealY10 <- apply(data.frame(postSite$ymu), 2, HPDI) 
str(SDRealY)

realYs10 <- data.frame(meanPredy)
head(realYs10)
realYs10$upperHPDI <- SDRealY10[1,]
realYs10$lowerHPDI <- SDRealY10[2,]


#plot predicted values against empirical ones (not including sigma_y)
#black points are teh mean values, coloured ones are the HPDI variance  

plot(realYs10$lowerHPDI~ bhclimClean$lte,col="purple", type = "p", pch = 16, xlab = "Observed hardiness (lte50)", ylab = "Predicted hardiness (lte50)")
points(realYs10$upperHPDI~ bhclimClean$lte,col="purple", pch = 16)
points(realYs10$meanPredy~ bhclimClean$lte,col="black", pch = 16)

varietyAlphasData <- data.frame(postSite$za_variety)  
varietyAlphas <- colMeans(varietyAlphasData) + mean(postSite$alpha_g)
varietyBetasData <- data.frame(postSite$zb_variety)
varietyBetas <- colMeans(varietyBetasData)+ mean(postSite$beta_g)

#plot different lines over real data 
plot(bhclimClean$lte~ bhclimClean$meanC,col="grey", type = "p", pch = 16, ylab = "Observed hardiness (lte50)", xlab = "Mean daily air temp (C)")
for( i in 1:length(varietyAlphas)){
	
	abline(a = varietyAlphas[i], b = varietyBetas[i])
}
abline(a = mean(postSite$alpha_g), b = mean(postSite$beta_g), col = 2, lwd=2 )

#predictions for each variety 

#variety effects
meanAlphaVar <- data.frame(postSite$za_variety * mean(postSite$var_sigma[,1]))
meanBetaVar <- data.frame(postSite$zb_variety * mean(postSite$var_sigma[,2]))

meanVarietyAlpha2 <- colMeans(meanAlphaVar)
meanBetaVariety <- colMeans(meanBetaVar)

names(meanAlphaVar) <- levels(as.factor(as.character(bhclimClean$variety)))
names(meanBetaVar) <- levels(as.factor(as.character(bhclimClean$variety)))

mcmc_intervals(meanAlphaVar) + geom_vline(xintercept = mean(postSite$alpha_g), linetype="dotted", color = "grey")  #intercepts 
mcmc_intervals(meanBetaVar ) + geom_vline(xintercept = mean(postSite$beta_g), linetype="dotted", color = "grey") #intercepts 

#site effects 
#-----
meanAlphaSite <- data.frame(postSite$alpha_site)
names(meanAlphaSite) <- levels(as.factor(as.character(bhclimClean$site2)))
mcmc_intervals(meanAlphaSite) + geom_vline(xintercept = mean(postSite$alpha_g), linetype="dotted", color = "grey")  #intercepts 
