rm(list = ls())
#script started by Faith Jones on the 5th March 2020, with year removed April 8th 

#running a mixed linear model of bud winter hardiness regressed against air temperature. There is grouping 
#on the intercept and slope for variety  


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


#try and simulate a multi level model of hardiness against temp partially pooled by variety 
#-------------------------------------------------------------------------------------
set.seed(16)
#model should be:

# y ~ Normal((alpha + alphaSite) + beta * x, sigma/eps)

#parameters 
#LTE50sim (y ) is simulated using parameters

#inputs
nrep <- 40 # number of reps of each variety and year (days sampled in a year and for a variety )
meanTemp <- 2
sigmaTemp <- 5
simTemps <- rnorm(nrep, meanTemp,sigmaTemp)

nvariety <- 40
varNames <- as.factor(c(1:nvariety)) # make 20 "varieties" named "1" to "20"


#parameters (mostly taken from the lmer model)

nObs <- nvariety*nrep # the number of observations  for each year and each variety combined 

#make a multivariate distribution of slopes and intercepts for each grouping effect
alpha <- -20 # overall gran mean alpha
betag <- 0.50 # overall grand mean beta

muAB <- c(alpha, betag)#combine two gran effects

#variety
sigma_vara <- 0.2 # standard deviation in intercepts for vatiety 
sigma_varb <- 0.3 # standard deviation in slopes for vatiety 
rho_var <- -0.7 # correlation between intercept and slope. Lower intercepts (more cold hardy) should have steeper slopes

sigmas_var <- c(sigma_vara, sigma_varb) # combine sigma values into a vector
Rho_var <- matrix(c(1, rho_var, rho_var, 1), nrow = 2) # correlation matrix
Sigma_var <- diag(sigmas_var) %*% Rho_var %*% diag(sigmas_var) # this does some matrix black magic
varEffects <- mvrnorm(nvariety, muAB, Sigma_var)#get overall slopes and intercepts for each year 

alphaVarObs <- rep(varEffects[,1], each = nrep) # replicate the value so i can a variety specific mean for each observation

betaVarObs <- rep(varEffects[,2], each = nrep) # replicate the value so i can a variety specific slope for each observation

#other model parameters (no grouing)
sigma <-  1
eps <- rnorm(nObs , 0, sigma)

#make columns for teh name of the year, variety and day of the year 
varNamesRep <- rep(varNames, each = nrep)


#sigma <- sqrt(sigma2)


simLTEVar <- alphaVarObs  + betaVarObs * simTemps + eps

#combine into a single data table

simVarData <- data.frame(cbind(simTemps, varNamesRep, simLTEVar))
str(simVarData)
simVarData[order(simVarData$varNames),]
simVarData$varNamesRep <- as.factor(simVarData$varNamesRep )

#prior predictive checks 
#---------------------------------------------


#range of temperatures to simulate over
Ni <- 10 #number of repeat runs of the model 
nObs <- 30 # number fo temperature observations per simulation 
preTemps <- rnorm(30, -15, 20) # i think this is a sensible range of winter temps

#how do i chose a prior for the covarience structure???? I guess that it needs to be a negative number. Normal as well?
R <- rlkjcorr(1e4, K = 2, eta = 2)
plot(density(rlkjcorr(Ni, K = 2, eta = 2)[,1,2]))#what this prior looks like 

#1st level parameters 
alpha_g <-  rnorm(Ni, -15, 12)
beta_g <- rlnorm(Ni, 0 , 1)
sigma_g <- rnorm(Ni, 0, 5)#i wont need thsi to simulate mu values 

#second level parameters 
Rho_varNi <- rlkjcorr(Ni, K = 2, eta = 2)[,1,2]#prior for correlation matrix 
Sigma_vara <- rnorm(Ni, 0, 1) # standard deviation in intercepts for variety  
Sigma_varb <- rnorm(Ni, 0, 1) # standard deviation in slopes for variety

#make a dataframe for the outputs of random effects 
n_vars  <- 20 # number of random effects 
varietyEffecSim <- rep(c(1:n_vars), times = Ni)

repetition <- rep(1:Ni, each = n_vars)
randomEffectsPre <- data.frame(varietyEffecSim)
randomEffectsPre$alpha_v <- NA # this will be the alpha values for each varity, not teh grand alpha 
randomEffectsPre$rep <- repetition
#randomEffectsPre$alpha_g <- rep(alpha_g , each = n_vars)
#randomEffectsPre$beta <- rep(beta, each = n_vars) Thsi will come from teh below loop so each variety has a different slope
#randomEffectsPre$x <- rep(preTemps, times = n_vars)


#loop for making 100 different repetitions of the model, where there are 20 varieties and 20 years 
#i then repeat the values so that there is a combination of variety and year for each value 
for (counter in 1:Ni){

	#counter <- 1

	#Variety effect on alpha and beta  
	muABs <- c(alpha_g[counter], beta_g[counter])#combine two grand effects

	sigma_varas <- Sigma_vara[counter]# standard deviation in intercepts for year 
	sigma_varbs <- Sigma_varb[counter] # standard deviation in slopes for year 
	rho_vars <- Rho_varNi[counter] # correlation between intercept and slope. Lower intercepts (more cold hardy) should have steeper slopes

	sigmas_vars <- c(sigma_varas, sigma_varbs) # combine sigma values into a vector
	Rho_vars <- matrix(c(1, rho_vars, rho_vars, 1), nrow = 2) # correlation matrix
	Sigma_vars <- diag(sigmas_vars) %*% Rho_vars %*% diag(sigmas_vars) # this does some matrix black magic
	varEffectss <- mvrnorm(n_vars, muABs, Sigma_vars)#get overall slopes and intercepts for each year 

	# alpha values for each variety 
	randomEffectsPre$alpha_v[randomEffectsPre$rep == counter] <- varEffectss[,1]

	# beta values for each variety 
	randomEffectsPre$beta_v[randomEffectsPre$rep == counter] <- varEffectss[,2]

}

plot(simVarData$ simLTEVar ~ simVarData$simTemps, col = "grey74")

for (i in 1:length(randomEffectsPre$alpha_v)){
	abline(a = randomEffectsPre$alpha_v[i], b = randomEffectsPre$beta_v[i], col = rgb(red = 1, green = 0, blue = 0, alpha = 0.8))
}

plot(density(randomEffectsPre$alpha_v))
plot(density(randomEffectsPre$beta_v))

#it is trying a few vrazy values, but generally ok i think?

#Run Stan Model on simulated data
#------------------------------------------


head(simVarData)

x <- I(simVarData$simTemps)
y <- simVarData$simLTEVar
N <- length(simVarData$simTemps)
variety <- as.integer(as.factor(simVarData$varNames ))
n_vars <- length(unique(varNamesRep))


#data passed to STan needs to be a list of named objects. names here need to match names in model code
#i make a LIST of the different varables, NOT data frame

stan_data4 <- list(N = N, x = x, y = y, n_vars = n_vars,  variety = variety )




#try a model with a less complicated structure (no covarience) 
#------------------------------------------------------


fit8 <- stan(file = "slope_varietySimple.stan", data = stan_data4, warmup = 2000, 
	iter = 8000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15))

launch_shinystan(fit8)

post8 <- extract.samples(fit8)

str(post8)

plot(density(post8$alpha_g))#-20 - overestimating this 

plot(density(post8$beta_g))#0.5 - overestimating this 

plot(density(post8$za_variety))

plot(density(post8$sigma_alpha_v))#0.3 - estimating ok 

plot(density(post8$varbeta))#

plot(density(post8$sigma_beta_v))#0.2 - estimating ok

#attemp at non centred parametrisation to get model working better
#---------------------------------------------------------------------


fit10 <- stan(file = "nonCentre_slopeVarietyCov.stan", data = stan_data4, warmup = 2000, 
	iter = 4000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15, adapt_delta = 0.80))

launch_shinystan(fit10)

#extra notes for teh chapter:

#Just because a marginal posterior overlaps zero does not mean we should think of it as zero.

#launch_shinystan(fit3)

post10 <- extract.samples(fit10)
str(post7)

#model with a full covarience structure and no non-centred parameterisation 
#-----------------------------------------------------------------------------

stan_data4 <- list(N = N, x = x, y = y, n_vars = J, N = N, variety = Variety)

fit6 <- stan(file = "slopeVarietyCov.stan", data = stan_data4, warmup = 2000, 
	iter = 4000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15, adapt_delta = 0.95))

launch_shinystan(fit6)

post <- extract.samples(fit6)
postd<-  as.data.frame(post)
pairs(postd[,c(1,2,3,113, 114, 45, 46, 47)])

str(postd)
names(postd)

str(post8)

plot(density(postd$alpha_g))#-20 - overestimating this 

plot(density(postd$beta_g))#0.5 - overestimating this 



#model with no correlation matrix and non-centred parameterisation on the sigma alpha var
#------------------------------------------------------------------------------------


fit9 <- stan(file = "noncentred_slope_varietySimple.stan", data = stan_data4, warmup = 2000, 
	iter = 4000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15))

launch_shinystan(fit9)

