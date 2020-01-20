rm(list = ls())
#script started by Faith Jones on the 10th Jan 2020 

#running a simpel linear model of hardiness againts temperature for chardonay
#using Carl's data

setwd("/home/faith/Documents/github/bcvin/hardiness/analyses/")


#libraries
#install.packages("reshape2")
library(reshape2)
library(ggplot2)
library(rstan)
library(lme4)
library(rstanarm)
library(truncnorm)


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


#simple lm

lmFit <- lm(lte ~ meanC, data = bhclim)
summary(lmFit)
plot(bhclim$lte ~ bhclim$meanC)
abline(lmFit, col = "red")

lmFit2 <- lm(lte ~ year, data = bhclim)
summary(lmFit2)
yearPlot <- ggplot(aes(x = year, y = lte), data = bhclim)
yearPlot + geom_boxplot()+theme_classic()

lmFit3 <- lm(lte ~ variety, data = bhclim)
summary(lmFit3)
varietyPlot <- ggplot(aes(x = variety, y = lte), data = bhclim)
varietyPlot + geom_boxplot()+
	theme_classic()+ ylab("LTE50")+
	theme(axis.text.x = element_text(angle = 90, hjust = 1))

sitePlot <- ggplot(aes(x = site, y = lte), data = bhclim)
sitePlot + geom_boxplot()+
	theme_classic()+ ylab("LTE50")+
	theme(axis.text.x = element_text(angle = 90, hjust = 1))



#simulating a linear model
#-----------------------------

#i dont knwo what thsi does, but it helps with reproducibility somehow
set.seed(16)

#parameter values taken from the linear mnodel lmFit
alpha <- -21.4
beta <- 0.53
sigma <- 0.6 
#sigma <- sqrt(sigma2)
nrep <- 200

meanTemp <- mean(bhclim$meanC)
sigmaTemp <- sd(bhclim$meanC)
simTemps <- rnorm(nrep, meanTemp,sigmaTemp)
eps <- rnorm(nrep, 0, sigma)

simLTE <- alpha + beta*simTemps + eps
plot(simLTE ~ simTemps)

#prior predictive check
#------------------------------

set.seed(16)

#simulate over the range of simulated temperatures simTemps


#here are teh priors used in the below stan model 
N <- 100
alphaPrior <- rnorm (1e4, 0, 30); #these should be weakly informative priors 
betaPrior <-  rnorm(1e4, 0, 10);
sigmaPrior <-  rnorm(1e4, 0, 10); 

plot(NULL, xlim = range(simTemps), ylim = c(-50, 50), xlab = "Temperature",
	ylab = "LTE50")
xbar <- mean(simTemps)
for ( i in 1:N ) curve( alphaPrior[i] + betaPrior[i]*(x - xbar) ,
from=min(simTemps) , to=max(simTemps) , add=TRUE ,
col=col.alpha("black",0.2) )

#these priors look terrible! I am now going to try and make some better ones

alphaPrior2 <- rtruncnorm (n=1e4, a = -Inf, b = -3, mean = -10, sd = 10) # my theory is that the LTE value starts at -3 (temperature where 
# green tissue dies of cold) so the LTE when temp is o the highest value it can be is -3 
dens(alphaPrior2)

betaPrior2 <- rlnorm(1e4, 0 , 1) # physiologically, teh relationship between LTE50 and temoerature should be 
#positive, so i use a lognormal distribution that onlu suggets positive numbers 
dens( betaprior2 , xlim=c(0,5) , adj=0.1 )

#plot again - still some pretty extream possibilities, but much better 
plot(NULL, xlim = range(simTemps), ylim = c(-50, 50), xlab = "Temperature",
	ylab = "LTE50")
xbar <- mean(simTemps)
for ( i in 1:N ) curve( alphaPrior2[i] + betaPrior2[i]*(x - xbar) ,
from=min(simTemps) , to=max(simTemps) , add=TRUE ,
col=col.alpha("black",0.2) )


#preparing the data
#-----------------------------

# I need to index the x values from 1 to n(x)

x <- I(simTemps)
y <- simLTE
N <- length(simTemps)

#data passed to STan needs to be a list of named objects. names here need to match names in model code
#i make a LIST of the different varables, NOT data frame
stan_data <- list(N = N, x = x, y = y)

#stan model 
#--------------------


write("// Stan model for simple linear regression including priors 
data {
 int < lower = 1 > N; // Sample size
 
 vector[N] x; // Predictor
 vector[N] y; // Outcome
}
parameters {
 real  < upper = -3 > alpha; // Intercept - truncated at maximum LTE50 of -3 degrees C 
 real beta; // Slope (regression coefficients)
 real < lower = 0 > sigma; // Error SD
}
model {
	alpha ~ normal (-10, 10); //these shoudl eb very weakly informative priors
	beta ~ lognormal(0, 1);
	sigma ~ normal(0, 10); 
	y ~ normal(alpha + x * beta , sigma);
}
generated quantities {
} // The posterior predictive distribution",

"stan_model3.stan")

stan_model3 <- "stan_model3.stan"

fit3 <- stan(file = stan_model3, data = stan_data, warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1)
fit3

posterior2 <- extract(fit3)
par(mfrow = c(1,3))

plot(posterior2$alpha, type = "l")
plot(posterior2$beta, type = "l")
plot(posterior2$sigma, type = "l")

traceplot(fit3)

stan_dens(fit3)

#this is how it compares to the original vales for parameters (blue lines)
plot(density(posterior2$alpha), main = "Alpha")
abline(v = alpha, col = 4, lty = 2)

plot(density(posterior2$beta), main = "Beta")
abline(v = beta, col = 4, lty = 2)

plot(density(posterior2$sigma), main = "Sigma")
abline(v = sigma, col = 4, lty = 2)

#probability of a positive relationship between temp and LTE50
sum(posterior2$beta > 0)/length(posterior2$beta) # 1 - its definitly a positive relationship 


#Run a hierarchical model using lme4
#----------------------------------------------

#model with varying intercepts
lmerModel1 <- lmer(lte ~ meanC + (1 |variety) + (1|site), data = bhclim)
summary(lmerModel1)

#model with varying slopes as well
lmerModel2 <- lmer(lte ~ meanC + (1 + meanC|variety) , data = bhclim)#error, doesnt fit


M1_stanlmer <- stan_lmer(lte ~ meanC + (1 |variety) + (1|site), 
	data = bhclim,
	seed = 16)
print(M1_stanlmer, digits = 2)
sims <- as.matrix(M1_stanlmer)
dim(sims)
head(sims)

#simulate data with random intercepts 
#------------------------------------
#------------------------------------

#extract the effect of each site from the M2_stanlmer model

M2_stanlmer <- stan_lmer(lte ~ meanC + (1|site), 
	data = bhclim,
	seed = 16)

print(M2_stanlmer, digits = 2)
simsSite <- as.matrix(M2_stanlmer)
head(simsSite)
dens(simsSite[,3])
siteAlpha <- colMeans(simsSite)[3:(ncol(simsSite)-2)]
siteNames <- unique(bhclim$site)[order(unique(bhclim$site))]
siteEffects <- data.frame(cbind(as.character(siteNames), siteAlpha))
colnames(siteEffects)[1] <- "siteName"



#i dont knwo what thsi does, but it helps with reproducibility somehow
set.seed(16)

#parameter values taken from the linear mnodel lmFit
alpha <- -21.4
beta <- 0.53
sigma2 <- 0.6 #(2.7/sqrt(nrow(bhclim)) = 0.055)
nrep <- 20*length(unique(bhclim$variety))

meanTemp <- mean(bhclim$meanC)
sigmaTemp <- sd(bhclim$meanC)
simTemps <- rnorm(nrep, meanTemp,sigmaTemp)
eps <- rnorm(nrep, 0, sigma)

#grouping variables 
variety <- bhclim$variety
site <- bhclim$site

#group level effects - Parameters taken from M1_stanlmer
#try with one random effect first (site)

randomeffects <- data.frame(cbind(as.character(variety), as.character(site)))

#variety 
#-----------------

varietySE <- 0.7/nrow(randomeffects)
varietySD <- 2  # setting the standard error variation from variety , taken from lmer model
varEffect <- rnorm(unique(variety), mean = 0 , sd = varietySD ) # random distribution of errors based on varietySE, oen for each variety 
vareffectFrame <- data.frame(unique(variety)) # make a dataframe to hold this info
vareffectFrame$VarEff <- varEffect # add variety effect to dataframe so we have variety and its effect 

lotsvarDataName <- rep(unique(variety), each = 20) # have 20 data points per variety 
lotsvarData <- rep(unique(varEffect), each = 20) # add standard error to that  
alphaVar <- lotsvarData # name teh effect fo variety for model building 

length(alphaVar)

plottingData <- data.frame(cbind(simLTEmixed, simTemps, as.character(lotsvarDataName)))
str(plottingData)
head(plottingData)
plottingData$simTemps <- as.numeric(as.character(plottingData$simTemps))
plottingData$simLTEmixed <- as.numeric(as.character(plottingData$simLTEmixed))

varPlotSim <- ggplot(aes(x = lotsvarDataName, y = simLTEmixed), data = plottingData)
varPlotSim + geom_boxplot()+
	theme_classic()+ ylab("LTE50")+
	theme(axis.text.x = element_text(angle = 90, hjust = 1))

varModelSite <- lmer(simLTEmixed ~ simTemps + (1|V3), data = plottingData)
summary(varModelSite)

#site
#-------------
#parameter values taken from the linear mnodel lmFit
alpha <- -21.4
beta <- 0.53
sigma2 <- 0.6 #(2.7/sqrt(nrow(bhclim)) = 0.055)
nrep <- 20*length(unique(bhclim$site))

meanTemp <- mean(bhclim$meanC)
sigmaTemp <- sd(bhclim$meanC)
simTemps <- rnorm(nrep, meanTemp,sigmaTemp)
eps <- rnorm(nrep, 0, sigma)

siteSD <- 1
siteEffect <- rnorm(unique(site), 0 , siteSD)
siteeffectFrame <- data.frame(unique(site))
siteeffectFrame$SiteEff <- siteEffect

lotssiteDataName <- rep(unique(site), each = 20)
lotssiteData <- rep(unique(siteEffect), each = 20)
length(lotssiteData)
alphaSite <- lotssiteData

length(alphaSite)


simLTEmixed <- alpha + alphaVar + beta*simTemps + eps
plot(simLTEmixed ~ simTemps)

plottingData2 <- data.frame(cbind(simLTEmixed, simTemps, as.character(lotssiteDataName)))
str(plottingData2)
head(plottingData2)
plottingData2$simTemps <- as.numeric(as.character(plottingData2$simTemps))
plottingData2$simLTEmixed <- as.numeric(as.character(plottingData2$simLTEmixed))

sitePlotSim <- ggplot(aes(x = lotssiteDataName, y = simLTEmixed), data = plottingData)
sitePlotSim + geom_boxplot()+
	theme_classic()+ ylab("LTE50")+
	theme(axis.text.x = element_text(angle = 90, hjust = 1))


simModelSite <- lmer(simLTEmixed ~ simTemps + (1|V3), data = plottingData2)

lm(simLTEmixed ~ simTemps, data = plottingData)

#try two levels of variation, site and variety
#------------------------------------------------------


#group level effects - Parameters taken from M1_stanlmer
#try with one random effect first (site)

randomeffects # a dataframe of the two random effects, site and variety, from actual data
names(randomeffects) <- c("variety", "site")
randomeffectsSite <- merge(randomeffects, siteeffectFrame, by.x = "site", by.y = "unique.site.")
randomeffectsALL <- merge(randomeffectsSite, vareffectFrame,  by.x = "variety", by.y = "unique.variety.")
head(randomeffectsALL)


nrep <- nrow(bhclim)
alpha <- -21.4
beta <- 0.53
sigma2 <- 0.6 #general noise
meanTemp <- mean(bhclim$meanC)
sigmaTemp <- sd(bhclim$meanC)
simTemps <- rnorm(nrep, meanTemp,sigmaTemp)

eps <- rnorm(nrep, 0, sigma)


siteEffectAlpha <- randomeffectsALL$SiteEff
varEffectAlpha <- randomeffectsALL$VarEff

simLTEmixed <- alpha + siteEffectAlpha + varEffectAlpha + beta*simTemps + eps
plot(simLTEmixed ~ simTemps)

#make a datagframe containing teh simulated data and levels 
simData2level <-  randomeffects
simData2level$simTenps <- simTemps
simData2level$simLTEmixed <- simLTEmixed

#test model - currently not working 
simModelSiteVar <- lmer(simLTEmixed ~ simTemps + (1|site) + (1|variety), data = simData2level)