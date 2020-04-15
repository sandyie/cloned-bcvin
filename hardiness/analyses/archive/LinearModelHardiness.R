#useful link - https://mc-stan.org/users/documentation/case-studies/rstan_workflow.html 
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
library(truncnorm) # truncated normal distribution 
library(fitdistrplus) # fitting a gamm adsitribution 
library(brms)
library(rethinking) # for HPDI function 
library(scales) # alpha making plotting points translucent 
library(bayesplot)# nice posterior check plots 
library(tidyr)
library(dplyr)

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

#prior predictive check - for a linear model
#------------------------------

set.seed(16)

#simulate over the range of simulated temperatures simTemps


#here are the priors used in the below stan model 
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



plot(bhclim$lte ~ bhclim$site)

#try and simulate a multi level model of hardiness against temp partially pooled by variety 
#-------------------------------------------------------------------------------------
set.seed(16)
#model should be:

# y ~ Normal((alpha + alphaSite) + beta * x, sigma/eps)

#parameters 
#LTE50sim (y ) is simulated using parameters
#inputs
nrep <- 30 # number of reps of each variety and year (days sampled in a year and for a variety )
meanTemp <- mean(bhclim$meanC) #2.03
sigmaTemp <- sd(bhclim$meanC) #4.81
simTemps <- rnorm(nrep, meanTemp,sigmaTemp)

nvariety <- 20
varNames <- as.factor(c(1:nvariety)) # make 20 "varieties" named "1" to "20"
nyear <- 20 # there are 20 years of data
yearNames <- as.factor(1:20) #name of each year 

#parameters (mostly taken from the lmer model)

nObs <- nyear*nvariety*nrep # the number of observations  for each year and each variety combined 

alpha <- -21.4
alphavar <- rep( rnorm(nvariety, 0, 0.3), each = nyear )# random 20 draws from a normal distribution with mean 0 and sd 0.3, repeated 20 times 
alphaVarObs <- rep(alphavar, each = nrep) # repeat each alpha for variable 30 times for each data observation 
alphaYear <- rep(rnorm(nyear, 0, 0.5), times = nvariety) # random effect of year, 20 years and each year has a each variety in it 
alphaYearObs <- rep(alphaYear, each = nrep) 
beta <- 0.52
sigma <-  0.6
eps <- rnorm(nObs , 0, sigma)

#make columns for teh name of the year, variety and day of the year 
varNamesRep <- rep(varNames, each = nyear)
varNamesObs <- rep(varNamesRep, each = nrep)
YearNameRep <- rep(1:nyear, times = nvariety)
yearNamesObs <- rep(YearNameRep, each = nrep)

#sigma <- sqrt(sigma2)

meanTemp <- mean(bhclim$meanC)
sigmaTemp <- sd(bhclim$meanC)
simTemps <- rnorm(nObs , meanTemp , sigmaTemp)
eps <- rnorm(nObs, 0, sigma)

simLTEVar <- alpha + alphaVarObs + alphaYearObs + beta*simTemps + eps
plot(simLTEVar ~ simTemps)

#combine into a single data table

simVarData <- data.frame(cbind(simTemps, varNamesObs, yearNamesObs, simLTEVar))
str(simVarData)
simVarData[order(simVarData$varNames),]
simVarData$varNamesObs <- as.factor(simVarData$varNamesObs )
simVarData$yearNamesObs <- as.factor(simVarData$yearNamesObs )

#try this simulated data in a model 

prior1 <- prior(normal(-10,10), class="Intercept")+prior(lognormal(0, 1),class="sd")+
prior(lognormal(0, 1),class="sigma") # sigma is overall variation, sd is variation from random effect 

#modelbrms <- brm(simLTEVar ~ simTemps + (1 |varNamesObs) + (1|yearNamesObs), 
#	data = simVarData,
#	family = gaussian(),
#	prior = prior1)


#varPoolM <- stan_lmer(simLTEVar ~ simTemps + (1 |varNames), 
#	data = simVarData,
#	seed = 16)
#print(varPoolM, digits = 2)


#ok, so this model is doing an ok job of predicting - 0.25 rather than 0.3. I could
#probably improve prediction with priors, but im going to try and do that in STAN

#Prior predictive checks 
#--------------------------------------

#Priors - 1st level
#
#beta ~ lognormal(0, 1)
#alpha_g ~ lormal(-15, 12), because -3 is minimum hardiness and few vines are going to be more hardy than -27 
#sigma_y ~ trunnorm(0, 5), because a variation of 10 degrees C around the mean seems quite generous
#
#2nd level
#
#sigma_v ~ trunnorm(0, 5), because I assume there is teh same variation from teh random effect as from everything else 
#

#rangge of temperatures to simulate over
Ni <- 100 #number of repeat runs of the model 
preTemps <- rnorm(Ni, -10, 20) # i think this is a sensible range of winter temps

#1st level parameters 
alpha_g <-  rnorm(Ni, -15, 12)
beta <- rlnorm(Ni, 0 , 1)

#make a dataframe for the outputs of random effects 
n_vars  <- 20 # number of random effects 
variety <- as.factor(c(1:n_vars))
yearSim <- as.factor(c(1:n_vars))
raneffect <- rep(1:n_vars, times = Ni )
repetition <- rep(1:Ni, each = n_vars)
randomEffectsPre <- data.frame(raneffect )
randomEffectsPre$sigma_v <- NA
randomEffectsPre$sigma_year <- NA
randomEffectsPre$rep <- repetition
randomEffectsPre$alpha_g <- rep(alpha_g , each = n_vars)
randomEffectsPre$beta <- rep(beta, each = n_vars)
randomEffectsPre$x <- rep(preTemps, times = n_vars)


#loop for making 100 different repetitions of the model, where there are 20 varieties and 20 years 
#i then repeat the values so that there is a combination of variety and year for each value 
for (counter in 1:Ni){

	sigma_vc <- rnorm(n_vars, 0, 5) # this is not truncated to reflect that var an make values bigger or smaller 
	sigma_yearc <- rnorm(n_vars, 0, 5)
	randomEffectsPre$sigma_v[randomEffectsPre$rep == counter] <- sigma_vc
	randomEffectsPre$sigma_year[randomEffectsPre$rep == counter] <- sigma_yearc

}

#prepare a dataframe for random effects year and variety 
twoRandomsVar <- rep(variety, each = length(yearSim))
yearSim2 <-  rep(yearSim, times = length(variety))
twoRandoms <- data.frame(rep(1:10, each = length(yearSim2)))
names(twoRandoms)[1] <- "simulation"
twoRandoms$varietySim <- rep(twoRandomsVar, times = 10) # 10 differebt varieties and years 
twoRandoms$yearSim <- rep(yearSim2, times = 10)
tail(randomEffectsPre)

#merge in the above random effects from the loop , but only for the first 20 repetations of the simulation 
randomReps <- merge(twoRandoms, randomEffectsPre[, c("raneffect", "sigma_v", "rep", "alpha_g", "beta")], by.x = c("varietySim", "simulation"), by.y = c("raneffect", "rep") ) 
randomReps2 <- merge(randomReps, randomEffectsPre[, c("raneffect", "sigma_year", "rep")], by.x = c("yearSim", "simulation"), by.y = c("raneffect", "rep") )


############# Start from here - predictive checks on the prior distribution 
x <- I(simVarData$simTemps)

# random effect of variety (Or year, they would be the same plot)
plot(NULL, xlim = range(preTemps), ylim = c(-50, 50), xlab = "Temperature", 
	ylab = "LTE50", sub = "Figure 6. The distribution of slopes suggested by my model with one effect")
xbar <- mean(preTemps)
for ( i in 1:nrow(randomEffectsPre) ) 
	curve( randomEffectsPre$alpha_g[i] + randomEffectsPre$sigma_v[i] + randomEffectsPre$beta[i]*(x - xbar) ,
from=min(preTemps) , to=max(preTemps) , add=TRUE , col = 2)
for ( i in 1:N ) 
	curve( alpha_g[i] + beta [i]*(x - xbar) ,
from=min(preTemps) , to=max(preTemps) , add=TRUE )

#random effects of variety and year combined 

plot(NULL, xlim = range(preTemps), ylim = c(-50, 50), xlab = "Temperature",
	ylab = "LTE50", sub = "Figure 7. The distribution of slopes suggested by my model with both effects")
xbar <- mean(x)
for ( i in 1:nrow(randomReps2) ) 
	curve( randomReps2$alpha_g[i] + randomReps2$sigma_v[i] + randomReps2$beta[i]*(x - xbar) ,
from=min(preTemps) , to=max(preTemps) , add=TRUE , col = 2)
for ( i in 1:10 ) 
	curve( alpha_g[i] + beta [i]*(x - xbar) ,
from=min(preTemps) , to=max(preTemps) , add=TRUE )


#two random effects model
#------------------------------
head(simVarData)

x <- I(simVarData$simTemps)
y <- simVarData$simLTEVar
N <- length(simVarData$simTemps)
Variety <- as.integer(as.factor(simVarData$varNames ))
year <- as.integer(as.factor(simVarData$yearNamesObs ))
J <- length(unique(varNamesObs))
k <- length(unique(yearNamesObs))

#data passed to STan needs to be a list of named objects. names here need to match names in model code
#i make a LIST of the different varables, NOT data frame
stan_data3 <- list(N = N, x = x, y = y, n_vars = J, N = N, K = k, year = year, variety = Variety)


write("//
// This Stan program defines a linear model predicting LTE50 from temperature, with partial pooling of variety and year 
//
// Stan model for partially pooled linear regression including priors 

data {

	//Level 1
	int < lower = 1 > N; // Sample size - number of observations
	vector[N] x; // Predictor
	vector[N] y; // Outcome

	//Level 2 
	int < lower = 1 > n_vars; // number of random effect levels (varieties) 
	int < lower = 1, upper = n_vars > variety[N]; // id of random effect (variety)

	int < lower = 1 > K; // number of random effect levels (years) 
	int < lower = 1, upper = K > year[N]; // id of random effect (year)

	}

parameters {

	//level 1
	real < upper = -3 > alpha_g; // mean intercept accross all varieties. Grand mean
	real beta; //slope accross all varieties
	real <lower =0> sigma_y; // overall variation accross observations

	//level 2
	real <lower = 0> sigma_v; // variation of intercept amoung varieties  
	real varmu[n_vars];

	real <lower = 0> sigma_k; // variation of intercept amoung varieties  
	real yearmu[K];

}
transformed parameters{
	//Individual mean 
	real ymu[N];

	//Individual mean calculation 
	for (i in 1:N){
		ymu[i] = alpha_g + varmu[variety[i]] + yearmu[year[i]];  
	}

}

model{
	//Level 1
	alpha_g ~ normal(-15,12); // prior for grand alpha, assumes intercept will negative and around -10.
	//i chose this because -3 is minimum hardiness (least hardy) and few vines can manage 
	//temps much lower than -27
	beta ~ lognormal(0,1);
	sigma_y ~ normal(0,5); // prior around estiamted mean LTE50.

	//Level 2
	varmu ~ normal(0,sigma_v); // prior for the effect of random factor on grand mean 
	sigma_v ~ normal(0, 5); // prior for the variety around levels of random factor. Same as sigma_y

	//Level 2
	yearmu ~ normal(0,sigma_k); // prior for the effect of random factor on grand mean 
	sigma_k ~ normal(0, 5); // prior for the variety around levels of random factor. Same as sigma_y

	//liklihood
	for (i in 1:N){
		y[i] ~ normal(ymu[i] + beta * x[i], sigma_y);
	}
}

generated quantities {
} // The posterior predictive distribution",

"stan_model6.stan")

stan_modelMulti6 <- "stan_model6.stan"


#fit6 <- stan(file = stan_modelMulti6, data = stan_data3, warmup = 2000, 
#	iter = 6000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15))

#Rhat values suggest convergence 

posterior6 <- extract.samples(fit6)
str(posterior6)

plot(density(posterior6$alpha)) # -21.4
plot(density(posterior6$beta))# 0.5
plot(density(posterior6$sigma_y))# 0.6
plot(density(posterior6$sigma_v))# 0.3 seems to be underestimating the influence of v a little 
plot(density(posterior6$sigma_k))# 0.5 overestimating a wee bit 

#plot post predictive checks
#------------------------------


#extract mean and HPDI values
mu.link <- function(temp) posterior6$alpha + posterior6$sigma_v + posterior6$sigma_k + posterior6$beta * temp
temp.seq <- seq(from = min(simTemps), to = max(simTemps), by = 1)
mu <- sapply(temp.seq, mu.link)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI)

#simulate values 
sim.lte50 <- sapply(temp.seq, function (temp)
	rnorm(
		n = length(posterior6$alpha),
		mean = posterior6$alpha + posterior6$sigma_v + posterior6$sigma_k + posterior6$beta * temp,
		sd = posterior6$sigma_y)
		)
lte50.HPDI <- apply(sim.lte50, 2, HPDI, prob = 0.89)


#plot model input data and posterior predictions
plot(y~x, col = alpha("royalblue3", 0.2), pch = 16)
lines(temp.seq, mu.mean)#draw mean value
shade(mu.HPDI, temp.seq)#draw HPDI around mean
shade(lte50.HPDI, temp.seq)#HPDI for simulated lte50
#it looks like there is less sigma around the mean for lower temperatures 

#run the model on real data
#-------------------------------------------


head(bhclim)
bhclimComplete <- bhclim[!is.na(bhclim$lte),]
nReal <- nrow(bhclimComplete)
x <- I(bhclimComplete$meanC)
y <- bhclimComplete$lte
year <- as.integer(as.factor(bhclimComplete$year))
n_year <- length(unique(bhclimComplete$year))
variety <- as.integer(as.factor(bhclimComplete$variety)) 
n_vars <- length(unique(bhclimComplete$variety))

stan_data_real <- list(N = nReal, x = x, y = y, n_vars = n_vars, K = n_year, year = year, variety = variety)


fit7 <- stan(file = stan_modelMulti6, data = stan_data_real, warmup = 1000, 
	iter = 6000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15))

precis(fit7)
posterior7 <- extract.samples(fit7)
str(posterior7)

plot(density(posterior7$alpha)) # 
mean(posterior7$alpha)
HPDI(as.vector(posterior7$alpha), prob = 0.89)# i dont know why this needs the as.vector, but without it 
#teh function throws an error message. *shrug*

plot(density(posterior7$beta))# 
plot(density(posterior7$sigma_y))# 
plot(density(posterior7$sigma_v))#  I think year variation explains more variation than Variety does
plot(density(posterior7$sigma_k))# 

#launch_shinystan(fit7)
color_scheme_set("red")

#extract mean and HPDI values
mu.link <- function(temp) posterior7$alpha + posterior7$sigma_v + posterior7$sigma_y + posterior7$sigma_k + posterior7$beta * temp

temp.seq <- seq(from = min(simTemps), to = max(simTemps), by = 1)
mu <- sapply(temp.seq, mu.link)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI)

plot(temp.seq, mu.mean)
#simulate values - I am not sure this is doing what I want 
#i dont knwo whether just adding the variation from random effects makes sense. Insead I think
#i should do a more complicated simulation with the random effect levels included

sim.lte50 <- sapply(temp.seq, function (temp)
	rnorm(
		n = length(posterior7$alpha),
		mean = posterior7$alpha + posterior7$sigma_v + posterior7$sigma_y + posterior7$sigma_k + posterior7$beta * temp,
		sd = posterior7$sigma_y)
		)
lte50.HPDI <- apply(sim.lte50, 2, HPDI, prob = 0.89)
lte50.mean <- apply(sim.lte50, 2, mean)
str(sim.lte50)

#plot model input data and posterior predictions
plot(y~x, col = alpha("royalblue3", 0.2), pch = 16)
lines(temp.seq, mu.mean)#draw mean value
shade(mu.HPDI, temp.seq)#draw HPDI around mean
shade(lte50.HPDI, temp.seq)#HPDI for simulated lte50

#extract the effect of variety on hardiness
#--------------------------------------------------

#make a dataframe of the how each variety varies from teh grand mean  
varietyEffects <- data.frame(posterior7$varmu)
colnames(varietyEffects) <- levels(bhclimComplete$variety)[1:13]#only the first 13 varietyies have data that is used in teh model
colnames(varietyEffects)[1] <- "noData"
#change to long format for ease of plotting 
longVarietyEffect <- varietyEffects %>% gather(key = variety, value = predLte50)
mcmc_intervals(varietyEffects)
#the variety effects seem to be making sense?

#a new dataframe where each variety effect is added to teh grand mean
VarietyEffectsPred <- varietyEffects +  posterior7$alpha
mcmc_intervals(VarietyEffectsPred)
#change to long format for ease of plotting 
longVarietyEffectPred <- VarietyEffectsPred %>% gather(key = variety, value = predLte50)

plot(lte50.mean  ~ simTemps)

#Extract teh effect of year on hardiness
#--------------------------------------------------
yearEffects <- data.frame(posterior7$yearmu)
colnames(yearEffects) <- levels(bhclimComplete$year)
mcmc_intervals(yearEffects)
meanYearFit <- apply(yearEffects, 2, mean)

#extract mean per year from original data
head(bhclim)
meanYear <- bhclim %>% group_by(Year) %>%
	summarise(meanTempYear = mean(meanC), minTempYear = min(meanC))
meanYear$modelLTE <- as.vector(meanYearFit)
plot(meanYear$meanTempYear, meanYear$modelLTE , pch = '')
abline(h = 0, col = "grey", lty = 2)
text(x =  meanYear$meanTempYear, y = meanYear$modelLTE, meanYear$Year)

plot(x = meanYear$minTempYear, y = meanYear$modelLTE , pch = '')
text(x =  meanYear$minTempYear, y = meanYear$modelLTE,meanYear$Year)
abline(h = 0, col = "grey", lty = 2)

plot(meanYear$minTempYear ~ meanYear$meanTempYear)
#there is something werid going on with the variation within year.
#it seems that the years with higher mean and minumum years 
#have the vines with the lowest LTE50 (most hardy). Surely it should be the 
#otheer way around?

#get predicted lte for each variety against tempereature 
#----------------------------------------------------------

sim.lte50

#simulate values for each year and variety 
for (vi in variety ){
	viLTE <- 

}