rm(list = ls())

#script started by Faith Jones on the 5th March 2020, with year removed April 8th, and simulated data from real data into 
#thsi script Aril 15th 2020 

#running a mixed linear model of bud winter hardiness regressed against air temperature. There is grouping 
#on the intercept and slope for variety  

#the best model at this time is slopeVarietyCov.stan (fit 6), which has a covarience structure and
#no non centred parameterisation 

#there are three more models in thsi script as well with either no covarience structure or non-centred parameterisation (or both)

#the normal and ncp version of teh covarience model both give good answers, but only when nobs was 60 rather than 40. before that teh model 
#with ncp underestimated slope. 

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

#try and simulate a multi level model of hardiness against temp partially pooled by variety 
#-------------------------------------------------------------------------------------
set.seed(16)
#model should be:

# y ~ Normal((alpha + alphaSite) + beta * x, sigma/eps)

#parameters 
#LTE50sim (y ) is simulated using parameters

#inputs
nrep <- 80 # number of reps of each variety 
meanTemp <- 2
sigmaTemp <- 5
simTemps <- rnorm(nrep, meanTemp,sigmaTemp)

nvariety <- 20
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
sigma <-  0.5
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


#model with a full covarience structure and no non-centred parameterisation 
#-----------------------------------------------------------------------------

#this model is (i think?) exactly the same as the process i used to simulate data. 
#It has a covariance structure for teh partial pooling of variety on slope and 
#intercept. There is no non-centred parameterisation. 

#This model ddoesnt give fitting warnings (it used to when i included year) but has to have a high adapt_delta
#

fit6 <- stan(file = "stan/slopeVarietyCov.stan", data = stan_data4, warmup = 5000, 
	iter = 6000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15, adapt_delta = 0.97))

launch_shinystan(fit6)

post <- extract.samples(fit6)
fit6sum <- summary(fit6)$summary

str(post)
names(post)

plot(density(post$alpha_g))#-20 - ok estimate

plot(density(post$beta_g))#0.5 - underestimating this a bit, but maybe ok?

plot(density(data.frame(post$Rho)[,2]))#doing a good job i think? Should be -0.7

plot(density(data.frame(post$var_sigma)[,1])) #Partial pooling on intercept. should be 0.2. Doing a good job i think. 

plot(density(data.frame(post$var_sigma)[,2])) #Partial pooling on slope. should be 0.3. Doing a good job i think. 

plot(density(post$sigma_y))#should be 1. Maybe underestimating a bit, but I think its ok.

# check some hyperparameters
Sigma_vara # hmm, this is  not right -- I am not finding the right comparison ... but would be good to check the hyper-parameters alpha and beta also
fit6sum[grep("var_alpha\\[", rownames(fit6sum)),"mean"]
# plot(fit6sum[grep("var_alpha\\[", rownames(fit6sum)),"mean"]~mua_sp)

#Predicted values
meanRealY <- colMeans(data.frame(post$realY))
SDRealY <- apply(data.frame(post$realY), 2, HPDI) 
str(SDRealY)

realYs <- data.frame(meanRealY)
head(realYs)
realYs$upperHPDI <- SDRealY[1,]
realYs$lowerHPDI <- SDRealY[2,]

#plot predicted valuea against temp
#black lines are the mean slopes, coloured ones are the HPDI varience 

plot(realYs$lowerHPDI ~ simVarData$simTemps,col="green", type = "l")
lines(realYs$upperHPDI ~ simVarData$simTemps,col="green")
lines(realYs$meanRealY ~ simVarData$simTemps,col="black")

#plot predicted values against empirical ones (not including sigma_y)
#black points are teh mean values, coloured ones are the HPDI variance  

plot(realYs$lowerHPDI, simVarData$simLTEVar,col="purple", type = "p", pch = 16)
points(realYs$upperHPDI, simVarData$simLTEVar,col="purple", pch = 16)
points(realYs$meanRealY, simVarData$simLTEVar,col="black", pch = 16)

#variety effects
varietyAlphas <- data.frame(post$a_b_variety[,,1])
meanVarietyAlpha <- colMeans(varietyAlphas)
varietyBetas <- data.frame(post$a_b_variety[,,2])
meanVarietyBeta <- colMeans(varietyBetas)

plot(meanVarietyAlpha ~ meanVarietyBeta)

#predictions for each variety 
mcmc_intervals(varietyAlphas) + geom_vline(xintercept = alpha, linetype="dotted", color = "grey")  #intercepts 
mcmc_intervals(varietyBetas)+ geom_vline(xintercept = betag, linetype="dotted", color = "grey") #intercepts 





#Other models:
#-----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------


#model with a correlation matrix and non-centred parameterisation 
#------------------------------------------------------------------------------------

#this is teh model i made woth partial pooling and a covariance structure for variety 
#on intercept and slope, and non-centred parameterisation. I dont know if it is needed
#because at the momment teh model without non-centred parameterisation is doing fine 

#this model has no divergent transitions warning or other warnings
#
fit9 <- stan(file = "stan/nonCentre_slopeVarietyCov.stan", data = stan_data4, warmup = 4000, 
	iter = 6000, chains = 4, cores = 4, thin = 1)

launch_shinystan(fit9)

fit9


post9 <- extract.samples(fit9)

str(post9)

plot(density(post9$alpha_g))#-20 - ok estimate. overestimating a little

plot(density(post9$beta_g))#0.5 - overestimating this a bit, but maybe ok?

plot(density(data.frame(post9$Rho)[,2]))#-0.7 - looks good

plot(density(data.frame(post9$var_sigma)[,1])) #Partial pooling on intercept. should be 0.2. Overestimating quite a bit

plot(density(data.frame(post9$var_sigma)[,2])) #Partial pooling on slope. should be 0.3. Overestimating a bit

plot(density(post9$sigma_y))#should be 0.5. Overestimatinga  little, but looks good . 




#a model with a less complicated structure (no covarience) 
#------------------------------------------------------

#Thid model has non non-cented parameters and no covarience. Just partial pooling on 
#the intercept and slope for variety

#It is not giving me any fitting warnings at 8000 itterations, but not predicting 
#the values as well as I would hope. 


fit8 <- stan(file = "stan/slope_varietySimple.stan", data = stan_data4, warmup = 4000, 
	iter = 6000, chains = 4, cores = 4, thin = 1)

launch_shinystan(fit8)

post8 <- extract.samples(fit8)

str(post8)

plot(density(post8$alpha_g))#-20 - overestimating this a bit, but not too bad

plot(density(post8$beta_g))#0.5 - overerestimating this a little, but ok. I had to up the 
#number of observations in each variety a lot to get a good estimation though (4 or under nobs was not great) 

plot(density(post8$sigma_alpha_v))#0.2 - estimating ok 

plot(density(post8$varbeta))#

plot(density(post8$sigma_beta_v))#0.3 - estimating ok



#attemp at non centred parametrisation to get model working better - no covariance and pcp on sigma alpha
#---------------------------------------------------------------------

#this model is very similar to model slope_varietySimple.stan, exept I added some
#non-parameterisation on the partial pooling around the intercept. I did this because
#I was getting transition errors and there was a banana shape without teh non-centred parameterisation

#This model gives no fitting warnings, and is doing an ok job of estimating parameters but not as good 
#as I would like 


fit10 <- stan(file = "stan/noncentred_slope_varietySimple.stan", data = stan_data4, warmup = 4000, 
	iter = 6000, chains = 4, cores = 4, thin = 1, control = list(max_treedepth = 15)) #treedepth needed here 

launch_shinystan(fit10)

post10 <- extract.samples(fit10)
str(post7)


plot(density(post10$alpha_g))#-20 - ok estimate

plot(density(post10$beta_g))#0.5 -overestimating this a bit, but maybe ok? 

plot(density(post10$sigma_alpha_v))#0.2 - estimating ok 

plot(density(post10$varbeta))#

plot(density(post10$sigma_beta_v))#0.3 - estimating ok


