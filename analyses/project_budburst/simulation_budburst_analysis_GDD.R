## Arterra_Budbreak phenological data analysis script
##
## Contributors: Geoffrey Legault
## Date: May 18 2020
##

## Load libraries
library(rstan)
options(mc.cores = parallel::detectCores())
library(shinystan)
options(browser = "chromium")

## Read Environment Canada data
envcanada <- read.csv("../output/temporary/environmentcanada_clean.csv", header = TRUE)

## Years 2012 and 2013 are missing some temperature measurements
envcanada <- subset(envcanada, !(Year %in% c(2012, 2013)))
## Order by Year and Day of Year
envcanada[order(envcanada$Year, envcanada$Day), ] <- envcanada

## Create subset of envcanada data (2001)
test1 <- matrix(c(subset(envcanada, Year == 2001)$MeanTemperature),
                  ncol = 1, byrow = FALSE)
            
## Function that calculates GDD with fixed threshold (Tmin) based on average temperatures
gdd.calc <- function(Tavg, Tmin, ...){
    cTt <- dim(Tavg)
    for(i in 1:length(Tavg)){
        if(isTRUE(Tavg[i] <= Tmin)){
            cTt[i] <- 0
        } else{
            cTt[i] <- Tavg[i] - Tmin
        }
    }
    return(cTt)
}

## Function that determines bud burst day based on average temperatures using gdd.calc function
budburst.sim <- function(Tavg, Tmin, nsamples, mu_gdd, sigma_gdd){
    gdd <- gdd.calc(Tavg, Tmin)
    bb <- dim(nsamples)
    for(i in 1:nsamples){
        gdd.threshold <- max(rnorm(n = 1, mean = mu_gdd, sd = sigma_gdd), 0) # truncated normal
        gdd.accumulate <- cumsum(gdd)
        bb[i] <- which(gdd.accumulate > gdd.threshold)[1]
    }
    return(bb)
}

## Set parameters
param <- list(Tmin = 5,
              mu_gdd = 702,
              sigma_gdd = 10)
nsamples <- 100

## Generate fake data
testbb <- budburst.sim(test1, 5, nsamples, param[["mu_gdd"]], param[["sigma_gdd"]])

## Create Stan data block
dat.stan <- list(n_X = nsamples,
                 X = testbb,
                 Tmin = 5,
                 n_Temperature = nrow(test1),
                 Temperature = c(test1)
                 )

## Fit model to data
fit.stan1 <- stan("Stan/gdd.stan",
                 data = dat.stan,
                 iter = 2000,
                 warmup = 1000,
                 chains = 4,
                 seed = 2020 ## , control = list(adapt_delta = .95)
                 )
## Summarize posterior distribution
summary(fit.stan1, pars = c("mu_gdd", "sigma_gdd"))$summary

## Diagnostics
launch_shinystan(fit.stan1)
