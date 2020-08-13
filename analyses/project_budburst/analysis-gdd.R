
## Budbreak phenological data analysis script
##
## Contributors: Geoffrey Legault
## Date: May 18 2020
##

## Load libraries
library(rstan)
library(shinystan)

## Set options
options(mc.cores = parallel::detectCores()) # Detect cores
options(browser = "chromium") # Set default browser

## Function for calculating GDD
gdd.calc <- function(X, Threshold){
    gdd <- dim(X)
    for(i in 1:length(X)){
        if(isTRUE(X[i] <= Threshold)){
            gdd[i] <- 0
        } else{
            gdd[i] <- X[i] - Threshold
        }
    }
    return(gdd)
}

## Read phenology data
bb.arterra <- read.csv("../output/temporary/arterra_bud_clean.csv", header = TRUE)

## Read Environment Canada data
envcanada <- read.csv("../output/temporary/environmentcanada_clean.csv", header = TRUE)

## 2012 is missing many temperature measurements
## 2013, 2017, 2018 are all missing some (~ 10 across the year)
bb.arterra <- subset(bb.arterra, !(year %in% c(2012)))
envcanada <- subset(envcanada, !(Year %in% c(2012)))
## Order by Year and Day of Year
envcanada[order(envcanada$Year, envcanada$Day), ] <- envcanada

## Create GDD column
bb.arterra$GDD <- NULL
## Calculate GDD using envcanada data (Threshold = 0 C)
for(i in 1:nrow(bb.arterra)){
    temp <- subset(envcanada, Year == bb.arterra$year[i])
    bb.arterra$GDD[i] <- sum(na.omit(gdd.calc(temp$MeanTemperature[1:bb.arterra$dayofyear[i]],
                                              Threshold = 0))) # omits NA (temporary)
}

## Create list of data for Stan
data.stan <- list(y = bb.arterra$GDD,
                  N = nrow(bb.arterra),
                  variety = as.numeric(as.factor(bb.arterra$variety)),
                  n_variety = length(unique(bb.arterra$variety)))

### Fit using Stan model
stanmodel1 <- stan("Stan/intpool.stan", data = data.stan, iter = 5000, warmup = 2500, chains = 4)

## Summarize posterior distribution
summary(stanmodel1, pars = c("mu_a_variety", "sigma_a_variety", "sigma_y"))$summary

## Diagnostics
launch_shinystan(stanmodel1)

extract.stanmodel1 <- extract(stanmodel1)

extract.stanmodel1$

varieties <- unique(bb.arterra$variety)
varieties[order(varieties)]

pdf(file = "Varieties.pdf", width = 12, height = 12, onefile = TRUE)
par(mfrow = c(4, 4))
for(i in 1:length(varieties)){
    plot(density(extract.stanmodel1$a_variety[, i]), main = varieties[i])
}
dev.off()
