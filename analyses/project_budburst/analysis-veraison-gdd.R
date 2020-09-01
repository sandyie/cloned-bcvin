
## Veraison phenological data analysis script
##
## Contributors: Geoffrey Legault
## Date: August 31 2020
##

## Load libraries
library(rstan)
library(shinystan)
library(rstanarm)

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
ver.arterra <- read.csv("../output/temporary/arterra_veraison_clean.csv", header = TRUE)

## Read Environment Canada data
envcanada <- read.csv("../output/temporary/environmentcanada_clean.csv", header = TRUE)

## 2012 is missing many temperature measurements
## 2013, 2017, 2018 are all missing some (~ 10 across the year)
ver.arterra <- subset(ver.arterra, !(year %in% c(2012)))
envcanada <- subset(envcanada, !(Year %in% c(2012)))
## Order by Year and Day of Year
envcanada[order(envcanada$Year, envcanada$Day), ] <- envcanada

## Create GDD column
ver.arterra$GDD <- NULL
## Calculate GDD using envcanada data (Threshold = 0 C)
for(i in 1:nrow(ver.arterra)){
    temp <- subset(envcanada, Year == ver.arterra$year[i])
    ver.arterra$GDD[i] <- sum(na.omit(gdd.calc(temp$MeanTemperature[1:ver.arterra$dayofyear[i]],
                                              Threshold = 0))) # omits NA (temporary)
}

## Create list of data for Stan
data.stan <- list(y = ver.arterra$GDD,
                  N = nrow(ver.arterra),
                  variety = as.numeric(as.factor(ver.arterra$variety)),
                  n_variety = length(unique(ver.arterra$variety)))

### Fit using Stan model (model 2 = with grand intercept)
stanmodel2 <- stan("Stan/intpool3.stan", data = data.stan, iter = 5000, warmup = 2500, chains = 4)

## Summarize posterior distributions (model 2)
summary(stanmodel2, pars = c("a_grand", "sigma_a_variety", "sigma_y", "a_variety"))$summary

## Diagnostics (model 2)
launch_shinystan(stanmodel2)

## Code for plotting
extract.stanmodel2 <- extract(stanmodel2)
varieties <- unique(ver.arterra$variety)
varieties <- varieties[order(varieties)]

pdf(file = "Varieties - Veraison.pdf", width = 12, height = 12, onefile = TRUE)

par(mfrow = c(4, 4))
for(i in 1:length(varieties)){
    plot(density(extract.stanmodel2$a_variety[, i], bw = 1), main = varieties[i], xlim = c(-150, 150))
    clip(-3000, 3000, 0, 1)
    abline(v = 0, lty = "dotted")
}

dev.off()
