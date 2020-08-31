
## Budbreak phenological data analysis script
##
## Contributors: Geoffrey Legault
## Date: May 18 2020
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

### Fit using Stan model (model 1 = no grand intercept)
stanmodel1 <- stan("Stan/intpool.stan", data = data.stan, iter = 5000, warmup = 2500, chains = 4)

### Fit using Stan model (model 2 = with grand intercept)
stanmodel2 <- stan("Stan/intpool2.stan", data = data.stan, iter = 5000, warmup = 2500, chains = 4)

## Summarize posterior distributions (model 1)
summary(stanmodel1, pars = c("mu_a_variety", "sigma_a_variety", "sigma_y", "a_variety"))$summary

## Summarize posterior distributions (model 2)
summary(stanmodel2, pars = c("a_grand", "sigma_a_variety", "sigma_y", "a_variety"))$summary

## Diagnostics (model 1)
launch_shinystan(stanmodel1)

## Diagnostics (model 2)
launch_shinystan(stanmodel2)

## Compare model 2 with rstanarm
rstanarmmodel <- stan_lmer(GDD ~ (1 | varietynum),
                           data = bb.arterra,
                           prior = normal(location = 0, scale = 10),
                           prior_intercept = normal(location = 400, scale = 50, autoscale = FALSE),
                           prior_aux = normal(location = 0, scale = 10),
                           seed = 202010,
                           iter = 5000,
                           chains = 4,
                           cores = 4)

## Summarize both
summary(rstanarmmodel)
summary(stanmodel2, pars = c("a_grand", "sigma_a_variety", "sigma_y", "a_variety"))$summary # varieties numbered but in same order


## Code for plotting
extract.stanmodel2 <- extract(stanmodel2)
varieties <- unique(bb.arterra$variety)
varieties <- varieties[order(varieties)]

pdf(file = "Varieties.pdf", width = 12, height = 12, onefile = TRUE)

par(mfrow = c(4, 4))
for(i in 1:length(varieties)){
    plot(density(extract.stanmodel2$a_variety[, i], bw = 1), main = varieties[i])
    clip(-100, 100, 0, 1)
    abline(v = 0, lty = "dotted")
}

dev.off()
