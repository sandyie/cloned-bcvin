
## Load library
library(rstan)
library(shinystan)

## Set cores
options(mc.cores = 4) # Parallelize chains
options(browser = "chromium") # Set browser

## Set seed
set.seed(20201002)

## Set temperature parameters
param <- list(amp = 5,
              period = 20,
              phase = 6,
              baseline = -10,
              sigma = 1.1,
              tt = c(1:200))

## Create temperature function
temperature.function <- function(tt, phase, period, amp, baseline, ...){
    amp * sin(2 * pi * (tt - phase) / period) + baseline
}

## Generate latent temperatures
latent.temperatures <- do.call(what = temperature.function, args = param)
### View

## Generate observed temperatures
observed.temperatures <- rnorm(n = length(latent.temperatures),
                               mean = latent.temperatures,
                               sd = param[["sigma"]])

## View latent and observed
par(mfrow = c(1, 2))
plot(latent.temperatures, type = "b", xlab = "Time", ylim = c(-17, -3))
plot(observed.temperatures, type = "b", xlab = "Time", ylim = c(-17, -3))

## Put data into list for Stan
data.stan <- list(N_temp_rec = length(latent.temperatures),
                  temp_time = param[["tt"]],
                  temp_rec = observed.temperatures)

## Fit using Stan
fit <- stan(file='fit_sineonly.stan',
            data = data.stan,
            warmup = 1000,
            iter = 2000,
            chains = 4)

## Compare estimates to true values
summary(fit, pars = c("temp_amp", "temp_per", "temp_phase", "temp_baseline", "temp_sigma"))$summary
t(param)

## Diagnose fit
launch_shinystan(fit)
