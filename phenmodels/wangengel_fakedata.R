## Trying to make fake data for Wang & Engel model ##
## Started 30 Oct 2018 ##
## By Lizzie ## 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

nphendat <- 100
daysperyr <- 300
climdat <- matrix(data=rnorm(nphendat*daysperyr, 25, 10), daysperyr, nphendat)

Tmin <- 0
Tmax <- 40
Topt <- 30
# bbdate <- rnorm(nphendat, 100, 15) # not needed
Fstar <- 25

#' Wang and Engel model. This model computes the rate of phenological 
#' development in response to temperature using Topt bounded by Tmin and Tmax
#' (below and above which there is no Temp action on the plant) in 3 parts
#'
#' Part 1- Alpha - based on fixed parameters
#' @param Tmin - Minimum temperature
#' @param Tmax - Maximum temperature
#' @param Topt - Optimum temperature
#' 

Alphafx <- function(Tmin, Tmax, Topt){
  Alpha <- log10(2)/(log10((Tmax-Tmin)/(Topt-Tmin)))
  return(Alpha)
}

#' Part 2- WangEngelfx - computes temperature action across all days of the year
#' @param Tmin - Minimum temperature
#' @param Tmax - Maximum temperature
#' @param Topt - Optimum temperature
#' @param Alpha - alpha parameter 
#' @param Tavg - daily average temperature
#' @return cTt - data frame with Temperature action on developement and its daily accumulated values - 
#' 
WangEngelfx <- function(Tmin, Tmax, Topt, Alpha, Tavg){
  cTt<-array(NA, dim=c(length(Tavg),2))
  colnames(cTt)<-c("Temp.action","Accum.Temp.action")
  for(i in 1:length(Tavg)){
    if (Tmin<Tavg[i] & Tavg[i]<Tmax){
      cTt[i,1] <- (2*(Tavg[i]-Tmin)^Alpha*(Topt-Tmin)^Alpha-(Tavg[i]-Tmin)^(2*Alpha))/((Topt-Tmin)^(2*Alpha))
    } 
    if (Tavg[i]<=Tmin | Tavg[i]>=Tmax){
      cTt[i,1] <-0
    }
  }
  cTt[,2]<-cumsum(cTt[,1])
  
  return(cTt)
}

alphahere <- Alphafx(Tmin, Tmax, Topt)

# Example for one year ... 
Tavg.oneyr <- rnorm(daysperyr, 25, 8)
test <- WangEngelfx(Tmin, Tmax, Topt, alphahere, Tavg.oneyr)
which.min(abs(test[,2] - Fstar)) 

# Example for nyears
fstar.days <- c()
for(i in c(1:ncol(climdat))){
    wangengel.thisyr <- WangEngelfx(Tmin, Tmax, Topt, alphahere, climdat[,i])
    fstar.days[i] <- which.min(abs(wangengel.thisyr[,2] - Fstar))
    }
hist(fstar.days)
