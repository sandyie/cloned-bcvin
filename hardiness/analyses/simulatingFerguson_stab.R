#Simulating_hardiness from Ferguson et al 2011, but using my own temperature data

setwd("/home/faith/Documents/github/bcvin/hardiness/analyses")

tempData <- read.csv("input/climhist_19812010.csv")

head(tempData)

#temperature values 
temps <- tempData$X2day
plot(temps)

# a list of parameters needed 
#-------------------------------

i <- 2 # dont start on 1 becuase then the follwoing asymptotic (clogs) bounds wont work 
tempValues <- c()3# a set temperature values for the winter 

hcInit <- -10.55 # start hardiness value. mean of earliest LTE50 in the fall 
hcMax <- -25.24 # maximum hardiness
hcMin <- -3 # min hardiness. Hardiness of green tissues 
tThreshEnd <- 11.75 # threshold temperature for accumilating cold during endodormancy 
tThreshEct <- 3.75 # threshold for accumilating worming during ectodormancy
kaEnd <- 0.14 # constant accumilation rate during endodormancy
kaEct <- 0.07 # constant accumilation rate during ectodormancy
kdEnd <- 0.01 # constant deaccumilation rate during endodormancy
kdEct <- 0.14 # constant deaccumilation during ectodormancy 
edb <- -750 # amount of chilling required to switch from endo to ecto dormancy 

#secondary parameters

cloga <- 1 - ((hcMin - hc[i-1])/hcMin-hcMax)
cloga <- 1 - ((hc[i-1]-hcMax/hcMin - hcMax)
tmean <- (max(temps) + min(temps))/2
dd <- tmean - temps[i]

