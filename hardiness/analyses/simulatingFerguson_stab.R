#Simulating_hardiness from Ferguson et al 2011, but using my own temperature data

rm(list = ls())
setwd("/home/faith/Documents/github/bcvin/hardiness/analyses")

library(rstan)
library(rethinking)
library(truncnorm)

#tempData <- read.csv("input/climhist_19812010.csv")#Carls historical temp data
tempData <- read.csv("input/DummyTempChard.csv")#temp data scraped from Fergusun 2011 figure of chardonay grape temps


#getting temperature data for each day 
fit1 <- lm(temperature ~ poly(Month, 4, raw = TRUE), data = tempData)# get a model that fits the data (mostly)
plot(tempData$temperature ~ tempData$Month)
lines(tempData$Month, predict(fit1, data.frame( x = tempData$Month)), col="red")
day <- 7/221 # number of months/number of dates 
simDays <- data.frame(Month = seq(1,8, day))
simDays$simTempsExact <- predict(fit1, newdata = simDays)
#add some scatter around those values
dist <- rnorm(length(simDays$simTempsExact), 0, 1.5)
simDays$simTemps <- simDays$simTempsExact + dist

# a list of parameters needed 
#-------------------------------

i1 <- 2 # dont start on 1 becuase then the follwoing asymptotic (clogs) bounds wont work 
tempValues <- simDays$simTemps # a set temperature values for the winter. I used data from the original manuscript  

#other parameters etimated by the model
hcInit <- -10.55 # start hardiness value. mean of earliest LTE50 in the fall 
hcMax <- -25.24 # maximum hardiness
hcMin <- -3 # min hardiness. Hardiness of green tissues 
tThreshEnd <- 11.75 # threshold temperature for accumilating cold during endodormancy 
tThreshEct <- 3.75 # threshold for accumilating warming during ectodormancy
kaEnd <- 0.14 # constant accumilation rate during endodormancy
kaEct <- 0.07 # constant accumilation rate during ectodormancy
kdEnd <- 0.01 # constant deaccumilation rate during endodormancy
kdEct <- 0.13 # constant deaccumilation during ectodormancy 
edb <- -750 # amount of chilling required to switch from endo to ecto dormancy 
sigma <- 1 # variation around teh mean. I made this number up 
eps <- rnorm(length(tempValues),0,0.5)

hcChange <- vector()
hcChange[i1-1] <- NA
hc <- vector()
hc[i1-1] <- hcInit

#initiating secondary parameters
cloga <- vector() #the correction putting absolute bounds on change in hc. Equations 5 and 6 of Fergusonetal2011
clogd <- vector()

#tempeperatures below the mean threshold values are chilling days, and above are heating
ddEnd <- tempValues - tThreshEnd 
ddEct <- tempValues - tThreshEct

#make a vector of chilling days only 
ddEndc <- ddEnd
ddEndc[ddEndc > 0 ] <- 0

#ddEndc <- ddEndc * -1 # i think these should be negative values?

ddEctc <- ddEct
ddEctc[ddEctc > 0 ] <- 0
#ddEctc <- ddEctc * -1

#vector of heating days only 
ddEndh <- ddEnd
ddEndh[ddEndh < 0] <- 0

ddEcth <- ddEct
ddEcth[ddEcth < 0] <- 0

#get accumilating degree growing days
accumilatedDD <- cumsum(ddEndc)

#main model for Endodormancy for teh first value i = 2
#------------------------------------
#cloga[i] <- 1 - ((hcMin - hc[i-1])/(hcMin-hcMax))
#clogd[i] <- 1 - ((hc[i-1]-hcMax)/(hcMin - hcMax))
#hcChange[i] <- (ddEndc[i] * kaEnd * cloga[i])+(ddEndh[i] * kdEnd* clogd[i])

#build a loop for all data 

for (i in (2:length(tempValues))){

	#i <- 150

	#calculate the asymptotic bound logistic function 
		cloga[i] <- 1 - ((hcMin - hc[i-1])/(hcMin-hcMax))
		clogd[i] <- 1 - ((hc[i-1]-hcMax)/(hcMin - hcMax))

	#track accumilated degree days to decide if we are in Endo or Ectodormancy
	
	if (accumilatedDD[i] > edb) {

		hcChange[i] <- (ddEndc[i] * kaEnd * cloga[i]) + (ddEndh[i] * kdEnd * clogd[i]) + eps[i]

	} else

		hcChange[i] <- (ddEctc[i] * kaEct * cloga[i]) + (ddEcth[i] * kdEct * clogd[i])+ eps[i]

	#get the actual hardiness value 
	hc[i] <- hc[i-1] + hcChange[i]
}

plot(hcChange)
plot(hc)	
plot(hc ~ tempValues)


#Making a model to test this data
#---------------------------------------------


#preparing the data - some repeated from above 
#-----------------------------

# I need to index the x values from 1 to n(x)

tempValues <- I(tempValues)
hc <- hc #keep y albeled as hc
N <- length(tempValues)

hcInit <- -10.55 # start hardiness value. mean of earliest LTE50 in the fall 
hcMin <- -3 # min hardiness. Hardiness of green tissues 
tThreshEnd <- 11.75 # threshold temperature for accumilating cold during endodormancy 
tThreshEct <- 3.75 # threshold for accumilating warming during ectodormancy
edb <- -750 # amount of chilling required to switch from endo to ecto dormancy 
hcMax <- -25.24 

#kaEnd <- 0.14 # constant accumilation rate during endodormancy
#kaEct <- 0.07 # constant accumilation rate during ectodormancy
#kdEnd <- 0.01 # constant deaccumilation rate during endodormancy
#kdEct <- 0.13 # constant deaccumilation during ectodormancy 

#data passed to STan needs to be a list of named objects. names here need to match names in model code
#i make a LIST of the different varables, NOT data frame
stan_data <- list(tempValues = tempValues, hc = hc, N = N, hcInit = hcInit, hcMin = hcMin, tThreshEnd = tThreshEnd, 
	tThreshEct = tThreshEct, edb = edb)

#i would like these parameters in teh model too:
#  threshold temperatures 
#  edb

#also maybe a level fo variation for variety. so i woudl need to add a level of variation around the 
#acclimation/deacclimation rates 

write("// discreat dynamic model of winegrape winter hardiness based on Fergusonetal2011
	// at the moment is simplified so that the threshold temperatures and edb are inputed parameters



data {

	int < lower = 1 > N; // Sample size - number of observations
	vector[N] tempValues; // Predictor. Air temperature
	vector[N] hc; // Outcome. LTE50

	real <upper = 1 > hcInit; // the initial hardiness, mean of autumn LTE50
	int <upper = 1 >  hcMin; // minimum LTE. set as -3 because LTE of green tissue  
	real tThreshEnd; // the temperature during auntum/winter between warming and chilling
	real tThreshEct; // the temperature during winter/spring between warming and chilling
	real <upper= 0> edb; // teh number of degree days needed to switch between endo and ecto. is minus because chilling is negative

	}

transformed data { // allows for preprocessing the data 

	//change in hardiness rather than absolute hardiness
	vector[N] hcChange;

	//degree growing days for autum/winter 
	vector[N] ddEnd; // degree growing days endothermy 
	vector[N] ddEndc ; // degree growing days chilling 
	vector[N] ddEndh ; // degree growing days warming  

	//degree growing days for winter/spring
	vector[N] ddEct ; // degree growing days ectothermy
	vector[N] ddEctc ; // degree growing days chilling 
	vector[N] ddEcth ; // degree growing days warming 

	// The correction putting absolute bounds on change in hc. Equations 5 and 6 of Fergusonetal2011
	vector [N] cloga; 
	vector [N] clogd; 

	// accumilating degree growing 
	vector[N] accumilatedDD; 

	//maximuym hardiness
	real <upper = 1 > hcMax; // maximum LTE
	hcMax = min(hc);

	for (i in 1:N){
		if (i == 1){
			hcChange[i] = 0;// set first change in hardiness to 0
			}
		else {
			hcChange[i] = hc[i] - hc[i-1] ;
			}
		
		// getting chilling and warming amounts for autum/winter
		ddEnd[i] = tempValues[i] - tThreshEnd;  // get degree growing days based on threshold temperature (endodormancy)
			
		if (ddEnd[i] < 0) // degree growing days warming are 0'ed out 
			ddEndc[i] = ddEnd[i];
		else ddEndc[i] = 0;

		if (ddEnd[i] > 0)// degree growing days chilling are 0'ed out 
			ddEndh[i] = ddEnd[i];
		else ddEndh[i] = 0;

		// getting chilling and warming amounts for autum/winter
		ddEct[i] = tempValues[i] - tThreshEct;   // get degree growing days based on threshold temperature (ectodormancy)
		if (ddEct[i] < 0)
			ddEctc[i] = ddEct[i];
		else ddEctc[i] = 0;

		if (ddEct[i] > 0)
			ddEcth[i] = ddEct[i];
		else ddEcth[i] = 0;	

		//calculating the clog parameters that bound maximum LTE50 change for a step 
		if (i ==1){
			cloga[i] = 1 - ((hcMin - hcInit)/(hcMin-hcMax));
			clogd[i] = 1 - ((hcInit-hcMax)/(hcMin - hcMax));
			}
		else{
			cloga[i] = 1 - ((hcMin - hc[i-1])/(hcMin-hcMax));
			clogd[i] = 1 - ((hc[i-1]-hcMax)/(hcMin - hcMax));
			}

		// hand coding accumilated sum because i am confused by cumulative_sum()
		if (1== 1){
			accumilatedDD[i] = ddEndc[i];
		}
		else{
			accumilatedDD[i] = accumilatedDD[i] + accumilatedDD[i-1];
			}
		}
}

parameters{
	//Acclimation and deacclimation rates 
	real <lower = 0> kaEnd; //   constant accumilation rate during endodormancy
	real <lower = 0> kaEct; //  constant accumilation rate during ectodormancy
	real <lower = 0> kdEnd; // constant deaccumilation rate during endodormancy
	real <lower = 0> kdEct; // constant deaccumilation rate during ectodormancy

	// general varience/error
	real <lower = 0> sigma; 

}


model{ 
	//assign priors
	kaEnd ~ normal(0,0.5 ); // bounded as positive. prior taken from Ferguson data 
	kaEct ~ normal(0,0.5 );
	kdEnd ~ normal(0,0.5 );
	kdEct ~ normal(0,0.5 );
	sigma ~ lognormal(0,1);

	// run the actual model 

	for(i in 1:N){

		if (accumilatedDD[i] <= edb) { // at the moment edb is fixed, but should eventually be a parameter of the data 
			hcChange[i] ~ normal((ddEndc[i] * kaEnd * cloga[i]) + (ddEndh[i] * kdEnd * clogd[i]), sigma);
		} else {
			hcChange[i] ~ normal ((ddEctc[i] * kaEct * cloga[i]) + (ddEcth[i] * kdEct * clogd[i]), sigma);	
		}
	}

}


generated quantities {
} // The posterior predictive distribution",

"stan_dynamic.stan")




stan_dynamic <- "stan_dynamic.stan"


DynamicFit <- stan(file = stan_dynamic, data = stan_data, warmup = 500, 
	iter = 2000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15))

posteriorDynamic <- extract(DynamicFit)
str(posteriorDynamic)

plot(density(posteriorDynamic$kaEnd)) # way WAY out. should be 0.14

plot(density(posteriorDynamic$kaEct)) # way out . should be 0.07

plot(density(posteriorDynamic$kdEnd)) # way WAY out. should be 0.01

plot(density(posteriorDynamic$kdEct)) # way out. shoudl be 0.13

plot(density(posteriorDynamic$sigma)) # close to expected 

#try a prior predictive check to help fix the model
#------------------------------------------

#my plan is to use the parameters to predict winter temperatures many times. 

#simulate parameters from the prior distribution 

#acclimation and deacclimation rates 
kaEnd2 <- rexp(100, 1)
kaEct2 <- rexp(100, 1)
kdEnd2 <- rexp(100, 1)
kdEct2 <- rexp(100, 1)

#other values for the model 
tempValues # simulated temerature values for one year 
edb #when it wtichnes from endo to ecto dormancy
tThreshEnd #threshold temperature for chilling in the endodormancy period 
tThreshEct # #threshold temperature for warming in the ectodormancy period 
hcInit# starting value for hardiness
hcMin # minimum hardiness. -3. 

#repeat code from above, but repeating 100 times for each run of teh prior prediction check 
i1 <- 2 # dont start on 1 becuase then the follwoing asymptotic (clogs) bounds wont work 
hcChange <- vector()
hcChange[i1-1] <- NA
hc <- vector()
hc[i1-1] <- hcInit

#initiating secondary parameters
cloga <- vector() #the correction putting absolute bounds on change in hc. Equations 5 and 6 of Fergusonetal2011
clogd <- vector()

#tempeperatures below the mean threshold values are chilling days, and above are heating
ddEnd <- tempValues - tThreshEnd 
ddEct <- tempValues - tThreshEct

#make a vector of chilling days only 
ddEndc <- ddEnd
ddEndc[ddEndc > 0 ] <- 0

#ddEndc <- ddEndc * -1 # i think these should be negative values?

ddEctc <- ddEct
ddEctc[ddEctc > 0 ] <- 0
#ddEctc <- ddEctc * -1

#vector of heating days only 
ddEndh <- ddEnd
ddEndh[ddEndh < 0] <- 0

ddEcth <- ddEct
ddEcth[ddEcth < 0] <- 0

#get accumilating degree growing days
accumilatedDD <- cumsum(ddEndc)

#main model for Endodormancy for teh first value i = 2
#------------------------------------
#cloga[i] <- 1 - ((hcMin - hc[i-1])/(hcMin-hcMax))
#clogd[i] <- 1 - ((hc[i-1]-hcMax)/(hcMin - hcMax))
#hcChange[i] <- (ddEndc[i] * kaEnd * cloga[i])+(ddEndh[i] * kdEnd* clogd[i])

#make a dataframe to input simulations from priors
priorCheckLTEs <- data.frame(matrix(NA, length(tempValues)*100, 5))
names(priorCheckLTEs) <- c("rep", "counter", "tempValues", "LTE50", "LTE50Change")
priorCheckLTEs$rep <- rep(1:100, each = length(tempValues))
priorCheckLTEs$tempValues <- rep(tempValues, times = 100)
priorCheckLTEs$counter <- rep(1:length(tempValues), times = 100)
priorCheckLTEs$LTE50[priorCheckLTEs$counter == 1] <- hcInit


#build a loop for all data 

for (i in (2:length(tempValues))){

	#i <- 150

	#calculate the asymptotic bound logistic function 
		cloga[i] <- 1 - ((hcMin - hc[i-1])/(hcMin-hcMax))
		clogd[i] <- 1 - ((hc[i-1]-hcMax)/(hcMin - hcMax))

	#track accumilated degree days to decide if we are in Endo or Ectodormancy
	for (ic in 1:100){

	
		if (accumilatedDD[i] > edb) {

			hcChange[i] <- (ddEndc[i] * kaEnd2[ic] * cloga[i]) + (ddEndh[i] * kdEnd2[ic] * clogd[i]) 

		} else

			hcChange[i] <- (ddEctc[i] * kaEct2[ic] * cloga[i]) + (ddEcth[i] * kdEct2[ic] * clogd[i])

		#get the actual hardiness value 
		hc[i] <- hc[i-1] + hcChange[i]

		priorCheckLTEs$LTE50[priorCheckLTEs$rep == ic & priorCheckLTEs$counter == i] <- hc[i]
		priorCheckLTEs$LTE50Change[priorCheckLTEs$rep == ic & priorCheckLTEs$counter == i] <- hcChange[i]


	}
}

plot(priorCheckLTEs$LTE50Change ~ priorCheckLTEs$counter)
plot(priorCheckLTEs$LTE50 ~ priorCheckLTEs$counter)
plot(priorCheckLTEs$LTE50 ~ priorCheckLTEs$tempValues)
plot(density(kaEnd2))