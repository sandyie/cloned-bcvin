rm(list = ls())
setwd("/home/faith/Documents/github/bcvin/hardiness/analyses")


#trialing my hardiness data on the data used by Ferguson 
# the data came from their excel spredasheet http://wine.wsu.edu/extension/weather/cold-hardiness/

library(rstan)
library(rethinking)
library(truncnorm)

#read in temp data from ferguson et al 2011/2014. It is Chardony data 
chardDataf <- read.csv("input/WashingtonStatecsFergusonData.csv")

head(chardDataf)

#select only the observed data 
chardObserved <- chardDataf[!is.na(chardDataf$Observed_Hc), ]

#get change in hardiness
chardDataf$HardinessChange <- c(0, diff(chardDataf$Predicted_Hc))

#parameter values
tempValues <- chardDataf$T_mean

hcInit <- -10.3 # start hardiness value. mean of earliest LTE50 in the fall 
hcMax <- -25.1 # maximum hardiness
hcMin <- -1.2 # min hardiness. Hardiness of green tissues 
tThreshEnd <- 13 # threshold temperature for accumilating cold during endodormancy 
tThreshEct <- 5 # threshold for accumilating warming during ectodormancy
kaEnd <- 0.142# constant accumilation rate during endodormancy
kaEct <- 0.20 # constant accumilation rate during ectodormancy
kdEnd <- 0.08 # constant deaccumilation rate during endodormancy
kdEct <- 0.10 # constant deaccumilation during ectodormancy 
edb <- -700 # amount of chilling required to switch from endo to ecto dormancy  
theta <- 7 # this is an exponentyial added in teh 2014 paper to help the clogs 

sigma <- 0.25 # variation around teh mean. I made this number up 
eps <- rnorm(length(tempValues),0,sigma )



#focusing on their data 
#---------------------------------------------

#they inc;lude the hardiness at budbreake, which is in a different column. I need to add that 

head(chardDataf)
bbData <- chardDataf[!is.na(chardDataf$Budbreak),]
chardDataf$Observed_Hc[!is.na(chardDataf$Budbreak)] <- bbData$Budbreak
chardObserved <- chardDataf[!is.na(chardDataf$Observed_Hc), ]


plot(chardObserved$T_mean, chardObserved$Observed_Hc)
plot(chardObserved$Observed_Hc ~ chardObserved$jday)

# values I expect are above 

#Run the model i built on the "real" data
#also maybe a level fo variation for variety. so i woudl need to add a level of variation around the 
#acclimation/deacclimation rates 

N <- I(nrow(chardObserved))
hcInit <- hcInit # thsi i just took from the spreadsheet provided. I am not sure how they calculate it. -10.3
hcMin <- hcMin  # again taken from teh Feguson paper . -1.2 


stan_data4 <- list(tempValues = chardObserved$T_mean, hc = chardObserved$Observed_Hc, N = N, hcInit = hcInit, hcMin = hcMin)


write("// discreat dynamic model of winegrape winter hardiness based on Fergusonetal2011
	// at the moment is simplified so that the threshold temperatures and edb are inputed parameters

data {

	int < lower = 1 > N; // Sample size - number of observations
	vector[N] tempValues; // Predictor. Air temperature
	vector[N] hc; // Outcome. LTE50

	real <upper = 1 > hcInit; // the initial hardiness, mean of autumn LTE50
	real <upper = 1 >  hcMin; // minimum LTE. set as -3 because LTE of green tissue  
	}

transformed data {
	//change in hardiness rather than absolute hardiness
	vector[N] hcChange;

	for (i in 1:N){
		if (i == 1){
			hcChange[i] = 0;// set first change in hardiness to 0
			}
		else {
			hcChange[i] = hc[i] - hc[i-1] ;
			}
		}

}

parameters {
	//Acclimation and deacclimation rates 
	real <lower = 0> kaEnd; //   constant accumilation rate during endodormancy
	real <lower = 0> kaEct; //  constant accumilation rate during ectodormancy
	real <lower = 0> kdEnd; // constant deaccumilation rate during endodormancy
	real <lower = 0> kdEct; // constant deaccumilation rate during ectodormancy

	//parameters around threshold temperatures and when to switch from endo to ecto
	real tThreshEnd; // thereshold temperature between chilling and warming for Endodormancy  
	real tThreshEct; // thereshold temperature between chilling and warming for Ectodormancy
	real <lower = 0> edb; //number of chilling days needed to switch to ectodormancy  

	// expinential for clogs
	real <lower = 0> theta; 

	// general varience/error
	real <lower = 0> sigma; 

}

transformed parameters { // allows for preprocessing the data 

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
			clogd[i] = 1 - ((hcInit-hcMax)/(hcMin - hcMax))^theta;
			}
		else{
			cloga[i] = 1 - ((hcMin - hc[i-1])/(hcMin-hcMax));
			clogd[i] = 1 - ((hc[i-1]-hcMax)/(hcMin - hcMax))^theta;
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

model{ 
	//assign priors
	kaEnd ~ normal(0,0.5 ); // bounded as positive. prior taken from Ferguson data 
	kaEct ~ normal(0,0.5 );
	kdEnd ~ normal(0,0.5 );
	kdEct ~ normal(0,0.5 );
	sigma ~ lognormal(0,1);
	tThreshEct ~ normal(0, 10); 
	tThreshEnd ~ normal(0, 10);
	edb ~ normal(700, 300);
	theta ~ normal(5,5);

	// run the actual model 

	for(i in 1:N){

		if (accumilatedDD[i] <= edb) { // edb is a parameter too  
			hcChange[i] ~ normal((ddEndc[i] * kaEnd * cloga[i]) + (ddEndh[i] * kdEnd * clogd[i]), sigma);
		} else {
			hcChange[i] ~ normal ((ddEctc[i] * kaEct * cloga[i]) + (ddEcth[i] * kdEct * clogd[i]), sigma);	
		}
	}

}


generated quantities {
} // The posterior predictive distribution",

"stan_dynamic3.stan")


stan_dynamic3 <- "stan_dynamic3.stan"


DynamicFit4 <- stan(file = stan_dynamic3, data = stan_data4, warmup = 1000, 
	iter = 5000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15))

posteriorDynamic <- extract(DynamicFit4)

#postderior checks - this model has very little idean what is going on 
kaEnd
plot(density(posteriorDynamic$kaEnd)) # 
mean(posteriorDynamic$kaEnd, prob = 0.89)

kaEct
plot(density(posteriorDynamic$kaEct)) #
kdEnd
plot(density(posteriorDynamic$kdEnd)) #  
kdEct
plot(density(posteriorDynamic$kdEct)) # . 
#sigma was estimated from teh data, and not provided by the Fergsum team 
plot(density(posteriorDynamic$sigma)) #
tThreshEnd
plot(density(posteriorDynamic$tThreshEnd)) 
tThreshEct
plot(density(posteriorDynamic$tThreshEct)) 
edb
plot(density(posteriorDynamic$edb)) 
theta
plot(density(posteriorDynamic$theta)) 

#Posterior predictions - simulate data based on posterior predictions
#-------------------------------------------
#NOT WORKING YET!!!!!

nObs <- 100 # number of observations of hardiness to be predicted 
nrep <- 200 # run teh model 200 times 

PosteriorPrediction <- data.frame(matrix(NA, nObs*nrep, 2))
names(PosteriorPrediction) <- c("rep", "predictedHc")
PosteriorPrediction$rep <- rep(1:nrep, each = nObs) 
for (i in 1:nrep){ # run the model 100 times

	
	tThreshEnd
	tThreshEct
	ddEndc
	ddEctc
	ddEndh
	ddEcth
	theta
	edb
	kaEnd
	kaEct
	kdEnd
	kdEct
	sigma
	eps <- rnorm(length())

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


	for (i in (2:length(tempValues))){

		#calculate the asymptotic bound logistic function 
		cloga[i] <- 1 - (hcMin - hc[i-1])/(hcMin-hcMax)
		clogd[i] <- 1 - ((hc[i-1]-hcMax)/(hcMin - hcMax))^theta

		#track accumilated degree days to decide if we are in Endo or Ectodormancy
	
		if (accumilatedDD[i] > edb) { # is teh value larger than -700. 

			hcChange[i] <- (ddEndc[i] * kaEnd * cloga[i]) + (ddEndh[i] * kdEnd * clogd[i]) + eps

		} else

		hcChange[i] <- (ddEctc[i] * kaEct * cloga[i]) + (ddEcth[i] * kdEct * clogd[i]) + eps

		#get the actual hardiness value 
		hc[i] <- hc[i-1] + hcChange[i]
	}


}


















































#simulate data based on these parameter values and the temperatures for 2009/2010
#----------------------------------------------------------------------------------
#i1 <- 2

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

	#i <- 2

	#calculate the asymptotic bound logistic function 
		cloga[i] <- 1 - (hcMin - hc[i-1])/(hcMin-hcMax)
		clogd[i] <- 1 - ((hc[i-1]-hcMax)/(hcMin - hcMax))^theta

	#track accumilated degree days to decide if we are in Endo or Ectodormancy
	
	if (accumilatedDD[i] > edb) { # is teh value larger than -700. 

		hcChange[i] <- (ddEndc[i] * kaEnd * cloga[i]) + (ddEndh[i] * kdEnd * clogd[i]) + eps

	} else

	hcChange[i] <- (ddEctc[i] * kaEct * cloga[i]) + (ddEcth[i] * kdEct * clogd[i]) + eps

	#get the actual hardiness value 
	hc[i] <- hc[i-1] + hcChange[i]
}

plot(hcChange)
plot(hc)	
plot(hc ~ tempValues) # In this plot the data looks more spreadout than i would expect 

# how do my simulated data compare to their predicted values?

plot(hc ~ chardDataf$Predicted_Hc)

plot(chardDataf$Predicted_Hc ~ tempValues)


# the simulated data generally predicts the model well, but there is an issue with early autumn 
# temperatures. In the predicted data from the excel file, bud hardiness starts at -10.3, and then 
# remains at this temperature until day 272. I can't find an explination for this. Accodring to their 
#parameters as I understand them, the hardiness should increase before decreasing because temperatures
# are above the threshold of 13 degrees C for warming. I might try starting the model at day 272 as see 
# how this affects things


#STAN MODEL 
#preparing the data - some repeated from above 
#-----------------------------

# I need to index the x values from 1 to n(x)

tempValues <- I(tempValues)
hc <- hc #keep y albeled as hc
N <- length(tempValues)

hcInit <- -10.3 # start hardiness value. 
hcMin <- -1.2 # min hardiness acording to Ferguson 2014 

#kaEnd <- 0.12 # constant accumilation rate during endodormancy
#kaEct <- 0.10 # constant accumilation rate during ectodormancy
#kdEnd <- 0.08 # constant deaccumilation rate during endodormancy
#kdEct <- 0.10 # constant deaccumilation during ectodormancy 
#tThreshend <- 13 or 10. I am not sure which. 
#tThreshect <- 5
#edb <- -700
#theta <- 7

#data passed to STan needs to be a list of named objects. names here need to match names in model code
#i make a LIST of the different varables, NOT data frame
stan_data3 <- list(tempValues = tempValues, hc = hc, N = N, hcInit = hcInit, hcMin = hcMin)

#i would like these parameters in teh model too:
#  threshold temperatures 
#  edb

#also maybe a level fo variation for variety. so i woudl need to add a level of variation around the 
#acclimation/deacclimation rates 


DynamicFit3 <- stan(file = stan_dynamic3, data = stan_data3, warmup = 1000, 
	iter = 5000, chains = 4, cores = 4, thin = 1, , control = list(max_treedepth = 15))


posteriorDynamic3 <- extract(DynamicFit3)
str(posteriorDynamic3)
#kaEnd <- 0.12 # constant accumilation rate during endodormancy
#kaEct <- 0.10 # constant accumilation rate during ectodormancy
#kdEnd <- 0.08 # constant deaccumilation rate during endodormancy
#kdEct <- 0.10 # constant deaccumilation during ectodormancy 
#tThreshend <- 13 or 10. I am not sure which. 
#tThreshect <- 5
#edb <- -700
#theta <- 7


plot(density(posteriorDynamic3$kaEnd)) # Looks good. should be 0.12

plot(density(posteriorDynamic3$kaEct)) # Looks ok. should be 0.10

plot(density(posteriorDynamic3$kdEnd)) # way WAY out. should be 0.08

plot(density(posteriorDynamic3$kdEct)) # Looks good shoudl be 0.10

plot(density(posteriorDynamic3$sigma)) # close to expected 0.5 

plot(density(posteriorDynamic3$edb)) # looks good. should be 700

plot(density(posteriorDynamic3$tThreshEct)) # 3.5. Maybe a bit of an underestimate

plot(density(posteriorDynamic3$tThreshEnd)) # 11. Under


#try a prior predictive check to help fix the model
#------------------------------------------

#my plan is to use the parameters to predict winter temperatures many times. 

#simulate parameters from the prior distribution 

#acclimation and deacclimation rates 
kaEnd2 <- rnorm(100, 0, 0.5)
kaEct2 <- rnorm(100,0, 0.5)
kdEnd2 <- rnorm(100,0, 0.5)
kdEct2 <- rnorm(100,0, 0.5)
tThreshEnd2 <- rtruncnorm(100,5, 10) 
tThreshEct2 <- rtruncnorm(100,5, 10)
edb2 <- rtruncnorm(100, mean = 700, sd = 300)
theta2<- rtruncnorm(100, mean = 5, sd = 5)

#other values for the model 
tempValues # simulated temerature values for one year 
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

priorCheckLTEs$ddEndc2 <- NA
priorCheckLTEs$ddEctc2 <- NA
priorCheckLTEs$ddEndh2 <- NA
priorCheckLTEs$ddEcth2 <- NA
priorCheckLTEs$cloga <- NA
priorCheckLTEs$clogd <- NA
priorCheckLTEs$accumilatedDD2 <- NA

#build a loop for all data 

for (reps in unique(priorCheckLTEs$rep)) { # get growing degree days for each repetition 

#	rep <- 1

	#tempeperatures below the mean threshold values are chilling days, and above are heating
	ddEnd2 <- tempValues - tThreshEnd2[reps] 
	ddEct2 <- tempValues - tThreshEct2[reps]

	#make a vector of chilling days only 
	ddEndc2 <- ddEnd2
	ddEndc2[ddEndc2 > 0 ] <- 0
	priorCheckLTEs$ddEndc2[priorCheckLTEs$rep == reps] <- ddEndc2

	#ddEndc <- ddEndc * -1 # i think these should be negative values?

	ddEctc2 <- ddEct2
	ddEctc2[ddEctc2 > 0 ] <- 0
	priorCheckLTEs$ddEctc2[priorCheckLTEs$rep == reps] <- ddEctc2

	#vector of heating days only 
	ddEndh2 <- ddEnd2
	ddEndh2[ddEndh2 < 0] <- 0
	priorCheckLTEs$ddEndh2[priorCheckLTEs$rep == reps] <- ddEndh2

	ddEcth2 <- ddEct2
	ddEcth2[ddEcth2 < 0] <- 0
	priorCheckLTEs$ddEcth2[priorCheckLTEs$rep == reps] <- ddEcth2


	#get accumilating degree growing days
	accumilatedDD2 <- cumsum(ddEndc2)
	priorCheckLTEs$accumilatedDD2[priorCheckLTEs$rep == reps] <- accumilatedDD2


}

for (i in (2:length(tempValues))){

	#i <- 150

	for (ic in 1:100){

	#calculate the asymptotic bound logistic function 
		priorCheckLTEs$cloga[priorCheckLTEs$rep == ic & priorCheckLTEs$counter == i] <- 1 - ((hcMin - hc[i-1])/(hcMin-hcMax))
		priorCheckLTEs$clogd[priorCheckLTEs$rep == ic & priorCheckLTEs$counter == i] <- 1 - ((hc[priorCheckLTEs$rep == ic& priorCheckLTEs$counter == i-1]-hcMax)/(hcMin - hcMax))^ theta[ic]

		#track accumilated degree days to decide if we are in Endo or Ectodormancy
		if (accumilatedDD[i] > edb) {

			hcChange[i] <- (priorCheckLTEs$ddEndc[priorCheckLTEs$rep == ic & priorCheckLTEs$counter == i] * kaEnd2[ic] * cloga[priorCheckLTEs$rep == ic & priorCheckLTEs$counter == i]) + (ddEndh[priorCheckLTEs$rep == ic & priorCheckLTEs$counter == i] * kdEnd2[ic] * clogd[priorCheckLTEs$rep == ic& priorCheckLTEs$counter == i]) 

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

#these prior predictive checks need finishing 