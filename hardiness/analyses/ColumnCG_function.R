#a function to replicate column CG in Carl's hardiness excel spreadsheet model
#currently untested


adjustcg <- function(period, doynum, LTEchange, cd, ce, month, day) 

	adjustcg <- c()
	adjustch <- c()
	adjustci <- c()
	adjustcj <- c()
	adjustck <- c()
	adjustcl <- c()
	adjustcm <- c()
	adjustcn <- c()
	adjustco <- c()


	# this function replicates the columns CG-co coding in Car's hardiness spreadsheet
	# these columns all need to be in the same function because they are are inticatly linked 
	# especially in the deacc period
	# input:
	# doynum = this is the day of teh year the data comes from
	# period = a vector indicating what period of hardiness the vine is in. It takes 
	# four values (and NA) - "Acc" teh autum when teh vince is acclimating to cold
	#				 "Max" teh first half of what i think is the period of max hardiness. 
	#				 For some reason Carl uses a slightly different tranformation half way through January
	#			       "Max2" the other half of the maximum hardiness period
	#			 	 "Deacc" the deacclimation period in teh spring 
	# LTEchange = this is equivalent to column CB in Carl's spreadsheet, and is teh change in LTE each day   
	# cd = column cd from Carl's spreadhseet, as calculated from function ColumnCC_function.R
	# ce = column cd from Carl's spreadhseet, as calculated from function ColumnCE_function.R
		



period <- climall$HardinessPeriod
doynum <- climall$doynum
cf <- climall$CF 
i <- 1
LTEchange <- climall$LTEchange
cd <- climall$CD 
ce <- climall$CE 
month <- climall$month
day <- climall$day 

	for(i in c(1:length(period)))
		{
		if( is.na(period[i]) == TRUE) { # these dont hav starter values before teh first acclimation period
				adjustcg[i] <- NA
				adjustch[i] <- NA
				adjustci[i] <- NA
				adjustcm[i] <- NA
				adjustcn[i] <- NA

		if( is.na(period[i]) == TRUE & !month == "Sep" & !day = 20) {#these do have started values before first acclimation value
				adjustcj[i] <- NA
				adjustck[i] <- NA
				adjustcl[i] <- NA
				adjustco[i] <- NA	

		#starter values for a few columns - teh values are guesses for september the 20th 
		#guesses are made by carl

		}else if (month == "Sep" & day = 20){
			adjustcj[i] <- 0.3
			adjustck[i] <- 0.3
			adjustc1[i] <- 0.3
			adjustc0[i] <- 0.3


		#acclimation phase for columns cg - co
		} else if (period[i] == "Acc"){
			adjustcg[i] <- NA
			adjustch[i] <- NA

			#=((CE46+CD46)*CB46)
			adjustci [i] <- (ce[i] + cd [i]) * LTEchange

			#=(CJ31+((CD32+CE32)*CB32)*CF32)
			cj[i]




		} else if (doynum [i] == 343){adjustcf [i] <- 0.7
		} else if (period[i] %in% c("Max", "Max2") & !doynum [i] == 343){adjustcf [i] <- (adjustcf [i-1] - 0.01)

		#=IF(AND(CO164<-23.5,CF165<0),CF165*0.1,IF(AND(CO164<-22.5,CF165<0),CF165*0.4,IF(AND(CO164<-21.5,CF165<0),CF165*0.7,1)))

		} else period[i] == "Deacc" {adjustcf [i] <- if (co[i]  < 23.5 & cf[i] < 0){adjustcf [i] <-cf[i] * 0.1
											} else if (co[i] < -22.5 & cf[i] < 0) {adjustcf [i] <-cf[i] * 0.4
											} else if (co < -21.5 & cf < 0) {adjustcf [i] <-cf[i] * 0.7
											} else adjustcf [i] <- 1
										}


	return(adjustcf)
}


