#a function to replicate column CG through CO in Carl's hardiness excel spreadsheet model
#these columns are closly connected in teh if/else stayements so need doing together in a single function :(

adjustcgtoco <- function(period, doynum, LTEchange, hitData, cd, ce, cf, year, month, day, caEstimateLTE) {

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
	#			 "Deacc" the deacclimation period in teh spring 
	# LTEchange = this is equivalent to column CB in Carl's spreadsheet, and is teh change in LTE each day   
	# cd = column cd from Carl's spreadhseet, as calculated from function ColumnCC_function.R
	# ce = column cd from Carl's spreadhseet, as calculated from function ColumnCE_function.R
	# cf = column cf from Carl's spreadshee, as calculated from function ColumnCF_function.R
	# hitData = this is equivalent to column CC in Carl's spreadsheet. It is the difference in temperature between
	#  the historical and recorded 2 day average temperatures 
	# this code assumes that there are no spring data for 2012, and that all other years have spring data	
	# caEstimateLTE = column ca of the spreadsheet, calculated using the hhardiness stab eqacc function. Etimated LTE each year, 
	# based off teh historical temperature data 


#period <- climallTest$HardinessPeriod
#doynum <- climallTest$doynum
#cf <- climallTest$CF 
#LTEchange <- climallTest$Estimate.LTE.day
#cd <- climallTest$CD 
#ce <- climallTest$CE 
#month <- climallTest$month
#day <- climallTest$day 
#year <- climallTest$Year
#caEstimateLTE <- climallTest$acc
#hitData <- climallTest$avgTdiff


	#get the day of teh year for  20th of september and teh first of march for each year 
	yearDates <- data.frame(matrix(NA,length(unique(na.omit(year))), 3))
	names(yearDates ) <- c("Year", "dateSep20", "dateMarch2")
	yearDates$Year <- unique(na.omit(year))
	
	yearDates$dateSep20[yearDates$Year == 2012] <- doynum[month == "Sep" & day == 20 & year == 2012]

	for(yeari in year[!year == 2012] ){#2012 has no spring data 
	yearDates$dateMarch2[yearDates$Year == yeari] <- doynum[month == "Mar" & day == 2 & year == yeari]
	yearDates$dateSep20[yearDates$Year == yeari] <- doynum[month == "Sep" & day == 20 & year == yeari]

	}

	for(i in c(1:length(period)))
		{
		#starter values for a few columns - the values are guesses for september the 20th 
		#guesses are made by carl
		if(doynum[i] ==  yearDates$dateSep20[yearDates$Year == year[i]]) {#these do have started values before first acclimation value
				adjustcj[i] <- 0.3
				adjustck[i] <- 0.3
				adjustcl[i] <- 0.3
				adjustco[i] <- 0.3

		} else if(period[i] == "noPeriod") { # these dont have starter values before teh first acclimation period
				adjustcg[i] <- NA
				adjustch[i] <- NA
				adjustci[i] <- NA
				adjustcm[i] <- NA
				adjustcn[i] <- NA
				adjustcj[i] <- NA
				adjustck[i] <- NA
				adjustcl[i] <- NA
				adjustco[i] <- NA

		#acclimation phase for columns cg - co
		} else if (period[i] == "Acc"){
			
			#CG
			adjustcg[i] <- NA
			
			#CH
			adjustch[i] <- NA

			#CI
			#=((CE26+CD26)*CB26)
			adjustci [i] <- (ce[i] + cd [i]) * LTEchange[i]

			#CJ
			#=(CJ31+((CD32+CE32)*CB32)*CF32)
			adjustcj[i] <- adjustcj[i-1] + ((cd[i] + ce[i])*LTEchange[i])* cf[i]

			#CK
			#=IF(AND(CJ25<-23.5,CI26<0),CI26*0.5,CI26)
			if (adjustcj [i-1] < -23.5 & adjustci [i] < 0){
				adjustck[i] <- adjustci[i]*0.5
				} else adjustck[i] <- adjustci[i]

			#CL
			#=IF(AND(CO25<-24.5,CC26>0),(CK26+0.2),CK26)
			if (adjustco [i-1] < -24.5 & hitData [i] > 0) {
				adjustcl [i] <-  adjustck[i] + 0.2
			} else adjustcl [i] <- adjustck [i]

			#CM 
			adjustcm [i] <- NA

			#CN
			adjustcn [i] <- NA

			#CO
			#=(CO25+CL26)
			adjustco[i] <- adjustco[i-1] + adjustcl[i]

		} else if (period[i] %in% c("Max", "Max2")) {

			#CG 
			if (month [i] == "Dec" & day [i] == 8){
			adjustcg[i] <- 0.7
			} else adjustcg [i] <- adjustcg[i-1] + 0.01 
			
			#CH 
			adjustch[i] <- NA
	
			#CI
			#=IF(CC104>2,(CB104*CD104*CE104*CG104),(CB104*CD104*CE104*CF104))
			if (hitData[i]  > 2) {adjustci [i] <- LTEchange[i] * cd[i]*ce[i]*adjustcg[i]
				} else adjustci[i] <- (LTEchange[i] * cd[i]*ce[i]*cf[i])

			#CK
			#=IF(AND(CJ106<-23.5,CI107<0),CI107*0.5,CI107)
			if (adjustcj [i-1] < -23.5 & adjustci [i] < 0){
				adjustck[i] <- adjustci[i] * 0.5
				} else adjustck[i] <- adjustci[i]

			#CJ
			#=IF((CJ103+(CI104))<-24.5,-24.5,(CJ103+(CK104)))
			if (adjustcj[i-1] + adjustci[i] < -24.5) {adjustcj[i] <- -24.5
			} else adjustcj[i] <- (adjustcj[i-1] + adjustck[i])  
					
			#CL
			#=IF(AND(CO103<-24.5,CC104>0),(CK104+0.2),CK104)
			if (adjustco [i-1] < -24.5 & hitData [i] > 0) {adjustcl[i] <- (adjustck[i] + 0.2)
			} else adjustcl[i] <- adjustck [i]

			#CM
			adjustcm[i] <- NA
	
			#CN 
			adjustcn[i]<- NA

			#CO 
			#=(CO163+CL164)
			adjustco[i] <- adjustco[i-1] + adjustcl[i]
		

		} else if (period[i] == "Deacc"){

			#CG
			#=IF(AND(CO164<-23.5,CF165<0),CF165*0.1,IF(AND(CO164<-22.5,CF165<0),CF165*0.4,IF(AND(CO164<-21.5,CF165<0),CF165*0.7,1)))
			if (adjustco[i-1]  < -23.5 & cf[i] < 0){adjustcg [i] <- cf[i] * 0.1
			} else if (adjustco[i-1] < -22.5 & cf[i] < 0) {adjustcg [i] <- cf[i] * 0.4
			} else if (adjustco[i-1] < -21.5 & cf[i] < 0) {adjustcg [i] <- cf[i] * 0.7
			} else adjustcg [i] <- 1
			

			#CH
			#=IF(AND(CO164<-23.5,CF165>0),CF165*1.2,IF(AND(CO164<-22.5,CF165>0),CF165*1.1,IF(AND(CO164<-21.5,CF165>0),CF165*1.05,1)))
			if (adjustco[i-1] < -23.5 & cf [i] > 0){adjustch [i] <- cf[i]*1.2
			} else if (adjustco[i-1] < -22.5 & cf [i] > 0){adjustch [i] <- cf[i] *1.1
			} else if (adjustco[i-1] < -21.5 & adjustcg[i] > 0) {adjustch[i] <- cf[i]*1.05
			} else adjustch [i] <- 1

			#CI
			#=IF(CH165*CG165=1,CF165,(CG165*CH165))
			if (adjustch[i] * adjustcg[i] == 1) {adjustci[i] <- cf[i]
			} else adjustci[i] <- adjustcg[i]*adjustch[i]

			#CK
			#=IF(AND(CJ164<-23.5,CI165<0),CI165*0.5,CI165)
			if (adjustcj [i-1] < -23.5 & adjustci [i] < 0){
				adjustck[i] <- adjustci[i]*0.5
				} else adjustck[i] <- adjustci[i]

			#CJ
			#=IF((CJ164+(CI165))<-24.5,-24.5,(CJ164+(CK165)))
			if (adjustcj[i-1] + adjustci[i] < -24.5) {adjustcj[i] <- -24.5
			} else adjustcj[i] <-  adjustcj[i-1] + adjustck[i]

			#CL
			#=IF(AND(CO164<-24.5,CC165>-2),(CK165+0.2),CK165)
			if ( adjustco[i-1] < -24.5 & hitData[i] > -2) {adjustcl[i] <- adjustck[i]+0.2
			} else adjustcl [i] <- adjustck[i]

			#CM
			#before March 2nd
			if (year[i] == 2012){ adjustcm[i] <- NA #there are no data for spring 2012
			} else if (doynum[i] < yearDates$dateMarch2[yearDates$Year == year[i]]){
			adjustcm[i] <- NA

			#after march 2nd
			#=IF(AND(CO188<(CA189-1.8),CC189>2),(CK189+0.2),CK189)
		
			} else if (doynum[i] >= yearDates$dateMarch2[yearDates$Year == year[i]]){			#after March 2nd 
					if (i == 1) {adjustcm[i] <- NA
					} else if (adjustco[i-1] < (caEstimateLTE [i] - 1.8 & hitData[i] > 2)){adjustcm[i] <- adjustck[i]+0.2
			     } else adjustcm[i] <- adjustck[i]
			}					

			
			#CN
			#=IF(CO164>-11,(CL165*0.5),CL165)
			if (adjustco[i-1] > -11) {adjustcn[i] <- adjustcl[i]*0.5
			} else adjustcn[i] <- adjustcl[i]

			#CO
			#=(CO169+CN170)
			adjustco[i] <- adjustco[i-1] + adjustcn[i]
		}
		}	
		columns <- data.frame(cbind(period, month, day, year, adjustcg, adjustch, adjustci,	adjustcj,adjustck,adjustcl, adjustcm,adjustcn,adjustco))
		names(columns ) <- c("hardynessPeriod", "month", "day", "Year", "CG", "CH", "CI", "CJ", "CK", "CL", "CM", "CN", "CO")
		return(columns )
		}
	
<<<<<<< HEAD

=======

>>>>>>> 189d162f4b7c9f8293b6ecdc725c1b474072ba74







