#a function to replicate column CF in Carl's hardiness excel spreadsheet model


adjustcf <- function(period, hisData, cd, ce, year, month, day, doynum ){
	yearDates <- data.frame(matrix(NA,length(unique(year)), 3))
	names(yearDates ) <- c("Year", "dateAcc", "dateMax")
	yearDates$Year <- unique(year)
	adjustcf <- c()
	
	
	# this function replicates the column CF coding in Car's hardiness spreadsheet
	# input:
	# hisData - thsi is the historical 2 day mean temperature equivalent to column BY in Car's spreadsheet  
	# ce - this is the values from column ce of the amazing spreadsheet 
	# cd - this is the values from column cd of the amazing spreadsheet 
	# year - this is teh year the data comes from 
	# month - this is the month the data comes from
	# day - this is the day the data comes from
	# doynum - this is the day of teh year the data comes from
	# period - this is what hardiness period the data is in 

	hisData  <- climall$meanC2day.hist
	ce <- climall$CE 
	cd <- climall$CD
	year <- climall$Year
	month <- climall$month
	day <- climall$day
	doynum <- climall$doynum
	period <- climall$HardinessPeriod

	#geting the days of the year for the different sections of the rest of teh function
	#this needs doing because of leap years  


	for (yeari in unique(year)){

		yearDates$dateAcc[yearDates$Year == yeari] <- doynum[month == "Oct" & day == 29 & year == yeari]
		yearDates$dateMax[yearDates$Year == yeari] <- doynum[month == "Dec" & day == 8 & year == yeari]
	}

	for(i in c(1:length(hisData)))
		{
		if( period[i] == "noPeriod") {adjustcf [i] <- NA
		} else if (period[i] == "Acc" & doynum[i] <= yearDates$dateAcc[yearDates$Year == year[i]] ){adjustcf [i] <- 1
		} else if (period[i] == "Acc" & !day[i] > yearDates$dateAcc[yearDates$Year == year[i]] ){adjustcf [i] <- adjustcf [i-1] - 0.007
		} else if (doynum[i]  == yearDates$dateMax[yearDates$Year == year[i]]){adjustcf [i] <- 1.3
		} else if (period[i] == "Max" & !doynum[i]  == yearDates$dateMax[yearDates$Year == year[i]] ){adjustcf [i] <- adjustcf [i-1] - 0.01
		} else if (period [i] == "Max2") {adjustcf [i] <- adjustcf [i-1] - 0.01
		
		#for Deacc there is an equation =IF(AND(BY165>3,CD165<0),0.1,(CD165*CE165))BY is the average 2 day mean 
		#historical temperature data climall$meanC2day.hist


		} else 
			if(hisData[i]> 3 & cd[i]< 0) {adjustcf [i] <- 0.1
				} else adjustcf [i] <- cd[i]*ce[i]

		}


	return(adjustcf)
}











