#a function to replicate column CF in Carl's hardiness excel spreadsheet model


adjustcf <- function(hisData, cd, ce, year, month, day, doynum, period){
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





head(climall)

#what day of the year is October 31st? This is the day that CF starts to decline from 1
climall$doynum[climall$month == "Oct" & climall$day == 29]

#for 2012 

climall2012 <- climall[climall$Year == 2012,]

	changeDateAcc <- climall2012 $doynum[climall2012$month == "Oct" & climall2012 $day == "30"]
	startDateMax <- climall2012 $doynum[climall2012$month == "Dec" & climall2012 $day == "08"]

	for(i in c(1:length(hisData)))
		{
		if(is.na(period[i]) == TRUE) {
				adjustcf [i] <- NA
			} 


#if the day of the year is more than  303 (changeDate) and in teh "Acc" hardiness period then the value for 
#cf shoudl be 1
	
	else if (period[i] == "Acc" & day < changeDateAcc ){
		adjustcf [i] <- 1
			}

#after this date the vlaue shoudl decline by 0.007 until december the 7th (end of accumilation period)

	else if (period[i] == "Acc" & !day < changeDateAcc ){
		adjustcf [i] <- adjustcf [i-1] - 0.007
			}

#in teh maximum accumilation period the values starts at 1.30 and declines by 0.01 until feb teh 7th when
#Deacc starts 





#for Deacc there is an equation =IF(AND(BY165>3,CD165<0),0.1,(CD165*CE165)). BY is teh average 2 day mean 
#historical temperature data climall$meanC2day.hist







