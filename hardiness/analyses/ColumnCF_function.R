#a function to replicate column CF in Carl's hardiness excel spreadsheet model


adjustcf <- function(hitData, cd, ce, year, month, day, doynum, period){
	adjustcf <- c()
	
	# this function replicates the column CF coding in Car's hardiness spreadsheet
	# input:

head(climall)

#what day of the year is October 31st? This is the day that CF starts to decline from 1
climall$doynum[climall$month == "Oct" & climall$day == 29]

#for 2012 

climall2012 <- climall[climall$Year == 2012,]

	changeDate <- climall2012 $doynum[climall2012$month == "Oct" & climall2012 $day == 29]

	for(i in c(1:length(hitData)))
		{
		if(is.na(period[i]) == TRUE) {
				adjustcf [i] <- NA
			} 


#if the day of the year is more than  303 (changeDate) and in teh "Acc" hardiness period then the value for 
#cf shoudl be 1
	
	else if (period[i] == "Acc"){


#after this date the vlaue shoudl decline by 0.007 until december the 7th (end of accumilation period)

#in teh maximum accumilation period the values starts at 1.30 and declines by 0.01 until feb teh 7th when
#Deacc starts 





#for Deacc there is an equation =IF(AND(BY165>3,CD165<0),0.1,(CD165*CE165)). BY is teh average 2 day mean 
#historical temperature data climall$meanC2day.hist







