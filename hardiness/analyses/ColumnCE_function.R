#a function to replicate column CE in Carl's hardiness excel spreadsheet model

adjustce <- function(period, hitData, LTEchange){
	adjustcd <- c()

	# this function replicates the column CE coding in Car's hardiness spreadsheet
	#for when teh temp difference is >0
	#input:
	#  period = a vector indicating what period of hardiness the vine is in. It takes 
	#  four values (and NA) - "Acc" teh autum when teh vince is acclimating to cold
	#				 "Max" teh first half of what i think is the period of max hardiness. 
	#				 For some reason Carl uses a slightly different tranformation half way through January
	#			       "Max2" the other half of the maximum hardiness period
	#			 	 "Deacc" the deacclimation period in teh spring 
	#  hitData = this is equivalent to column CC in Carl's spreadsheet. It is the difference in temperature between
	#  the historical and recorded 2 day average temperatures 
	#  LTEchange = this is equivalent to column CB in Carl's spreadsheet, and is teh change in LTE each day   


      for(i in c(1:length(hitData)))
	{
		if(period[i] == "noPeriod") {
				adjustcd[i] <- NA
			} 

		#function for the first subset of dat a- i think accumilating hardiness?
		#=IF(CC26>5,0.8,IF(CC26>4,0.85,IF(CC26>3,0.9,IF(CC26>2,0.95,IF(CC26>1,0.98,IF(CC26>0,1,0))))))
		else if (period[i] == "Acc"){
				if(is.na(hitData[i])==TRUE) {
      			  adjustcd[i] <- NA} 
				else if (hitData[i] > 5) {adjustcd[i] <- 0.8} 
				else if (hitData[i] > 4) {adjustcd[i] <- 0.85} 
				else if(hitData[i] > 3) {adjustcd[i] <- 0.9} 
				else if(hitData[i]> 2) {adjustcd[i] <- 0.95} 
				else if(hitData[i]> 1) {adjustcd[i] <- 0.98} 
				else if(hitData[i]> 0) {adjustcd[i] <- 1} 
				else adjustcd[i] <- 0 
			} 

		#function for teh next subset - i think the first maximum hardiness period
		#i dont really understand why there are two hardines periods....
		#=IF(CC104>5,1.3,IF(CC104>4,1.1,IF(CC104>3,0,IF(CC104>2,-0.5,IF(CC104>1,-1,IF(CC104>0,-1.1,1))))))
		else if (period[i] == "Max"){
				if(is.na(hitData[i])==TRUE) {adjustcd[i] <- NA} 
				else if (hitData[i]> 5){adjustcd[i] <- 1.3}
				else if (hitData[i]> 4) {adjustcd[i] <- 1.1}
          			else if (hitData[i]> 3) {adjustcd[i] <- 0}
       		 	else if (hitData[i]> 2) {adjustcd[i] <- -0.5}
            	     	else if (hitData[i]> 1){adjustcd[i] <- -1}
				else if(hitData[i]> 0) {adjustcd[i] <- -1.1} 
				else adjustcd[i] <- 1 # 

			} 

		#function for teh third period - i think another section of maximum hardiness?
		#=IF(CC134>5,1.3,IF(CC134>4,1.1,IF(CC134>3,1,IF(CC134>2,0.8,IF(CC134>1,0.5,IF(CC134>0,-0.5,1))))))

		else if (period[i] == "Max2"){
				if(is.na(hitData[i])==TRUE) {adjustcd[i] <- NA} 
				else if (hitData[i]> 5){adjustcd[i] <- 1.3}
				else if (hitData[i]> 4) { adjustcd[i] <- 1.1}
          			else if (hitData[i]> 3) { adjustcd[i] <- 1}
       		 	else if (hitData[i]> 2) { adjustcd[i] <- 0.8}
            	     	else if (hitData[i]> 1){adjustcd[i] <- 0.5}
				else if(hitData[i]> 0) {adjustcd[i] <- -0.5} 
				else adjustcd[i] <- 1 # 

			}


		#function for the fourth period - i think deacclimation? This needs the LTEchange data (column CC) as well
#		=IF(CC170>7,CB170*2,IF(CC170>4,CB170*1.8,IF(CC170>3,CB170*1.6,
#		IF(CC170>2,CB170*1.4,IF(CC170>1,CB170*1.2,IF(CC170>0,CB170*1.1,IF(CC170>-1,CB170*1.1,1)))))))

		else 
				if(is.na(hitData[i])==TRUE) {adjustcd[i] <- NA} 
				else if(hitData[i] > 7) {adjustcd[i] <- LTEchange[i]*2.0} 
  	 			else if(hitData[i] > 4) {adjustcd[i] <- LTEchange[i]*1.8}
				else if(hitData[i] > 3) {adjustcd[i] <- LTEchange[i]*1.6}
      			else if(hitData[i] > 2) {adjustcd[i] <- LTEchange[i]*1.4} 
    	 			else if(hitData[i] > 1) {adjustcd[i] <- LTEchange[i]*1.2}
				else if(hitData[i] > 0) {adjustcd[i] <- LTEchange[i]*1.1} 
 				else if(hitData[i] > -1) {adjustcd[i] <- LTEchange[i]*1.1}
      			else adjustcd[i] <- 1
	}
	return(adjustcd)
}








