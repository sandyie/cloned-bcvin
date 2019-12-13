1#a function to replicate column CD in Carl's hardiness excel spreadsheet model


adjustcd <- function(period, hitData, LTEchange){
	
	# thsi function replicates the column CD coding in Car's hardiness spreadsheet
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


	adjustcd <- c()
      for(i in c(1:length(hitData)))
	{
		if(period[i] == "noPeriod" ) {
				adjustcd[i] <- NA
			} 

		#function for the first subset of dat a- i think accumilating hardiness?
		#=IF(CC26<-8,1.8,IF(CC26<-5,1.6,IF(CC26<-4,1.4,IF(CC26<-3,1.2,IF(CC26<-2,1.1,IF(CC26<0,1,0))))))
		else if (period[i] == "Acc"){
				if(is.na(hitData[i])==TRUE) {
      			  adjustcd[i] <- NA} 
				else if (hitData[i]< -7) {adjustcd[i] <- 1.8} 
				else if (hitData[i]< -5) {adjustcd[i] <- 1.6} 
				else if(hitData[i]< -4) {adjustcd[i] <- 1.4} 
				else if(hitData[i]< -3) {adjustcd[i] <- 1.2} 
				else if(hitData[i]< -2) {adjustcd[i] <- 1.1} 
				else if(hitData[i]< -0) {adjustcd[i] <- 1} 
				else adjustcd[i] <- 0 # 
			} 

		#function for teh next subset - i think the first maximum hardiness period
		#i dont really understand why there are two hardines periods....
		#=IF(CC104<-4,-1.4,IF(CC104<-3,-1.3,IF(CC104<-2,-1.25,IF(CC104<-1,-1.2,IF(CC104<0,-1.15,1)))))
		else if (period[i] == "Max"){
				if(is.na(hitData[i])==TRUE) {adjustcd[i] <- NA} 
				else if (hitData[i]< -4){adjustcd[i] <- -1.4}
				else if (hitData[i]< -3) { adjustcd[i] <- -1.3}
          			else if (hitData[i]< -2) { adjustcd[i] <- -1.25}
       		 	else if (hitData[i]< -1) { adjustcd[i] <- -1.2}
            	     	else if (hitData[i]< -0){adjustcd[i] <- -1.15}
				else adjustcd[i] <- 1 

			} 

		#function for teh third period - i think another section of maximum hardiness?
		#=IF(CC143<-4,-1.7,IF(CC143<-3,-1.6,IF(CC143<-2,-1.5,IF(CC143<-1,-1.25,IF(CC143<0,-1,1)))))

		else if (period[i] == "Max2"){
				if(is.na(hitData[i])==TRUE) {adjustcd[i] <- NA} 
				else if (hitData[i]< -4) { adjustcd[i] <- -1.7}
      		 	else if (hitData[i]< -3) {adjustcd[i] <- -1.6}
          		 	else if (hitData[i]< -2) {adjustcd[i] <- -1.5}
       		 	else if(hitData[i]< -1) {adjustcd[i] <- -1.25} 
				else if(hitData[i]< -0) {adjustcd[i] <- -1} 
            		else adjustcd[i] <- 1
			}

		#function for the fourth period - i think deacclimation? This needs the LTEchange data (column CC) as well
		#=IF(CC193<-9,CB193*-2,IF(CC193<-7,CB193*-1.5,IF(CC193<-5,CB193*-1.1,IF(CC193<-4,CB193*-1,IF(CC193<-3,CB193*-0.5,IF(CC193<-2,CB193*0.2,IF(CC193<-1,CB193*1.1,1)))))))
		else 
				if(is.na(hitData[i])==TRUE) {adjustcd[i] <- NA} 
				else if(hitData[i]< -9) {adjustcd[i] <- LTEchange[i]*-2.0} 
  	 			else if(hitData[i]< -7) {adjustcd[i] <- LTEchange[i]*-1.5}
				else if(hitData[i]< -5) {adjustcd[i] <- LTEchange[i]*-1.1}
      			else if(hitData[i]< -4) {adjustcd[i] <- LTEchange[i]*-1} 
    	 			else if(hitData[i]< -3) {adjustcd[i] <- LTEchange[i]*-0.5}
				else if(hitData[i]< -2) {adjustcd[i] <- LTEchange[i]*0.2} 
 				else if(hitData[i]< -1) {adjustcd[i] <- LTEchange[i]*1.1}
      			else adjustcd[i] <- 1
	}
	return(adjustcd)
}









