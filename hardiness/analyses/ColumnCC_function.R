#a function to replicate column CD in Carl's hardiness excel spreadsheet model


#function for the first subset of dat a- i think accumilating hardiness?
#=IF(CC26<-8,1.8,IF(CC26<-5,1.6,IF(CC26<-4,1.4,IF(CC26<-3,1.2,IF(CC26<-2,1.1,IF(CC26<0,1,0))))))

adjustcdPerAcc  <- function(hitData){
    adjustcd <- c()
    for(i in c(1:length(hitData)))
	{
	if(is.na(hitData[i])==TRUE) {
        adjustcd[i] <- NA} else {
   	 if(hitData[i]< -7) {
      	 adjustcd[i] <- 1.8} else {
   	 if(hitData[i]< -5) {
      	adjustcd[i] <- 1.6} else {
    	 if(hitData[i]< -4) {
     	      adjustcd[i] <- 1.4} else {
       if(hitData[i]< -3) {
            adjustcd[i] <- 1.2} else {
    	 if(hitData[i]< -2) {
       	adjustcd[i] <- 1.1} else {
       if(hitData[i]< -0) {
            adjustcd[i] <- 1} else {
                adjustcd[i] <- 0} # 
               }
              }
            }
          }
        }
     }
  }
    return(adjustcd)
}
adjustcdPerAcc (climAcc$avgTdiff)

#function for teh next subset - i think the first maximum hardiness period
#i dont really understand why there are two hardines periods....
#=IF(CC104<-4,-1.4,IF(CC104<-3,-1.3,IF(CC104<-2,-1.25,IF(CC104<-1,-1.2,IF(CC104<0,-1.15,1)))))

adjustcdPerMax1  <- function(hitData){
    adjustcd <- c()
    for(i in c(1:length(hitData)))
	{
	if(is.na(hitData[i])==TRUE) {
        adjustcd[i] <- NA} else {
		if(hitData[i]< -4) {
      		 adjustcd[i] <- -1.4} else {
       	if(hitData[i]< -3) {
          		adjustcd[i] <- -1.3} else {
    		if(hitData[i]< -2) {
       		adjustcd[i] <- -1.25} else {
       	if(hitData[i]< -1) {
            	adjustcd[i] <- -1.2} else {
       	if(hitData[i]< -0) {
            	adjustcd[i] <- -1.15} else {
                adjustcd[i] <- 0} # need to check this very last statement is correct
               }
              }
            }
          }
	}
	}
    return(adjustcd)
}
#climMax <- climall[climall$HardinessPeriod == "Max" & !is.na(climall$HardinessPeriod),]
#climMax$CD <- adjustcdPerMax1 (climMax$avgTdiff)# seems to be working fine 

#function for teh third period - i think another section of maximum hardiness?
#=IF(CC143<-4,-1.7,IF(CC143<-3,-1.6,IF(CC143<-2,-1.5,IF(CC143<-1,-1.25,IF(CC143<0,-1,1)))))

adjustcdPerMax2  <- function(hitData){
    adjustcd <- c()
    for(i in c(1:length(hitData)))
	{
	if(is.na(hitData[i])==TRUE) {
        adjustcd[i] <- NA} else {
		if(hitData[i]< -4) {
      		 adjustcd[i] <- -1.7} else {
       	if(hitData[i]< -3) {
          		adjustcd[i] <- -1.6} else {
    		if(hitData[i]< -2) {
       		adjustcd[i] <- -1.5} else {
       	if(hitData[i]< -1) {
            	adjustcd[i] <- -1.25} else {
       	if(hitData[i]< -0) {
            	adjustcd[i] <- -1} else {
                adjustcd[i] <- 1} # need to check this very last statement is correct
               }
              }
            }
          }
	}
	}
    return(adjustcd)
}

#climMax2 <- climall[climall$HardinessPeriod == "Max2" & !is.na(climall$HardinessPeriod),]
#climMax2$CD <- adjustcdPerMax2 (climMax2$avgTdiff)# seems to be working fine 

#function for the fourth period - i think deacclimation? This needs the LTEchange data (column CC) as well
#=IF(CC193<-9,CB193*-2,IF(CC193<-7,CB193*-1.5,IF(CC193<-5,CB193*-1.1,IF(CC193<-4,CB193*-1,IF(CC193<-3,CB193*-0.5,IF(CC193<-2,CB193*0.2,IF(CC193<-1,CB193*1.1,1)))))))

adjustcdPerDeacc  <- function(hitData, LTEchange){
    adjustcd <- c()
    for(i in c(1:length(hitData)))
	{
	if(is.na(hitData[i])==TRUE) {
        adjustcd[i] <- NA} else {
  	 if(hitData[i]< -9) {
      	 adjustcd[i] <- LTEchange[i]*-2.0} else {
   	 if(hitData[i]< -7) {
      	adjustcd[i] <- LTEchange[i]*-1.5} else {
    	 if(hitData[i]< -5) {
     	      adjustcd[i] <- LTEchange[i]*-1.1} else {
       if(hitData[i]< -4) {
            adjustcd[i] <- LTEchange[i]*-1} else {
    	 if(hitData[i]< -3) {
       	adjustcd[i] <- LTEchange[i]*-0.5} else {
       if(hitData[i]< -2) {
            adjustcd[i] <- LTEchange[i]*0.2} else {
	 if(hitData[i]< -1) {
            adjustcd[i] <- LTEchange[i]*1.1} else {
                adjustcd[i] <- 1} # need to check this very last statement is correct
               }
              }
            }
          }
        }
	 }
	}
	}
    return(adjustcd)
}

adjustcdPerDeacc  <- function(hitData, LTEchange){
    adjustcd <- c()
    for(i in c(1:length(hitData)))
	{
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

#climDeacc <- climall[climall$HardinessPeriod == "Deacc" & !is.na(climall$HardinessPeriod),]
#climDeacc$CD <- adjustcdPerDeacc (climDeacc$avgTdiff, climDeacc$accdiffmax)# seems to be working fine, 
#but there are differences because the accdiffmax is different from column CB in soem cases
 
adjustcd <- function(period, hitData, LTEchange){

#period <- climall$HardinessPeriod
#hitData <- climall$avgTdiff
#LTEchange <-climall$accdiffmax

	adjustcd <- c()
      for(i in c(1:length(hitData)))
	{
		if(is.na(period[i]) == TRUE) {
				adjustcd[i] <- NA
			} 

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
				else adjustcd[i] <- 0 # need to check this very last statement is correct

			} 

		#function for the fourth period - i think deacclimation? This needs the LTEchange data (column CC) as well
		#=IF(CC193<-9,CB193*-2,IF(CC193<-7,CB193*-1.5,IF(CC193<-5,CB193*-1.1,IF(CC193<-4,CB193*-1,IF(CC193<-3,CB193*-0.5,IF(CC193<-2,CB193*0.2,IF(CC193<-1,CB193*1.1,1)))))))
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

climall$CD <- adjustcd(climall$HardinessPeriod, climall$avgTdiff, climall$accdiffmax)
View(climall)






