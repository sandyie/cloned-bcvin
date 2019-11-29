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
                adjustcd[i] <- 0} # need to check this very last statement is correct
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
climMax <- climall[climall$HardinessPeriod == "Max" & !is.na(climall$HardinessPeriod),]

climMax$CD <- adjustcdPerMax1 (climMax$avgTdiff)
View(climMax)





