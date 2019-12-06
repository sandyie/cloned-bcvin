#a function to replicate column CG in Carl's hardiness excel spreadsheet model

adjustcg <- function(period, doynum, co, cf) 

	adjustcg <- c()

	# this function replicates the column CG coding in Car's hardiness spreadsheet
	# input:
	# doynum - this is the day of teh year the data comes from
	# period - this is what hardiness period the data is in 
	# co - column CO from Carl's spreadheet 
	# cf - column CF from Carl's spreadsheet 


	for(i in c(1:length(period)))
		{
		if( is.na(period[i]) == TRUE) {adjustcf [i] <- NA
		} else if (period[i] == "Acc"){adjustcf [i] <- NA
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


