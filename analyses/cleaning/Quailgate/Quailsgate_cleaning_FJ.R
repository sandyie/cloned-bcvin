# Faith attempt to clean quailsgate data. 
#I am going to focus on the 2000-2012 data to to begin with.
#Code started Jan 6th, 2020

#still a wokrk in pocess!

rm(list = ls())
options(stringsAsFactors = FALSE)

#libraries
library(XLConnect) # for talking to excel
library(data.table) # for binding lists together
library(dplyr)

#read in spreadsheets
#--------------------------------------------------

setwd("/home/faith/Documents/github/bcvin/data/quailsgate")
list.files()#check what files are present 

#brix and variety data 2000-2012


#phenology data 2000-2012
#

#connect to excel file
phenology0012 <- XLConnect::loadWorkbook ( "PHENOLOGICAL_DATA_ed_MY.xls" , create = TRUE )

sheetNamesPheno0012 <- getSheets(phenology0012)#lists the sheet names within an excel file 
#drop 2012 David sheet because i dont know what it is 
sheetNamesPheno00122 <- sheetNamesPheno0012 [!sheetNamesPheno0012 == "David 2012"] 

#open all the year's spreadsheets containing phenology data 
phenology2012 <- readWorksheet(phenology0012, sheet = "2012", startRow = 0, endRow = 10,
startCol = 0, endCol = 0)

#make an empty list to hold the data 
phenology0012List <- list()
phenology0012ListCh <- list() # this one is all characters to help with merging 


#loop to open up all the sheets, assign them names, and clean columns 
for(sheetname in sheetNamesPheno00122)

	{
	sheet <- paste("phenology", sheetname, sep = "") 
	phenSheetn <- readWorksheet(phenology0012, sheet = sheetname, 
		startRow = 0, endRow = 100, startCol = 0, endCol = 0)

	phenSheetn$Year <- sheetname # make a year column 
	phenology0012List[[sheet]] <- phenSheetn
}


# the error is because there are some cells in excel that are conveted to nas

#2009 has no verasion data 
#2008 seems to have extra verasion data that i dont understand 
#2006 has picking date rather than verasion date
#2005 and 2004 have 50% verasion date rather than 80% verasion date, and have pick date  
#2001 has 50% verasion   
#no 2000 data

#remove "total" rows

for (yearData in sheetNamesPheno00122){
	#yearData <- "2008"

	sheet <- paste("phenology", yearData, sep = "") 
	phenSheeti <- phenology0012List[[sheet]]

	#remove "total" rows 
	if("Total" %in% phenSheeti$Variety){
		totalRowStart <- min(rownames (phenSheeti[phenSheeti$Variety == "Total",]))
		phenSheeti <- phenSheeti[-(totalRowStart:nrow(phenSheeti)),]
	}

	#matching all the columns - i do thsi in teh loop because sometimes sheets have the same mistake 
	#there are sometimes two columns called average berry weight in teh same year 
	colnames(phenSheeti)[colnames(phenSheeti)== "Vineyard."] <- "Vineyard"

	colnames(phenSheeti)[colnames(phenSheeti)== "Row."] <- "Row"
	colnames(phenSheeti)[colnames(phenSheeti)== "Row_spacing"] <- "Row.Spacing"
	colnames(phenSheeti)[colnames(phenSheeti)== "Row.Spacing.1"] <- "Row.Spacing"

	colnames(phenSheeti)[colnames(phenSheeti)== "Av.BerryWeight.Grams"] <- "Av.Berry.Weight.Grams"
	colnames(phenSheeti)[colnames(phenSheeti)== "Av.BerryWeight.Grams.1"] <- "Av.Berry.Weight.Grams.1"
	colnames(phenSheeti)[colnames(phenSheeti)== "Berry.weight"] <- "Av.Berry.Weight.Grams"

	colnames(phenSheeti)[colnames(phenSheeti)== "Av.BunchWeight.Grams"] <- "Av.Bunch.Weight.Grams"
	colnames(phenSheeti)[colnames(phenSheeti)== "Av.BunchWeight.Grams.1"] <- "Av.Bunch.Weight.Grams.1"
	colnames(phenSheeti)[colnames(phenSheeti)== "Bunch.weight"] <- "Av.Bunch.Weight.Grams"


	colnames(phenSheeti)[colnames(phenSheeti)== "Clusters"] <- "Clust.per.Vine"
	colnames(phenSheeti)[colnames(phenSheeti)== "Clust.per.2Vines"] <- "Clust.per.Vine"	

	colnames(phenSheeti)[colnames(phenSheeti)== "Berries"] <- "Berries.per.Cluster"
	colnames(phenSheeti)[colnames(phenSheeti)== "Berries.per.Clust"] <- "Berries.per.Cluster"	

	colnames(phenSheeti)[colnames(phenSheeti)== "Lag"] <- "Lag.Phase.G.Vine"
	colnames(phenSheeti)[colnames(phenSheeti)== "Lag.date"] <- "Lag.Phase.Date"		
	colnames(phenSheeti)[colnames(phenSheeti)== "Lag.Phase.Date.1"] <- "Lag.Phase.Date"		
	
	colnames(phenSheeti)[colnames(phenSheeti)== "Lag.estimate."] <- "Lag.Phase.Date"	

	colnames(phenSheeti)[colnames(phenSheeti)== "Lag.est"] <- "Lag.Estimate.t"
	colnames(phenSheeti)[colnames(phenSheeti)== "Lag.estimate.t"] <- "Lag.Estimate.t"	

	colnames(phenSheeti)[colnames(phenSheeti)== "Veraison.Estimate..Berry."] <- "Berry.Veraison.Estimate.Kg"

	colnames(phenSheeti)[colnames(phenSheeti)== "Bunch.Veraison.Estimate"] <- "Bunch.Veraison.Estimate.Kg"
	colnames(phenSheeti)[colnames(phenSheeti)== "Veraison.Estimate..Bunch."] <- "Bunch.Veraison.Estimate.Kg"
	colnames(phenSheeti)[colnames(phenSheeti)== "Bunch.Veraison.Estimate.Kg.1"] <- "Bunch.Veraison.Estimate.Kg"
		#i am not sure about the berry and bunch verasion columns, because of the "...1" appearing on the end 

	colnames(phenSheeti)[colnames(phenSheeti)== "Lag.Phase.G.2vines"] <- "Lag.Phase.G.Vine"	
	colnames(phenSheeti)[colnames(phenSheeti)== "LagWeight.Grams"] <- "Lag.Phase.G.Vine"

	colnames(phenSheeti)[colnames(phenSheeti)== "X80.BloomDate"] <- "Flowering"
	colnames(phenSheeti)[colnames(phenSheeti)== "X80.Flowering.Date"] <- "Flowering"		

	colnames(phenSheeti)[colnames(phenSheeti)== "Yr.Planted"] <- "Planted"


	colnames(phenSheeti)[colnames(phenSheeti)== "X50.Veraison"] <- "X50.Veraison.Date"


	#add year column 
	phenSheeti$Year <- yearData

	

	phenology0012ListCh[[sheet]] <- data.frame(lapply(phenSheeti, as.character))

}

#combine the years into a single dataset where all the data are in character format
phen0012AllCh <- data.table::rbindlist(phenology0012ListCh, fill = TRUE)
phen0012AllCh$Year <- as.numeric(phen0012AllCh$Year )

head(phen0012AllCh[order(phen0012AllCh$Year)])
phen0012AllCh <- data.table(phen0012AllCh %>%
  mutate_all(as.character) )

phen0012AllCh$Vineyard <- NULL # this column is only used for totals that we will remove anyway
phen0012AllCh$Planted # we need to decide what to do with years with a slash in them 
phen0012AllCh$Acres <- as.numeric(phen0012AllCh$Acres)

#clean flowering dates 
#need to decide what to do with time slots

#lag dates
phen0012AllCh$Lag.Phase.Date  
longLagDates <- phen0012AllCh$Lag.Phase.Date [grep(" 00:00:00", phen0012AllCh$Lag.Phase.Date)]
phen0012AllCh$Lag.Phase.Date  [phen0012AllCh$Lag.Phase.Date   %in% longLagDates] <- gsub(
	" 00:00:00", "", phen0012AllCh$Lag.Phase.Date)
phen0012AllCh$Lag.Phase.Date <- as.Date(phen0012AllCh$Lag.Phase.Date,  "%Y-%m-%d")

#lag phase gram

phen0012AllCh$Lag.Phase.G.Vine <- gsub(" g", "", phen0012AllCh$Lag.Phase.G.Vine)
phen0012AllCh$Lag.Phase.G.Vine <- gsub("g", "", phen0012AllCh$Lag.Phase.G.Vine)
phen0012AllCh$Lag.Phase.G.Vine [phen0012AllCh$Lag.Phase.G.Vine == "na"] <- NA
phen0012AllCh$Lag.Phase.G.Vine  <- as.numeric(phen0012AllCh$Lag.Phase.G.Vine )

#50% verasion date - need to decide what to do with date ranges 
phen0012AllCh$X50.Veraison.Date 

#pick weight
phen0012AllCh$Pick.Weight.Grams <- as.numeric(phen0012AllCh$Pick.Weight.Grams)

#Year 
phen0012AllCh$Year <- as.numeric(phen0012AllCh$Year)

#Plants 
phen0012AllCh$Plants <- as.numeric(phen0012AllCh$Plants)

#Row SPacing ok as character

#Budburts - need to decide what to do with date ranges 
phen0012AllCh$Budburst

#cluster per vine
phen0012AllCh$Clust.per.Vine <- as.numeric(phen0012AllCh$Clust.per.Vine) 



#pick date
phen0012AllCh$Pick.Date <- as.Date(phen0012AllCh$Pick.Date,  "%Y-%m-%d")


str(phen0012AllCh)









#clean the column names - notes for completing the cleaning 

phenology0012ListCh <- as.character(phenology0012List)
nrow(phenology0012List$phenology2012)
str(phenology0012List)

#cleaning 2012 
#----------------------
phenology2012data <- phenology0012List$phenology2012
str(phenology2012data) 

phenology2012data$Budburst <- as.Date(phenology2012data$Budburst, "%Y-%m-%d %p")
phenology2012data$Lag.Phase.Date <- as.Date()


#cleaing the flowering date column because two dates were incorrectly inputed into the original spreadsheet 
oddDates <-phenology2012data$Flowering[grep("_", phenology2012data$Flowering)]
extraFloweringDates <- as.Date(phenology2012data$Flowering [phenology2012data$Flowering %in% oddDates] ,"%d_%b")
phenology2012data$Flowering [phenology2012data$Flowering %in% oddDates] <- as.character(extraFloweringDates)
phenology2012data$Flowering [!phenology2012data$Flowering %in% oddDates] <- gsub(
	" 00:00:00", "", phenology2012data$Flowering [!phenology2012data$Flowering %in% oddDates])
phenology2012data$Flowering <- as.Date(phenology2012data$Flowering, "%Y-%m-%d")


#cleaning 2011
#-------------

phenology2011data <- phenology0012List$phenology2011
str(phenology2011data)

#budburts dates 
oddDates11 <- phenology2011data$Budburst[grep("--", phenology2011data$Budburst)]
BudburstDate11 <- as.Date(phenology2011data$Budburst[phenology2011data$Budburst %in%oddDates11], "%d--%b")
phenology2011data$Budburst [phenology2011data$Budburst %in% oddDates] <- as.character(BudburstDate11 )
phenology2011data$Budburst [!phenology2011data$Budburst %in% oddDates11] <- gsub(
	" 00:00:00", "", phenology2011data$Budburst [!phenology2011data$Budburst %in% oddDates11])
phenology2011data$Budburst <- as.Date(phenology2011data$Budburst, "%Y-%m-%d")

#flowering dates 
phenology2011data$Flowering <- dplyr::na_if(phenology2011data$Flowering,"?")
phenology2011data$Flowering  <- as.Date(phenology2011data$Flowering , "%Y-%m-%d")

#Cluster per vine
phenology2011data$Clust.per.Vine <- as.numeric(phenology2011data$Clust.per.Vine )

#cleaning 2010









