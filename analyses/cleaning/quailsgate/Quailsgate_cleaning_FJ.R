# Faith attempt to clean quailsgate data. 
#I am going to focus on the 2000-2012 data to to begin with.
#Code started Jan 6th, 2020

#still a wokrk in pocess!

rm(list = ls())
options(stringsAsFactors = FALSE)

#libraries
library(data.table) # for binding lists together
library(dplyr)
library(readr) # getting numbers from strings 
library(tidyr) # for reshaping data

#read in spreadsheets
#--------------------------------------------------

#phenology data 2001-2012
setwd("/home/faith/Documents/github/bcvin/analyses/input/quailsgate/phenologicaldata")
sheetNames <- list.files()#check what files are present 

#drop 2012 David sheet because i dont know what it is, but it doesnt seem to have phenology data?
sheetNames <- sheetNames [!sheetNames == "David2012_PhenologicalData.csv"] 
sheetNames <- sheetNames[!sheetNames == "PhenologyData2001to2012.csv"]
sheetNameList <- gsub(".csv", "", sheetNames)

#make an empty list to hold the data 
phenology0012List <- list()
phenology0012ListCh <- list() # this one is all characters to help with merging 


#read in all the year's spreadsheets containing phenology data 
#loop to open up all the sheets, assign them names, and add them too the list 
for(sheetname in sheetNames)

	{
	nameList <- gsub(".csv", "", sheetname)
	phenology0012List[[nameList]] <- read.csv(sheetname)
}

str(phenology0012List)


#2009 has no verasion data 
#2008 seems to have extra verasion data that i dont understand 
#2006 has picking date rather than verasion date
#2005 and 2004 have 50% verasion date rather than 80% verasion date, and have pick date  
#2001 has 50% verasion   
#no 2000 data

#remove "total" rows


for (yearData in sheetNameList){
	#yearData <- "2008"

	phenSheeti <- phenology0012List[[yearData]]

	#remove "total" rows 
	if("Total" %in% phenSheeti$Variety){
		totalRowStart <- min(rownames (phenSheeti[phenSheeti$Variety == "Total",]))
		phenSheeti <- phenSheeti[-(totalRowStart:nrow(phenSheeti)),]
	}

	#matching all the columns - i do thsi in the loop because sometimes sheets have the same mistake 
	#there are sometimes two columns called average berry weight in teh same year 

	#2008 had two different columns for average bunch and berry weights. I dont know what the difference is. 

	colnames(phenSheeti)[colnames(phenSheeti)== "Vineyard."] <- "Vineyard"

	colnames(phenSheeti)[colnames(phenSheeti)== "Row."] <- "Row"
	colnames(phenSheeti)[colnames(phenSheeti)== "Row_spacing"] <- "Row.Spacing"
	colnames(phenSheeti)[colnames(phenSheeti)== "Row.Spacing.1"] <- "Row.Spacing"

	colnames(phenSheeti)[colnames(phenSheeti)== "Av.BerryWeight.Grams"] <- "Av.Berry.Weight.Grams"
	colnames(phenSheeti)[colnames(phenSheeti)== "Av.BerryWeight.Grams.1"] <- "Av.Berry.Weight.Grams.1" # this gets used for 2008 data
	colnames(phenSheeti)[colnames(phenSheeti)== "Berry.weight"] <- "Av.Berry.Weight.Grams"

	colnames(phenSheeti)[colnames(phenSheeti)== "Av.BunchWeight.Grams"] <- "Av.Bunch.Weight.Grams"
	colnames(phenSheeti)[colnames(phenSheeti)== "Av.BunchWeight.Grams.1"] <- "Av.Bunch.Weight.Grams.1"# this gets used for 2008 data
	colnames(phenSheeti)[colnames(phenSheeti)== "Bunch.weight"] <- "Av.Bunch.Weight.Grams"

	colnames(phenSheeti)[colnames(phenSheeti)== "Clusters"] <- "Clust.per.Vine"
	colnames(phenSheeti)[colnames(phenSheeti)== "Clust.per.2Vines"] <- "Clust.per.Vine"	#I dont know why 2003 has 2/vines, but assume it is the same as vines. 

	colnames(phenSheeti)[colnames(phenSheeti)== "Berries"] <- "Berries.per.Cluster"
	colnames(phenSheeti)[colnames(phenSheeti)== "Berries.per.Clust"] <- "Berries.per.Cluster"	

	colnames(phenSheeti)[colnames(phenSheeti)== "Lag"] <- "Lag.Phase.G.Vine"
	colnames(phenSheeti)[colnames(phenSheeti)== "Lag.date"] <- "Lag.Phase.Date"		
	colnames(phenSheeti)[colnames(phenSheeti)== "Lag.Phase.Date.1"] <- "Lag.Phase.Date"		
	
	colnames(phenSheeti)[colnames(phenSheeti)== "Lag.estimate."] <- "Lag.Phase.Date"	

	colnames(phenSheeti)[colnames(phenSheeti)== "Lag.est"] <- "Lag.Estimate.t"
	colnames(phenSheeti)[colnames(phenSheeti)== "Lag.estimate.t"] <- "Lag.Estimate.t"	
	colnames(phenSheeti)[colnames(phenSheeti)== "Lag.estimate"] <- "Lag.Estimate.t"	

	colnames(phenSheeti)[colnames(phenSheeti)== "Veraison.Estimate..Berry."] <- "Berry.Veraison.Estimate.Kg"

	colnames(phenSheeti)[colnames(phenSheeti)== "Bunch.Veraison.Estimate"] <- "Bunch.Veraison.Estimate.Kg"
	colnames(phenSheeti)[colnames(phenSheeti)== "Veraison.Estimate..Bunch."] <- "Bunch.Veraison.Estimate.Kg"
	colnames(phenSheeti)[colnames(phenSheeti)== "Bunch.Veraison.Estimate.Kg.1"] <- "Bunch.Veraison.Estimate.Kg"
		#i am not sure about the berry and bunch verasion columns, because of the "...1" appearing on the end 

	colnames(phenSheeti)[colnames(phenSheeti)== "Lag.Phase.G.2vines"] <- "Lag.Phase.G.Vine"	
	colnames(phenSheeti)[colnames(phenSheeti)== "LagWeight.Grams"] <- "Lag.Phase.G.Vine"

	colnames(phenSheeti)[colnames(phenSheeti)== "X80.BloomDate"] <- "Flowering" #maybe we shoudl keep the 80%? Or do they always use 80%? 
	colnames(phenSheeti)[colnames(phenSheeti)== "X80.Flowering.Date"] <- "Flowering"		

	colnames(phenSheeti)[colnames(phenSheeti)== "Yr.Planted"] <- "Planted"


	colnames(phenSheeti)[colnames(phenSheeti)== "X50.Veraison"] <- "X50.Veraison.Date"


	#add year column 
	phenSheeti$Year <- gsub("_PhenologicalData", "", yearData)

	
	phenology0012ListCh[[yearData]] <- data.frame(lapply(phenSheeti, as.character))

}

#combine the years into a single dataset where all the data are in character format
phen0012AllCh <- data.table::rbindlist(phenology0012ListCh, fill = TRUE)
phen0012AllCh$Year <- as.numeric(phen0012AllCh$Year )


head(phen0012AllCh[order(phen0012AllCh$Year)])
phen0012AllCh <- data.table(phen0012AllCh %>%
  mutate_all(as.character) )

phen0012AllCh$Vineyard <- NULL # this column is only used for totals that we will remove anyway

#date planted 
#make a comments column 
phen0012AllCh$CommentsPlanted <- NA #make a comments column 
phen0012AllCh$Planted # we need to decide what to do with years with a slash in them 
phen0012AllCh$CommentsPlanted[phen0012AllCh$Planted %like% "/"] <- paste("dates planted range ", 
	phen0012AllCh$Planted[phen0012AllCh$Planted %like% "/"], sep = " ") # add thsi infor to comments section 
phen0012AllCh$Planted <- gsub("\\/..", "", phen0012AllCh$Planted)
phen0012AllCh$Planted <- as.numeric(phen0012AllCh$Planted)

#Acres
phen0012AllCh$Acres <- as.numeric(phen0012AllCh$Acres)

#clean flowering dates 
#need to decide what to do with time slots
phen0012AllCh$Flowering
phen0012AllCh$CommentsFlowering <- paste("original flowering date was" , phen0012AllCh$Flowering, sep = " ")
phen0012AllCh$Flowering[phen0012AllCh$Flowering %in% c("?", "n/a", "na", "Grafts")] <- NA
phen0012AllCh$Flowering2 <- gsub("-Jun", " June", phen0012AllCh$Flowering)
phen0012AllCh$Flowering2 <- gsub("-May", " May", phen0012AllCh$Flowering2)
phen0012AllCh$Flowering2 <- gsub("-Apr", " April", phen0012AllCh$Flowering2)
phen0012AllCh$Flowering2 <- gsub(" Apr", " April", phen0012AllCh$Flowering2)
phen0012AllCh$Flowering2 <- gsub("Jun-", "June ", phen0012AllCh$Flowering2)
phen0012AllCh$Flowering2 <- gsub("_Jun", " June", phen0012AllCh$Flowering2)
phen0012AllCh$Flowering2 <- gsub("Jun", "June", phen0012AllCh$Flowering2)
phen0012AllCh$Flowering2 <- gsub("ee", "e", phen0012AllCh$Flowering2)
phen0012AllCh$Flowering2 <- gsub("-Jul", " July", phen0012AllCh$Flowering2)
phen0012AllCh$Flowering2 <- gsub("Jul-", "July ", phen0012AllCh$Flowering2)
phen0012AllCh$Flowering2 <- gsub("\\-..", "", phen0012AllCh$Flowering2)
phen0012AllCh$Flowering2 <- gsub("j", "J", phen0012AllCh$Flowering2)
phen0012AllCh$Flowering2 <- gsub("^7J ", "7 J", phen0012AllCh$Flowering2)
phen0012AllCh$Flowering2 <- gsub("(July+)+(0[0-9])", "\\2,\\1" , phen0012AllCh$Flowering2)
phen0012AllCh$Flowering2 <- gsub("(^[[:alpha:]]+) ([0-9][0-9])", '\\2 \\1',  phen0012AllCh$Flowering2)

floweringDate <- readr::parse_number(phen0012AllCh$Flowering2) # select just teh numbers
floweringMonth <- gsub("[[:digit:]]| ", "", phen0012AllCh$Flowering2) # select just the months
FloweringDateNoYear <- paste(floweringMonth, floweringDate, sep = "-")#make up a string of numbers for the date
FloweringDateYear <- paste(phen0012AllCh$Year, FloweringDateNoYear, sep = "-")
FloweringDateYear [FloweringDateYear  %in% c("-NA", "NA-NA")] <- NA
phen0012AllCh$Flowering <- as.Date(FloweringDateYear, format = "%Y-%B-%d" )#convert to as.date format

#lag dates - just a simple conversion to dates 
phen0012AllCh$Lag.Phase.Date  
longLagDates <- paste(phen0012AllCh$Year, phen0012AllCh$Lag.Phase.Date, sep = "-" )
phen0012AllCh$Lag.Phase.Date <- as.Date(longLagDates,  "%Y-%d-%b")

#lag phase gram

phen0012AllCh$Lag.Phase.G.Vine <- gsub(" g", "", phen0012AllCh$Lag.Phase.G.Vine)
phen0012AllCh$Lag.Phase.G.Vine <- gsub("g", "", phen0012AllCh$Lag.Phase.G.Vine)
phen0012AllCh$Lag.Phase.G.Vine [phen0012AllCh$Lag.Phase.G.Vine == "na"] <- NA
phen0012AllCh$Lag.Phase.G.Vine  <- as.numeric(phen0012AllCh$Lag.Phase.G.Vine )

#50% verasion date - need modeify date ranges 
phen0012AllCh$X50.Veraison.Date 
phen0012AllCh$CommentsVerasion <- paste("original verasion date is ", phen0012AllCh$X50.Veraison.Date, sep = "")
phen0012AllCh$X50.Veraison.Date <- gsub("^Aug-", "Aug ", phen0012AllCh$X50.Veraison.Date )
phen0012AllCh$X50.Veraison.Date <- gsub("^Sep-", "Sep ", phen0012AllCh$X50.Veraison.Date )
phen0012AllCh$X50.Veraison.Date <- gsub("-Sep", " Sep", phen0012AllCh$X50.Veraison.Date )
phen0012AllCh$X50.Veraison.Date <- gsub("-Aug", " Aug", phen0012AllCh$X50.Veraison.Date )
phen0012AllCh$X50.Veraison.Date <- gsub("\\-..", "", phen0012AllCh$X50.Veraison.Date )

verasionDate <- readr::parse_number(phen0012AllCh$X50.Veraison.Date) # select just teh numbers
verasionMonth <- gsub("[[:digit:]]| ", "", phen0012AllCh$X50.Veraison.Date) # select just the months
verasionDateNoYear <- paste(verasionMonth, verasionDate, sep = "-")
phen0012AllCh$X50.Veraison.Date <- paste(phen0012AllCh$Year, verasionDateNoYear, sep = "-")
phen0012AllCh$X50.Veraison.Date <- as.Date(phen0012AllCh$X50.Veraison.Date, format = "%Y-%b-%d")

#pick weight
phen0012AllCh$Pick.Weight.Grams <- as.numeric(phen0012AllCh$Pick.Weight.Grams)

#Year - already clean 

#Plants 
phen0012AllCh$Plants <- as.numeric(phen0012AllCh$Plants)

#Row SPacing ok as character

#Budburts - need to decide what to do with date ranges 
phen0012AllCh$Budburst
phen0012AllCh$CommentsBudburst <- paste("original budburst date is ", phen0012AllCh$Budburst, sep = "")
budburst <- phen0012AllCh$Budburst
budburst[budburst %in% c("?", "na")] <- NA
budburst <- gsub("Apr-" , "Apr ", budburst)
budburst <- gsub("May-" , "May ", budburst)
budburst[budburst == "Apr30May1"] <- "Apr 30"
budburst <- gsub("-Apr" , " Apr", budburst)
budburst <- gsub("-May" , " May", budburst)
budburst <- gsub("\\-..", "", budburst)
budburst[budburst == "29Apr"] <- "Apr 29"
budburst[budburst == "4May"] <- "May 4"

budburstDates <- readr::parse_number(budburst)
budburstMonths <-  gsub("[[:digit:]]| ", "", budburst)
budBusrtDatenoYear <- paste(budburstDates, budburstMonths, sep = "-")
yearBudburst <- paste(phen0012AllCh$Year, budBusrtDatenoYear, sep = "-")
phen0012AllCh$Budburst <- as.Date(yearBudburst, format = "%Y-%d-%b")

#cluster per vine
phen0012AllCh$Clust.per.Vine <- as.numeric(phen0012AllCh$Clust.per.Vine) 

#pick date
pickDate <- paste(phen0012AllCh$Year, phen0012AllCh$Pick.Date, sep = "-" )
phen0012AllCh$Pick.Date <- as.Date(pickDate ,  "%Y-%d-%b")

#spurs.per.block
phen0012AllCh$Spurs.per.Block

#berries per cluster 
phen0012AllCh$Clust.per.Vine <- as.numeric(phen0012AllCh$Clust.per.Vine)

#Row
phen0012AllCh$Row <- as.numeric(phen0012AllCh$Row)

#Av.Berry.Weight.Grams
berryWeight <- phen0012AllCh$Av.Berry.Weight.Grams
berryWeight <- gsub(" g", "", berryWeight)
berryWeight <- sub("^\\.", "0.", berryWeight)
phen0012AllCh$Av.Berry.Weight.Grams <- as.numeric(berryWeight)

#80% verasion date
verasionDate80 <- phen0012AllCh$X80.Veraison.Date
verasionDate80Year <- paste(phen0012AllCh$Year, verasionDate80 , sep = "-")
phen0012AllCh$X80.Veraison.Date <- as.Date(verasionDate80Year, format = "%Y-%d-%b")

#Av.Berry.Weight.Grams.1
phen0012AllCh$Av.Berry.Weight.Grams.1 <- as.numeric(phen0012AllCh$Av.Berry.Weight.Grams.1 )

#Av.Bunch.Weight.Grams.1
phen0012AllCh$Av.Bunch.Weight.Grams.1 <- as.numeric(phen0012AllCh$Av.Bunch.Weight.Grams.1 )

#Lag.Estimate.t
phen0012AllCh$Lag.Estimate.t <- as.numeric(phen0012AllCh$Lag.Estimate.t )

#Berry.Veraison.Estimate.Kg
phen0012AllCh$Berry.Veraison.Estimate.Kg <- as.numeric(phen0012AllCh$Berry.Veraison.Estimate.Kg )

#Bunch.Veraison.Estimate.Kg
phen0012AllCh$Bunch.Veraison.Estimate.Kg <- as.numeric(phen0012AllCh$Bunch.Veraison.Estimate.Kg )

#Bunch.Veraison.Estimate.Kg.1
phen0012AllCh$Bunch.Veraison.Estimate.Kg.1 <- as.numeric(phen0012AllCh$Bunch.Veraison.Estimate.Kg.1 )

#X2007.Actual
phen0012AllCh$X2007.Actual <- as.numeric(phen0012AllCh$X2007.Actual)

#Percentage
phen0012AllCh$Percentage <- as.numeric(phen0012AllCh$Percentage)

#Notes - these are fine

#Veraison.Estimate
phen0012AllCh$Veraison.Estimate <- as.numeric(phen0012AllCh$Veraison.Estimate)

#X2008.Actual
phen0012AllCh$X2008.Actual <- as.numeric(phen0012AllCh$X2008.Actual)

head(phen0012AllCh)


#make a column for phenology date and one for phenology event
#------------------------------------

phenologyColumns <- c("Flowering", "Lag.Phase.Date", "X50.Veraison.Date", "X80.Veraison.Date", "Budburst", "Pick.Date")

#remove columns i just used for teh cleaning process
phen0012AllCh$Flowering2 <- NULL

PhenologyData<- phen0012AllCh %>%
	gather(key = "phenologyEvent", value = "phenologyDate", phenologyColumns, na.rm = TRUE)

#remove the comments columns for now
PhenologyData$CommentsBudburst <- NULL
PhenologyData$CommentsPlanted <- NULL
PhenologyData$CommentsVerasion <- NULL
PhenologyData$CommentsFlowering	<- NULL

head(PhenologyData)
write.csv( PhenologyData, "/home/faith/Documents/github/bcvin/analyses/cleaning/quailsgate/PhenologyData2001to2012.csv")

#Cleaning teh newer data 
#
#-------------------------------------------------------

pheno1216Data2 <- read.csv("/home/faith/Documents/github/bcvin/analyses/input/quailsgate/qg_PhenoDataReport_2012-2016.csv", skip = 2)
#this code remove the first 2 rows 

#Remove empty rows 
pheno1216Data3 <- pheno1216Data2[!pheno1216Data2$Location...Sub == "",]
pheno1216Data <- pheno1216Data3[!pheno1216Data3$Location...Sub =="Q G ~ Q   u   a   i   l   s       G   a   t   e       E   s   t   a   t   e       W   i   n   e   r   y   .",]
head(pheno1216Data)

#Split the vineyard name from the blocks

vinetardAndCode <- lapply(strsplit(as.character(pheno1216Data$Location...Sub), "/") , "[" , 1)
pheno1216Data$VineyardCode <- lapply(strsplit(as.character(vinetardAndCode), " - ") , "[" , 1)
pheno1216Data$Vineyard <- lapply(strsplit(as.character(vinetardAndCode), " - ") , "[" , 2)

pheno1216Data$blocks<- lapply(strsplit(pheno1216Data$Location...Sub, "/") , "[" , 2)

#make a function or loop that goes though each block value, sees how many blocks there are,
#makes a new row for each block, and checks the right phenology data is in the right cell 