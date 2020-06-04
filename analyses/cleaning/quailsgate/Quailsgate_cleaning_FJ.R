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
library(stringr) # for detecting strings of characters (/n in my case)

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

	#remove "total" and "T-bar" rows 
	phenSheeti <- phenSheeti[!phenSheeti$Variety == "Total",]
	phenSheeti <- phenSheeti[!phenSheeti$Variety == "T-bar",]
	
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

	#get Vineyard names 
	tail(phenSheeti)

	#There are Osoyoos blocks thare are unhelpfully labeles. I looked at VineyardMapsDataCompiled.csv to fix this
	#these bloks are only in the 2006 data onwardd. Before that I assume it is just the main vineyard data 

		phenSheeti$VineyardCode <- NA

	if("Vineyard" %in% colnames(phenSheeti) & "PAT" %in% phenSheeti$Field){
		osRow1<- which(phenSheeti$Field == "Osy") #rown number where Osoyoos vineyards blocks start 
		patRow <- which(phenSheeti$Field == "PAT") # Patrichia????
		mrtRow <- which(phenSheeti$Field == "MRT")#I assume Martyna Vineyard 
		manRow <- which(phenSheeti$Vineyard == "Mannhardt")

		phenSheeti$Vineyard[1:(osRow-1)] <- "Quails Gate Vineyard" # I assume most blocks are the main site vineyard
		phenSheeti$VineyardCode[1:(osRow-1)] <- "QGV"

		phenSheeti$Vineyard[osRow1:(patRow - 1)] <- "Osoyoos Vineyard"
		phenSheeti$VineyardCode[osRow1:(patRow - 1)] <- "OSY"

		phenSheeti$Vineyard[patRow] <- "Patricia Vineyard"
		phenSheeti$VineyardCode[patRow] <- "PAT"

		phenSheeti$Vineyard[mrtRow:(manRow[1] - 1)] <- "Martyna Vineyard"
		phenSheeti$VineyardCode[mrtRow:(manRow[1] - 1)] <- "MRT"

		phenSheeti$Vineyard[manRow] <- "Mannhardt"
		phenSheeti$VineyardCode[manRow] <- "MAN"
	

	} else if("Vineyard" %in% colnames(phenSheeti) & !"East" %in% phenSheeti$Field & !"PAT" %in% phenSheeti$Field){
		osRow <- which(phenSheeti$Field == "Osy")
		manRow <- which(phenSheeti$Vineyard == "Mannhardt")

		phenSheeti$Vineyard[1:(osRow-1)] <- "Quails Gate Vineyard" # I assume most blocks are the main site vineyard
		phenSheeti$VineyardCode[1:(osRow-1)] <- "QGV"

		phenSheeti$Vineyard[osRow:(manRow[1]-1)] <- "Osoyoos Vineyard" # I assume most blocks are the main site vineyard
		phenSheeti$VineyardCode[osRow:(manRow[1]-1)] <- "OSY"

				phenSheeti$Vineyard[manRow] <- "Mannhardt"
		phenSheeti$VineyardCode[manRow] <- "MAN"

	} else if("Vineyard" %in% colnames(phenSheeti) & "East" %in% phenSheeti$Field){
		ekRow <- which(phenSheeti$Field == "East") # the row with East Kelowna Vineyard
		osRow <- which(phenSheeti$Field == "Osy")
		manRow <- which(phenSheeti$Vineyard == "Mannhardt-Sunnyside")

		phenSheeti$Vineyard[1:(ekRow-1)] <- "Quails Gate Vineyard" # 
		phenSheeti$VineyardCode[1:(ekRow-1)] <- "QGV"

		phenSheeti$Vineyard[ekRow:(osRow-1)] <- "East Kelowna Vineyard" #
		phenSheeti$VineyardCode[ekRow:(osRow-1)] <- "EKE"

		phenSheeti$Vineyard[osRow:(manRow[1] - 1)] <- "Osoyoos Vineyard" #
		phenSheeti$VineyardCode[osRow:(manRow[1]-1)] <- "OSY"

		phenSheeti$Vineyard[manRow] <- "Mannhardt"
		phenSheeti$VineyardCode[manRow] <- "MAN"

	} else{

		phenSheeti$Vineyard <- NA

		phenSheeti$Vineyard <- "Quails Gate Vineyard" # I assume most blocks are the main site vineyard
		phenSheeti$VineyardCode <- "QGV"

	}

	phenology0012ListCh[[yearData]] <- data.frame(lapply(phenSheeti, as.character)) 
}

#combine the years into a single dataset where all the data are in character format
phen0012AllCh <- data.table::rbindlist(phenology0012ListCh, fill = TRUE)
phen0012AllCh$Year <- as.numeric(phen0012AllCh$Year )

head(phen0012AllCh[order(phen0012AllCh$Year)])
phen0012AllCh <- data.table(phen0012AllCh %>%
  mutate_all(as.character) )

#Adding Field block names for Oysyoos and Martyana vineyards based on the data scraped from maps (VineyardMapsDataCompiled.csv)
#no blocks, just Field IDs I think 
#----Osoyoos "Old Foch". 
phen0012AllCh$Field[phen0012AllCh$Vineyard == "Osoyoos" & phen0012AllCh$Variety == "Old Foch" ] <- 5 #planted 1978 so I assume is "old"
#----Osoyoons "Young Foch". Needs 3 rows i think because three blocks of Foch planted 2006-7
phen0012AllCh$Field[phen0012AllCh$Vineyard == "Osoyoos" & phen0012AllCh$Variety == "Young Foch" ] <- 1 #planted 2006-7 so I assume is "young"
FochRows <- do.call("rbind", replicate(2, phen0012AllCh[phen0012AllCh$Vineyard == "Osoyoos" & phen0012AllCh$Variety == "Young Foch" ], simplify = FALSE)) 
FochRows$Field <- rep(c(4, 6), each = 7)
phen0012AllCh <- rbind(phen0012AllCh, FochRows)
#----Osoyoos "Syrah"
phen0012AllCh$Field[phen0012AllCh$Vineyard == "Osoyoos" & phen0012AllCh$Variety == "Syrah" ] <- 2
#----Osoyoos "Chenin Blanc"
phen0012AllCh$Field[phen0012AllCh$Vineyard == "Osoyoos" & phen0012AllCh$Variety == "Chenin Blanc" ] <- 3

#----Martyna Riesling 
phen0012AllCh$Field[phen0012AllCh$Vineyard == "Martyna" & phen0012AllCh$Variety == "Riesling"] <-  1 # there are 3 Riesling blocks 
riesRows <- do.call("rbind", replicate(2, phen0012AllCh[phen0012AllCh$Vineyard == "Martyna" & phen0012AllCh$Variety == "Riesling" ], simplify = FALSE)) 
riesRows$Field <- rep(c(2, 4), each = 2)
phen0012AllCh <- rbind(phen0012AllCh, riesRows)
#-----Martyna Gewurtz
phen0012AllCh$Field[phen0012AllCh$Vineyard == "Martyna" & phen0012AllCh$Variety == "Gewurztraminer"] <-  2 # there are 2 Gewurtz blocks 
G2 <- phen0012AllCh[phen0012AllCh$Vineyard == "Martyna" & phen0012AllCh$Variety == "Gewurztraminer",] 
G2$Field[G2$Vineyard == "Martyna" & G2$Variety == "Gewurztraminer"] <-  5
phen0012AllCh <- rbind(phen0012AllCh, G2)

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

PhenologyData <- phen0012AllCh %>%
	gather(key = "phenologyEvent", value = "phenologyDate", phenologyColumns, na.rm = TRUE)

#remove the comments columns for now
PhenologyData$CommentsBudburst <- NULL
PhenologyData$CommentsPlanted <- NULL
PhenologyData$CommentsVerasion <- NULL
PhenologyData$CommentsFlowering	<- NULL

head(PhenologyData)
trimws(PhenologyData$BlockID)
#write.csv( PhenologyData, "/home/faith/Documents/github/bcvin/analyses/cleaning/quailsgate/PhenologyData2001to2012.csv")

#Cleaning the newer data 
#
#-------------------------------------------------------

# remove the first 2 rows of th ephenology 2012-16 data because we dont want them  
pheno1216Data2 <- read.csv("/home/faith/Documents/github/bcvin/analyses/input/quailsgate/qg_PhenoDataReport_2012-2016.csv", skip = 2)

#this is data taken from maps, and has which blocks have which varieties planted. 
varietyInfo <- read.csv("/home/faith/Documents/github/bcvin/analyses/input/quailsgate/vineyardmaps/VineyardMapsDataCompiled.csv")

#removing a few empty columns
pheno1216Data2$Note.....................................Acres <- NULL
pheno1216Data2$X.2 <- NULL
pheno1216Data2$Type <- NULL
pheno1216Data2$X.4 <- NULL
pheno1216Data2$acres <- NULL # We can get this from the variety data

#Correct names - my best guess based on loking at the data sheet

names(pheno1216Data2)[c(2,4,5)] <- c("Type", "PhenoEvent", "Acres")


#Remove empty rows 
pheno1216Data3 <- pheno1216Data2[!pheno1216Data2$Location...Sub == "",]
pheno1216Data <- pheno1216Data3[!pheno1216Data3$Location...Sub =="Q G ~ Q   u   a   i   l   s       G   a   t   e       E   s   t   a   t   e       W   i   n   e   r   y   .",]
head(pheno1216Data)


#Split the vineyard name from the blocks

vinetardAndCode <- lapply(strsplit(as.character(pheno1216Data$Location...Sub), "/") , "[" , 1)
pheno1216Data$VineyardCode <- lapply(strsplit(as.character(vinetardAndCode), " - ") , "[" , 1)
pheno1216Data$Vineyard <- lapply(strsplit(as.character(vinetardAndCode), " - ") , "[" , 2)

pheno1216Data$blocks <- lapply(strsplit(pheno1216Data$Location...Sub, "/") , "[" , 2)

#strip white space from the columns
pheno1216Data$blocks <- trimws(pheno1216Data$blocks)
pheno1216Data$Vineyard <- trimws(pheno1216Data$Vineyard)
pheno1216Data$VineyardCode <- trimws(pheno1216Data$VineyardCode)

#Tidy the phenology column up a bit 
unique(pheno1216Data$PhenoEvent)
pheno1216Data$PhenoEvent[pheno1216Data$PhenoEvent %in% c("Full bloom", "FULL BLOOM")] <- "Full Bloom"
pheno1216Data$PhenoEvent[pheno1216Data$PhenoEvent == "Full bloom-Foch"] <- "Foch Full Bloom"
pheno1216Data$PhenoEvent[pheno1216Data$PhenoEvent %in% c("budbreak", "BudBreak", "Budbreak!!", "Budbreak")] <- "Budburst"

#Select easier to deal with rwos without complicated phenology values
phenDataOne <- pheno1216Data[!str_detect(pheno1216Data$PhenoEvent, "\n"),]
phenDataOne$BlockID <- NA
head(phenDataOne)
#make a function or loop that goes though each block value, sees how many blocks there are,
#makes a new row for each block

blockDataAll <- list() #make a list to hold rows 


for (n in 1:nrow(phenDataOne)){ #loop through all the phenology data that is simple is fix 
	rowData <- phenDataOne [n,]
	rowData$blocks
	splitBlocks <- unlist(strsplit(rowData$blocks,split=','))
	nBlock <- length(splitBlocks) # how many blocks are there?
	rowDataRep <- do.call("rbind", replicate(nBlock, rowData, simplify = FALSE)) #repeat row as many times as there are blocks

	for (nb in 1:nBlock) {
		rowDataRep$BlockID[nb]<- splitBlocks[nb] #put block names where they belong 
	}
	blockDataAll[[n]] <- rowDataRep
}

cleanBlocksOne <- rbindlist(blockDataAll)


#select phenology values with more than one phenology date in one row so I can correct this individually
#these are a bit trickier so I am going to go through each one seperatly instead of making a loop
#especially because the variety is sometimes used to say which date goes with which block, and variety data 
#is not in this spreadsheet. I had to look at VITICULTURE_SUMMARY.xls in the data folder to see which blocks
#have which varieties in them.
#I realise I will need to incude variety data as well in the final spreadsheet   

phenDataMTO <- pheno1216Data[str_detect(pheno1216Data$PhenoEvent, "\n"),]
phenDataMTO$BlockID <- NA
phenDataMTO$Comments <- NA 

#1st row - "Full Bloom\nSYRAH-Full Bloom June-02"
#block 2 is Syrah, block 3 is Chenin Blanc
PhenoR1 <- phenDataMTO[1,]
PhenoR1$blocks

rowData1 <- do.call("rbind", replicate(2, PhenoR1, simplify = FALSE)) #repeat row as many times as there are blocks
rowData1$BlockID [1] <- "B2" # Syrah
rowData1$BlockID [2] <-  "B3" #ChenB

rowData1$Comments <- rowData1$PhenoEvent # keep original value for reference 
rowData1$PhenoEvent <- "Full Bloom" # correct phenology description

rowData1$Date [1] <-  "2016-06-02" # add correct date to the Syrah (block B2) Data 

#2nd row - "Full Bloom\nSYRAH-Full Bloom June-04"
#block 2 is Syrah, block 3 is Chenin Blanc

PhenoR2 <- phenDataMTO[2,]
PhenoR2$blocks

rowData2 <- do.call("rbind", replicate(3, PhenoR2, simplify = FALSE)) #repeat row as many times as there are blocks
splitBlocks2 <- unlist(strsplit(rowData2$blocks[1],split=','))#get block names 

rowData2$BlockID [1] <- splitBlocks2[1] # Chardonnay, but has the different phenology date 
rowData2$BlockID [2] <- splitBlocks2[2] # Chardonnay
rowData2$BlockID [3] <- splitBlocks2[3] # Chardonnay

rowData2$Comments <- rowData2$PhenoEvent # save the info in case we need it 
rowData2$PhenoEvent <- "Full Bloom" # correct phenology description

rowData2$Date [1] <-  "2015-06-04" # add correct date to the Syrah (block B2) Data 


#3rd row - Chardonnay block F3B4 has a different date.
#I think F4 should be F3B4 because the other blocks are not Chardonnay and QGV doesnt use block IDs withonly a field number

PhenoR3 <- phenDataMTO[3,]
PhenoR3$blocks

rowData3 <- do.call("rbind", replicate(4, PhenoR3, simplify = FALSE)) #repeat row as many times as there are blocks
splitBlocks3 <- unlist(strsplit(rowData3$blocks[1],split=','))#get block names 

rowData3$BlockID [1] <- splitBlocks3[1] # 
rowData3$BlockID [2] <- splitBlocks3[2] # 
rowData3$BlockID [3] <- splitBlocks3[3] #
rowData3$BlockID [4] <- "F3B4" # Chardonnay? 

rowData3$Comments <- rowData3$PhenoEvent # save the info in case we need it 
rowData3$PhenoEvent <- "Full Bloom" # correct phenology description

rowData3$Date [4] <-  "2015-06-04" # add correct date to block F3B4 Chardonnay June 04 

#4th row - Pinot Noir block F5 B5 full bloom June 05
# I think this is spposed to be block "F5" in the block ID

PhenoR4 <- phenDataMTO[4,]
PhenoR4$blocks

rowData4 <- do.call("rbind", replicate(4, PhenoR4, simplify = FALSE)) #repeat row as many times as there are blocks
splitBlocks4 <- unlist(strsplit(rowData4$blocks[1],split=','))#get block names 

rowData4$BlockID [1] <- splitBlocks4[1] # 
rowData4$BlockID [2] <- splitBlocks4[2] # 
rowData4$BlockID [3] <- splitBlocks4[3] #
rowData4$BlockID [4] <- "F5B5" # Pinot Noir? 

rowData4$Comments <- rowData4$PhenoEvent # save the info in case we need it 
rowData4$PhenoEvent <- "Full Bloom" # correct phenology description

rowData4$Date [4] <-  "2015-06-05" # add correct date to the Syrah (block B2) Data 

#5th row
#I assume block "F" is  F1B7

PhenoR5 <- phenDataMTO[5,]
PhenoR5$blocks

rowData5 <- do.call("rbind", replicate(4, PhenoR5, simplify = FALSE)) #repeat row as many times as there are blocks
splitBlocks5 <- unlist(strsplit(rowData5$blocks[1],split=','))#get block names 

rowData5$BlockID [1] <- splitBlocks5[1] # 
rowData5$BlockID [2] <- splitBlocks5[2] # 
rowData5$BlockID [3] <- splitBlocks5[3] #
rowData5$BlockID [4] <- "F1B7" # Chasselas? 

rowData5$Comments <- rowData5$PhenoEvent # save the info in case we need it 
rowData5$PhenoEvent <- "Budburst" # correct phenology description

rowData5$Date [3:4] <-  "2015-04-24" #


#6th row
PhenoR6 <- phenDataMTO[6,]
PhenoR6$blocks

rowData6 <- do.call("rbind", replicate(3, PhenoR6, simplify = FALSE)) #repeat row as many times as there are blocks
splitBlocks6 <- unlist(strsplit(rowData6$blocks[1],split=','))#get block names 

rowData6$BlockID [1] <- splitBlocks6[1] # 2-6 Chenin Blanc
rowData6$BlockID [2] <- splitBlocks6[2] # 5-5 Pinto noir
rowData6$BlockID [3] <- splitBlocks6[3] # 5-6 Pinot noir 

rowData6$Comments <- rowData6$PhenoEvent # save the info in case we need it 
rowData6$PhenoEvent <- "Budburst" # correct phenology description

rowData6$Date [1] <-  "2015-04-13" # add correct date to the Syrah (block B2) Data 

#7th row
#this one was a bit odd because there are extra blocks mentioned in the phenology column, I think I sorted it ok
#by adding in the Riesling blocks and assuming the original date refers to the Gewurtzbudbreake
PhenoR7 <- phenDataMTO[7,]
PhenoR7$blocks

rowData7 <- do.call("rbind", replicate(5, PhenoR7, simplify = FALSE)) #repeat row as many times as there are blocks
splitBlocks7 <- unlist(strsplit(rowData7$blocks[1],split=','))#get block names 

rowData7$BlockID [1] <- splitBlocks7[1] # Gewurztraminer
rowData7$BlockID [2] <- splitBlocks7[2] # Gewurztraminer
rowData7$BlockID [3] <- "B1" # Riesilng
rowData7$BlockID [4] <- "B2" # Riesilng
rowData7$BlockID [5] <- "B4" # Riesilng 

rowData7$Comments <- rowData7$PhenoEvent # save the info in case we need it 
rowData7$PhenoEvent <- "Budburst" # correct phenology description

rowData7$Date [3:5] <-  "2015-04-20" # add correct date to the Syrah (block B2) Data 

#8th row
#
PhenoR8 <- phenDataMTO[8,]
PhenoR8$blocks

rowData8 <- do.call("rbind", replicate(2, PhenoR7, simplify = FALSE)) #repeat twice because another block is mentioned in the PhenoEVent column
 
rowData8$BlockID[1] <- PhenoR8$blocks # only one block mentioned in blocks column, other one in PhenoEvent
rowData8$BlockID[2] <- "B2" # Syrah

rowData8$Comments <- rowData8$PhenoEvent # save the info in case we need it 
rowData8$PhenoEvent <- "Budburst" # correct phenology description

rowData8$Date [2] <-  "2015-04-15" # add correct date to the Syrah (block B2) Data 

#Just run teh resto of the lines through the same loop I used on the easier to clean data 
NoExtraPhenoData <- phenDataMTO[9:nrow(phenDataMTO),]

blockDataAll2 <- list() #make a list to hold rows 


for (n in 1:nrow(NoExtraPhenoData)){ #loop through all the phenology data that is simple is fix 
	rowData <- NoExtraPhenoData [n,]
	rowData$blocks
	splitBlocks <- unlist(strsplit(rowData$blocks,split=','))
	nBlock <- length(splitBlocks) # how many blocks are there?
	rowDataRep <- do.call("rbind", replicate(nBlock, rowData, simplify = FALSE)) #repeat row as many times as there are blocks

	for (nb in 1:nBlock) {
		rowDataRep$BlockID[nb]<- splitBlocks[nb] #put block names where they belong 
	}
	blockDataAll2[[n]] <- rowDataRep
}

cleanBlocksOne2 <- rbindlist(blockDataAll2)

#Change yeild estimate to a better label

cleanBlocksOne2$Comments <- as.character(cleanBlocksOne2$Comments )#this has to happen for the code below to work 
cleanBlocksOne2[str_detect(cleanBlocksOne2$PhenoEvent, "clusters/vine"), "Comments"] <- cleanBlocksOne2[str_detect(cleanBlocksOne2$PhenoEvent, "clusters/vine"), "PhenoEvent"] 
cleanBlocksOne2$PhenoEvent[str_detect(cleanBlocksOne2$PhenoEvent, "clusters/vine")] <- "Yeild Estimate" 


#Bring the data together
#-------------------------

phenoData12to16 <- data.frame(rbind(cleanBlocksOne, cleanBlocksOne2,rowData1, rowData2, rowData3,
	rowData4, rowData5, rowData6,rowData7, fill=TRUE))

#Combine with variety Info
#------------------------------

head(varietyInfo)

#Get block codes to match the phenology data
varietyInfo$BlockID2 <- paste("F", varietyInfo$block, sep = "")
varietyInfo$BlockID <- gsub("-","B",varietyInfo$BlockID2)

#thsi vineyard has a different nameing convention 
varietyInfo$BlockID[varietyInfo$Vineyard == "Martyna Vineyard"] <- gsub("F", "B", varietyInfo$BlockID[varietyInfo$Vineyard == "Martyna Vineyard"]
)
 
varietyInfo$BlockID2 <- NULL#remove thsi now because I dont need it any more

#rename block column to avoid confusion 
names(varietyInfo)[3] <- "OriginalBlockName"

#Tidy vineyard column so both data sheets match
unique(varietyInfo$vineyard) 
unique(phenoData12to16$Vineyard)
varietyInfo$Vineyard <- as.factor(varietyInfo$vineyard)
levels(varietyInfo$Vineyard) <- c("Blue Fox Vineyard", "Mannhardt", "Martyna Vineyard", "Osoyoos Vineyard", "Quails Gate Vineyard")
varietyInfo$Vineyard <- as.character(varietyInfo$Vineyard)

#Merge varety info
phenoData12to16Var <- merge(phenoData12to16, varietyInfo, by = c("BlockID", "Vineyard"))

phenoData12to16Var$vineyard <- NULL #we dont need this now, Vineyard is better


#Merge the newer and older data together into a single data sheet
#--------------------------------------------------------------

head(phenoData12to16Var)
head(PhenologyData)

#Make sure important columns have matching column names 
names(phenoData12to16Var)
names(PhenologyData)

names(PhenologyData)[names(PhenologyData) == "phenologyEvent"] <- "PhenoEvent"
names(PhenologyData)[names(PhenologyData) == "Acres"] <- "acres"
names(phenoData12to16Var)[names(phenoData12to16Var) == "year"] <- "YearPlanted"
names(PhenologyData)[names(PhenologyData) == "Planted"] <- "YearPlanted"
names(phenoData12to16Var)[names(phenoData12to16Var) == "Comments"] <- "Notes"
names(phenoData12to16Var)[names(phenoData12to16Var) == "Date"] <- "phenologyDate"
names(phenoData12to16Var)[names(phenoData12to16Var) == "variety"] <- "Variety"
names(phenoData12to16Var)[names(phenoData12to16Var) == "Location...Sub"] <- "OriginalVineyardBlocks"


#combine field and block in older data 
PhenologyData$BlockID <- paste(PhenologyData$Field, PhenologyData$Block, sep = "")

#Remove superfluous columns
PhenologyData$X <- NULL #when data was taken from maps. Not needed for this.
PhenologyData$Field <- NULL
PhenologyData$Block <- NULL

phenoData12to16Var$date <- NULL #when data was taken from maps. Not needed for this.
phenoData12to16Var$Acres <- NULL # i am keeping data scraped from map
phenoData12to16Var$blocks <- NULL # i am keeping data scraped from map
phenoData12to16Var$Type <- NULL

#Add some columsn so rbinding is easier 
PhenologyData$rootstock <- NA
PhenologyData$clone <- NA
PhenologyData$OriginalVineyardBlocks <- NA
PhenologyData$OriginalBlockName <- NA
PhenologyData$Notes <- NA

str(PhenologyData)
str(phenoData12to16Var)

phenoData12to16Var$phenologyDate <- as.Date(phenoData12to16Var$phenologyDate, format = "%Y-%m-%d")

names(PhenologyData)[names(PhenologyData) %in% names(phenoData12to16Var)]


phenologQGAll <-  data.frame(rbind(PhenologyData,phenoData12to16Var, fill=TRUE)) 
rbindlist(list(PhenologyData,phenoData12to16Var), fill = TRUE)

phenologQGAll <- bind_rows(PhenologyData,phenoData12to16Var, .id = "df")
head(phenologQGAll)

#Checking all looks ok
unique(phenologQGAll$PhenoEvent)
unique(phenologQGAll$phenologyDate)
unique(phenologQGAll$Vineyard) # where is the Martyna data?


#remove "vineyard"
phenologQGAll$Vineyard <- gsub(" Vineyard", "", phenologQGAll$Vineyard)

phenologQGAll$df <- NULL # I just needed this to make the rbind work. 

#write out the table
#-------------------------------

write.csv(phenologQGAll, "/home/faith/Documents/github/bcvin/analyses/cleaning/quailsgate/quailsGatePhenology.csv")