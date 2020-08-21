############## Bcvin Brix Sourcing Script (PA) ##############

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Setting working directory
setwd("/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning")

#Setting path variable, this can be changed
dirpath <- "/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning/"

#Sourcing 2004-2018 cleaning scripts
source(paste(dirpath, "sebfarm_brix_clean2004.R", sep = ""))
source(paste(dirpath, "sebfarm_brix_clean2005.R", sep = ""))
source(paste(dirpath, "sebfarm_brix_clean2006.R", sep = ""))
source(paste(dirpath, "sebfarm_brix_clean2007.R", sep = ""))
source(paste(dirpath, "sebfarm_brix_clean2008.R", sep = ""))
source(paste(dirpath, "sebfarm_brix_clean2009.R", sep = ""))
source(paste(dirpath, "sebfarm_brix_clean2010.R", sep = ""))
source(paste(dirpath, "sebfarm_brix_clean2011.R", sep = ""))
source(paste(dirpath, "sebfarm_brix_clean2012.R", sep = ""))
source(paste(dirpath, "sebfarm_brix_clean2013.R", sep = ""))
source(paste(dirpath, "sebfarm_brix_clean2014.R", sep = ""))
source(paste(dirpath, "sebfarm_brix_clean2015.R", sep = ""))
source(paste(dirpath, "sebfarm_brix_clean2017.R", sep = ""))
source(paste(dirpath, "sebfarm_brix_clean2018.R", sep = ""))

#Binding all years into one dataframe
SebF_Brix <- rbind(SebF2004, SebF2005, SebF2006, SebF2007, SebF2008, SebF2009, SebF2010, SebF2011,
                   SebF2012, SebF2013, SebF2014, SebF2015, SebF2017, SebF2018)

#Removing empty rows and rows with NA
#SebF_Brix <- SebF_Brix[!(SebF_Brix$vineyard==""), ] #empty
SebF_Brix <- SebF_Brix[!(is.na(SebF_Brix$value)), ] #NA

#Removing empty rows
SebF_Brix <- SebF_Brix[!(SebF_Brix$vineyard==""), ] 

#Examining brix
brix <- SebF_Brix[which(SebF_Brix$event=="brix"), ]
brix <- brix[order(brix$block, brix$vineyard), ]

#Export Final Output
setwd("/Users/phoebeautio/desktop/bcvin/analyses/output/sebfarm_clean")
write.csv(SebF_Brix, "sebfarm_brix_complete.csv", row.names = F)
