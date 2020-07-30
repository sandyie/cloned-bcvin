############## Bcvin Brix Sourcing Script (PA) ##############

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

setwd("/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning")

#Sourcing 2004-2018 cleaning scripts
source("/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning/sebfarm_brix_clean2004.R")
source("/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning/sebfarm_brix_clean2005.R")
source("/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning/sebfarm_brix_clean2006.R")
source("/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning/sebfarm_brix_clean2007.R")
source("/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning/sebfarm_brix_clean2008.R")
source("/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning/sebfarm_brix_clean2009.R")
source("/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning/sebfarm_brix_clean2010.R")
source("/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning/sebfarm_brix_clean2011.R")
source("/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning/sebfarm_brix_clean2012.R")
source("/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning/sebfarm_brix_clean2013.R")
source("/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning/sebfarm_brix_clean2014.R")
source("/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning/sebfarm_brix_clean2015.R")
source("/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning/sebfarm_brix_clean2017.R")
source("/Users/phoebeautio/Desktop/bcvin/analyses/cleaning/sebfarm_cleaning/sebfarm_brix_clean2018.R")

#Binding all years into one dataframe
SebF_Brix <- rbind(SebF2004, SebF2005, SebF2006, SebF2007, SebF2008, SebF2009, SebF2010, SebF2011,
                   SebF2012, SebF2013, SebF2014, SebF2015, SebF2017, SebF2018)

#Removing rows with NA
SebF_Brix <- SebF_Brix[!(is.na(SebF_Brix$value)), ]

#Addressing issue with vineyard code and block number

#Export Final Output
setwd("/Users/phoebeautio/desktop/bcvin/analyses/output/sebfarm_clean")
write.csv(SebF_Brix, "sebfarm_brix_complete.csv", row.names = F)
