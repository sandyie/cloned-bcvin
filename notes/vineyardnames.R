#getting list of vineyard names for Lizzie to give to Pat and Carl
# list of all vineyards in the phenology data from growers

#housekeeping
rm(list = ls())
options(stringsAsFactors = FALSE)

setwd("~/Documents/git/bcvin/analyses/")

#load in datasets
sebb <- read.csv("output/sebfarm_clean/sebfarm_brix_complete.csv")
sebp <- read.csv("cleaning/sebfarm_cleaning/seb_pheno_clean.csv")
art <- read.csv("cleaning/arterra/arterra_pheno_clean.csv")
qg <- read.csv("cleaning/quailsgate/quailsGatePhenology.csv")
map <- read.csv("input/quailsgate/vineyardmaps/VineyardMapsDataCompiled.csv")

company <- "QuailsGate"
qg <- cbind(qg, company)
map <- cbind(map, company)
colnames(qg)[colnames(qg) == "Vineyard"] <- "vineyard"

all.vin <- rbind(sebb[, 1:2], sebp[,2:3], art[,2:3], qg[,c(9,36)], map[,c(1,9)])

avin <- all.vin[!duplicated(all.vin),]
write.csv(avin, "~/Documents/vineyardnames.csv")
