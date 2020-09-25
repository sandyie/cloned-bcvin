## Started 4 September 2020 ##
## By Lizzie ##

##Edited by Faith Sep 8th to make plots for growers, and saved as a new R script. 
## Edited by Geoff Sep 23rd to include updated inputs

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/vinmisc/bcvin/analyses/lab") 
} else if(length(grep("faith", getwd())>0)){
  setwd("/home/faith/Documents/github/bcvin/bcvin/analyses/lab") 
} else setwd("~/Documents/git/ospree/analyses")

## Load library
library(ggplot2)

## Read data
d <- read.csv("../input/lab/brix_working_all.csv", header = TRUE, stringsAsFactors = FALSE)

vineInfo <- read.csv("../input/lab/plantsInfo_working.csv")

## Quality checks
checkme <- subset(d, vineyard=="")
unique(checkme$brix)

d <- subset(d, vineyard!="")

unique(d$vineyard)

d$vineyard[which(d$vineyard=="Dark Horse")] <- "DarkHorse"
d$vineyard[which(d$vineyard=="Quails Gate Estate")] <- "QuailsGate"

table(d$vineyard)

ggplot(d, aes(x=brix)) +
    geom_histogram(fill="dodgerblue") +
    facet_wrap(.~vineyard)

ggplot(d, aes(x=brix, group=vineyard, fill=vineyard)) +
    geom_histogram(position = "identity", alpha = 0.2) # bleack! Not so handy...

#Add in vine info
#-------------------------------
head(vineInfo)
names(vineInfo)
head(d)
dInfo <- merge(d, vineInfo[,c(1,4)], by = "plantID")
head(dInfo)

#split by sampling date
#---------------------------
dInfoAug <- dInfo[dInfo$date_sampled %in% c("2020/08/25", "2020/08/26", "2020/08/27"),]
dInfoSep9to15 <- dInfo[dInfo$date_sampled %in% c("2020/09/09", "2020/09/9", "2020/09/10", "2020/09/14", "2020/09/15"),]
table(dInfoSep9to15[,c("date_brix", "vineyard")])#Checking that these data represent a single time point per vineyard 
plot(dInfoSep9to15$date_sampled ~ dInfoSep9to15$vineyard)


#plots for Quail's Gate - August dates 
#-----------------------------
unique(dInfo$vineyard)
qgData2 <- dInfoAug[dInfoAug$vineyard %in% c("Mannhardt", "QuailsGate"), ]
head(qgData2)


ggplot(qgData2, aes(x=brix, group=block , fill=block)) +
    geom_histogram(position = "identity", alpha = 0.1, aes(colour = block)) + 
    theme_classic() +
    facet_wrap(~ vineyard)

manData <- qgData2[qgData2$vineyard == "Mannhardt",]
qgData <- qgData2[qgData2$vineyard == "QuailsGate",]

manPlot <- ggplot(manData, aes(x=brix, group=block , fill=block)) +
    geom_histogram(position = "identity", alpha = 0.1, aes(colour = block)) + 
    theme_classic() +
    labs (x = "Brix value (degrees)", y = "Count")+ 
    facet_wrap(~ variety)+ 
    ggtitle("Mannhardt Brix August 26 2020") +
    theme(text = element_text(size = 20))

png("figures/Mannhardt26AugBrix.png", width = 800, height = 500)
manPlot
dev.off()

qgData <- qgData[!is.na(qgData$row),]

qgmainPlot <- ggplot(qgData, aes(x=brix, group=block , fill=block)) +
    geom_histogram(position = "identity", alpha = 0.1, aes(colour = block)) + 
    theme_classic() +
    labs (x = "Brix value (degrees)", y = "Count") + 
    facet_wrap(~ variety)+ 
    ggtitle("Quail's Gate Brix August 26 2020") +
    theme(text = element_text(size = 20))

png("figures/QGMain26AugBrix.png", width = 1000, height = 700)
qgmainPlot
dev.off()


#plots for Quail's Gate - September 9-15th dates 
#-----------------------------
unique(dInfoSep9to15$vineyard)
qgData2Sep915 <- dInfoSep9to15[dInfoSep9to15$vineyard %in% c("Mannhardt", "QuailsGate"), ]
head(qgData2Sep915)


ggplot(qgData2Sep915, aes(x=brix, group=block , fill=block)) +
    geom_histogram(position = "identity", alpha = 0.1, aes(colour = block)) + 
    theme_classic() +
    facet_wrap(~ vineyard)

manData2 <- qgData2Sep915[qgData2Sep915$vineyard == "Mannhardt",]
qgData2 <- qgData2Sep915[qgData2Sep915$vineyard == "QuailsGate",]

manPlot2 <- ggplot(manData2, aes(x=brix, group=block , fill=block)) +
    geom_histogram(position = "identity", alpha = 0.1, aes(colour = block)) + 
    theme_classic() +
    labs (x = "Brix value (degrees)", y = "Count")+ 
    facet_wrap(~ variety)+ 
    ggtitle("Mannhardt Brix September 14-15 2020") +
    theme(text = element_text(size = 20))

png("figures/Mannhardt15SepBrix.png", width = 800, height = 500)
manPlot2
dev.off()

qgData2 <- qgData2[!is.na(qgData2$row),]

qgmainPlot2 <- ggplot(qgData2, aes(x=brix, group=block , fill=block)) +
    geom_histogram(position = "identity", alpha = 0.1, aes(colour = block)) + 
    theme_classic() +
    labs (x = "Brix value (degrees)", y = "Count") + 
    facet_wrap(~ variety)+ 
    ggtitle("Quail's Gate Brix September 14 2020") +
    theme(text = element_text(size = 20))+ scale_x_continuous(
  breaks=c(12,14,16, 18, 20, 22, 24))

png("figures/QGMain15SepBrix.png", width = 1000, height = 700)
qgmainPlot2
dev.off()




#Plots for Artera - August 
#-------------------------------

unique(dInfo$vineyard)
aData <- dInfoAug[dInfoAug$vineyard %in% c("DarkHorse", "NKMIP", "McIntyre"), ]
head(aData)

aData <- aData[!is.na(aData$row),]
unique(aData$block)
aData[aData$block == "1",]

ggplot(aData, aes(x=brix, group=block , fill=block)) +
    geom_histogram(position = "identity", alpha = 0.1, aes(colour = block)) + 
    theme_classic() +
    facet_wrap(~ variety)

nkData <- aData[aData$vineyard == "NKMIP",]
dhData <- aData[aData$vineyard == "DarkHorse",]
mcData <- aData[aData$vineyard == "McIntyre",]

nkPlot <- ggplot(nkData, aes(x=brix, group=block , fill=block)) +
    geom_histogram(position = "identity", alpha = 0.1, aes(colour = block)) + 
    theme_classic() +
    labs (x = "Brix value (degrees)", y = "Count")+ 
    facet_wrap(~ variety)+
    scale_x_continuous(labels = scales::number_format(accuracy = 1))+ 
    ggtitle("NK'MIP Brix August 25 2020") +
    theme(text = element_text(size = 20))

png("figures/NKMIP25AugBrix.png", width = 800, height = 500)
nkPlot
dev.off()


dhPlot <- ggplot(dhData, aes(x=brix, group=block , fill=block)) +
    geom_histogram(position = "identity", alpha = 0.1, aes(colour = block)) + 
    theme_classic() +
    labs (x = "Brix value (degrees)", y = "Count") + 
    facet_wrap(~ variety)+
    scale_x_continuous(labels = scales::number_format(accuracy = 1))+ 
    ggtitle("Dark Horse Brix August 25 2020") +
    theme(text = element_text(size = 20))

png("figures/DarkHorse25AugBrix.png", width = 800, height = 500)
dhPlot
dev.off()

mcPlot <- ggplot(mcData, aes(x=brix, group=block , fill=block)) +
    geom_histogram(position = "identity", alpha = 0.1, aes(colour = block)) + 
    theme_classic() +
    labs (x = "Brix value (degrees)", y = "Count")+ 
    facet_wrap(~ variety)+
    scale_x_continuous(labels = scales::number_format(accuracy = 1))+ 
    ggtitle("McIntyre Brix August 25 2020") +
    theme(text = element_text(size = 20))

png("figures/McIntyre25AugBrix.png", width = 800, height = 500)
mcPlot
dev.off()




#Plots for Artera - Sep9-15
#-------------------------------

unique(dInfo$vineyard)
aData2 <- dInfoSep9to15[dInfoSep9to15$vineyard %in% c("DarkHorse", "NKMIP", "McIntyre"), ]
head(aData2)

aData2 <- aData2[!is.na(aData2$row),]
unique(aData2$block)
aData[aData2$block == "1",]

ggplot(aData2, aes(x=brix, group=block , fill=block)) +
    geom_histogram(position = "identity", alpha = 0.1, aes(colour = block)) + 
    theme_classic() +
    facet_wrap(~ variety)

nkData2 <- aData2[aData2$vineyard == "NKMIP",]
dhData2 <- aData2[aData2$vineyard == "DarkHorse",]
mcData2 <- aData2[aData2$vineyard == "McIntyre",]

nkPlot2<- ggplot(nkData2, aes(x=brix, group=block , fill=block)) +
    geom_histogram(position = "identity", alpha = 0.1, aes(colour = block)) + 
    theme_classic() +
    labs (x = "Brix value (degrees)", y = "Count")+ 
    facet_wrap(~ variety)+
    scale_x_continuous(labels = scales::number_format(accuracy = 1))+ 
    ggtitle("NK'MIP Brix September 10 2020") +
    theme(text = element_text(size = 20))

png("figures/NKMIP10SepBrix.png", width = 800, height = 500)
nkPlot2
dev.off()


dhPlot2 <- ggplot(dhData2, aes(x=brix, group=block , fill=block)) +
    geom_histogram(position = "identity", alpha = 0.1, aes(colour = block)) + 
    theme_classic() +
    labs (x = "Brix value (degrees)", y = "Count") + 
    facet_wrap(~ variety)+
    scale_x_continuous(labels = scales::number_format(accuracy = 1))+ 
    ggtitle("Dark Horse Brix September 10 2020") +
    theme(text = element_text(size = 20))

png("figures/DarkHorse10SepBrix.png", width = 800, height = 500)
dhPlot2
dev.off()

mcPlot2 <- ggplot(mcData2, aes(x=brix, group=block , fill=block)) +
    geom_histogram(position = "identity", alpha = 0.1, aes(colour = block)) + 
    theme_classic() +
    labs (x = "Brix value (degrees)", y = "Count")+ 
    facet_wrap(~ variety)+
    scale_x_continuous(labels = scales::number_format(accuracy = 1))+ 
    ggtitle("McIntyre Brix September 9 2020") +
    theme(text = element_text(size = 20))

png("figures/McIntyre9SepBrix.png", width = 800, height = 500)
mcPlot2
dev.off()
