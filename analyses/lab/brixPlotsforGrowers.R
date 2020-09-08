## Started 4 September 2020 ##
## By Lizzie ##

##Edited by Faith Sep 8th to make plots for growers, and saved as a new R script. 


# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/vinmisc/bcvin/analyses/lab") 
} else if(length(grep("faith", getwd())>0)){
  setwd("/home/faith/Documents/github/bcvin/bcvin/analyses/lab") 
} else setwd("~/Documents/git/ospree/analyses")

library(ggplot2)

d1 <- read.csv("..//input/lab/brix_25Aug_FJ.csv")
d2 <- read.csv("..//input/lab/brix_25Aug_GL.csv")
d3 <- read.csv("..//input/lab/brix_25Aug_PA.csv")
d4 <- read.csv("..//input/lab/brix_25Aug_misc.csv")

vineInfo <- read.csv("/home/faith/Documents/github/bcvin/bcvin/analyses/input/lab/plantsInfo_working.csv")

d <- rbind(d4, d3, d2, d1) 

checkme <- subset(d, vineyard=="")
unique(checkme$brix)

d <- subset(d, vineyard!="")

unique(d$vineyard)

d$vineyard[which(d$vineyard=="Dark Horse")] <- "DarkHorse"
d$vineyard[which(d$vineyard=="Quail's Gate")] <- "QuailsGate"
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

#plots for Quail's Gate
#-----------------------------
unique(dInfo$vineyard)
qgData2 <- dInfo[dInfo$vineyard %in% c("Mannhardt", "QuailsGate"), ]
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

#Plots for Artera
#-------------------------------

unique(dInfo$vineyard)
aData <- dInfo[dInfo$vineyard %in% c("DarkHorse", "NKMIP", "McIntyre"), ]
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