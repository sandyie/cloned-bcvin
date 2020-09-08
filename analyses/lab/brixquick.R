## Started 4 September 2020 ##
## By Lizzie ##


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
