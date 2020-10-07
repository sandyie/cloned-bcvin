## Started 23 September 2020 ##
## By Geoff Legault  ##

## Modified from earlier scripts by Elizabeth Wolkovich and Faith Jones

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

## Load library
library(ggplot2)
library(vioplot)

## Read data
### brix
d <- read.csv("../input/lab/brix_working_all.csv", header = TRUE, stringsAsFactors = FALSE)
### plant information
vineInfo <- read.csv("../input/lab/plantsInfo_working.csv")

## Quality checks
table(d$vineyard)
unique(d$date_sampled)

## Merge brix with plantID table
d <- merge(d, vineInfo[, c("plantID", "variety")])

## Remove spaces in names
d$vineyard[which(d$vineyard == "Dark Horse")] <- "DarkHorse"
d$vineyard[which(d$vineyard == "Quails Gate Estate")] <- "QuailsGate"
## Replace Geoff with GL
d$labMember[which(d$labMember == "Geoff")] <- "GL"
## Replace fj with FJ
d$labMember[which(d$labMember == "fj")] <- "FJ"

## Clean errors
### Fix wrong sampling dates (entry error)
d[which(d$variety == "Shiraz" & d$date_sampled == "2020/08/26"), "date_sampled"] <- c("2020/08/25")
### Fix wrong sampling dates (transposition)
d[which(d$variety == "Chardonnay" & d$date_sampled == "2020/09/11"), "date_sampled"] <- c("2020/09/09")
d[which(d$variety == "Chardonnay" & d$date_brix == "2020/09/09"), "date_brix"] <- c("2020/09/11")
d[which(d$plantID %in% c(768, 814)), "vineyard"] <- c("QuailsGate")

## Make time series plot for each variety
### list of varieties
varieties <- unique(d$variety)
## Plotting parameters
xlim  <- range(as.Date(d$date_sampled))
ylim  <- c(0, 30)
### colors for vineyards
cbbPalette <- c("#00000066", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")
## plots
pdf(file = "figures/Brix varieties.pdf", height = 6, width = 10, onefile = TRUE)
par(mar = c(6, 6, 2, 2))
for(i in 1:length(varieties)){
    temp <- subset(d, variety == varieties[i])
    ## Empty plot to get bounds correct
    plot(NA,
         xlim = c(xlim[1], xlim[2]),
         ylim = c(ylim[1], ylim[2]),
         xlab = "Date",
         ylab = expression(paste("Brix (", degree, ")")),
         main = varieties[i],
         bty = "n",         
         xaxt = "n",
         yaxt = "n",
         cex.lab = 1.2,
         cex.axis = 1.4)
    axis(side = 1, at = seq.Date(xlim[1], xlim[2], by = 5), labels = seq.Date(xlim[1], xlim[2], by = 5), tcl = -.5)
    axis(side = 2, at = seq(ylim[1], ylim[2], by = 5), tcl = -.5, las = 1)
    temp.vineyards <- unique(temp$vineyard)
    legend("bottomright", inset = 0.05, col = c(cbbPalette[1:length(temp.vineyards)]), pch = 16, legend = temp.vineyards)
    for(j in 1:length(temp.vineyards)){
        temp2 <- subset(temp, vineyard == temp.vineyards[j])
        temp.plantID <- unique(temp2$plantID)
        for(k in 1:length(temp.plantID)){
            temp3 <- subset(temp2, plantID == temp.plantID[k])
            points(brix ~ as.Date(date_sampled), data = temp3, col = cbbPalette[j], type = "b", lwd = 1, lty = "dotted", pch = 16, cex = 1.3)
        }
    }
}
dev.off()


pdf(file = "figures/Brix varieties - Violins.pdf", height = 6, width = 10, onefile = TRUE)
par(mar = c(6, 6, 2, 2))
for(i in 1:length(varieties)){
    temp <- subset(d, variety == varieties[i])
    ## Empty plot to get bounds correct
    plot(NA,
         xlim = c(xlim[1], xlim[2]),
         ylim = c(ylim[1], ylim[2]),
         xlab = "Date",
         ylab = expression(paste("Brix (", degree, ")")),
         main = varieties[i],
         bty = "n",         
         xaxt = "n",
         yaxt = "n",
         cex.lab = 1.2,
         cex.axis = 1.4)
    axis(side = 1, at = seq.Date(xlim[1], xlim[2], by = 3), labels = seq.Date(xlim[1], xlim[2], by = 3), tcl = -.5)
    axis(side = 2, at = seq(ylim[1], ylim[2], by = 5), tcl = -.5, las = 1)
    temp.vineyards <- unique(temp$vineyard)
    legend("bottomright", inset = 0.05, col = c(cbbPalette[1:length(temp.vineyards)]), pch = 16, legend = temp.vineyards)
    for(j in 1:length(temp.vineyards)){
        temp2 <- subset(temp, vineyard == temp.vineyards[j])
        if(nrow(temp2) == 0) next
        temp.dates <- unique(temp2$date_sampled)
        for(k in 1:length(temp.dates)){
            temp3 <- subset(temp2, date_sampled == temp.dates[k])
            vioplot(x = temp3$brix,
                    col = cbbPalette[j],
                    horizontal = FALSE,
                    at = as.Date(temp.dates[k]),
                    add = TRUE,
                    lty = 0,
                    pchMed = 21,
                    colMed = "black",
                    colMed2 = "white",
                    rectCol = rgb(0, 0.25, 0.75, alpha = .5),
                    lineCol = NA)
        }
    }
}
dev.off()

