## just making a file for brix labels... 29 July 2020 Mira Garner
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("~/Documents/git/bcvin/fieldwork/")

info <- read.csv("plantsInfo.csv")
head(info)

# subset just the plant currently in experiment
now <- info[which(info$notes != "FLAGS REMOVED"),]
unique(now$notes) #check that all removed plants are out

labels <- now[,c("vineyard", "block", "row.number", "plantID")]
head(labels)

write.csv(labels, "~/Documents/git/bcvin/fieldwork/brixlabels/brixlabelsheet.csv")
#end

#make mini for testing
mini <- labels[which(labels$vineyard == "Dark Horse"),]

write.csv(mini, "~/Documents/git/bcvin/fieldwork/brixlabels/minilabelsheet.csv")


