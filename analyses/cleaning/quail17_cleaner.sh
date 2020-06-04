setwd("/User/miragarner/Documents/git/bcvin/analyses/cleaning")
og_quail17<- read.csv("/User/miragarner/Documents/git/bcvin/analyses/input/qg_phenology2017_19COPY.csv")

# add column for company: Quail's Gate
company <- "QuailsGate"
NEXTDATA <- cbind(<<DATA>>, company)

# add column for vineyard - split from block
# add column for block - what are we considering a block (is it block 7 or block 7-4, weird block/vineyard names - manually split??)
#### make character column and use separate()? Or online solutions?

# add column for variety
#### need to look at maps and Asset Identifier, is there a way to create another spreadsheet of the asset id with it's variety then match with asset id in pheno spreadsheet?
#library(dplyr)
#<<blocks.vars>> %>%
    select(block, variety) %>%
    distinct() %>%
    right_join(<<DATA>>, by = 'block')
# OR
#merge(<<DATA>>, unique(blocks.vars)[, c("block", "variety")], by="block")

# split date into columns: year, month, day
###Question: which date is right?? Diff btwn TaskDate and DateOfObs
library(lubridate)
library(dplyr)
<<DF>>$year <- year(mdy(<<DF>>$<DATE>))
<<DF>>$month <- month(mdy(<<DF>>$<<DATE>>))
<<DF>>$day <- day(mdy(<<DF>>$<<DATE>>))

# add column for event - what do stages mean?


# add column for value (Brix value or percent flowering)

# add notes column
#Other.observations <- notes
#library(data.table)
#setnames(data, old=c("old_name","another_old_name"), new=c("new_name", "another_new_name"))
# OR colnames(data)[colnames(data)=="old_name"] <- "new_name" for single columns

# reorder columns
fin_quail17 <- select(<<DATA>>, c("company", "vineyard", "block", "variety", "year", "month", "day", "event", "value", "notes"))
