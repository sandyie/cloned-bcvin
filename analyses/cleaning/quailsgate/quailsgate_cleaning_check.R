library(tidyverse)
library(reshape2)
library(lubridate)

#script check for Quails Gate
setwd("C:/Ecology Lab/R/bcvin_git/bcvin/analyses")

quails <- read.csv("cleaning/quailsgate/quailsGatePhenology.csv")
rawquails <- read.csv("cleaning/quailsgate/PhenologyData2001to2012.csv")

#checking if all columns have entries
quails$Notes
quails$rootstock
quails$clone
quails$OriginalVineyardBlocks
quails$OriginalBlockName
quails$vineyard

quails$Notes[!is.na(quails$Notes)] 
quails$rootstock[!is.na(quails$rootstock)]
quails$clone[!is.na(quails$clone)]
quails$OriginalVineyardBlocks[!is.na(quails$OriginalVineyardBlocks)]
quails$OriginalBlockName[!is.na(quails$OriginalBlockName)]
quails$vineyard[!is.na(quails$vineyard)]
#all columns do have entries


#verify that dates for budburst < flowering < veraison 
#I had to use multiple columns (Variety, YearPlanted, Year, and either Berries.per.Cluster / Row) to create unique identifiers to correctly bind the data
#The first uses Berries.per.Cluster and the second uses Row
quails_berries_f <- dplyr::select(quails, c(Variety, Year, YearPlanted, PhenoEvent, Berries.per.Cluster, phenologyDate)) %>%
  as_tibble()%>%
  mutate(day = yday(ymd(quails$phenologyDate))) %>%
  filter(Berries.per.Cluster > 0, PhenoEvent == "Flowering") %>%
  pivot_wider(names_from = PhenoEvent, values_from = day)
  
quails_berries_b <- dplyr::select(quails, c(Variety, Year, YearPlanted, PhenoEvent, Berries.per.Cluster, phenologyDate)) %>%
  as_tibble()%>%
  mutate(day = yday(ymd(quails$phenologyDate))) %>%
  filter(Berries.per.Cluster > 0, PhenoEvent == "Budburst") %>%
  pivot_wider(names_from = PhenoEvent, values_from = day)

quails_berries_v <- dplyr::select(quails, c(Variety, Year, YearPlanted, PhenoEvent, Berries.per.Cluster, phenologyDate)) %>%
  as_tibble()%>%
  mutate(day = yday(ymd(quails$phenologyDate))) %>%
  filter(Berries.per.Cluster > 0, PhenoEvent == "X80.Veraison.Date")%>%
  pivot_wider(names_from = PhenoEvent, values_from = day)

quails_berries_joined <- inner_join(quails_berries_b, quails_berries_f, by = c("Berries.per.Cluster", "Variety", "YearPlanted", "Year"))%>%
  inner_join(. , quails_berries_v, by = c("Berries.per.Cluster", "Variety", "YearPlanted", "Year")) %>%
  select(-c(phenologyDate.x, phenologyDate.y, phenologyDate))%>%
  mutate(budburst_less_flowering = Flowering - Budburst) %>%
  mutate(flowering_less_veraison = X80.Veraison.Date - Flowering)

quails_berries_joined$budburst_less_flowering[quails_berries_joined$budburst_less_flowering > 0] <- "TRUE"
quails_berries_joined$budburst_less_flowering[quails_berries_joined$budburst_less_flowering <= 0] <- "FALSE"

quails_berries_joined$flowering_less_veraison[quails_berries_joined$flowering_less_veraison > 0] <- "TRUE"
quails_berries_joined$flowering_less_veraison[quails_berries_joined$flowering_less_veraison <= 0] <- "FALSE"




quails_row_f <- dplyr::select(quails, c(Variety, Year, YearPlanted, PhenoEvent, Row, phenologyDate)) %>%
  as_tibble()%>%
  mutate(day = yday(ymd(quails$phenologyDate))) %>%
  filter(Row > 0, PhenoEvent == "Flowering") %>%
  pivot_wider(names_from = PhenoEvent, values_from = day)

quails_row_b <- dplyr::select(quails, c(Variety, Year, YearPlanted, PhenoEvent, Row, phenologyDate)) %>%
  as_tibble()%>%
  mutate(day = yday(ymd(quails$phenologyDate))) %>%
  mutate(Nrow = row_number())%>%
  filter(Row > 0, PhenoEvent == "Budburst") %>%
  pivot_wider(names_from = PhenoEvent, values_from = day)%>%
  select(-Nrow)

quails_row_v <- dplyr::select(quails, c(Variety, Year, YearPlanted, PhenoEvent, Row, phenologyDate)) %>%
  as_tibble()%>%
  mutate(day = yday(ymd(quails$phenologyDate))) %>%
  mutate(Nrow = row_number())%>%
  filter(Row > 0, PhenoEvent == "X80.Veraison.Date")%>%
  pivot_wider(names_from = PhenoEvent, values_from = day)%>%
  select(-Nrow)

quails_row_joined <- inner_join(quails_row_b, quails_row_f, by = c("Row", "Variety", "Year", "YearPlanted"))%>%
  inner_join(. , quails_row_v, by = c("Row", "Variety", "Year", "YearPlanted")) %>%
  select(-c(phenologyDate.x, phenologyDate.y, phenologyDate))%>%
  mutate(budburst_less_flowering = Flowering - Budburst) %>%
  mutate(flowering_less_veraison = X80.Veraison.Date - Flowering)

quails_row_joined$budburst_less_flowering[quails_row_joined$budburst_less_flowering > 0] <- "TRUE"
quails_row_joined$budburst_less_flowering[quails_row_joined$budburst_less_flowering <= 0] <- "FALSE"

quails_row_joined$flowering_less_veraison[quails_row_joined$flowering_less_veraison > 0] <- "TRUE"
quails_row_joined$flowering_less_veraison[quails_row_joined$flowering_less_veraison <= 0] <- "FALSE"


#All matching rows follow the logic pattern
quails_row_joined$flowering_less_veraison
quails_row_joined$budburst_less_flowering

quails_berries_joined$flowering_less_veraison
quails_berries_joined$budburst_less_flowering


#boxplots for phenology event day by year

vars <- c("Flowering", "Budburst", "X80.Veraison.Date", "X50.Veraison.Date", "Full Bloom", "Variety")
quails1 <- quails %>%
  mutate(day = yday(ymd(quails$phenologyDate))) %>%
  filter(PhenoEvent %in% vars )%>%
  pivot_wider(names_from = PhenoEvent, values_from = day)

boxp <- ggplot(data = quails1)

boxp +
  geom_boxplot(mapping = aes(x = as.factor(Year), y = Flowering), outlier.color = "red")+
  xlab("Year")+
  ylab("Flowering Day")
boxp + 
  geom_boxplot(mapping = aes(x = as.factor(Year), y = Budburst), outlier.color = "red")+
  xlab("Year")+
  ylab("Budburst Day")
boxp +
  geom_boxplot(mapping = aes(x = as.factor(Year), y = X80.Veraison.Date), outlier.color = "red") +
  xlab("Year")+
  ylab("80% Veraison Day")
boxp +
  geom_boxplot(mapping = aes(x = as.factor(Year), y = X50.Veraison.Date), outlier.color = "red") +
  xlab("Year")+
  ylab("50% Veraison Day")
boxp +
  geom_boxplot(mapping = aes(x = Flowering, y = Variety), outlier.color = "red")+
  xlab("Variety")+
  ylab("Flowering Day")


ggplot(data = quails1) + 
  geom_point(mapping = aes(x = Flowering, y = Variety, color = as.factor(Year)))

#2008 seemed to have quite a bit of outliers
check2008 <- quails1$Flowering[!is.na(quails1$Flowering) ]
summary(check2008)
less_1st_quartile <- check2008[check2008 < 167]
under160 <- check2008[check2008 < 160]

#joining data without Veraison. The X80 and X50 Veraison bottlenecked the number of samples
#scatterplot of Flowering & Budburst days
quails2 <- inner_join(quails_row_b, quails_row_f, by = c("Row", "Variety", "Year", "YearPlanted"))
quails3 <- inner_join(quails_berries_b, quails_berries_f, by = c("Berries.per.Cluster", "Variety", "YearPlanted", "Year"))

ggplot(quails2) +
geom_point(mapping = aes(x = Flowering, y = Budburst, col = Variety ))

ggplot(quails3) +
  geom_point(mapping = aes(x = Flowering, y = Budburst, col = Variety ))

#I'm not sure why there is a lot of NA years in the cleaned Quailsgate data. There are no NA's in the "quailsgate/PhenologyData2001to2012.csv" file
YearNA_cleaned <- quails$Year[is.na(quails$Year)]
length(YearNA_cleaned)#507 NA year values

YearNA_raw <- rawquails$Year[is.na(rawquails$Year)]
length(YearNA_raw)#0 NA year values

#there are more rows in the cleaned data than the raw. This may not be and issue but this indicates that some columns had been changed to values
nrow(quails) #2062
nrow(rawquails) #1846

#Curious if the amount of observations for each PhenoEvent is identical in both datasets
flowering_clean <- quails1$Flowering[!is.na(quails1$Flowering)]
length(flowering_clean)#498
flowering_raw <- rawquails$phenologyEvent[rawquails$phenologyEvent == "Flowering"]
length(flowering_raw)#554

budburst_clean <- quails1$Budburst[!is.na(quails1$Budburst)]
length(budburst_clean)#568
#Hashed out code is to verify that my mutations on the original dataset didn't effect the quality of the data
#budburst_test<- quails$PhenoEvent[quails$PhenoEvent == "Budburst"]
#length(budburst_test) #568
budburst_raw <- rawquails$phenologyEvent[rawquails$phenologyEvent == "Budburst"]
length(budburst_raw)#495


