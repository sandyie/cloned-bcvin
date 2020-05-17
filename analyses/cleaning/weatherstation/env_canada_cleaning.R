
## Create list of files in environmentcanada data folder
env.list <- list.files(path = "../../../data/environmentcanada/", pattern = "*.csv", full.names = TRUE)

## Read files and put into a list
env.data <- lapply(env.list, read.csv)

## Extract desired values and smoosh into one data.frame
env.data2 <- do.call(rbind, lapply(env.data, FUN = function(X){
    temp <- data.frame(
        StationName = X[, "Station.Name"],
        Year = X[, "Year"],
        Day = 1:length(X[, "Year"]),
        MeanTemperature = X[, "Mean.Temp...C."])
    return(temp)}))

## Order by year
env.data2 <- env.data2[order(env.data2$Year), ]

## Save to temporary folder
write.csv(x = env.data2, file = "../../output/temporary/environmentcanada_clean.csv", row.names = FALSE)
