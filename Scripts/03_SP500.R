
# Import and Tidy ---------------------------------------------------------
indexData <- list.files(path = "Data/indextracking", pattern = "*.csv", full.names = TRUE)
indexData %<>%
  map_dfr(read_csv, .id = "Index")

indexData$`Index` %<>%
  plyr::mapvalues(from = c(1:3), to = c("ACWI",
                                        "DJI",
                                        "SP500"))
indexData$Date <- as.Date(indexData$Date, format = "%m/%d/%Y", origin = "1970-01-01")




#i need a systematic way to clean and tidy data from multiple csvs
# Annual Returns ----------------------------------------------------------
#Average ann rt final -begin/n

annIndexYrs <- as.Date(c("2016-04-01", "2020-03-01"))
indexDataYrRtACWI <- .01*(indexData$`Adj Close`[60]- indexData$`Adj Close`[1]/(indexData$`Adj Close`[1]))
indexDataYrRtDJI <- (
  (indexData$`Adj Close`[61]-indexData$`Adj Close`[120])/(indexData$`Adj Close`[120]))

indexDataYrSP500 <- ((indexData$`Adj Close`[121]-indexData$`Adj Close`[180])/(indexData$`Adj Close`[180]))

indexDataRt <- as.data.frame(rbind(indexDataYrRtACWI,indexDataYrRtDJI,indexDataYrSP500))
indexDataRt$Date <- rep(c("2016-2021"), times = 3)
