
# Import Data -------------------------------------------------------------
portfolioRead <- list.files(path = "Data/assets", pattern = "*.csv", full.names = TRUE)
portfolioData <- sapply(portfolioRead, read_csv, simplify = FALSE) %>%
  bind_rows(.id = "id")

dividendRead <- list.files(path = "Data/dividends", pattern = "*.csv", full.names = TRUE)
dividendData <- sapply(dividendRead, read_csv, simplify = FALSE) %>%
  bind_rows(.id = "id")

portfolioData$id <- plyr::revalue(portfolioData$id, c("Data/assets/ABT.csv" = "Abbott",
                                                      "Data/assets/AMZN.csv" = "Amazon",
                                                      "Data/assets/DGcsv" = "Dollar General",
                                                      "Data/assets/JPM.csv" = "JPMorg",
                                                      "Data/assets/LYV.csv" = "LiveNat",
                                                      "Data/assets/MAR.csv" = "Marriott",
                                                      "Data/assets/TM.csv" = "Toyota",
                                                      "Data/assets/UNH.csv" = "UNHealth",
                                                      "Data/assets/VGT.csv" = "VGETF",
                                                      "Data/assets/VOO.csv" = "VGIndex"))

#EDIT
#Adjusted closing price. Subtract out dividends
#actually, do i add them back in? hmm
# If month and year of portfolio data == month and year of dividend data, subtract close - dividends
#maybe create data set that is reduced down only to month and year?
#remember to filter out the correct data from the master set
------
  
#Can I write a function to this? Maybe a for loop?
unique(portfolioData[1])
abbottPfo <- filter(portfolioData, id == "Data/assets/ABT.csv")
amazonPfo <- filter(portfolioData, id == "Data/assets/AMZN.csv")
dollargPfo <- filter(portfolioData, id == "Data/assets/DGcsv")
jpmorgPfo <- filter(portfolioData, id == "Data/assets/JPM.csv")
livenatPfo <- filter(portfolioData, id == "Data/assets/LYV.csv")
marriottPfo <- filter(portfolioData, id == "Data/assets/MAR.csv")
toyotaPfo <- filter(portfolioData, id == "Data/assets/TM.csv")
uhealthPfo <- filter(portfolioData, id == "Data/assets/UNH.csv")
vanguardetfPfo <- filter(portfolioData, id == "Data/assets/VGT.csv")
vanguardindexPfo <- filter(portfolioData, id == "Data/assets/VOO.csv")


# Adjusted Closing Prices Plot -------------------------------------------------

portfolioAdjClose <- as.tibble(cbind(portfolioData$id, portfolioData$`Adj Close`, portfolioData$Date))

colnames(portfolioAdjClose) <- c("id", "Adj Close", "Date")

portfolioAdjClose$`Adj Close` <- as.numeric(portfolioAdjClose$`Adj Close`)

portfolioAdjClose$Date <- as.numeric(portfolioAdjClose$Date) %>%
  as.Date(portfolioAdjClose$Date, format = "%Y-%m-%d" , origin = "1970-01-01")

portfolioAdjClose$id <- as.factor(portfolioAdjClose$id)





#Omit AMZN for consistent Y axis scaling
AdjCloseNoAmz <- portfolioAdjClose %>%
  filter(!id %in% "Amazon") %>%
ggplot(aes(x = `Date`, y = `Adj Close`), colour = id) +
  geom_point() +
  facet_wrap(~id, nrow = 2) 
  
print(AdjCloseNoAmz)

















