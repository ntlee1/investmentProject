
# Import Data -------------------------------------------------------------
portfolioRead <- list.files(path = "Data/assets", pattern = "*.csv", full.names = TRUE)
portfolioData <- sapply(portfolioRead, read_csv, simplify = FALSE) %>%
  bind_rows(.id = "id")

portfolioData$id <- plyr::revalue(portfolioData$id, c("Data/assets/ABT.csv" = "Abbott",
                                                      "Data/assets/AMZN.csv" = "Amazon",
                                                      "Data/assets/DG.csv" = "Dollar General",
                                                      "Data/assets/JPM.csv" = "JPMorg",
                                                      "Data/assets/LYV.csv" = "LiveNat",
                                                      "Data/assets/MAR.csv" = "Marriott",
                                                      "Data/assets/TM.csv" = "Toyota",
                                                      "Data/assets/UNH.csv" = "UNHealth",
                                                      "Data/assets/VGT.csv" = "VGETF",
                                                      "Data/assets/VOO.csv" = "VGIndex"))
portfolioData$id <- as.factor(portfolioData$id)

dividendRead <- list.files(path = "Data/dividends", pattern = "*.csv", full.names = TRUE)
dividendData <- sapply(dividendRead, read_csv, simplify = FALSE) %>%
  bind_rows(.id = "id")



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

#Omit AMZN for consistent Y axis scaling
AdjCloseNoAmz <- portfolioData %>%
  filter(!id %in% "Amazon") %>%
ggplot(aes(x = `Date`, y = `Adj Close`)) +
  geom_point(size = 0.75) +
  facet_wrap(~id, nrow = 2, ncol = 5) +
  labs(title = "RiD Adjusted Portfolio Assets Closing Price 2016-2021", y = "Adjusted Close (USD)") +
  ggthemes::theme_solarized() 

ggplotly(AdjCloseNoAmz)

AdjCloseAmz <- portfolioData %>%
  filter(id %in% "Amazon") %>%
  ggplot(aes(x = `Date`, y = `Adj Close`), colour = id) +
  geom_point(size = 1) +
  facet_wrap(~id, nrow = 1, ncol = 1) +
  labs(title = "RiD Adjusted Portfolio Assets Closing Price 2016-2021", y = "Adjusted Close (USD)") +
  ggthemes::theme_solarized()
  
ggplotly(AdjCloseAmz)


# Monthly Returns ----------------------------------------------------------
mnthReturns <- portfolioData

















