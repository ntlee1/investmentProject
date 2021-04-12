
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


#Monthly Returns Function: Takes a dataframe with multiple factors in the same column and gives monthly returns for each different type of asset ----------------------------------------------------------
#EDIT
#Do I need to use different variable names to avoid overwriting global variables?
#Instead of close col row start and close col row end, use the id name for robustness
#ERROR: closeColRowEnd = 600 

#NOTES
#I solved the assignment issue by putting the actual read variable inside, i think this means everything should be self contained including any read csv functions
#add to master R notes


assetReturnList <- function(data, dataColumnClose, closeColRowStart, closeColRowEnd) {
  aslPortfolioData <<- data
  
  yourDataCloseColumn <<- dataColumnClose
  
  aslMnthReturns <- aslPortfolioData %>%
    mutate(closeReturnMonthly = (yourDataCloseColumn/lag(yourDataCloseColumn, n = 1) - 1))
  
  aslMnthReturns$closeReturnMonthly[1:(closeColRowStart)] <- NA
  aslMnthReturns$closeReturnMonthly[(closeColRowEnd + 1):nrow(aslPortfolioData)] <- NA
  aslMnthReturns$closeReturnMonthly[is.na(aslMnthReturns$closeReturnMonthly)] <- 0
  
  print(paste("This is monthly returns for",aslPortfolioData$id[closeColRowStart]))
  print(aslMnthReturns[closeColRowStart:closeColRowEnd,])
}


# EDIT: Failed attempt at creating for loop to iteratively cycle through args [3,4] of assetReturnList() -----------------------------------------------------------------

allMnthClose <- list(assetReturnList(portfolioData, portfolioData$`Adj Close`, 1, 60),
                     assetReturnList(portfolioData, portfolioData$`Adj Close`, 61, 120),
                     assetReturnList(portfolioData, portfolioData$`Adj Close`, 121, 180),
                     assetReturnList(portfolioData, portfolioData$`Adj Close`, 181, 240),
                     assetReturnList(portfolioData, portfolioData$`Adj Close`, 241, 300),
                     assetReturnList(portfolioData, portfolioData$`Adj Close`, 301, 360),
                     assetReturnList(portfolioData, portfolioData$`Adj Close`, 361, 420),
                     assetReturnList(portfolioData, portfolioData$`Adj Close`, 421, 480),
                     assetReturnList(portfolioData, portfolioData$`Adj Close`, 481, 540),
                     assetReturnList(portfolioData, portfolioData$`Adj Close`, 541, 599))

                     


allMnthClose <- bind_rows(allMnthClose)


# Monthly Returns Plot ----------------------------------------------------
mnthReturnPlot <- ggplot(allMnthClose, aes(x = Date, y = closeReturnMonthly)) +
  geom_point(size = 0.75) +
  facet_wrap(~id) +
  labs(title = "Monthly Returns 2016-2021 RiD Portfolio",
       y = "Monthly Returns") +
  ggthemes::theme_solarized()

ggplotly(mnthReturnPlot)



# Average monthly return --------------------------------------------------
#EDIT: For loop?

unique(allMnthClose$id)
avgMthReturnList <- list(filter(allMnthClose, id == "Abbott"),
                     filter(allMnthClose, id == "Amazon"),
                     filter(allMnthClose, id == "Dollar General"),
                     filter(allMnthClose, id == "JPMorg"),
                     filter(allMnthClose, id == "LiveNat"),
                     filter(allMnthClose, id == "Marriott"),
                     filter(allMnthClose, id == "Toyota"),
                     filter(allMnthClose, id == "UNHealth"),
                     filter(allMnthClose, id == "VGETF"),
                     filter(allMnthClose, id == "VGIndex"))

allMnthClose %>%
  dplyr::group_by(id) %>%
  mean(closeReturnMonthly)

mean(allMnthClose$closeReturnMonthly)

lapply(avgMthReturnList, mean)
