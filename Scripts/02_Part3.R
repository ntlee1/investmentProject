
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


# Monthly Returns Function ----------------------------------------------------------
 #Working Function for Market Returns
#EDIT should i use different variable names so i dont interfere with scripts? Yes! Clean up variable names
#EDIT I solved the assignment issue by putting the actual read variable inside, i think this means everything should be self contained including any read csv functions
#EDIT i wonder how i can generalize the read function? so i can use with any csv file
#EDIT how can i generalize to put in a vector of all start and end asset row positions?
#EDIT add to master R notes
#EDIT how can i automatically generate list of start and end row positions for each new factor
#EDIT add function explanation

assetReturnList <- function(priorAssetRowEnd, nextAssetRowStart){
  aslReadData <- list.files(path = "Data/assets", pattern = "*.csv", full.names = TRUE)
  aslPortfolioData <- sapply(aslReadData, read_csv, simplify = FALSE) %>%
    bind_rows(.id = "id")
  
  aslPortfolioData$id <- plyr::revalue(aslPortfolioData$id, c("Data/assets/ABT.csv" = "Abbott",
                                                        "Data/assets/AMZN.csv" = "Amazon",
                                                        "Data/assets/DG.csv" = "Dollar General",
                                                        "Data/assets/JPM.csv" = "JPMorg",
                                                        "Data/assets/LYV.csv" = "LiveNat",
                                                        "Data/assets/MAR.csv" = "Marriott",
                                                        "Data/assets/TM.csv" = "Toyota",
                                                        "Data/assets/UNH.csv" = "UNHealth",
                                                        "Data/assets/VGT.csv" = "VGETF",
                                                        "Data/assets/VOO.csv" = "VGIndex"))
  aslPortfolioData$id <- as.factor(aslPortfolioData$id)

  aslMnthReturns <- aslPortfolioData %>%
    mutate(closeReturn = (aslPortfolioData$`Adj Close`/ lag(aslPortfolioData$`Adj Close`, n = 1) - 1))
  
  aslMnthReturns$closeReturn[1:(priorAssetRowEnd + 1)] <- NA
  aslMnthReturns$closeReturn[nextAssetRowStart:nrow(aslPortfolioData)] <- NA
  aslMnthReturns$closeReturn[is.na(aslMnthReturns$closeReturn)] <- 0

  print(paste("This is monthly returns for",aslPortfolioData$id[priorAssetRowEnd + 1]))
  print(aslMnthReturns[priorAssetRowEnd + 1:nextAssetRowStart -1,])
}
















#Make yourDataColumnName more robust
#must be exact match and include ticks if required
assetReturnList <- function(yourData, yourDataColumnName, priorAssetRowEnd, nextAssetRowStart){
  aslPortfolioData <<- yourData
  
yourDataCloseColumn <<- yourDataColumnName

  aslMnthReturns <- aslPortfolioData %>%
    mutate(closeReturn = (yourDataColumnName/lag(yourDataColumnName, n = 1) - 1))
  
  aslMnthReturns$closeReturn[1:(priorAssetRowEnd + 1)] <- NA
  aslMnthReturns$closeReturn[nextAssetRowStart:nrow(aslPortfolioData)] <- NA
  aslMnthReturns$closeReturn[is.na(aslMnthReturns$closeReturn)] <- 0
  
  print(paste("This is monthly returns for",aslPortfolioData$id[priorAssetRowEnd + 1]))
  print(aslMnthReturns[priorAssetRowEnd + 1:nextAssetRowStart -1,])
}




assetReturnList(portfolioData,portfolioData$`Adj Close`, 0, 61)


portfolioData$Close















# Monthly Returns Plot ----------------------------------------------------
#EDIT Now i need to figure out how to use the asl function to create plots
mnthReturnsPlot <- ggplot(mnthReturns, aes(x = `Date`, y = closeReturn)) +
  geom_line() +
  facet_wrap(~id) + 
  scale_y_continuous(limits = c(-1,1))

ggplotly(mnthReturnsPlot)
















