
# Import Data -------------------------------------------------------------
portfolioData <-  list.files(path = "Data/assets", pattern = "*.csv", full.names = TRUE)
portfolioData %<>%
  map_dfr(read_csv, .id = "Asset Name")

#Revalue only accepts character
portfolioData$`Asset Name` %<>%
  plyr::mapvalues(from = c(1:10), to = c("Abbott",
                                         "Amazon",
                                         "Dollar General",
                                         "JPMorg",
                                         "LiveNat",
                                         "Marriott",
                                         "Toyota",
                                         "UNHealth",
                                         "VGETF",
                                         "VGIndex"))

tail(portfolioData)



# Adjusted Closing Prices Plot -------------------------------------------------

#Omit AMZN for consistent Y axis scaling with facet_wrap
adjCloseNoAmz <- portfolioData %>%
  filter(!`Asset Name` %in% "Amazon") %>%
  ggplot(aes(x = `Date`, y = `Adj Close`)) +
  geom_point(size = 0.75) +
  facet_wrap(~`Asset Name`, nrow = 2, ncol = 5) +
  labs(title = "RiD Adjusted Portfolio Assets Closing Price 2016-2021", y = "Adjusted Close (USD)") +
  ggthemes::theme_solarized() 
ggplotly(adjCloseNoAmz)


adjCloseAmz <- portfolioData %>%
  filter(`Asset Name` %in% "Amazon") %>%
  ggplot(aes(x = `Date`, y = `Adj Close`)) +
  geom_point(size = 1) +
  facet_wrap(~`Asset Name`, nrow = 1, ncol = 1) +
  labs(title = "RiD Adjusted Portfolio Assets Closing Price 2016-2021", y = "Adjusted Close (USD)") +
  ggthemes::theme_solarized()

ggplotly(adjCloseAmz)


#Monthly Returns Function ----------------------------------------------------------

#na wont convert to zero
#For some reason ifelse function cycles over my code more than necessary
#dont know how to handle na in function
#Split the data up, apply function to individual lists, then flatten
lagRt <- function(x) {
  (x-lag(x, n = 1))/x
}


#60 mnths 10 assets #Convert to vector?
portfolioDataMthRt <- portfolioData %>%
  split(rep(1:10, each = 60)) %>%
  lapply("[", c("Adj Close")) 


#How should i handle NAs? 
#double flatten to get vector
portfolioData$MthRt <- portfolioDataMthRt %>%
  map(lagRt) %>%
  flatten %>%
  flatten %>%
  unlist 


# Monthly Returns Plot ----------------------------------------------------
mnthReturnPlot <- ggplot(portfolioData, aes(x = Date, y = MthRt)) +
  geom_point(size = 0.75) +
  facet_wrap(~`Asset Name`) +
  labs(title = "Monthly Returns 2016-2021 RiD Portfolio",
       y = "Monthly Returns") +
  ggthemes::theme_solarized()

ggplotly(mnthReturnPlot)



# Average Annual return --------------------------------------------------
#Filter data first
#Used which to match data
#Sorted data by order


#April 01 to Mar 01 #Which to return all matches
annYrs <- c(seq.Date(from = as.Date("2016-04-01"),
                     to = as.Date("2020-04-01"), "years"),
            seq.Date(from = as.Date("2017-03-01"), to = as.Date("2021-03-01"),
                     "years"))


#Must be a dataframe to subset with []
portfolioDataAnnRt <- portfolioData[c(which(portfolioData$Date %in% annYrs)),] %>%
  split(rep(1:10, each = 10)) %>%
  lapply("[", c("Adj Close")) %>%
  flatten %>%
  flatten %>%
  unlist %>%
  as.data.frame 

portfolioDataAnnRt <- portfolioDataAnnRt[c(seq(from = 0, to = 100, by = 2)),]




#Remove every odd point because we want yearly not semi annual returns
ascDate <- annYrs[order(annYrs)]
ascDate <- as.data.frame(ascDate[c(seq(from = 0, to = 10, by = 2))],)
ascDate <- rep(ascDate, times = 10) %>%
  flatten %>%
  flatten %>%
  unlist
ascDate <- as.Date(ascDate, format = "%Y-%m-%d", origin = "1970-01-01")


annAssetNms  <- rep(c("Abbott",
                       "Amazon",
                       "Dollar General",
                       "JPMorg",
                       "LiveNat",
                       "Marriott",
                       "Toyota",
                       "UNHealth",
                       "VGETF",
                       "VGIndex"), each = 5)



#data frame is better than cbind because it can hold multiple data types
portfolioDataAnnRt <- data.frame(annAssetNms,portfolioDataAnnRt, ascDate)
colnames(portfolioDataAnnRt) <- c("Asset Name", "Annual Return", "Date")


# Annual Returns Plot -----------------------------------------------------

#Without Amazon
annReturnPlotNoAmz <-
  portfolioDataAnnRt %>%
  filter(!`Asset Name` %in% "Amazon") %>%
  ggplot(aes(x = Date, y = `Annual Return`)) +
  geom_point(size = 0.75) +
  facet_wrap(~`Asset Name`) +
  labs(title = "Annual Returns 2016-2021 RiD Portfolio",
       y = "Annual Returns") +
  ggthemes::theme_solarized()

ggplotly(annReturnPlotNoAmz)

#With Amazon
annReturnPlotAmz <-
  portfolioDataAnnRt %>%
  filter(`Asset Name` %in% "Amazon") %>%
  ggplot(aes(x = Date, y = `Annual Return`)) +
  geom_point(size = 1) +
  labs(title = "Annual Returns 2016-2021 RiD Portfolio",
       y = "Annual Returns") +
  ggthemes::theme_solarized()

ggplotly(annReturnPlotAmz)

# Average Monthly + Annual Return Table -----------------------------------
#Am i supposed to find the average return for each asset over the past 5 years for both monthly and annual? 
#What is the formula?




# Annual Portfolio Std Deviations ---------------------------------------------
#get annual standard dev for each asset by apply sd to lists of 12

#Annualized Sd == Mnth Returns * Sqrt(12)
sdAnnFun <- function(x){
  sd(x, na.rm = TRUE)*sqrt(12)
}

portfolioDataAnnSd <- portfolioData %>%
  split(rep(1:50, each = 12)) %>%
  lapply("[", c("MthRt"))

#Custom Function because I dont know how to apply sdAnnFun to each element of a sublist
sublistFun <- function(x){
  map(portfolioDataAnnSd[[x]], sdAnnFun)
}

annAssetNmsSd  <- rep(c("Abbott",
                        "Amazon",
                        "Dollar General",
                        "JPMorg",
                        "LiveNat",
                        "Marriott",
                        "Toyota",
                        "UNHealth",
                        "VGETF",
                        "VGIndex"), each = 5)

#Remember to use character and not numeric then parse to date with format and origin specified
annAssetSdYrs <- rep(c("2016-04-01", "2017-04-01", "2018-04-01", "2019-04-01", "2020-04-01"), times = 10) %>%
  as.Date(format = "%Y-%m-%d", origin = "1970-01-01") %>%
  print
class(annAssetSdYrs)


#Issue adding annAssetNmsSd in as.data.frame, handled seperately
portfolioDataAnnSdDf <- map(c(1:50), sublistFun) %>%
  cbind %>%
  unlist %>%
  as.data.frame

portfolioDataAnnSdDf$Asset <- annAssetNmsSd
portfolioDataAnnSdDf$Year <- annAssetSdYrs
  
colnames(portfolioDataAnnSdDf) <- c("Yearly Standard Deviation", "Asset", "Ending Year")

portfolioDataAnnSdDf <- portfolioDataAnnSdDf %>%
  relocate(`Yearly Standard Deviation`, .after = `Asset`)


# Annual Std Dev Plot -----------------------------------------------------

portfolioDataAnnSdPlot <- ggplot(portfolioDataAnnSdDf, aes(x = `Ending Year`, y = `Yearly Standard Deviation`)) +
  geom_point(size = 0.75) +
  facet_wrap(~`Asset`) +
  labs(title = "Annual Std. Dev 2016-2021 RiD Portfolio",
       y = "Annual Returns") +
  ggthemes::theme_solarized()

ggplotly(portfolioDataAnnSdPlot)



# Weighted Monthly Returns ------------------------------------------------
#Use portfolioWeights
#Weighted monthly returns is just weighted return * portfolio weight


wtMthPortfolio <- portfolioWeights[rep(seq_len(nrow(portfolioWeights)), each = 60),]
portfolioData$WtReturn2021 <- portfolioData$MthRt*wtMthPortfolio$`2021`
portfolioData$WtReturn2030 <- portfolioData$MthRt*wtMthPortfolio$`2030`
portfolioData$WtReturn2040 <- portfolioData$MthRt*wtMthPortfolio$`2040`


# WtMonthReturnPlot -------------------------------------------------------

wtMthPortfolioPlot <- ggplot(portfolioData, aes(x = Date)) +
  geom_point(aes(y = `WtReturn2021`,
             colour = "2021")) +
  geom_point(aes(y = `WtReturn2030`,
                 colour = "2030")) +
  geom_point(aes(y = `WtReturn2040`,
                 colour = "2040")) +
  facet_wrap(~`Asset Name`) +
  labs(y = "Weighted Return",
       colour = "Year")
ggplotly(wtMthPortfolioPlot)  



# Weighted Yearly Returns -------------------------------------------------
#same as monthly..


wtYrPortfolio <- portfolioWeights[rep(seq_len(nrow(portfolioWeights)), each = 5),]


portfolioDataAnnRt$WtReturn2021 <- portfolioDataAnnRt$`Annual Return`*wtYrPortfolio$`2021`
portfolioDataAnnRt$WtReturn2030 <- portfolioDataAnnRt$`Annual Return`*wtYrPortfolio$`2030`
portfolioDataAnnRt$WtReturn2040 <- portfolioDataAnnRt$`Annual Return`*wtYrPortfolio$`2040`

colnames(portfolioDataAnnRt)
# wtYearReturnPlot --------------------------------------------------------
#Without Amazon

wtYrPortfolioPlotNoAmz <- 
  portfolioDataAnnRt %>%
  filter(!`Asset Name` %in% "Amazon") %>%
  ggplot(aes(x = Date)) +
  geom_point(aes(y = `WtReturn2021`,
                 colour = "2021")) +
  geom_point(aes(y = `WtReturn2030`,
                 colour = "2030")) +
  geom_point(aes(y = `WtReturn2040`,
                 colour = "2040")) +
  facet_wrap(~`Asset Name`) +
  labs(y = "Weighted Return",
       colour = "Year")
ggplotly(wtYrPortfolioPlotNoAmz)  

#with amazon

wtYrPortfolioPlotAmz <- 
  portfolioDataAnnRt %>%
  filter(`Asset Name` %in% "Amazon") %>%
  ggplot(aes(x = Date)) +
  geom_point(aes(y = `WtReturn2021`,
                 colour = "2021")) +
  geom_point(aes(y = `WtReturn2030`,
                 colour = "2030")) +
  geom_point(aes(y = `WtReturn2040`,
                 colour = "2040")) +
  labs(y = "Weighted Return",
       colour = "Year")
ggplotly(wtYrPortfolioPlotAmz)  


# Annual Portfolio Standard Devs ------------------------------------------
portfolioDataAnnSdDf$wtDev2021 <- portfolioDataAnnSdDf$`Yearly Standard Deviation`*wtYrPortfolio$`2021`
portfolioDataAnnSdDf$wtDev2030 <- portfolioDataAnnSdDf$`Yearly Standard Deviation`*wtYrPortfolio$`2030`
portfolioDataAnnSdDf$wtDev2040 <- portfolioDataAnnSdDf$`Yearly Standard Deviation`*wtYrPortfolio$`2040`

# wtYearStdDevPlot --------------------------------------------------------
#Without Amazon

wtYrPortfolioPlotAnnSdNoAmz <- 
  portfolioDataAnnSdDf %>%
  filter(!`Asset` %in% "Amazon") %>%
  ggplot(aes(x = `Ending Year`)) +
  geom_point(aes(y = `wtDev2021`,
                 colour = "2021")) +
  geom_point(aes(y = `wtDev2030`,
                 colour = "2030")) +
  geom_point(aes(y = `wtDev2040`,
                 colour = "2040")) +
  facet_wrap(~`Asset`) +
  labs(y = "Weighted Std. Dev",
       colour = "Year")
ggplotly(wtYrPortfolioPlotAnnSdNoAmz)  

#with amazon

wtYrPortfolioPlotAnnSdAmz <- 
  portfolioDataAnnSdDf %>%
  filter(`Asset` %in% "Amazon") %>%
  ggplot(aes(x = `Ending Year`)) +
  geom_point(aes(y = `wtDev2021`,
                 colour = "2021")) +
  geom_point(aes(y = `wtDev2030`,
                 colour = "2030")) +
  geom_point(aes(y = `wtDev2040`,
                 colour = "2040")) +
  facet_wrap(~`Asset`) +
  labs(y = "Weighted Std. Dev",
       colour = "Year")
ggplotly(wtYrPortfolioPlotAnnSdAmz)  
