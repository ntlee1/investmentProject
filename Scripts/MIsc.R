
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
mthRt <- function(x) {
  (x-lag(x, n = 1))/x
}


#60 mnths 10 assets #Convert to vector?
portfolioDataMthRt <- portfolioData %>%
  split(rep(1:10, each = 60)) %>%
  lapply("[", c("Adj Close")) 


#How should i handle NAs? 
#double flatten to get vector
portfolioData$MthRt <- portfolioDataMthRt %>%
  map(mthRt) %>%
  flatten %>%
  flatten %>%
  unlist 







# EDIT: Failed attempt at creating for loop to iteratively cycle through args [3,4] of assetReturnList() -----------------------------------------------------------------


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
#Cant get mean to work, workaround used
#Actually i need average monthly return for each year

unique(allMnthClose$id)
avgMthReturnFilter <- list((((filter(portfolioData, Date == "2017-03-01")[7]) - (filter(portfolioData, Date == "2016-04-01")[7]))/(filter(portfolioData, Date == "2016-04-01")[7]))/12,
                           (((filter(portfolioData, Date == "2018-03-01")[7]) - (filter(portfolioData, Date == "2017-04-01")[7]))/(filter(portfolioData, Date == "2017-04-01")[7]))/12,
                           (((filter(portfolioData, Date == "2019-03-01")[7]) - (filter(portfolioData, Date == "2018-04-01")[7]))/(filter(portfolioData, Date == "2018-04-01")[7]))/12,
                           (((filter(portfolioData, Date == "2020-03-01")[7]) - (filter(portfolioData, Date == "2019-04-01")[7]))/(filter(portfolioData, Date == "2019-04-01")[7]))/12,
                           (((filter(portfolioData, Date == "2021-03-01")[7]) - (filter(portfolioData, Date == "2020-04-01")[7]))/(filter(portfolioData, Date == "2020-04-01")[7]))/12)


avgMthReturnFilterTbl <- rbind(avgMthReturnFilter[[1]],
                               avgMthReturnFilter[[2]],
                               avgMthReturnFilter[[3]],
                               avgMthReturnFilter[[4]],
                               avgMthReturnFilter[[5]])
avgMthReturnFilterTbl$Asset <- c(levels(allMnthClose$id))
avgMthReturnFilterTbl$Year <- c(rep(c(2017, 2018, 2019, 2020, 2021), times = c(10, 10, 10, 10, 10)))
colnames(avgMthReturnFilterTbl)[1] <- c("Average Monthly Return")

#Plot
avgAnnualReturnFilterPlot <- ggplot(avgMthReturnFilterTbl, aes(x = Year, y = `Average Monthly Return`)) +
  geom_point() +
  facet_wrap(~Asset) +
  labs(title = "Average Monthly Return RiD Portfolio") +
  ggthemes::theme_solarized()

ggplotly(avgAnnualReturnFilterPlot, dynamicTicks = TRUE) %>%
  layout(title = list(text = paste0("Average Monthly Return RiD Portfolio (Year Start April 01)")))


#Average Annual Return -----------------------------------------------------------
#Simplify
avgAnnualReturnFilter <- list(((filter(portfolioData, Date == "2017-03-01")[7] - filter(portfolioData, Date == "2016-04-01")[7])/filter(portfolioData, Date == "2016-04-01")[7]),
                              ((filter(portfolioData, Date == "2018-03-01")[7] - filter(portfolioData, Date == "2017-04-01")[7])/filter(portfolioData, Date == "2017-04-01")[7]),
                              ((filter(portfolioData, Date == "2019-03-01")[7] - filter(portfolioData, Date == "2018-04-01")[7])/filter(portfolioData, Date == "2018-04-01")[7]),
                              ((filter(portfolioData, Date == "2020-03-01")[7] - filter(portfolioData, Date == "2019-04-01")[7])/filter(portfolioData, Date == "2019-04-01")[7]),
                              ((filter(portfolioData, Date == "2021-03-01")[7] - filter(portfolioData, Date == "2020-04-01")[7])/filter(portfolioData, Date == "2020-04-01")[7]))

avgAnnualReturnTbl <- rbind(avgAnnualReturnFilter[[1]],
                            avgAnnualReturnFilter[[2]],
                            avgAnnualReturnFilter[[3]],
                            avgAnnualReturnFilter[[4]],
                            avgAnnualReturnFilter[[5]])
avgAnnualReturnTbl$Asset <- c(levels(allMnthClose$id))
avgAnnualReturnTbl$Year <- c(rep(c(2017, 2018, 2019, 2020, 2021), times = c(10, 10, 10, 10, 10)))
colnames(avgAnnualReturnTbl)[1] <- c("Average Annual Return")

#Plot
avgAnnualReturnPlot <- ggplot(avgAnnualReturnTbl, aes(x = Year, y = `Average Annual Return`)) +
  geom_point() +
  facet_wrap(~Asset) +
  labs(title = "Average Annual Return RiD Portfolio") +
  ggthemes::theme_solarized()

ggplotly(avgAnnualReturnPlot, dynamicTicks = TRUE) %>%
  layout(title = list(text = paste0("Average Annual Return RiD Portfolio (Year Start April 01)")))

# Annual Portfolio Std Deviations ---------------------------------------------
#Mnth Returns * Sqrt(12)
#Maybe add monthly return for mar 01 2016
#EDIT mod workaround
allMnthCloseMod <- allMnthClose
allMnthCloseMod[600,] <- allMnthClose[599,]
allMnthCloseMod[600, 2] <- as.Date("2021-03-01")

annPortfolioSdSplit <- split(allMnthCloseMod, rep(1:50, each = 12))


#comical sd workaround learn purrr
testy <- sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`1`[9])))))

annPortfolioSdList <- list(sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`1`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`2`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`3`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`4`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`5`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`6`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`7`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`8`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`9`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`10`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`11`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`12`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`13`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`14`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`15`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`16`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`17`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`18`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`19`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`20`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`21`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`22`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`23`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`24`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`25`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`26`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`27`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`28`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`29`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`30`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`31`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`32`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`33`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`34`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`35`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`36`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`37`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`38`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`39`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`40`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`41`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`42`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`43`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`44`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`45`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`46`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`47`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`48`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`49`[9]))))),
                           sd(as.numeric(unlist(transpose(list(annPortfolioSdSplit$`50`[9])))))
)

unique(portfolioData$id)
annPortfolioSdListYrs <-  c(rep(c(2017, 2018, 2019, 2020, 2021), times = c(10)))
annPortfolioSdListNms <- c(rep(c("Abbott",
                                 "Amazon",
                                 "Dollar General",
                                 "JPMorg",
                                 "LiveNat",
                                 "Marriott",
                                 "Toyota",
                                 "UNHealth",
                                 "VGETF",
                                 "VGIndex"), times = c(10)))

annPortfolioSdListNms <- as.character(unlist(annPortfolioSdListNms))
annPortfolioSdList <-  as.numeric(unlist(annPortfolioSdList))
annPortfolioSdListYrs <- as.numeric(unlist(annPortfolioSdListYrs))
annPortfolioSdFinal <- cbind(annPortfolioSdListNms, annPortfolioSdList, annPortfolioSdListYrs)
annPortfolioSdFinal <- unlist(annPortfolioSdFinal)
annPortfolioSdFinal <- as.tibble(annPortfolioSdFinal)
annPortfolioSdFinal$annPortfolioSdListYrs <- as.Date(annPortfolioSdFinal$annPortfolioSdListYrs, format = "%Y")
annPortfolioSdFinal
































