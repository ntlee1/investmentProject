
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


portfolioDataAnnRt <- portfolioData[c(which(portfolioData$Date %in% annYrs)),] %>%
  split(rep(1:10, each = 10)) %>%
  lapply("[", c("Adj Close")) %>%
  map(lagRt) %>%
  flatten %>%
  flatten %>%
  unlist 

ascDate <- annYrs[order(annYrs)] %>%
  rep(times = 10)

annAssetNms  <- rep(c("Abbott",
                       "Amazon",
                       "Dollar General",
                       "JPMorg",
                       "LiveNat",
                       "Marriott",
                       "Toyota",
                       "UNHealth",
                       "VGETF",
                       "VGIndex"), each = 10)



#data frame is better than cbind because it can hold multiple data types
portfolioDataAnnRt <- data.frame(annAssetNms,portfolioDataAnnRt, ascDate)
colnames(portfolioDataAnnRt) <- c("Asset Name", "Annual Return", "Date")


# Annual Returns Plot -----------------------------------------------------

annReturnPlot <- ggplot(portfolioDataAnnRt, aes(x = Date, y = `Annual Return`)) +
  geom_point(size = 0.75) +
  facet_wrap(~`Asset Name`) +
  labs(title = "Annual Returns 2016-2021 RiD Portfolio",
       y = "Annual Returns") +
  ggthemes::theme_solarized()

ggplotly(annReturnPlot)



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

#Apply sd to each list after converting to dbl



















