
# Install Packages --------------------------------------------------------


pkgReq <- c("tidyverse",
            "colorspace",
            "crosstalk",
            "here",
            "lubridate",
            "plotly",
            "RColorBrewer",
            "readxl",
            "scales"
            )
lapply(pkgReq, require, character.only = TRUE)

# Create Asset Weights ------------------------------------------------------
asset <- 
  tibble(
  x = c("AbbottLabs",
                "Amazon",
                "DollarGeneral",
                "JPMorganChase",
                "LiveNation",
                "Marriott",
                "Toyota",
                "UnitedHealth",
                "VGTechETF",
                "VG500IndexFund"),
  
  )

levelsStock <- as.factor(c("AbbottLabs",
                           "Amazon",
                           "DollarGeneral",
                           "JPMorganChase",
                           "LiveNation",
                           "Marriott",
                           "Toyota",
                           "UnitedHealth"))

levelsIndexEtf <- as.factor(c("VGTechETF",
                              "VG500IndexFund"))

# Weights Randomizer ------------------------------------------------------
#Randomize weights for each asset class to create balanced stock picks and to hedge against picking and staying with any one bad stock long run
#Downside is a good stock pick may not be held over time
weightsRandomizerStock <- as.tibble((runif(8, min = 1, max = 2)))
weightsRandomizerETFIndex <- as.tibble((runif(2, min = 1, max = 2)))
#2021 80% Stocks 20% ETFs + Index
weightsStock2021 <- (weightsRandomizer/sum(weightsRandomizer))*.8
weightsEtfIndex2021 <- (weightsRandomizerETFIndex/sum(weightsRandomizerETFIndex))*.2
#2030 65% Stocks 35% ETFs + Index
weightsStock2030 <- (weightsRandomizer/sum(weightsRandomizer))*.65
weightsEtfIndex2030 <- (weightsRandomizerETFIndex/sum(weightsRandomizerETFIndex))*.35
#2040 30% Stocks 70% ETFs + Index
weightsStock2040 <- (weightsRandomizer/sum(weightsRandomizer))*.30
weightsEtfIndex2040 <- (weightsRandomizerETFIndex/sum(weightsRandomizerETFIndex))*.70

weightsStockAll <- cbind(weightsStock2021, weightsStock2030, weightsStock2040)
colnames(weightsAllStockYrs) <- c("2021", "2030", "2040")

weightsAllEtfIndexAll <- cbind(weightsEtfIndex2021, weightsEtfIndex2030, weightsEtfIndex2040)
colnames(weightsAllEtfIndexYrs) <- c("2021", "2030", "2040")

#Master Portfolio Weights
portfolioStock <- cbind(levelsStock, weightsStockAll)
colnames(portfolioStock) <- c("Asset","2021", "2030", "2040")

portfolioEtfIndex <- cbind(levelsIndexEtf, weightsAllEtfIndexAll)
colnames(portfolioEtfIndex) <- c("Asset","2021", "2030", "2040")
portfolioMaster <- rbind(portfolioStock, portfolioEtfIndex)

# Create Factor Levels for Sorting ----------------------------------------



# Plot Asset Weights ------------------------------------------------------

assetWeight2021 <- ggplot(asset2021, aes(x = asset, y = assetWeights)) +
  geom_bar(stat = 'identity',
           fill = "cadetblue3") +
  labs(x = "Asset",
       y = "Asset Weight",
       title = "2021 Asset Weights for myOverpricedTM Retirement Fund"
  ) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16))
print(assetWeight2021)

assetWeight2030 <- ggplot(asset2030, aes(x = asset, y = assetWeights)) +
  geom_bar(stat = 'identity',
           fill = "cadetblue3") +
  labs(x = "Asset",
       y = "Asset Weight",
       title = "2030 Asset Weights for myOverpricedTM Retirement Fund"
  ) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16))

print(assetWeight2030)

assetWeight2040 <- ggplot(asset2040, aes(x = asset, y = assetWeights)) +
  geom_bar(stat = 'identity',
           fill = "cadetblue3") +
  labs(x = "Asset",
       y = "Asset Weight",
       title = "2040 Asset Weights for myOverpricedTM Retirement Fund"
  ) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16))

print(assetWeight2040)


# Plot Asset Factors ------------------------------------------------------


