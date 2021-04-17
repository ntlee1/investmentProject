
# Required Packages --------------------------------------------------------

pkgReq <- c("tidyverse",
            "colorspace",
            "crosstalk",
            "here",
            "lubridate",
            "plotly",
            "RColorBrewer",
            "readxl",
            "scales",
            "ggthemes",
            "magrittr",
            "rlist"
            )
lapply(pkgReq, require, character.only = TRUE)

# Create Asset Weights ------------------------------------------------------
asset <- 
  tibble(
    x = c("Abbott",
          "Amazon",
          "Dollar General",
          "JPMorg",
          "LiveNat",
          "Marriott",
          "Toyota",
          "UNHealth",
          "VGETF",
          "VG500Index"),
  
  )

levelsStock <- as.factor(c("Abbott",
                           "Amazon",
                           "Dollar General",
                           "JPMorg",
                           "LiveNat",
                           "Marriott",
                           "Toyota",
                           "UNHealth"))

levelsIndexEtf <- as.factor(c("VGETF",
                              "VG500Index"))

# Weights Randomizer ------------------------------------------------------
#Randomize weights for each asset class to create balanced stock picks and to hedge against picking and staying with any one bad stock long run
#Downside is a good stock pick may not be held over time
weightsRandomizerStock <- as.tibble((runif(8, min = 1, max = 2)))
weightsRandomizerETFIndex <- as.tibble((runif(2, min = 1, max = 2)))
#2021 80% Stocks 20% ETFs + Index
weightsStock2021 <- (weightsRandomizerStock/sum(weightsRandomizerStock))*.8
weightsEtfIndex2021 <- (weightsRandomizerETFIndex/sum(weightsRandomizerETFIndex))*.2
#2030 65% Stocks 35% ETFs + Index
weightsStock2030 <- (weightsRandomizerStock/sum(weightsRandomizerStock))*.65
weightsEtfIndex2030 <- (weightsRandomizerETFIndex/sum(weightsRandomizerETFIndex))*.35
#2040 30% Stocks 70% ETFs + Index
weightsStock2040 <- (weightsRandomizerStock/sum(weightsRandomizerStock))*.30
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

portfolioWeights <- rbind(portfolioStock, portfolioEtfIndex)



# Plot Asset Weights ------------------------------------------------------
plotWeights <- ggplot(portfolioWeights, aes(x = Asset)) +
  geom_point(aes(y = `2021`, colour = "2021"),
             size = 4) +
  geom_point(aes(y = `2030`, colour = "2030"),
             size = 4) +
  geom_point(aes(y = `2040`,colour = "2040"),
             size = 4) +
  scale_colour_manual(name = "Year", values = c(`2021` = "red", `2030` = "steelblue", `2040` = "springgreen4")) +
  labs(y = "Asset Weight",
       title = "RetireinDebt 2021-2040 Portfolio Asset Weights",
       subtitle = "RiD uses a randomized weights algorithm to hedge against holding onto underperforming assets in the long run") +
  ggthemes::theme_solarized() 


ggplotly(plotWeights, dynamicTicks = TRUE) %>%
  layout(title = list(text = paste0("RetireinDebt 2021-2040 Portfolio Asset Weights",
                                    "<br>",
                                    "<sup>",
                                    "RiD uses a randomized weights algorithm to hedge against holding onto underperforming assets in the long run")))




