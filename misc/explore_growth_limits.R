library("tidyverse")
necn7 <- read.csv("C:/Users/Sam/Documents/Research/Extension-NECN-Succession/testing/Core7-NECN7-SingleCell/NECN-succession-monthly-log.csv")
necn6 <- read.csv("C:/Users/Sam/Documents/Research/Extension-NECN-Succession/testing/Core7-NECN6.9-SingleCell/NECN-succession-monthly-log.csv")

necn <- necn7 %>%
  left_join(necn6, by = c("Time", "Month")) %>%
  filter(Time < 10)
plot(necn$SoilWaterContent.x, type = "l", col = "blue",
     xlab = "month_sequence",
     ylab = "Soil moisture (cm)",
     ylim = c(-2, 10))
lines(necn$SoilWaterContent.y, type = "l", col = "red")



necn <- read.csv("C:/Users/Sam/Documents/Research/Extension-NECN-Succession/testing/Core7-NECN7-SingleCell/NECN-calibrate-log.csv")
necn6 <- read.csv("C:/Users/Sam/Documents/Research/Extension-NECN-Succession/testing/Core7-NECN6.9-SingleCell/NECN-calibrate-log.csv")
necn7$row <- as.character(1:nrow(necn7))
necn6$row <- as.character(1:nrow(necn6))
necn <- necn6 %>%
  # left_join(necn6, by = c("row")) %>%
  filter(Year < 5)
plot(necn$GrowthLimitSoilWater.x, type = "l", col = "blue",
     xlab = "month_sequence",
     ylab = "Soil moisture (cm)")
lines(necn$GrowthLimitSoilWater.y, type = "l", col = "red")

plot(necn$GrowthLimitSoilWater.x + rnorm(nrow(necn), 0, 0.02) ~ I(necn$GrowthLimitSoilWater.y + rnorm(nrow(necn), 0, 0.02)))

sum(necn$GrowthLimitSoilWater.x)
sum(necn$GrowthLimitSoilWater.y)


necn <- read.csv("C:/Users/Sam/Documents/Research/Extension-NECN-Succession/testing/Core7-NECN7-SingleCell/NECN-calibrate-log.csv") %>%
  filter(Year <= 5)
climate <- read.csv("C:/Users/Sam/Documents/Research/Extension-NECN-Succession/testing/Core7-NECN7-SingleCell/NECN-succession-monthly-log.csv") %>%
  filter(Time <= 5)
necn <- cbind(necn, climate)
plot(necn$GrowthLimitSoilWater ~ necn$AirTemp)
plot(necn$GrowthLimitSoilWater+ rnorm(n = nrow(necn), 0, 0.02) ~ 
       I(necn$meanSoilWaterContent + rnorm(n = nrow(necn), 0, 0.02)))
plot(necn$GrowthLimitSoilWater+ rnorm(n = nrow(necn), 0, 0.02) ~ 
       I(necn$PET + rnorm(n = nrow(necn), 0, 0.02)))
plot(necn$GrowthLimitT ~ necn$AirTemp)
plot(necn$GrowthLimitLAI ~ necn$SiteLAI)
plot(necn$GrowthLimitLAIcompetition ~ necn$SiteLAI)

plot(necn$GrowthLimitT, type = "l", ylim = c(0,1))
lines(necn$GrowthLimitSoilWater, type = "l", col = "blue")
lines(necn$GrowthLimitN, col = "red")
lines(necn$GrowthLimitLAI, col = "orange")
lines(necn$GrowthLimitLAIcompetition, col = "green")
