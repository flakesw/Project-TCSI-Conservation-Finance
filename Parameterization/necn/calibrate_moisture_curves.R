moisturecurve2 <- 0.1
moisturecurve3 <- 1

soilwatercontent <- 0.2
Ratio_AvailWaterToPET <- seq(0, 2, length.out = 1000)

intercept = moisturecurve2 * soilwatercontent
          
slope <- 1.0 / (moisturecurve3 - intercept)
               
WaterLimit = 1.0 + slope * (Ratio_AvailWaterToPET - moisturecurve3)

WaterLimit <- ifelse(WaterLimit > 1, 1, WaterLimit)
WaterLimit <- ifelse(WaterLimit < 0.01, 0.01, WaterLimit)

plot(WaterLimit ~ Ratio_AvailWaterToPET)

