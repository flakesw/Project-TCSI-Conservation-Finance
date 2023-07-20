moisturecurve2 <- 1
moisturecurve3 <- 1.5

soilwatercontent <- 5
Ratio_AvailWaterToPET <- seq(0, 5, length.out = 1000)

intercept = moisturecurve2 * soilwatercontent
          
slope <- 1.0 / (moisturecurve3 - intercept)
               
WaterLimit = 1.0 + slope * (Ratio_AvailWaterToPET - moisturecurve3)

plot(WaterLimit ~ Ratio_AvailWaterToPET)
