# explore fire spread equations

inv.logit <- function(x) exp(x)/(1+exp(x))

log_odds <- B0 + B1 * FWI + B2 * FF + B3 * WS

p <- inv.logit(log_odds)

clim_data <- read.csv("./calibration data/climate/Climate_10_regions_historical.csv")

max(clim_data$FWI)
max(clim_data$windspeed) #effective windspeed varies up to 45 historically

#original parameters
B0 <- -35
B1 <- 0.62
B2 <- 0.52
B3 <- 0.31

#parameters from Scheller et al
B0 <- -36.0 
B1 <- 0.6 
B2 <- 0.022
B3 <- 0.915 

FWI <- seq(0, 85, length.out = 1000)
FF <- 0.5
WS <- 20


log_odds <- B0 + B1 * FWI + B2 * FF + B3 * WS 

plot(inv.logit(log_odds) ~ FWI)


FWI <- 30
FF <- seq(0, 1, length.out = 1000)
WS <- 10

log_odds <- B0 + B1 * FWI + B2 * FF + B3 * WS 

plot(inv.logit(log_odds) ~ FF)



#testing
B0 <- -13
B1 <- 0.16
B2 <- 5.0
B3 <- 0.2 

FWI <- seq(1, 80, length.out = 1000)
FF <- 0.1
WS <- 20

log_odds <- B0 + B1 * FWI + B2 * FF + B3 * WS 

plot(inv.logit(log_odds) ~ FWI)

