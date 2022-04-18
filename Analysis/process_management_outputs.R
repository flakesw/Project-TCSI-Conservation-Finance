#Process management intensity layers


library("raster")
library("tidyverse")
library("sf")
library("vioplot")

subset_landscape <- TRUE

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance/")



mgmt_dir <- "D:/Data/TCSI/calibrating fire spread/Scenario1 - test spread - run 1/harvest/"

harvest_list <- list.files(mgmt_dir, pattern = "biomass")

matches <- regmatches(list.files(mgmt_dir, pattern = "biomass"), gregexpr("[[:digit:]]+", list.files(mgmt_dir, pattern = "biomass"))) %>%
  unlist() %>% 
  as.numeric()
harvest_list <- harvest_list[order(matches)]

harvest_stack <- stack(paste0(mgmt_dir, harvest_list))

total_biomass <- sum(harvest_stack)

