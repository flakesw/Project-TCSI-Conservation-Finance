#Process management intensity layers
library("terra")
library("tidyverse")
library("sf")
library("vioplot")


mgmt_dir <- "E:/TCSI LANDIS/Scenario3 - historical - run1/harvest/"

harvest_list <- list.files(mgmt_dir, pattern = "biomass")

matches <- regmatches(list.files(mgmt_dir, pattern = "biomass"), gregexpr("[[:digit:]]+", list.files(mgmt_dir, pattern = "biomass"))) %>%
  unlist() %>% 
  as.numeric()
harvest_list <- harvest_list[order(matches)]

harvest_stack <- terra::rast(paste0(mgmt_dir, harvest_list))

total_biomass_1 <- terra::tapp(harvest_stack, index=1, fun=sum)

plot(total_biomass_1)



# setwd("")

mgmt_dir <- "E:/tcsi_for_nick/Scenario6  - historical - Run 1/harvest/"

harvest_list <- list.files(mgmt_dir, pattern = "biomass")

matches <- regmatches(list.files(mgmt_dir, pattern = "biomass"), gregexpr("[[:digit:]]+", list.files(mgmt_dir, pattern = "biomass"))) %>%
  unlist() %>% 
  as.numeric()
harvest_list <- harvest_list[order(matches)]

harvest_stack <- terra::rast(paste0(mgmt_dir, harvest_list))

total_biomass6 <- terra::tapp(harvest_stack, index=1, fun=sum)

plot(total_biomass6)







