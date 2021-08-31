# process outputs for the subset landscape

library("raster")
library("tidyverse")
library("sf")

## SCRPPLE outputs
setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance/subset_scenario3")

# setwd("./subset_scenario/social-climate-fire")

fire_summary <- read.csv("./scrapple-summary-log.csv")
events <- read.csv("./scrapple-events-log.csv")

subset_poly <- sf::st_read("../masks_boundaries/subset_polygon/subset_polygon.shp")

test <- raster("social-climate-fire/fire-intensity-33.img")
plot(test)
# test2 <- test
# values(test2) <- ifelse(values(test > 1), 1, 0)
# test3 <- raster::clump(test2)
# plot(test3)
#TODO convert to points, find concave hull
  

plot(events$TotalBiomassMortality ~ events$MeanFWI) # fires only kill trees at high FWI
plot(events$MeanSpreadProbability ~ events$MeanFWI) #really high fire spread at high FWI
plot(events$MeanDNBR ~ events$MeanFWI) #DNBR is not very sensitive to FWI, except for three high-severity fires?

#number of cells that burned -- matches summary file more or less
# cellStats(test, function(i, ...) sum(i >= 2)) # larger than summary by 2 for some timesteps?

fire_summary$TotalBurnedSites <- fire_summary$TotalBurnedSitesAccidental + 
                                    fire_summary$TotalBurnedSitesLightning + 
                                      fire_summary$TotalBurnedSitesRx
fire_summary$TotalFires <- fire_summary$NumberFiresAccidental + 
                                       fire_summary$NumberFiresLightning + 
                                       fire_summary$NumberFiresRx
(fire_summary$TotalBurnedSites / fire_summary$TotalFires) #average area burned per fire by year

#summarize short data for study area
# short <- sf::st_read("../calibration data/short/Data/FPA_FOD_20210617.gdb", layer = "Fires")
# tcsi_polygon <- sf::st_read("../masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
#   st_transform(crs(short))
# 
# short_tcsi <- st_intersection(short, sf::st_make_valid(tcsi_polygon))
# 
# #write as RDS; shapefile generates errors and cannot write as a .gdb file
# saveRDS(short_tcsi, "../calibration data/short_tcsi/short_tcsi.RDS")

short_tcsi <- readRDS("../calibration data/short_tcsi/short_tcsi.RDS") %>%
  # st_transform(crs(subset_poly)) %>%
  # st_intersection(subset_poly) %>%
  dplyr::filter(FIRE_SIZE >= 8) #filter to size of a cell, in acres %>%

short_ca <- readRDS("../calibration data/short_ca.RDS") %>%
  dplyr::filter(FIRE_SIZE >= 8)

area_tcsi <- sf::st_read("../masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  sf::st_area()

area_ca <- sf::st_read("../masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  summarise(do.union = TRUE) %>% 
  st_make_valid() %>%
  smoothr::fill_holes(threshold = 0.5) %>%
  st_transform(crs(short_ca)) %>%
  sf::st_area()
area_subset <- sf::st_area(subset_polygon)
subset_proportion_tcsi <- area_subset / area_tcsi
subset_proportion_ca <- area_subset / area_ca

#Fires per year
hist(short_tcsi$FIRE_YEAR)
short_tcsi_by_year <- stats::aggregate(short_tcsi$FOD_ID, 
                                       by = list(short_tcsi$FIRE_YEAR), FUN = length)
short_ca_by_year <- stats::aggregate(short_ca$FOD_ID, 
                                       by = list(short_ca$FIRE_YEAR), FUN = length)


#too few fires by an order of magnitude, or way too many fires
hist(fire_summary$TotalFires)
hist(short_tcsi_by_year$x)
hist(short_ca_by_year$x)

short_tcsi_by_year_type <- stats::aggregate(short_tcsi, 
                                            by = list(short_tcsi$FIRE_YEAR, short_tcsi$NWCG_CAUSE_CLASSIFICATION), FUN = length)
short_ca_by_year_type <- stats::aggregate(short_ca, 
                                          by = list(short_ca$FIRE_YEAR, short_ca$NWCG_CAUSE_CLASSIFICATION), FUN = length)

# mean number of fires is way too high without adjustment to parameters
# slightly too high after correcting for area size
hist(fire_summary$NumberFiresAccidental)
mean(fire_summary$NumberFiresAccidental)
var(fire_summary$NumberFiresAccidental)
hist(short_tcsi_by_year_type[short_tcsi_by_year_type$Group.2 == "Human", ]$FOD_ID* subset_proportion_tcsi)
mean(short_tcsi_by_year_type[short_tcsi_by_year_type$Group.2 == "Human", ]$FOD_ID) * subset_proportion_tcsi
var(short_tcsi_by_year_type[short_tcsi_by_year_type$Group.2 == "Human", ]$FOD_ID * subset_proportion_tcsi)
hist(short_ca_by_year_type[short_ca_by_year_type$Group.2 == "Human", ]$FOD_ID* subset_proportion_ca)
mean(short_ca_by_year_type[short_ca_by_year_type$Group.2 == "Human", ]$FOD_ID) * subset_proportion_ca
var(short_ca_by_year_type[short_ca_by_year_type$Group.2 == "Human", ]$FOD_ID * subset_proportion_ca)

# mean number of fires is way too high without adjustment to parameters
# slightly too high after correcting for area size
hist(fire_summary$NumberFiresLightning)
mean(fire_summary$NumberFiresLightning)
hist(short_tcsi_by_year_type[short_tcsi_by_year_type$Group.2 == "Natural", ]$FOD_ID* subset_proportion_tcsi)
mean(short_tcsi_by_year_type[short_tcsi_by_year_type$Group.2 == "Natural", ]$FOD_ID) * subset_proportion_tcsi
hist(short_ca_by_year_type[short_tcsi_by_year_type$Group.2 == "Natural", ]$FOD_ID* subset_proportion_ca)
mean(short_ca_by_year_type[short_tcsi_by_year_type$Group.2 == "Natural", ]$FOD_ID) * subset_proportion_ca

#area burned?
hist(fire_summary$TotalBurnedSitesAccidental/3.24) #convert to ha
mean(fire_summary$TotalBurnedSitesAccidental/3.24)
hist(short_tcsi_by_year_type[short_tcsi_by_year_type$Group.2 == "Human", ]$FIRE_SIZE * 2.47* subset_proportion_tcsi) #convert to ha
mean(short_tcsi_by_year_type[short_tcsi_by_year_type$Group.2 == "Human", ]$FIRE_SIZE * 2.47)* subset_proportion_tcsi
hist(short_ca_by_year_type[short_ca_by_year_type$Group.2 == "Human", ]$FIRE_SIZE * 2.47* subset_proportion_ca) #convert to ha
mean(short_ca_by_year_type[short_ca_by_year_type$Group.2 == "Human", ]$FIRE_SIZE * 2.47)* subset_proportion_ca

hist(fire_summary$TotalBurnedSitesLightning/3.24) #convert to ha
mean(fire_summary$TotalBurnedSitesLightning/3.24)
hist(short_tcsi_by_year_type[short_tcsi_by_year_type$Group.2 == "Natural", ]$FIRE_SIZE * 2.47) #convert to ha
mean(short_tcsi_by_year_type[short_tcsi_by_year_type$Group.2 == "Natural", ]$FIRE_SIZE * 2.47)
hist(short_ca_by_year_type[short_ca_by_year_type$Group.2 == "Natural", ]$FIRE_SIZE * 2.47) #convert to ha
mean(short_ca_by_year_type[short_ca_by_year_type$Group.2 == "Natural", ]$FIRE_SIZE * 2.47)

#area burned per fire
hist(events$TotalSitesBurned/3.24) #convert to ha
hist(short_tcsi$FIRE_SIZE * 2.47)
hist(short_ca$FIRE_SIZE * 2.47)
