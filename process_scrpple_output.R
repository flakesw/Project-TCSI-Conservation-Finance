# process outputs for the subset landscape

library("raster")

## SCRPPLE outputs
setwd("C:/Users/Sam/Documents/Research/California forest economics/subset_scenario")

# setwd("./subset_scenario/social-climate-fire")

fire_summary <- read.csv("./scrapple-summary-log.csv")
events <- read.csv("./scrapple-events-log.csv")

subset_poly <- sf::st_read("../masks_boundaries/subset_polygon/subset_polygon.shp")

# test <- raster("social-climate-fire/fire-intensity-29.img")
# plot(test)

plot(events$TotalBiomassMortality ~ events$MeanFWI) # fires only kill trees at high FWI

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
  st_transform(crs(subset_poly)) %>%
  st_intersection(subset_poly) %>%
  dplyr::filter(FIRE_SIZE > 3.24) #filter to half the size of a cell, in acres

#Fires per year
hist(short_tcsi$FIRE_YEAR)
short_tcsi_by_year <- stats::aggregate(short_tcsi$FOD_ID, 
                                       by = list(short_tcsi$FIRE_YEAR), FUN = length)

#too few fires by an order of magnitude, or way too many fires
hist(fire_summary$TotalFires)
hist(short_tcsi_by_year$FOD_ID)

short_tcsi_by_year_type <- stats::aggregate(short_tcsi, 
                                            by = list(short_tcsi$FIRE_YEAR, short_tcsi$NWCG_CAUSE_CLASSIFICATION), FUN = length)

#way too few accidental fires, ~3 times too few for subset landscape with all fires,
#but 8 times too many for fires filtered by size
hist(fire_summary$NumberFiresAccidental)
mean(fire_summary$NumberFiresAccidental)
hist(short_tcsi_by_year_type[short_tcsi_by_year_type$Group.2 == "Human", ]$FOD_ID)
mean(short_tcsi_by_year_type[short_tcsi_by_year_type$Group.2 == "Human", ]$FOD_ID)

#lightning fires are pretty accurate if filtering at >3.24; filter at >4 includes very few fires
hist(fire_summary$NumberFiresLightning)
mean(fire_summary$NumberFiresLightning)
hist(short_tcsi_by_year_type[short_tcsi_by_year_type$Group.2 == "Natural", ]$FOD_ID)
mean(short_tcsi_by_year_type[short_tcsi_by_year_type$Group.2 == "Natural", ]$FOD_ID)

#fire sizes?
hist(fire_summary$TotalBurnedSitesAccidental/3.24) #convert to ha
mean(fire_summary$TotalBurnedSitesAccidental/3.24)
hist(short_tcsi_by_year_type[short_tcsi_by_year_type$Group.2 == "Human", ]$FIRE_SIZE * 2.47) #convert to ha
mean(short_tcsi_by_year_type[short_tcsi_by_year_type$Group.2 == "Human", ]$FIRE_SIZE * 2.47)
