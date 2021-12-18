
##---------------------------------------------------------------------------------
# Attempt #2
library("tidyverse")
library("tidyselect")
library("sf")
library("cffdrs")
library("lubridate")
library("smoothr")
library("progress")
# library("ncdf4")

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance")

# import the full dataset -- only if you haven't subset it already
# short_full <- sf::st_read("./calibration data/short/Data/FPA_FOD_20210617.gdb",
#                           layer = "Fires") %>%
#               sf::st_transform(crs = "EPSG:2163")

ecoregions <- raster("./input_rasters_tcsi/categorical/TCSI_ecoregions.tif")
ecoregion_size <- table(values(ecoregions))[-1]

#short dataset already extracted for the whole sierra
short_ca <- readRDS("./calibration data/short_ca.RDS")%>%
  filter(FIRE_SIZE >= 8)

subset_polygon <- sf::st_read("./masks_boundaries/subset_polygon/subset_polygon.shp") %>%
  sf::st_transform(crs(short_ca))

tcsi_polygon <- sf::st_read("./masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  sf::st_transform(crs(short_ca))

fwi_landis <- read.csv("./calibration data/climate-future-input-log_for_calibration.csv")
#add a date column
fwi_landis$date <- parse_date_time(as.character(fwi_landis$Year), orders = "y")
yday(fwi_landis$date) <- fwi_landis$Timestep

fwi_tcsi <- fwi_landis %>%
  dplyr::select(c("date", "EcoregionIndex", "FWI")) %>%
  pivot_wider(names_from = "EcoregionIndex", values_from = "FWI")
fwi_tcsi$fwi <- apply(fwi_tcsi[, -1], 1, FUN = function(x) weighted.mean(x, w = ecoregion_size))

# import a shapefile of Sierra Nevada regions, merge them, remove holes, reproject
# short data was already extracted to this mask
ca_region <- sf::st_read("./masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  summarise(do.union = TRUE) %>% 
  st_make_valid() %>%
  smoothr::fill_holes(threshold = 0.5) %>%
  st_transform(crs(short_ca))

ca_region_wgs84 <- st_transform(ca_region, crs = "EPSG:4326")

# subset the data to smaller extent -- only if you haven't done it already
# short_ca <- sf::st_intersection(short_full, ca_region)
# saveRDS(short_ca, file = "./calibration data/short_ca.RDS")

table(short_ca$FIRE_YEAR) #first year is 1992

short_tcsi <- readRDS("./calibration data/short_tcsi/short_tcsi.RDS")%>%
  filter(FIRE_SIZE >= 8)

short_ca$date_clean <- lubridate::parse_date_time(short_ca$DISCOVERY_DATE, order = "ymd")

short_by_day <- aggregate(short_ca, by = list(short_ca$date_clean, short_ca$NWCG_CAUSE_CLASSIFICATION), FUN = length)[, c(1:3)] %>%
  rename(date = Group.1, cause = Group.2, n.fires = FOD_ID)

short_tcsi$date_clean <- lubridate::parse_date_time(short_tcsi$DISCOVERY_DATE, order = "ymd")

short_tcsi_by_day <- aggregate(short_tcsi, by = list(short_tcsi$date_clean, short_tcsi$NWCG_CAUSE_CLASSIFICATION), FUN = length)[, c(1:3)] %>%
  rename(date = Group.1, cause = Group.2, n.fires = FOD_ID)

hist(short_ca$fwi)

all_fwi_data_merge_lightning <- dplyr::left_join(fwi_tcsi[, c("date", "fwi")], subset(short_by_day, cause == "Natural")) %>%
  dplyr::mutate(n.fires = replace_na(n.fires, 0)) %>%
  dplyr::filter(date > as.Date("1992-01-01"))
all_fwi_data_merge_accidental <- dplyr::left_join(fwi_tcsi[, c("date", "fwi")], subset(short_by_day, cause == "Human"))%>%
  dplyr::mutate(n.fires = replace_na(n.fires, 0))%>%
  dplyr::filter(date > as.Date("1992-01-01"))

tcsi_fwi_data_merge_lightning <- dplyr::left_join(fwi_tcsi[, c("date", "fwi")], subset(short_tcsi_by_day, cause == "Natural")) %>%
  dplyr::mutate(n.fires = replace_na(n.fires, 0)) %>%
  dplyr::filter(date > as.Date("1992-01-01"))
tcsi_fwi_data_merge_accidental <- dplyr::left_join(fwi_tcsi[, c("date", "fwi")], subset(short_tcsi_by_day, cause == "Human"))%>%
  dplyr::mutate(n.fires = replace_na(n.fires, 0))%>%
  dplyr::filter(date > as.Date("1992-01-01"))

plot(n.fires ~ fwi, data = all_fwi_data_merge_lightning)
plot(n.fires ~ fwi, data = all_fwi_data_merge_accidental)

area_ca_region <- sf::st_area(tcsi_poly)
area_subset <- sf::st_area(subset_polygon)
scaling_coef <- as.numeric(area_subset/area_ca_region)
log(scaling_coef)

#-------------------------------------------------------------------------------
# fitting  models
library(pscl)

lightning_model <- zeroinfl(as.numeric(n.fires)~as.numeric(fwi), data=all_fwi_data_merge_lightning, dist="poisson")
summary(lightning_model)
plot(predict(lightning_model) ~ as.numeric(all_fwi_data_merge_lightning$n.fires)) #not a great fit, but it's daily
scaled_coef <- coef(lightning_model)[1] + log(scaling_coef)

accidental_model <- zeroinfl(as.numeric(n.fires)~as.numeric(fwi), data=all_fwi_data_merge_accidental, dist="poisson")
summary(accidental_model)
plot(predict(accidental_model) ~ as.numeric(all_fwi_data_merge_accidental$n.fires))
scaled_coef <- coef(accidental_model)[1] + log(scaling_coef)

lightning_tcsi <- zeroinfl(as.numeric(n.fires)~as.numeric(fwi), data=tcsi_fwi_data_merge_lightning, dist="poisson")
summary(lightning_tcsi)
plot(predict(lightning_tcsi) ~ as.numeric(tcsi_fwi_data_merge_lightning$n.fires)) #not a great fit, but it's daily

accidental_tcsi <- zeroinfl(as.numeric(n.fires)~as.numeric(fwi), data=tcsi_fwi_data_merge_accidental, dist="poisson")
summary(accidental_tcsi)
plot(predict(lightning_tcsi) ~ as.numeric(tcsi_fwi_data_merge_lightning$n.fires)) #not a great fit, but it's daily

coef(lightning_tcsi) + log(as.numeric(subset_proportion_tcsi))
coef(accidental_tcsi) + log(as.numeric(subset_proportion_tcsi))
#models fail to capture really crazy fire-years

plot(n.fires ~ fwi, data = all_fwi_data_merge_lightning)
plot(predict(lightning_model) ~ all_fwi_data_merge_lightning_test$fwi)
plot(n.fires ~ fwi, data = all_fwi_data_merge_accidental)
plot(predict(accidental_model) ~ all_fwi_data_merge_accidental$fwi)

# comparisons
#what day is the mean fire?
mean(yday(short_by_day$date))
