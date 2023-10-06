
##---------------------------------------------------------------------------------
# Attempt #2
library("tidyverse")
library("tidyselect")
library("sf")
library("cffdrs")
library("lubridate")
library("smoothr")
library("progress")
library("terra")
# library("ncdf4")

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance")

# short dataset: https://www.fs.usda.gov/rds/archive/Catalog/RDS-2013-0009.5
# import the full dataset -- only if you haven't subset it already
# short_full <- sf::st_read("./calibration data/short/Data/FPA_FOD_20210617.gdb",
#                           layer = "Fires") %>%
#               sf::st_transform(crs = "EPSG:2163")

ecoregions <- rast("./Models/Inputs/input_rasters_reproject/TCSI_ecoregions.tif")
ecoregion_size <- table(values(ecoregions))[-1]

#short dataset already extracted for the whole sierra
short_ca <- readRDS("./Parameterization/calibration data/short_ignitions/short_sierra.RDS")%>%
  filter(FIRE_SIZE >= 8)

subset_polygon <- sf::st_read("./Models/Inputs/masks_boundaries/subset_polygon/subset_polygon.shp") %>%
  sf::st_transform(crs(short_ca))

tcsi_polygon <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  sf::st_transform(crs(short_ca))

fwi_landis <- read.csv("./Parameterization/calibration data/climate/Climate-future-input-log.csv")
#add a date column
fwi_landis$date <- parse_date_time(as.character(fwi_landis$Year), orders = "y")
yday(fwi_landis$date) <- fwi_landis$Timestep

#get weighted mean of FWI for the whole study area
fwi_tcsi <- fwi_landis %>%
  dplyr::select(c("date", "EcoregionIndex", "FWI")) %>%
  pivot_wider(names_from = "EcoregionIndex", values_from = "FWI")
fwi_tcsi$fwi <- apply(fwi_tcsi[, -1], 1, FUN = function(x) mean(x))

# import a shapefile of Sierra Nevada regions, merge them, remove holes, reproject
# short data was already extracted to this mask
ca_region <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  summarise(do.union = TRUE) %>% 
  st_make_valid() %>%
  smoothr::fill_holes(threshold = 0.5) %>%
  st_transform(crs(short_ca))

ca_region_wgs84 <- st_transform(ca_region, crs = "EPSG:4326")

# subset the data to smaller extent -- only if you haven't done it already
# short_ca <- sf::st_intersection(short_full, ca_region)
# saveRDS(short_ca, file = "./calibration data/short_ca.RDS")

table(short_ca$FIRE_YEAR) #first year is 1992

short_tcsi <- readRDS("./Parameterization/calibration data/short_ignitions/short_tcsi.RDS")%>%
  filter(FIRE_SIZE >= 8)

short_ca$date_clean <- lubridate::parse_date_time(as.Date(short_ca$DISCOVERY_DATE), orders = "ymd")

short_by_day <- aggregate(short_ca, by = list(short_ca$date_clean, short_ca$NWCG_CAUSE_CLASSIFICATION), FUN = length)[, c(1:3)] %>%
  rename(date = Group.1, cause = Group.2, n.fires = FOD_ID)

short_tcsi$date_clean <- lubridate::parse_date_time(as.Date(short_tcsi$DISCOVERY_DATE), order = "ymd")

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

area_ca <- sf::st_area(ca_region)
area_tcsi <- sf::st_area(tcsi_polygon)
area_subset <- sf::st_area(subset_polygon)
scale_ca_to_tcsi <- as.numeric(area_tcsi / area_ca)
scale_tcsi_to_subset <- as.numeric(area_subset / area_tcsi)

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

lightning_tcsi_poisson <- glm(as.numeric(n.fires)~as.numeric(fwi), data=tcsi_fwi_data_merge_lightning, family = poisson(link = "log"))
summary(lightning_tcsi_poisson)
accidental_tcsi_poisson <- glm(as.numeric(n.fires)~as.numeric(fwi), data=tcsi_fwi_data_merge_accidental, family = poisson(link = "log"))
summary(accidental_tcsi_poisson)

lightning_poisson <- glm(as.numeric(n.fires)~as.numeric(fwi), data=all_fwi_data_merge_lightning, family = poisson(link = "log"))
summary(lightning_poisson)
accidental_poisson <- glm(as.numeric(n.fires)~as.numeric(fwi), data=all_fwi_data_merge_accidental, family = poisson(link = "log"))
summary(accidental_poisson)

#reduce the intercept for the poisson models to modify CA model to TCSI
coef(lightning_poisson)[1] + log(scale_ca_to_tcsi)
coef(accidental_poisson)[1] + log(scale_ca_to_tcsi)

#reduce the intercept for the poisson models to modify CA model to subset
coef(lightning_poisson)[1] + log(scale_ca_to_tcsi) + log(scale_tcsi_to_subset)
coef(accidental_poisson)[1] + log(scale_ca_to_tcsi) + log(scale_tcsi_to_subset)

newdata <- data.frame(fwi = seq(0, 60, length.out = 200))

#zero-inflated allows for a "plateau" in ignitions
#poisson is always increasing
#all of them miss the peak in ignitions around fwi = 40
plot(as.numeric(n.fires)~as.numeric(fwi), data=all_fwi_data_merge_lightning)
lines(predict(lightning_model, newdata = newdata) ~ newdata$fwi, type = "l") #zinfl
lines(predict(lightning_poisson, newdata = newdata, type = "response")  ~ newdata$fwi) #poisson
lines(predict(lightning_tcsi, newdata = newdata) / scale_ca_to_tcsi  ~ newdata$fwi) #zinfl for TCSI
lines(predict(lightning_tcsi_poisson, newdata = newdata, type = "response") / scale_ca_to_tcsi  ~ newdata$fwi) #poisson for TCSI


plot(as.numeric(n.fires)~as.numeric(fwi), data=all_fwi_data_merge_accidental)
lines(predict(accidental_model, newdata = newdata) ~ newdata$fwi)
lines(predict(accidental_poisson, newdata = newdata, type = "response") ~ newdata$fwi)
lines(predict(accidental_tcsi, newdata = newdata) / scale_ca_to_tcsi ~ newdata$fwi)
lines(predict(accidental_tcsi_poisson, newdata = newdata, type = "response") / scale_ca_to_tcsi ~ newdata$fwi)


#----------------------------------
#Compare across an average year
#really nice fit here for the whole Sierra region
#almost no difference between the poisson and 

average_year_lightning <- all_fwi_data_merge_lightning %>%
  mutate(jday = format(date, "%j")) %>%
  group_by(jday) %>%
  summarise(mean_ignitions = mean(n.fires),
            mean_fwi = mean(fwi))

plot(mean_fwi ~ jday, data = average_year_lightning,
     type = "l",
     axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)                             # Add new plot
plot(mean_ignitions ~ jday, data = average_year_lightning)
lines(predict(lightning_model, newdata = data.frame(fwi = average_year_lightning$mean_fwi)) ~ average_year_lightning$jday)
lines(predict(lightning_poisson, newdata = data.frame(fwi = average_year_lightning$mean_fwi), type = "response") ~ 
        average_year_lightning$jday)
sum(average_year_lightning$mean_ignitions)
sum(predict(lightning_model, newdata = data.frame(fwi = average_year_lightning$mean_fwi))) #scale number of ignitions?
log(sum(average_year_lightning$mean_ignitions)/sum(predict(lightning_model, newdata = data.frame(fwi = average_year_lightning$mean_fwi))))
#add 0.186 to intercept


average_year_accidental <- all_fwi_data_merge_accidental %>%
  mutate(jday = format(date, "%j")) %>%
  group_by(jday) %>%
  summarise(mean_ignitions = mean(n.fires),
            mean_fwi = mean(fwi))


plot(mean_fwi ~ jday, data = average_year_accidental,
     type = "l",
     axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)                             # Add new plot
plot(mean_ignitions ~ jday, data = average_year_accidental)
lines(predict(accidental_model, newdata = data.frame(fwi = average_year_accidental$mean_fwi)) ~ average_year_accidental$jday)
lines(predict(accidental_poisson, newdata = data.frame(fwi = average_year_accidental$mean_fwi), type = "response") ~ 
        average_year_accidental$jday)
sum(average_year_accidental$mean_ignitions)
sum(predict(accidental_model, newdata = data.frame(fwi = average_year_accidental$mean_fwi))) #scale number of ignitions?

#----------------
# just for TCSI region
# not enough ignitions for a good fit here

average_year_lightning <- tcsi_fwi_data_merge_lightning %>%
  mutate(jday = format(date, "%j")) %>%
  group_by(jday) %>%
  summarise(mean_ignitions = mean(n.fires),
            mean_fwi = mean(fwi))

plot(mean_fwi ~ jday, data = average_year_lightning,
     type = "l",
     axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)                             # Add new plot
plot(mean_ignitions ~ jday, data = average_year_lightning)
lines(predict(lightning_tcsi, newdata = data.frame(fwi = average_year_lightning$mean_fwi)) ~ average_year_lightning$jday)
lines(predict(lightning_tcsi_poisson, newdata = data.frame(fwi = average_year_lightning$mean_fwi), type = "response") ~ 
        average_year_lightning$jday)

average_year_accidental <- tcsi_fwi_data_merge_accidental %>%
  mutate(jday = format(date, "%j")) %>%
  group_by(jday) %>%
  summarise(mean_ignitions = mean(n.fires),
            mean_fwi = mean(fwi))


plot(mean_fwi ~ jday, data = average_year_accidental,
     type = "l",
     axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)                             # Add new plot
plot(mean_ignitions ~ jday, data = average_year_accidental)
lines(predict(accidental_tcsi, newdata = data.frame(fwi = average_year_accidental$mean_fwi)) ~ average_year_accidental$jday)
lines(predict(accidental_tcsi_poisson, newdata = data.frame(fwi = average_year_accidental$mean_fwi), type = "response") ~ 
        average_year_accidental$jday)


plot(n.fires ~ fwi, data = all_fwi_data_merge_lightning)
plot(predict(lightning_model) ~ all_fwi_data_merge_lightning_test$fwi)
plot(n.fires ~ fwi, data = all_fwi_data_merge_accidental)
plot(predict(accidental_model) ~ all_fwi_data_merge_accidental$fwi)

# comparisons
#what day is the mean fire?
mean(yday(short_by_day$date))
