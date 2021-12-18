# process outputs for the subset landscape
# TODO:
# focus on variables: area burned, area burned at high intensity, AGB, biodiversity, 
#           pyrodiversity, location of fire (e.g. WUI vs wildlands), harvested biomass, emissions
#  ideas
#  --What is the role of fuels in recent fires?
#  --If fires are less constrained by fuels, does that suggest a different management path?
#  --linkage between beetles and fire

library("raster")
library("tidyverse")
library("sf")
library("vioplot")

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance")

years <- 1:41
#need to summarize fire data to 5-year chunks to compare with NECN data
year_bins <- cut(years, breaks = seq(0,40, by = 5))

subset_landscape <- TRUE
subset_poly <- sf::st_read("C:/Users/Sam/Documents/Research/TCSI conservation finance/masks_boundaries/subset_polygon/subset_polygon.shp")


## SCRPPLE outputs
# setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance/subset_scenario1")


#what are the names for the scenarios? 
scenarios <- data.frame(scenario_name = c("subset_scenario1", "subset_scenario2", "subset_scenario3",
               "subset_scenario4", "subset_scenario5", "subset_scenario6",
               "subset_scenario1_miroc", "subset_scenario2_miroc", "subset_scenario3_miroc",
               "subset_scenario4_miroc", "subset_scenario5_miroc", "subset_scenario6_miroc"),
               climate = c(rep("Historical", 6), rep("RCP8.5", 6)),
               management = rep(c("Minimal", "Minimal", "Moderate", "Moderate", "Maximal", "Maximal"), 2))

scenarios <- scenarios[c(1,3,6,7,9,12), ]
scenarios$id <- as.character(c(1,2,3,4,5,6))

#----------------------------
#climate data
#

historical <- read.csv("./subset_scenario1/Climate-future-input-log.csv") %>%
  group_by(Year) %>%
  filter(Timestep > 210 & Timestep < 300) %>%
  summarise(mean_fwi = mean(FWI),
            max_fwi = max(FWI))
miroc <- read.csv("./subset_scenario1_miroc/Climate-future-input-log.csv")%>%
  group_by(Year) %>%
  filter(Timestep > 210 & Timestep < 300) %>%
  summarise(mean_fwi = mean(FWI),
            max_fwi = max(FWI))

plot(historical$max_fwi ~ I(historical$Year - 1980), type = "l",
     ylim = c(40, 90), lwd = 2, col = "#1b9e77",
     xlab = "Year",
     ylab = "Fire Weather Index")
lines(miroc$max_fwi ~ I(miroc$Year-2020), col = "#d95f02", lwd = 2)
# lines(historical$max_fwi ~ historical$Year, col = "#1b9e77", lwd = 2)
# lines(miroc$max_fwi ~ I(miroc$Year-40), col = "#d95f02", lwd = 2)
# abline(h = mean(historical$max_fwi))
# abline(h = mean(miroc$max_fwi))

#process SCRPPLE data

scr_paths <- paste0("./", scenarios$scenario_name, "/social-climate-fire")
scr_summary_paths <- paste0("./", scenarios$scenario_name, "/scrapple-summary-log.csv")

#import fire summary data
scr_summaries <- lapply(scr_summary_paths, read.csv) %>%
  bind_rows(.id = "id") %>%
  mutate(TotalBurnedSites = TotalBurnedSitesAccidental + 
                            TotalBurnedSitesLightning + 
                            TotalBurnedSitesRx,
         TotalNumberFires = NumberFiresAccidental + 
                            NumberFiresLightning + 
                            NumberFiresRx)

scr_summaries %>% mutate(TotalBurnedSites = TotalBurnedSites * 3.24 / 0.18) -> scr_summaries


#plot number of fires over time for each scenario

ggplot(scr_summaries[scr_summaries$id %in% c(1,4), ], aes(x = SimulationYear, y = TotalBurnedSites,
                             colour = id)) +
  geom_point() + 
  geom_line() + 
  scale_color_manual(values=c("#1b9e77", "#d95f02"), labels = c("Historical", "RCP8.5")) +
  ylab("Area burned (ha)")


#-------------------------------------------------------------------------------
# process fire rasters

get_burn_intensity <- function(raster, intensity){
  return(sum(values(raster) >= intensity))
}


intensity_paths <- paste0(rep(scr_paths, each = length(years)), "/fire-intensity-", years, ".img")

high_intensity_cells <- NA
for(i in 1:length(intensity_paths)){
  #TODO remake this a purrr::map workflow
  high_intensity_cells[i] <- raster(intensity_paths[i]) %>% 
    get_burn_intensity(., 3)
}

scr_summaries$TotalSitesHighIntensity <- high_intensity_cells


#make a map of number of times burned for each scenario
for(i in 1:length(scr_paths)){
  fire_stack <- stack(paste0(rep(scr_paths[i], each = length(years)), "/fire-intensity-", years, ".img"))

  max_intensity <- max(fire_stack)
  max_intensity <- reclassify(max_intensity, c(4, 999, 4))
  
  fire_count <- sum(reclassify(fire_stack, c(0,2,0,2,999,1), right = FALSE))
  
  writeRaster(max_intensity, paste0("./analysis/max_intensity_", scenarios$scenario_name[i], ".tif"), overwrite = TRUE)
  writeRaster(max_intensity, paste0("./analysis/n_burned_", scenarios$scenario_name[i], ".tif"), overwrite = TRUE)
}

#TODO extract more information from rasters?

#aggregate to five-year chunks
scr_summaries_5_year <- scr_summaries %>%
  dplyr::mutate(year_round = plyr::round_any(SimulationYear, 5, f = ceiling)) %>%
  dplyr::group_by(id, year_round) %>%
  dplyr::summarise(across(where(is.numeric), sum))


ggplot(scr_summaries_5_year, aes(x = year_round, y = TotalBurnedSitesLightning,
                          colour = id)) +
  geom_point() + geom_smooth()

ggplot(scr_summaries_5_year, aes(x = year_round, y = TotalSitesHighIntensity,
                                 colour = id)) +
  geom_point() + geom_smooth()

#seems like some Rx fires got out of control? #TODO check on fire intensity for Rx vs other fires
plot(scr_summaries_5_year$TotalSitesHighIntensity ~ scr_summaries_5_year$TotalBurnedSitesRx)
plot(scr_summaries_5_year$TotalSitesHighIntensity ~ scr_summaries_5_year$TotalBurnedSites)


#high intensity fire
ggplot(scr_summaries[scr_summaries$id %in% c(1,4), ], aes(x = SimulationYear, y = TotalSitesHighIntensity,
                                                          colour = id)) +
  geom_point() + 
  geom_line() + 
  scale_color_manual(values=c("#1b9e77", "#d95f02"), labels = c("Historical", "RCP8.5")) +
  ylab("Area burned (ha)")



#maps of fire intensity
library("viridis")
cuts <- c(0,1,2,3,4)
pal <- c("white", "gray", viridis(50)[10], viridis(50)[20], viridis(50)[30])

map1 <- raster("./analysis/max_intensity_subset_scenario1.tif")
plot(map1, col= pal)
map2 <- raster("./analysis/max_intensity_subset_scenario3.tif")
plot(map2, col = pal)
map3 <- raster("./analysis/max_intensity_subset_scenario1_miroc.tif")
plot(map3, col = pal)
map4 <- raster("./analysis/max_intensity_subset_scenario3_miroc.tif")
plot(map4, col = pal)
# map3 <- raster("./analysis/max_intensity_subset_scenario6.tif")
# map4 <- raster("./analysis/max_intensity_subset_scenario6_miroc.tif")
#-------------------------------------------------------------------------------
# process NECN outputs
necn_annual_paths <- paste0("./", scenarios$scenario_name, "/NECN-succession-log-short.csv")

necn_summaries <- lapply(necn_annual_paths, read.csv) %>%
  bind_rows(.id = "id") 

necn_summaries <- left_join(necn_summaries, scenarios)
necn_summaries$management <- factor(necn_summaries$management, levels = unique(necn_summaries$management))

ggplot(necn_summaries, aes(x = Time, y = AGB, colour = climate, 
                           shape = management))+ 
  geom_point(size = 2) + 
  geom_smooth(se = FALSE, aes(linetype=management, color=climate)) + 
  scale_color_manual(values=c("#1b9e77", "#d95f02"), labels = c("Historical", "RCP8.5"))

ggplot(necn_summaries, aes(x = Time, y = SOMTC, colour = id)) + 
  geom_point() + 
  geom_smooth()

plot(necn_summaries$AGB ~ scr_summaries_5_year$TotalBurnedSitesAccidental)
plot(necn_summaries$AGB ~ scr_summaries_5_year$TotalSitesHighIntensity)
plot(necn_summaries$AGB ~ scr_summaries_5_year$TotalSitesHighIntensity)

#total high intensity fire versus aboveground biomass
necn_all <- necn_summaries %>%
  filter(Time == 40) 

high_intensity_fire_all <- scr_summaries %>%
  group_by(id) %>%
  summarise(total_high_intensity_fire = sum(TotalSitesHighIntensity),
            total_fire = sum(TotalBurnedSites))

plot(necn_all$AGB ~ high_intensity_fire_all$total_high_intensity_fire)

plot(high_intensity_fire_all$total_high_intensity_fire ~ high_intensity_fire_all$total_fire)


#-------------------------------------------------------------------------------
# process NECN rasters

necn_paths <- paste0("./", scenarios, "/NECN")


#-------------------------------------------------------------------------------
# beetles

beetle_all_years_paths <- paste0("./", scenarios$scenario_name, "/bda_log.csv")

beetle_all_year_summaries <- lapply(beetle_all_years_paths, read.csv) %>%
  bind_rows(.id = "id") %>%
  mutate(id = as.factor(id))

beetle_condensed_year <- beetle_all_year_summaries %>%
  dplyr::group_by(id, Time) %>%
  dplyr::summarise(CohortsKilled = sum(CohortsKilled),
            DamagedSites = sum(DamagedSites),
            TotalBiomassMortality = sum(TotalBiomassMortality))
            #MeanSeverity = weighted.mean(MeanSeverity, DamagedSites)) #figure this out



ggplot(beetle_condensed_year, aes(x = Time, y = TotalBiomassMortality, colour = id)) + 
  geom_point() + 
  geom_smooth()


beetle_summaries_5_year <- beetle_condensed_year %>%
  dplyr::mutate(SimulationYear = plyr::round_any(Time, 5, f = ceiling)) %>%
  dplyr::group_by(id, SimulationYear) %>%
  dplyr::summarise(across(where(is.numeric), sum)) %>% left_join(scenarios)
beetle_summaries_5_year$management <- factor(beetle_summaries_5_year$management, levels = unique(beetle_summaries_5_year$management))

ggplot(beetle_summaries_5_year, aes(x = SimulationYear, y = TotalBiomassMortality, colour = climate, 
                                    shape = management))+ 
  geom_point(size = 2) + 
  geom_smooth(se = FALSE, aes(linetype=management, color=climate)) + 
  scale_color_manual(values=c("#1b9e77", "#d95f02"), labels = c("Historical", "RCP8.5"))

beetles_all_summary <- beetle_summaries_5_year %>%
  group_by(id) %>%
  summarise(all_beetle_mortality = sum(TotalBiomassMortality))
  

#-------------------------------------------------------------------------------
# species maps
map1 <- raster("./subset_scenario6_miroc/biomass/AbieConc-0.img")
map2 <- raster("./subset_scenario6_miroc/biomass/AbieConc-40.img")
map3 <- map2 - map1
plot(map3)

#-------------------------------------------------------------------------------
# old stuff


# setwd("./subset_scenario/social-climate-fire")

fire_summary <- read.csv("./scrapple-summary-log.csv")
fire_summary7 <- fire_summary
events <- read.csv("./scrapple-events-log.csv")


# test <- raster("social-climate-fire/fire-intensity-39.img")
# plot(test)
# test2 <- test
# values(test2) <- ifelse(values(test > 1), 1, 0)
# test3 <- raster::clump(test2)
# plot(test3)
#TODO convert to points, find concave hull
  
# 
# plot(events$TotalBiomassMortality ~ events$MeanFWI) # fires only kill trees at high FWI
# plot(events$MeanSpreadProbability ~ events$MeanFWI) #really high fire spread at high FWI
# plot(events$MeanDNBR ~ events$MeanFWI) #DNBR is not very sensitive to FWI, except for three high-severity fires?
# plot(events$MeanDNBR ~ events$SimulationYear)
# plot(events$TotalBiomassMortality ~ events$SimulationYear)


#number of cells that burned -- matches summary file more or less
# cellStats(test, function(i, ...) sum(i >= 2)) # larger than summary by 2 for some timesteps?

fire_summary7$TotalBurnedSites <- fire_summary$TotalBurnedSitesAccidental + 
                                    fire_summary$TotalBurnedSitesLightning + 
                                      fire_summary$TotalBurnedSitesRx
fire_summary7$TotalFires <- fire_summary$NumberFiresAccidental + 
                                       fire_summary$NumberFiresLightning + 
                                       fire_summary$NumberFiresRx
(fire_summary7$TotalBurnedSites / fire_summary$TotalFires) #average area burned per fire by year

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
area_subset <- sf::st_area(subset_poly)
subset_proportion_tcsi <- ifelse(subset_landscape, area_subset / area_tcsi, 1)
subset_proportion_ca <- ifelse(subset_landscape, area_subset / area_ca, area_tcsi / area_ca)

#Fires per year
hist(short_tcsi$FIRE_YEAR)
short_tcsi_by_year <- short_tcsi %>%
  sf::st_drop_geometry() %>%
  mutate(FIRE_YEAR = factor(FIRE_YEAR, 1992:2018)) %>% #workaround to make years with no fire have area burned = 0
  group_by(FIRE_YEAR) %>%
  count()

short_ca_by_year <- short_ca %>%
  sf::st_drop_geometry() %>%
  mutate(FIRE_YEAR = factor(FIRE_YEAR, 1992:2018)) %>% #workaround to make years with no fire have area burned = 0
  group_by(FIRE_YEAR) %>%
  count()


#scenario1: ignitions are pretty good for sierra; too  many for TCSI
#subset_scenario2: ignitions between tcsi and sierra; long right tail
#subset_scenario4: too many fires!
#subset_scenario5: pretty good really; maybe too many zeroes
hist(fire_summary$TotalFires)
mean(fire_summary$TotalFires)
hist(short_tcsi_by_year$n * subset_proportion_tcsi)
mean(short_tcsi_by_year$n * subset_proportion_tcsi)
hist(short_ca_by_year$n * subset_proportion_ca)
mean(short_ca_by_year$n * subset_proportion_ca)
vioplot(fire_summary2$TotalFires, fire_summary3$TotalFires, fire_summary4$TotalFires, fire_summary5$TotalFires,
        short_tcsi_by_year$n * subset_proportion_tcsi, short_ca_by_year$n * subset_proportion_ca,
        col = c(rep("gray", 4), "blue", "blue"),
        xlab = "Scenario",
        ylab = "Total fires per year")

n_fires_short_tcsi_by_year_type <- short_tcsi %>%
  sf::st_drop_geometry() %>%
  mutate(FIRE_YEAR = factor(FIRE_YEAR, 1992:2018)) %>% #workaround to make years with no fire have area burned = 0
  group_by(FIRE_YEAR, NWCG_CAUSE_CLASSIFICATION) %>%
  count() %>%
  rename(cause = NWCG_CAUSE_CLASSIFICATION)

n_fires_short_ca_by_year_type <-  short_ca %>%
  sf::st_drop_geometry() %>%
  mutate(FIRE_YEAR = factor(FIRE_YEAR, 1992:2018)) %>% #workaround to make years with no fire have area burned = 0
  group_by(FIRE_YEAR, NWCG_CAUSE_CLASSIFICATION) %>%
  count() %>%
  rename(cause = NWCG_CAUSE_CLASSIFICATION)

area_burned_short_tcsi_by_year_type <- short_tcsi %>%
  sf::st_drop_geometry() %>%
  mutate(FIRE_YEAR = factor(FIRE_YEAR, 1992:2018)) %>% #workaround to make years with no fire have area burned = 0
  group_by(FIRE_YEAR, NWCG_CAUSE_CLASSIFICATION) %>%
  summarise(area_burned = sum(FIRE_SIZE)) %>%
  mutate(FIRE_YEAR = as.numeric(FIRE_YEAR)) %>%
  rename(cause = NWCG_CAUSE_CLASSIFICATION)
  
area_burned_short_ca_by_year_type <- short_ca %>%
  sf::st_drop_geometry() %>%
  mutate(FIRE_YEAR = factor(FIRE_YEAR, 1992:2018)) %>% #workaround to make years with no fire have area burned = 0
  group_by(FIRE_YEAR, NWCG_CAUSE_CLASSIFICATION) %>%
  summarise(area_burned = sum(FIRE_SIZE)) %>%
  mutate(FIRE_YEAR = as.numeric(FIRE_YEAR)) %>%
  rename(cause = NWCG_CAUSE_CLASSIFICATION)

#-------------------------------------------------------------------------------
#fire occurrence, accidental
# scenario1, number of fires between TCSI and sierra
# subset_scenario, waaaay too many fires!
# subset_scenario2: somewhat too few fires
# subset_secnario3: way too few fires
# subset_scenario4: dead on for accidental fires
# subset_scenario5: too few fires
hist(fire_summary$NumberFiresAccidental)
mean(fire_summary$NumberFiresAccidental)
var(fire_summary$NumberFiresAccidental)
hist(n_fires_short_tcsi_by_year_type[n_fires_short_tcsi_by_year_type$cause == "Human", ]$n * subset_proportion_tcsi)
mean(n_fires_short_tcsi_by_year_type[n_fires_short_tcsi_by_year_type$cause == "Human", ]$n) * subset_proportion_tcsi
var(n_fires_short_tcsi_by_year_type[n_fires_short_tcsi_by_year_type$cause == "Human", ]$n * subset_proportion_tcsi)
hist(n_fires_short_ca_by_year_type[n_fires_short_ca_by_year_type$cause == "Human", ]$n * subset_proportion_ca)
mean(n_fires_short_ca_by_year_type[n_fires_short_ca_by_year_type$cause == "Human", ]$n) * subset_proportion_ca
var(n_fires_short_ca_by_year_type[n_fires_short_ca_by_year_type$cause == "Human", ]$n * subset_proportion_ca)

vioplot(fire_summary$NumberFiresAccidental, n_fires_short_tcsi_by_year_type[n_fires_short_tcsi_by_year_type$cause == "Human", ]$n,
        n_fires_short_ca_by_year_type[n_fires_short_ca_by_year_type$cause == "Human", ]$n * subset_proportion_ca)

# fire occurrence, lightning
# scenario1: far too many lightning fires
# subset scenario: waaay too many lightning fires
# subset_scenario2: somewhat more lightning fires than sierra and tcsi
# subset_scenario3: low median, long tail. Mean in between tcsi and sierra, high variance
# subset_scenario4: way too many fires
# subset_scenario5: too few fires, but long right tail
hist(fire_summary$NumberFiresLightning)
mean(fire_summary$NumberFiresLightning)
var(fire_summary$NumberFiresLightning)
hist(n_fires_short_tcsi_by_year_type[n_fires_short_tcsi_by_year_type$cause == "Natural", ]$n * subset_proportion_tcsi)
mean(n_fires_short_tcsi_by_year_type[n_fires_short_tcsi_by_year_type$cause == "Natural", ]$n * subset_proportion_tcsi)
var(n_fires_short_tcsi_by_year_type[n_fires_short_tcsi_by_year_type$cause == "Natural", ]$n * subset_proportion_tcsi)
hist(n_fires_short_ca_by_year_type[n_fires_short_ca_by_year_type$cause == "Natural", ]$n * subset_proportion_ca)
mean(n_fires_short_ca_by_year_type[n_fires_short_ca_by_year_type$cause == "Natural", ]$n) * subset_proportion_ca
var(n_fires_short_ca_by_year_type[n_fires_short_ca_by_year_type$cause == "Natural", ]$n * subset_proportion_ca)

vioplot(fire_summary$NumberFiresLightning, n_fires_short_tcsi_by_year_type[n_fires_short_tcsi_by_year_type$cause == "Natural", ]$n * subset_proportion_tcsi,
        n_fires_short_ca_by_year_type[n_fires_short_ca_by_year_type$cause == "Natural", ]$n * subset_proportion_ca)

#area burned per year?
#scenario1: about half as many hectares as TCSI; far less than sierra
#subset_scenario: somewhat too much fire not crazy though
#subset_scenario2: low mean, but long right tail
#subset_scenario3: low mean, long right tail
#subset_scenario4: mean is too high, median too low -- long right tail
# subset_scneario5: same as above
hist(fire_summary$TotalBurnedSitesAccidental*3.24) #convert to ha
mean(fire_summary$TotalBurnedSitesAccidental*3.24)
var(fire_summary$TotalBurnedSitesAccidental*3.24)
hist(area_burned_short_tcsi_by_year_type[area_burned_short_tcsi_by_year_type$cause == "Human", ]$area_burned / 2.47 * subset_proportion_tcsi) #convert to ha
mean(area_burned_short_tcsi_by_year_type[area_burned_short_tcsi_by_year_type$cause == "Human", ]$area_burned / 2.47) * subset_proportion_tcsi
var(area_burned_short_tcsi_by_year_type[area_burned_short_tcsi_by_year_type$cause == "Human", ]$area_burned / 2.47 * subset_proportion_tcsi)
hist(area_burned_short_ca_by_year_type[area_burned_short_ca_by_year_type$cause == "Human", ]$area_burned * subset_proportion_ca) #convert to ha
mean(area_burned_short_ca_by_year_type[area_burned_short_ca_by_year_type$cause == "Human", ]$area_burned / 2.47)* subset_proportion_ca
var(area_burned_short_ca_by_year_type[area_burned_short_ca_by_year_type$cause == "Human", ]$area_burned / 2.47* subset_proportion_ca) #convert to ha
vioplot(fire_summary7$TotalBurnedSitesAccidental*3.24, area_burned_short_tcsi_by_year_type[area_burned_short_tcsi_by_year_type$cause == "Human", ]$area_burned / 2.47  * subset_proportion_tcsi,
        area_burned_short_ca_by_year_type[area_burned_short_ca_by_year_type$cause == "Human", ]$area_burned / 2.47 * subset_proportion_ca,
        col = c("gray", "blue", "blue"))

#scenario1: about twice as much fire for tcsi; 1/3 of sierra
# subset_scenario: way too much fire!
# subset_scenario2: looks pretty good
# subset_scenario3: no fires?
# subset_scenario4: mean is pretty good, a few crazy fire years but they don't look too bad really
# subset_scenario5: same as above
hist(fire_summary$TotalBurnedSitesLightning*3.24) #convert to ha
mean(fire_summary$TotalBurnedSitesLightning*3.24)
var(fire_summary$TotalBurnedSitesLightning*3.24)
hist(area_burned_short_tcsi_by_year_type[area_burned_short_tcsi_by_year_type$cause == "Natural", ]$area_burned / 2.47 * subset_proportion_tcsi) #convert to ha
mean(area_burned_short_tcsi_by_year_type[area_burned_short_tcsi_by_year_type$cause == "Natural", ]$area_burned / 2.47) * subset_proportion_tcsi
var(area_burned_short_tcsi_by_year_type[area_burned_short_tcsi_by_year_type$cause == "Natural", ]$area_burned / 2.47 * subset_proportion_tcsi)
hist(area_burned_short_ca_by_year_type[area_burned_short_ca_by_year_type$cause == "Natural", ]$area_burned * subset_proportion_ca) #convert to ha
mean(area_burned_short_ca_by_year_type[area_burned_short_ca_by_year_type$cause == "Natural", ]$area_burned / 2.47)* subset_proportion_ca
var(area_burned_short_ca_by_year_type[area_burned_short_ca_by_year_type$cause == "Natural", ]$area_burned / 2.47* subset_proportion_ca) #convert to ha
vioplot(fire_summary7$TotalBurnedSitesLightning*3.24, area_burned_short_tcsi_by_year_type[area_burned_short_tcsi_by_year_type$cause == "Natural", ]$area_burned / 2.47  * subset_proportion_tcsi,
        area_burned_short_ca_by_year_type[area_burned_short_ca_by_year_type$cause == "Natural", ]$area_burned / 2.47 * subset_proportion_ca)
#-----------------------------------------------------------------------------
#area burned per fire
#scenario1: fires are too small on average
#subset_scenario2: too small
#subset_scenario3: too small
#subset_scenario4: too small
#subset_scenario5: too small
hist(log(events$TotalSitesBurned * 3.24)) #convert to ha
mean(log(events$TotalSitesBurned * 3.24))
hist(log(short_tcsi$FIRE_SIZE / 2.47))
mean(log(short_tcsi$FIRE_SIZE / 2.47))
hist(log(short_ca$FIRE_SIZE / 2.47))
mean(log(short_ca$FIRE_SIZE / 2.47))
vioplot(log(events$TotalSitesBurned*3.24/0.18), log(short_tcsi$FIRE_SIZE / 2.47),
        log(short_ca$FIRE_SIZE / 2.47), col = c("gray", "blue", "blue"))

#time of year of fires
#scenario1: timing is good!
#subset_scenario2: a little too late in year
#subset_scenario5: a little too late
hist(events$InitialDayOfYear)
hist(short_tcsi$DISCOVERY_DOY)
hist(short_ca$DISCOVERY_DOY)
vioplot(events$InitialDayOfYear, short_tcsi$DISCOVERY_DOY, short_ca$DISCOVERY_DOY)
