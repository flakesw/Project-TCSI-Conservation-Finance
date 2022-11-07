# process outputs for the subset landscape

library("tidyverse")
library("sf")
library("vioplot")

subset_landscape <- FALSE


## SCRPPLE outputs
# setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance/")

# setwd("./subset_scenario/social-climate-fire")



#summarize short data for study area
# short <- sf::st_read("../calibration data/short/Data/FPA_FOD_20210617.gdb", layer = "Fires")
# tcsi_polygon <- sf::st_read("../masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
#   st_transform(crs(short))
# 
# short_tcsi <- st_intersection(short, sf::st_make_valid(tcsi_polygon))
# 
# #write as RDS; shapefile generates errors and cannot write as a .gdb file
# saveRDS(short_tcsi, "../calibration data/short_tcsi/short_tcsi.RDS")

short_tcsi <- readRDS("./Parameterization/calibration data/short_ignitions/short_tcsi.RDS") %>%
  # st_transform(crs(subset_poly)) %>%
  # st_intersection(subset_poly) %>%
  dplyr::filter(FIRE_SIZE >= 8) #filter to size of a cell, in acres %>%

short_ca <- readRDS("./Parameterization/calibration data/short_ignitions/short_sierra.RDS") %>%
  dplyr::filter(FIRE_SIZE >= 8)

area_tcsi <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  sf::st_area()

area_ca <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  summarise(do.union = TRUE) %>% 
  st_make_valid() %>%
  smoothr::fill_holes(threshold = 0.5) %>%
  st_transform(sf::st_crs(short_ca)) %>%
  sf::st_area()


subset_poly <- sf::st_read("./Models/Inputs/masks_boundaries/subset_polygon/subset_polygon.shp")
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

#-------------------------------------------------------------------------------
# Import SCRPPLE data
#-------------------------------------------------------------------------------
# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.

#what folder do all the runs to be analyze live in?
scenario_folder <- "E:/TCSI LANDIS/LANDIS runs"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/Model runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) %>%
  `[`(grep("Scenario", .))
# scenarios <- scenarios[-1]

#some helper functions
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = as.character(flnm),
           run_name = basename(substr(flnm, 0, regexpr("/[^/]*$", flnm)))) 
  
}

get_mgmt <- function(scenario){
  list.files(scenario, pattern = "Scenario") %>%
    pluck(1) %>%
    as.character() %>%
    strsplit(x = ., split = "[.]") %>%
    pluck(1, 1) %>%
    strsplit(x = ., split = "[_]") %>%
    pluck(1, 1)
}

get_climate <- function(scenario){
  list.files(scenario, pattern = "NECN_Succession") %>%
    pluck(1) %>%
    as.character() %>%
    strsplit(x = ., split = "[.]") %>%
    pluck(1, 1)
}

scenario_type <- data.frame(run_name = character(length(scenarios)), 
                            mgmt = character(length(scenarios)),
                            climate = character(length(scenarios)))

scenario_type <- scenario_type %>%
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(4, 1)))) %>%
  mutate(mgmt = unlist(map(scenarios, get_mgmt))) %>%
  mutate(climate = ifelse(grepl(pattern = "miroc", run_name), "MIROC", 
                          ifelse(grepl(pattern = "cnrm", run_name), "CNRM", "Historical"))) 

# scenario_type$fire_model <- rep(c("fixed", "mixed"), each = 3)

fire_summaries <- paste0(scenarios, "/scrapple-summary-log.csv")  %>%
    purrr::map_df(~read_plus(.)) %>%
    left_join(scenario_type, c("run_name" = "run_name"))

#----------------------

fire_summaries$TotalBurnedSites <- fire_summaries$TotalBurnedSitesAccidental + 
  fire_summaries$TotalBurnedSitesLightning + 
  fire_summaries$TotalBurnedSitesRx
fire_summaries$TotalFires <- fire_summaries$NumberFiresAccidental + 
  fire_summaries$NumberFiresLightning + 
  fire_summaries$NumberFiresRx
(fire_summaries$TotalBurnedSites / fire_summaries$TotalFires) #average area burned per fire by year


#------------------------------------
#SCRPPLE events
fire_events <- paste0(scenarios, "/scrapple-events-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name"))


#-------------------------------------------------------------------------------
# process fire rasters
# this can take a long time

get_burn_intensity <- function(raster, intensity){
  return(sum(terra::values(raster) >= intensity))
}

years <- 1:81
#need to summarize fire data to 5-year chunks to compare with NECN data
year_bins <- cut(years, breaks = seq(0,81, by = 5))

intensity_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "/fire-intensity-", years, ".img")

high_intensity_cells <- NA
for(i in 1:length(intensity_paths)){
  #TODO remake this a purrr::map workflow
  high_intensity_cells[i] <- terra::rast(intensity_paths[i]) %>% 
    get_burn_intensity(., 4)
}

fire_summaries$TotalSitesHighIntensity <- high_intensity_cells

#TODO extract more information from rasters?

## aggregate to five-year chunks
# scr_summaries_5_year <- scr_summaries %>%
#   dplyr::mutate(year_round = plyr::round_any(SimulationYear, 5, f = ceiling)) %>%
#   dplyr::group_by(id, year_round) %>%
#   dplyr::summarise(across(where(is.numeric), sum))

#-------------------------------------------------------------------------------
#Compare ignitions
#scenario1: ignitions are pretty good for sierra; too  many for TCSI
#subset_scenario2: ignitions between tcsi and sierra; long right tail
#subset_scenario4: too many fires!
#subset_scenario5: pretty good really; maybe too many zeroes
hist(fire_summaries$TotalFires)
mean(fire_summaries$TotalFires)
hist(short_tcsi_by_year$n * subset_proportion_tcsi)
mean(short_tcsi_by_year$n * subset_proportion_tcsi)
hist(short_ca_by_year$n * subset_proportion_ca)
mean(short_ca_by_year$n * subset_proportion_ca)
vioplot(fire_summaries$TotalFires, short_tcsi_by_year$n * subset_proportion_tcsi, short_ca_by_year$n * subset_proportion_ca)

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
# means are too high -- close match for overall Sierra though?
hist(fire_summaries$NumberFiresAccidental)
mean(fire_summaries$NumberFiresAccidental)
var(fire_summaries$NumberFiresAccidental)
hist(n_fires_short_tcsi_by_year_type[n_fires_short_tcsi_by_year_type$cause == "Human", ]$n * subset_proportion_tcsi)
mean(n_fires_short_tcsi_by_year_type[n_fires_short_tcsi_by_year_type$cause == "Human", ]$n) * subset_proportion_tcsi
var(n_fires_short_tcsi_by_year_type[n_fires_short_tcsi_by_year_type$cause == "Human", ]$n * subset_proportion_tcsi)
hist(n_fires_short_ca_by_year_type[n_fires_short_ca_by_year_type$cause == "Human", ]$n * subset_proportion_ca)
mean(n_fires_short_ca_by_year_type[n_fires_short_ca_by_year_type$cause == "Human", ]$n) * subset_proportion_ca
var(n_fires_short_ca_by_year_type[n_fires_short_ca_by_year_type$cause == "Human", ]$n * subset_proportion_ca)

vioplot(fire_summaries[fire_summaries$climate == "Historical", ]$NumberFiresAccidental, 
        n_fires_short_tcsi_by_year_type[n_fires_short_tcsi_by_year_type$cause == "Human", ]$n * subset_proportion_tcsi,
        n_fires_short_ca_by_year_type[n_fires_short_ca_by_year_type$cause == "Human", ]$n * subset_proportion_ca)


fire_summaries %>% group_by(run_name) %>% summarise(mean = mean(NumberFiresAccidental))

log(1.757576/0.7306379)
  
# TO CALIBRATE:
# Mean number of fires (lambda) is too low -- to increase in the count model 
# in SCRPPLE, find the ratio between observed and desired # of fires, take the log,
# and add that to the intercept. 



# fire occurrence, lightning
#mean is too high
hist(fire_summaries$NumberFiresLightning)
mean(fire_summaries$NumberFiresLightning)
var(fire_summaries$NumberFiresLightning)
hist(n_fires_short_tcsi_by_year_type[n_fires_short_tcsi_by_year_type$cause == "Natural", ]$n * subset_proportion_tcsi)
mean(n_fires_short_tcsi_by_year_type[n_fires_short_tcsi_by_year_type$cause == "Natural", ]$n * subset_proportion_tcsi)
var(n_fires_short_tcsi_by_year_type[n_fires_short_tcsi_by_year_type$cause == "Natural", ]$n * subset_proportion_tcsi)
hist(n_fires_short_ca_by_year_type[n_fires_short_ca_by_year_type$cause == "Natural", ]$n * subset_proportion_ca)
mean(n_fires_short_ca_by_year_type[n_fires_short_ca_by_year_type$cause == "Natural", ]$n) * subset_proportion_ca
var(n_fires_short_ca_by_year_type[n_fires_short_ca_by_year_type$cause == "Natural", ]$n * subset_proportion_ca)

vioplot(fire_summaries[fire_summaries$climate == "Historical", ]$NumberFiresLightning,
        n_fires_short_tcsi_by_year_type[n_fires_short_tcsi_by_year_type$cause == "Natural", ]$n * subset_proportion_tcsi,
        n_fires_short_ca_by_year_type[n_fires_short_ca_by_year_type$cause == "Natural", ]$n * subset_proportion_ca)

test <- fire_summaries %>% group_by(run_name) %>% summarise(mean = mean(NumberFiresAccidental))

log(0.6942149/0.2211845)

vioplot(NumberFiresLightning ~ run_name, data = fire_summaries)
vioplot(NumberFiresAccidental ~ run_name, data = fire_summaries)
vioplot(NumberFiresRx ~ run_name, data = fire_summaries) 



#-------------------------------------------------------------------------------
#area burned per year?
#waaaaay too  much area burned

#run 5 is pretty close!

hist(fire_summaries$TotalBurnedSitesAccidental*3.24) #convert to ha
mean(fire_summaries$TotalBurnedSitesAccidental*3.24)
fire_summaries %>% group_by(fire_model) %>% summarise(mean = mean(TotalBurnedSitesAccidental))
var(fire_summaries$TotalBurnedSitesAccidental*3.24)
hist(area_burned_short_tcsi_by_year_type[area_burned_short_tcsi_by_year_type$cause == "Human", ]$area_burned / 2.47 * subset_proportion_tcsi) #convert to ha
mean(area_burned_short_tcsi_by_year_type[area_burned_short_tcsi_by_year_type$cause == "Human", ]$area_burned / 2.47) * subset_proportion_tcsi
var(area_burned_short_tcsi_by_year_type[area_burned_short_tcsi_by_year_type$cause == "Human", ]$area_burned / 2.47 * subset_proportion_tcsi)
hist(area_burned_short_ca_by_year_type[area_burned_short_ca_by_year_type$cause == "Human", ]$area_burned * subset_proportion_ca) #convert to ha
mean(area_burned_short_ca_by_year_type[area_burned_short_ca_by_year_type$cause == "Human", ]$area_burned / 2.47)* subset_proportion_ca
var(area_burned_short_ca_by_year_type[area_burned_short_ca_by_year_type$cause == "Human", ]$area_burned / 2.47* subset_proportion_ca) #convert to ha
vioplot(fire_summaries[fire_summaries$climate == "Historical", ]$TotalBurnedSitesAccidental*3.24, 
        area_burned_short_tcsi_by_year_type[area_burned_short_tcsi_by_year_type$cause == "Human", ]$area_burned / 2.47  * subset_proportion_tcsi,
        area_burned_short_ca_by_year_type[area_burned_short_ca_by_year_type$cause == "Human", ]$area_burned / 2.47 * subset_proportion_ca,
        names = c("Simulated", "Observed TCSI", "Observed Sierra"),
        ylab = "Area burned per year (hectares)")


test <- fire_summaries %>% group_by(run_name) %>% summarise(mean = max(TotalBurnedSitesAccidental)*3.24)



#lightning fires
#waaay too much area burned!!
#too much fire in all scenarios -- by a lot!
hist(fire_summaries$TotalBurnedSitesLightning*3.24) #convert to ha
mean(fire_summaries$TotalBurnedSitesLightning*3.24)
fire_summaries %>% group_by(fire_model) %>% summarise(mean = mean(TotalBurnedSitesLightning))
var(fire_summaries$TotalBurnedSitesLightning*3.24)
hist(area_burned_short_tcsi_by_year_type[area_burned_short_tcsi_by_year_type$cause == "Natural", ]$area_burned / 2.47 * subset_proportion_tcsi) #convert to ha
mean(area_burned_short_tcsi_by_year_type[area_burned_short_tcsi_by_year_type$cause == "Natural", ]$area_burned / 2.47) * subset_proportion_tcsi
var(area_burned_short_tcsi_by_year_type[area_burned_short_tcsi_by_year_type$cause == "Natural", ]$area_burned / 2.47 * subset_proportion_tcsi)
hist(area_burned_short_ca_by_year_type[area_burned_short_ca_by_year_type$cause == "Natural", ]$area_burned * subset_proportion_ca) #convert to ha
mean(area_burned_short_ca_by_year_type[area_burned_short_ca_by_year_type$cause == "Natural", ]$area_burned / 2.47)* subset_proportion_ca
var(area_burned_short_ca_by_year_type[area_burned_short_ca_by_year_type$cause == "Natural", ]$area_burned / 2.47* subset_proportion_ca) #convert to ha
vioplot(fire_summaries[fire_summaries$climate == "Historical", ]$TotalBurnedSitesLightning*3.24, 
        area_burned_short_tcsi_by_year_type[area_burned_short_tcsi_by_year_type$cause == "Natural", ]$area_burned / 2.47  * subset_proportion_tcsi,
        area_burned_short_ca_by_year_type[area_burned_short_ca_by_year_type$cause == "Natural", ]$area_burned / 2.47 * subset_proportion_ca)

test <- fire_summaries %>% group_by(run_name) %>% summarise(mean = mean(TotalBurnedSitesLightning)*3.24) 

test <- fire_summaries %>% group_by(run_name) %>% summarise(mean = mean(TotalBurnedSitesRx)*3.24) 


#compare among treatments
vioplot(TotalBurnedSitesLightning ~ run_name, data = fire_summaries)
vioplot(TotalBurnedSitesAccidental ~ run_name, data = fire_summaries)
vioplot(TotalBurnedSitesRx ~ run_name, data = fire_summaries) 

vioplot(TotalBiomassMortalityLightning ~ run_name, data = fire_summaries)
vioplot(TotalBiomassMortalityAccidental ~ run_name, data = fire_summaries)

boxplot(TotalBurnedSitesLightning ~ run_name, data = fire_summaries)
boxplot(TotalBurnedSitesAccidental ~ run_name, data = fire_summaries)
boxplot(TotalBurnedSitesRx ~ run_name, data = fire_summaries)

fire_summaries %>%
  group_by(climate, mgmt) %>%
  summarise(fire = mean(TotalBurnedSitesLightning) / 3.24) 

fire_summaries %>%
  group_by(climate, mgmt) %>%
  summarise(fire = mean(TotalBurnedSitesAccidental) / 3.24) 




#-----------------------------------------------------------------------------
#area burned per fire

fire_events_accidental <-fire_events %>%
  filter(IgnitionType == "Accidental")
fire_events_lightning <-fire_events %>%
  filter(IgnitionType == "Lightning")

#wrong shape for fire size distribution, not enough small fires
hist(log(fire_events_accidental$TotalSitesBurned * 3.24)) #convert to ha
mean(log(fire_events_accidental$TotalSitesBurned * 3.24))
hist(log(short_tcsi[short_tcsi$NWCG_CAUSE_CLASSIFICATION == "Human", ]$FIRE_SIZE / 2.47))
mean(log(short_tcsi[short_tcsi$NWCG_CAUSE_CLASSIFICATION == "Human", ]$FIRE_SIZE / 2.47))
hist(log(short_ca[short_ca$NWCG_CAUSE_CLASSIFICATION == "Human", ]$FIRE_SIZE / 2.47))
mean(log(short_ca[short_ca$NWCG_CAUSE_CLASSIFICATION == "Human", ]$FIRE_SIZE / 2.47))
vioplot(log(fire_events_accidental$TotalSitesBurned *3.24), 
        log(short_tcsi[short_tcsi$NWCG_CAUSE_CLASSIFICATION == "Human", ]$FIRE_SIZE / 2.47),
        log(short_ca[short_ca$NWCG_CAUSE_CLASSIFICATION == "Human", ]$FIRE_SIZE / 2.47))

fire_events_accidental %>% group_by(run_name) %>% summarise(mean = mean(log(TotalSitesBurned)))


hist(log(fire_events_lightning$TotalSitesBurned * 3.24)) #convert to ha
mean(log(fire_events_lightning$TotalSitesBurned * 3.24))
hist(log(short_tcsi[short_tcsi$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$FIRE_SIZE / 2.47))
mean(log(short_tcsi[short_tcsi$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$FIRE_SIZE / 2.47))
hist(log(short_ca[short_ca$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$FIRE_SIZE / 2.47))
mean(log(short_ca[short_ca$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$FIRE_SIZE / 2.47))
vioplot(log(fire_events_lightning$TotalSitesBurned*3.24), 
        log(short_tcsi[short_tcsi$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$FIRE_SIZE / 2.47),
        log(short_ca[short_ca$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$FIRE_SIZE / 2.47))


fire_events_lightning %>% group_by(run_name) %>% summarise(mean = mean(log(TotalSitesBurned)))

#time of year of fires
#accidental ignitions
hist(fire_events_accidental$InitialDayOfYear)
hist(short_tcsi[short_tcsi$NWCG_CAUSE_CLASSIFICATION == "Human", ]$DISCOVERY_DOY)
hist(short_ca[short_ca$NWCG_CAUSE_CLASSIFICATION == "Human", ]$DISCOVERY_DOY)
vioplot(fire_events_accidental$InitialDayOfYear, short_tcsi[short_tcsi$NWCG_CAUSE_CLASSIFICATION == "Human", ]$DISCOVERY_DOY, 
        short_ca[short_tcsi$NWCG_CAUSE_CLASSIFICATION == "Human", ]$DISCOVERY_DOY)

#lightning igntions
hist(fire_events_lightning$InitialDayOfYear)
hist(short_tcsi[short_tcsi$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$DISCOVERY_DOY)
hist(short_ca[short_tcsi$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$DISCOVERY_DOY)
vioplot(fire_events_lightning$InitialDayOfYear, short_tcsi[short_tcsi$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$DISCOVERY_DOY, 
        short_ca[short_tcsi$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$DISCOVERY_DOY)


#-------------------------------------------------------------------------------
# Fire over time

plot(fire_summaries$TotalBurnedSitesAccidental ~ fire_summaries$SimulationYear)

fire_summaries$Year <- fire_summaries$SimulationYear + 2020

fire_summaries$TotalBurnedAcresRx <- fire_summaries$TotalBurnedSitesRx * 8

ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalBurnedAcresRx)) + 
  geom_point(color="steelblue") + 
  labs(title = "Prescribed burn area",
       subtitle = "by management scenario and climate scenario",
       y = "Area burned (acres)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate, nrow = 3, ncol = 3)

ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalBurnedSitesAccidental * 8)) + 
  geom_point(color="steelblue") + 
  labs(title = "Accidental burn area",
       subtitle = "by management scenario and climate scenario",
       y = "Area burned (acres)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ climate + mgmt, nrow = 3, ncol = 3)

ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalBurnedSitesLightning)) + 
  geom_point(color="steelblue") + 
  labs(title = "Prescribed burn area",
       subtitle = "by management scenario and climate scenario",
       y = "Area burned (acres)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate, nrow = 3, ncol = 3)


#-------------------------------------------------------------------------------
# High-intensity fire


ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalSitesHighIntensity * 8)) + 
  geom_point(color="steelblue") + 
  labs(title = "Prescribed burn area",
       subtitle = "by management scenario and climate scenario",
       y = "Area burned (acres)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate, nrow = 3, ncol = 3)

ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalSitesHighIntensity/TotalBurnedSites)) + 
  geom_point(color="steelblue") + 
  labs(title = "Prescribed burn area",
       subtitle = "by management scenario and climate scenario",
       y = "Area burned (acres)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate, nrow = 3, ncol = 3)



events_sum <- fire_events_accidental %>%
  group_by(SimulationYear, run_name) %>%
  summarise(mean_dnbr = weighted.mean(MeanDNBR, TotalSitesBurned),
            mgmt = mgmt[1],
            climate = climate[1])

ggplot(data = events_sum, mapping = aes(x = SimulationYear, y = mean_dnbr)) + 
  geom_point(color="steelblue") + 
  labs(title = "Mean DNBR",
       subtitle = "by management scenario and climate scenario",
       y = "DNBR", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate, nrow = 3, ncol = 3)



#-------------------------------------------------------------------------------
# Fire over time -- biomass burned
fire_summaries <- fire_summaries %>%
  group_by(run_name) %>%
  mutate(CumBiomassMort = cumsum(TotalBiomassMortalityAccidental)) %>%
  ungroup()

fire_summaries %>% 
  filter(Year == 2100, climate == "MIROC", mgmt == "Scenario1") %>% 
  select(CumBiomassMort)  %>%
  unlist() %>%
  mean()

fire_summaries %>% 
  filter(Year == 2100, climate == "MIROC", mgmt == "Scenario6") %>% 
  select(CumBiomassMort)  %>%
  unlist() %>%
  mean()


ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalBiomassMortalityRx)) + 
  geom_point(color="steelblue") + 
  labs(title = "Prescribed burn area",
       subtitle = "by management scenario and climate scenario",
       y = "Biomass burned (units)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate, nrow = 3, ncol = 3)

ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalBiomassMortalityAccidental)) + 
  geom_point(color="steelblue") + 
  labs(title = "Biomass killed by fire",
       subtitle = "by management scenario and climate scenario",
       y = "Biomass killed (g m-2)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ climate + mgmt, nrow = 3, ncol = 3)

ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalBiomassMortalityLightning)) + 
  geom_point(color="steelblue") + 
  labs(title = "Prescribed burn area",
       subtitle = "by management scenario and climate scenario",
       y = "Area burned (acres)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate, nrow = 3, ncol = 3)

ggplot(data = fire_summaries[fire_summaries$climate != "CNRM", ], mapping = aes(x = Year, y = CumBiomassMort*(120*120)/1000/1000)) + 
  geom_point(color="steelblue") + 
  labs(title = "Cumulative biomass burned",
       subtitle = "by management scenario and climate scenario",
       y = "Biomass (Mg)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate, nrow = 2, ncol = 3, dir = "v")

### compare fire and beetles
#TODO where did the bda summary come from?

combined <- fire_summaries %>%
  left_join(dplyr::select(bda_summaries2, !c("mgmt", "climate")),  by = c("run_name", "Year")) %>%
  mutate((across(c("TotalBiomassKilled", "TotalSitesAffected"), ~replace(., is.na(.), 0)))) %>%
  mutate(TotalBiomassWildfire = TotalBiomassMortalityAccidental + TotalBiomassMortalityLightning,
         TotalBurnedSites = TotalBurnedSitesAccidental + TotalBurnedSitesLightning) %>%
  filter(climate != "CNRM")

ggplot() + 
  labs(title = "Area affected by fire and beetles",
       subtitle = "by management scenario and climate scenario",
       y = "Area affected (acres)", x = "Year") + 
  geom_smooth(color="steelblue", data = combined, mapping = aes(x = Year, y = TotalBurnedSites * 8)) + 
  geom_smooth(color="green", data = combined, mapping = aes(x = Year, y = TotalSitesAffected * 8)) + 
  facet_wrap(~ mgmt + climate, dir = "v") + 
  scale_color_manual(name = "", values = c("TotalBurnedSites" = "steelblue", "TotalSitesAffected" = "green"))

ggplot() + 
  labs(title = "Biomass killed by fire and beetles",
       subtitle = "by management scenario and climate scenario",
       y = "Biomass killed (Mg)", x = "Year") + 
  geom_smooth(color="steelblue", data = combined, mapping = aes(x = Year, y = TotalBiomassWildfire*(120*120)/1000/1000)) + 
  geom_smooth(color="green", data = combined, mapping = aes(x = Year, y = TotalBiomassKilled*(120*120)/1000/1000)) +
  facet_wrap(~ mgmt + climate, dir = "v")





#-------------------------------------------------------------------------------
# Get fires which occur after a harvest

# Match harvests and fires -- overlap and time since treatment
# Get harvest intensity and fire intensity

