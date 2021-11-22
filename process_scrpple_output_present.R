# process outputs for the subset landscape
# TODO:
# focus on variables: area burned, area burned at high intensity, AGB, biodiversity, 
#           pyrodiversity, location of fire (e.g. WUI vs wildlands), harvested biomass, emissions
#  a good idea for a paper! What is the role of fuels in recent fires?
#  If fires are less constrained by fuels, does that suggest a different management path?


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
scenarios <- c("subset_scenario1", "subset_scenario2", "subset_scenario3",
               "subset_scenario4", "subset_scenario5", "subset_scenario6",
               "subset_scenario1_miroc", "subset_scenario2_miroc", "subset_scenario3_miroc")

#process SCRPPLE data

scr_paths <- paste0("./", scenarios, "/social-climate-fire")
scr_summary_paths <- paste0("./", scenarios, "/scrapple-summary-log.csv")

#import fire summary data
scr_summaries <- lapply(scr_summary_paths, read.csv) %>%
  bind_rows(.id = "id") %>%
  mutate(TotalBurnedSites = TotalBurnedSitesAccidental + 
                            TotalBurnedSitesLightning + 
                            TotalBurnedSitesRx,
         TotalNumberFires = NumberFiresAccidental + 
                            NumberFiresLightning + 
                            NumberFiresRx)

#plot number of fires over time for each scenario
ggplot(scr_summaries, aes(x = SimulationYear, y = TotalBurnedSitesAccidental,
                             colour = id)) +
  geom_point()


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
    get_burn_intensity(., 4)
}

scr_summaries$TotalSitesHighIntensity <- high_intensity_cells

#TODO extract more information from rasters?

#aggregate to five-year chunks
scr_summaries_5_year <- scr_summaries %>%
  dplyr::mutate(year_round = plyr::round_any(SimulationYear, 5, f = ceiling)) %>%
  dplyr::group_by(id, year_round) %>%
  dplyr::summarise(across(where(is.numeric), sum))

#-------------------------------------------------------------------------------
# process NECN outputs
necn_annual_paths <- paste0("./", scenarios, "/NECN-succession-log-short.csv")

necn_summaries <- lapply(necn_annual_paths, read.csv) %>%
  bind_rows(.id = "id") 


ggplot(necn_summaries, aes(x = Time, y = AGB, colour = id)) + 
  geom_point() + 
  geom_smooth()
ggplot(necn_summaries, aes(x = Time, y = SOMTC, colour = id)) + 
  geom_point() + 
  geom_smooth()



plot(necn_summaries$AGB ~ scr_summaries_5_year$TotalBurnedSitesAccidental)
plot(necn_summaries$AGB ~ scr_summaries_5_year$TotalSitesHighIntensity)
plot(necn_summaries$AGB ~ scr_summaries_5_year$TotalSitesHighIntensity)

#seems like some Rx fires got out of control? #TODO check on fire intensity for Rx vs other fires
plot(scr_summaries_5_year$TotalSitesHighIntensity ~ scr_summaries_5_year$TotalBurnedSitesRx)
plot(scr_summaries_5_year$TotalSitesHighIntensity ~ scr_summaries_5_year$TotalBurnedSites)


#-------------------------------------------------------------------------------
# process NECN rasters

necn_paths <- paste0("./", scenarios, "/NECN")






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
vioplot(log(events$TotalSitesBurned*3.24), log(short_tcsi$FIRE_SIZE / 2.47),
        log(short_ca$FIRE_SIZE / 2.47), col = c("gray", "blue", "blue"))

#time of year of fires
#scenario1: timing is good!
#subset_scenario2: a little too late in year
#subset_scenario5: a little too late
hist(events$InitialDayOfYear)
hist(short_tcsi$DISCOVERY_DOY)
hist(short_ca$DISCOVERY_DOY)
vioplot(events$InitialDayOfYear, short_tcsi$DISCOVERY_DOY, short_ca$DISCOVERY_DOY)
