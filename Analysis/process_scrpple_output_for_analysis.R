# process SCRPPLE outputs and create maps of fire, fire intensity, and figures
# of fire over time

library("tidyverse")
library("sf")
library("vioplot")
library("terra")

#-------------------------------------------------------------------------------
# Import SCRPPLE data
#-------------------------------------------------------------------------------
# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.

#what folder do all the runs to be analyze live in?
# scenario_folder <- "E:/TCSI LANDIS/LANDIS runs"
scenario_folder <- "./Models/Model runs"

scenarios <- list.dirs(scenario_folder, recursive = FALSE) %>%
  `[`(grep("Scenario", .))
scenarios <- scenarios[c(2,4,6,8,10,12,14)]
# scenarios <- scenarios[-c(1)]
# scenarios <- c(scenarios, "C:/Users/swflake/Documents/TCSI-conservation-finance/Models/Model runs/Scenario6 - miroc - test necnv7")
# scenarios <- scenarios[c(85, 73)]
# scenarios <- scenarios[c(1,2,3,4,7)]
# scenarios <- scenarios[c(6:10, 16, 94:96)]

#some helper functions
read_plus <- function(flnm) {
  read_csv(flnm, show_col_types = FALSE) %>% 
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

#set scenarios manually if needed
# scenario_type$mgmt <- c(rep("Scenario1", 5), "Scenario10", "Scenario7", "Scenario8", "Scenario9")


fire_summaries <- paste0(scenarios, "/scrapple-summary-log.csv")  %>%
    purrr::map_df(~read_plus(.)) %>%
    left_join(scenario_type, c("run_name" = "run_name"))

# fire_summaries[325:405, ]$mgmt <- "new"
# fire_summaries[325:405, ]$climate <- "miroc"
#----------------------

fire_summaries$TotalBurnedSites <- fire_summaries$TotalBurnedSitesAccidental + 
  fire_summaries$TotalBurnedSitesLightning + 
  fire_summaries$TotalBurnedSitesRx
fire_summaries$TotalFires <- fire_summaries$NumberFiresAccidental + 
  fire_summaries$NumberFiresLightning + 
  fire_summaries$NumberFiresRx
(fire_summaries$TotalBurnedSites / fire_summaries$TotalFires) #average area burned per fire by year


test <- fire_summaries %>%
  filter(climate == "Historical") %>%
  group_by(mgmt, run_name) %>%
  summarise(rx = sum(TotalBurnedSitesRx) / 275414 / 80) %>%
  group_by(mgmt) %>%
  summarise(rx = mean(rx))

#------------------------------------
#SCRPPLE events
fire_events <- paste0(scenarios, "/scrapple-events-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name"))


#-------------------------------------------------------------------------------
# process fire rasters
# this can take a long time

#first need to separate wildfire and rx fire

get_burn_intensity <- function(raster, intensity){
  return(sum(terra::values(raster) >= intensity))
}

years <- 1:81
#need to summarize fire data to 5-year chunks to compare with NECN data
#first need to run script to separate wildfire and rx fire
year_bins <- cut(years, breaks = seq(0,81, by = 5))

intensity_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "wildfire_intensity-", years, ".tif")

high_intensity_cells <- NA
for(i in 1:length(intensity_paths)){
  #TODO remake this a purrr::map workflow
  high_intensity_cells[i] <- terra::rast(intensity_paths[i]) %>%
    get_burn_intensity(., 5)

  }

fire_summaries$TotalSitesHighIntensity <- high_intensity_cells

fuel_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "fine-fuels-", years, ".img")

fine_fuel_landscape_average <- NA
for(i in 1:length(intensity_paths)){
  #TODO remake this a purrr::map workflow
  fuel_rast <- terra::rast(fuel_paths[i])
  fuel_cells <- terra::values(fuel_rast)[terra::values(fuel_rast)>0]
  fine_fuel_landscape_average[i] <- mean(fuel_cells)
}

fire_summaries$LandscapeFineFuels <- fine_fuel_landscape_average

#TODO extract more information from rasters?

# aggregate to five-year chunks
# scr_summaries_5_year <- scr_summaries %>%
#   dplyr::mutate(year_round = plyr::round_any(SimulationYear, 5, f = ceiling)) %>%
#   dplyr::group_by(id, year_round) %>%
#   dplyr::summarise(across(where(is.numeric), sum))


test <- fire_summaries %>% 
  # filter(climate == "Historical") %>%
  group_by(run_name) %>%
  summarise(HighIntensity = sum(TotalSitesHighIntensity),
            FineFuels = mean(LandscapeFineFuels),
            FineFuelVar = var(LandscapeFineFuels)/FineFuels,
            TotalSites = sum(TotalBurnedSitesAccidental + TotalBurnedSitesLightning),
            climate = climate[1],
            mgmt = mgmt[1])

boxplot(TotalSites ~ mgmt, data = test)
boxplot(HighIntensity ~ mgmt, data = test)
boxplot(I(HighIntensity/TotalSites) ~ mgmt, data = test)
boxplot(FineFuels ~ mgmt, data = test)
boxplot(FineFuelVar ~ mgmt, data = test)

test2 <- fire_events %>%
  # filter(climate == "Historical") %>%
  group_by(run_name) %>%
  summarise(MeanFineFuels = mean(MeanFineFuels),
            MeanLadderFuels = mean(MeanLadderFuels),
            climate = climate[1],
            mgmt = mgmt[1])

boxplot(MeanFineFuels ~ mgmt, data = test2)
boxplot(MeanLadderFuels ~ mgmt, data = test2)


#-----------------------------------------------------------------------------
#area burned per fire

fire_events_accidental <-fire_events %>%
  filter(IgnitionType == "Accidental")
fire_events_lightning <-fire_events %>%
  filter(IgnitionType == "Lightning")

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
  facet_wrap(~ mgmt + climate)

ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalBurnedSitesAccidental * 8)) + 
  geom_point(color="steelblue") + 
  labs(title = "Accidental burn area",
       subtitle = "by management scenario and climate scenario",
       y = "Area burned (acres)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ climate + mgmt)

ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalBurnedSitesLightning * 8)) + 
  geom_point(color="steelblue") + 
  labs(title = "Lightning burn area",
       subtitle = "by management scenario and climate scenario",
       y = "Area burned (acres)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate)

# ggplot(data = fire_summaries[fire_summaries$Year > 2025, ], 
#        mapping = aes(x = Year, y = LandscapeFineFuels)) + 
#   geom_point(color="steelblue") + 
#   labs(title = "Landscape average fine fuel load",
#        subtitle = "by management scenario and climate scenario",
#        y = "Fine fuel load (g m-2)", x = "Year") + 
#   geom_smooth( color = "black") + 
#   facet_wrap(~ mgmt + climate)

# 
# fire_subset <- fire_summaries[which(grepl("Scenario8|Scenario9|Scenario10|Scenario7", fire_summaries$run_name)), ]
# 
# ggplot(data = fire_subset[fire_subset$climate == "Historical", ], mapping = aes(x = Year, y = TotalBurnedSitesAccidental * 8)) + 
#   geom_point(color="steelblue") + 
#   labs(title = "Accidental burn area",
#        subtitle = "by management scenario and climate scenario",
#        y = "Area burned (acres)", x = "Year") + 
#   geom_smooth( color = "black") + 
#   facet_wrap(~ filename)


#-------------------------------------------------------------------------------
# High-intensity fire


ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalSitesHighIntensity * 3.24)) + 
  geom_point(color="steelblue") + 
  labs(title = "Areaburned at high intensity",
       subtitle = "by management scenario and climate scenario",
       y = "Area burned (hectares)", x = "Year") + 
  geom_smooth( color = "black", se = FALSE) + 
  # facet_wrap(~ mgmt + climate) + 
  facet_wrap(~ run_name) + 
  scale_y_log10()

ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalSitesHighIntensity/(TotalBurnedSitesAccidental + TotalBurnedSitesLightning))) + 
  geom_point(color="steelblue") + 
  labs(title = "Proportion of sites burned at high intensity",
       subtitle = "by management scenario and climate scenario",
       y = "Proportion of area burned at high intensity", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate)
  # facet_wrap(~ run_name)


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
  facet_wrap(~ mgmt + climate)
  # facet_wrap(~ run_name)


#-------------------------------------------------------------------------------
# Fire over time -- biomass burned

fire_summaries <- fire_summaries %>%
  group_by(run_name) %>%
  mutate(CumBiomassMort = cumsum(TotalBiomassMortalityAccidental+TotalBiomassMortalityLightning)) %>%
  ungroup()


ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalBiomassMortalityRx)) + 
  geom_point(color="steelblue") + 
  labs(title = "Prescribed burn area",
       subtitle = "by management scenario and climate scenario",
       y = "Biomass burned (units)", x = "Year") + 
  geom_smooth( color = "black") +
  facet_wrap(~run_name)
  # facet_wrap(~ mgmt + climate)
  # facet_wrap(~ mgmt + climate, nrow = 3, ncol = 3)

ggplot(data = fire_summaries, mapping = aes(x = Year, y = I(TotalBiomassMortalityLightning+TotalBiomassMortalityAccidental))) + 
  geom_point(color="steelblue") + 
  labs(title = "Biomass killed by fire",
       subtitle = "by management scenario and climate scenario",
       y = "Biomass killed (g)", x = "Year") + 
  geom_smooth( color = "black") + 
  # facet_wrap(~ mgmt + climate)
  facet_wrap(~run_name)

ggplot(data = fire_summaries, mapping = aes(x = Year, y = I((TotalBiomassMortalityLightning+TotalBiomassMortalityAccidental)*(180*180)/10000/1000))) + 
  geom_point(color="steelblue") + 
  labs(title = "Biomass killed by wildfire",
       subtitle = "by management scenario and climate scenario",
       y = "Biomass killed (Mg)", x = "Year") + 
  geom_smooth( color = "black") + 
  scale_y_log10() + 
  # facet_wrap(~ mgmt + climate)
  facet_wrap(~run_name)

ggplot(data = fire_summaries, 
       mapping = aes(x = Year, y = CumBiomassMort*(180*180)/10000/1000)) + 
  geom_point(color="steelblue") + 
  labs(title = "Cumulative biomass burned",
       subtitle = "by management scenario and climate scenario",
       y = "Biomass (Mg)", x = "Year") + 
  # geom_smooth(aes(color = filename)) + 
  facet_wrap(~ mgmt + climate)
  # facet_wrap(~run_name)
  # facet_wrap(~ mgmt + climate, nrow = 2, ncol = 3, dir = "v")

### compare fire and beetles
#bda_summaries comes from process_bda_tables.R

combined <- fire_summaries %>%
  left_join(dplyr::select(bda_summaries2, !c("mgmt", "climate")),  by = c("run_name", "Year")) %>%
  mutate((across(c("TotalBiomassKilled", "TotalSitesAffected"), ~replace(., is.na(.), 0)))) %>%
  mutate(TotalBiomassWildfire = TotalBiomassMortalityAccidental + TotalBiomassMortalityLightning,
         TotalBurnedSites = TotalBurnedSitesAccidental + TotalBurnedSitesLightning) %>%
  filter(climate != "CNRM")

combined_long <- combined %>%
  dplyr::rename(Wildfire = TotalBiomassWildfire,
                Insects = TotalBiomassKilled) %>%
  tidyr::pivot_longer(cols = c(Wildfire, Insects),
                      names_to = "Disturbance_type",
                      names_ptypes = list("TotalBiomassWildfire"="Wildfire", "TotalBiomassKilled"="Insects"),
                      values_to = "Biomass") %>%
  dplyr::mutate(Biomass = Biomass * 180*180/10000/100)

ggplot() + 
  labs(title = "Area affected by fire and beetles",
       subtitle = "by management scenario and climate scenario",
       y = "Area affected (acres)", x = "Year") + 
  geom_smooth(color="steelblue", data = combined[combined$climate == "Historical", ], mapping = aes(x = Year, y = TotalBurnedSites * 8)) + 
  geom_smooth(color="green", data = combined[combined$climate == "Historical", ], mapping = aes(x = Year, y = TotalSitesAffected * 8)) + 
  facet_wrap(~ mgmt + climate, dir = "v") + 
  scale_color_manual(name = "", values = c("TotalBurnedSites" = "steelblue", "TotalSitesAffected" = "green"))

ggplot() + 
  labs(title = "Biomass killed by fire and beetles",
       subtitle = "by management scenario and climate scenario",
       y = "Biomass killed (Mg year-1)", x = "Year") + 
  geom_smooth(data = combined_long[combined_long$climate == "Historical", ], 
              mapping = aes(x = Year, y = Biomass, colour = Disturbance_type), se = FALSE)  +
  facet_wrap(~mgmt) 





#-------------------------------------------------------------------------------
# Get fires which occur after a harvest

# Match harvests and fires -- overlap and time since treatment
# Get harvest intensity and fire intensity

