# Wrangle the harvest summary tables

dirs <- paste0("E:/tcsi_for_nick/Scenario6 - historical - Run ", c(1:5),"/")

library(tidyverse)

# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.

#what folder do all the runs to be analyze live in?
scenario_folder <- "E:/TCSI LANDIS/LANDIS runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE)[-c(1:4)]
# scenarios <- scenarios[-1]

flnm <- paste0(scenarios, "harvest/summary-log.csv")

#some helper functions
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = as.character(flnm),
           run_name = strsplit(flnm, "/")[[1]][4]) #changed this haphazardly; TODO make more flexible
  
}

get_mgmt <- function(scenario){
  list.files(scenario, pattern = "Scenario") %>%
    pluck(1) %>%
    as.character() %>%
    strsplit(x = ., split = "[.]") %>%
    pluck(1, 1)  %>%
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

harvest_summaries <- paste0(scenarios, "/harvest/summary-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name"))

harvest_summaries2 <- harvest_summaries %>%
  group_by(run_name, Time) %>%
  summarise(TotalBiomassHarvested_sum = sum(TotalBiomassHarvested),
            TotalSitesHarvested_sum = sum(HarvestedSites),
            mgmt = mgmt[1],
            climate = climate[1]) 


harvest_summaries2_no_industrial <- harvest_summaries %>%
  filter(!grepl("PCT|CC", .$Prescription)) %>%
  group_by(run_name, Time) %>%
  summarise(TotalBiomassHarvested_sum = sum(TotalBiomassHarvested),
            TotalSitesHarvested_sum = sum(HarvestedSites),
            mgmt = mgmt[1],
            climate = climate[1]) 

harvest_summaries3 <- harvest_summaries %>%
  group_by(run_name) %>%
  summarise(TotalBiomassHarvested_sum = sum(TotalBiomassHarvested),
            TotalSitesHarvested_sum = sum(HarvestedSites),
            mgmt = mgmt[1],
            climate = climate[1])

#-------------------------------------------------------------------------------
# Figures
#-------------------------------------------------------------------------------

#Harvest over time
# plot(harvest_summaries$HarvestedSites ~ harvest_summaries$Time)

harvest_summaries2$AcresHarvested <- harvest_summaries2$TotalSitesHarvested_sum * (180 * 180) / 4046
harvest_summaries2_no_industrial$HectaresHarvested <- harvest_summaries2_no_industrial$TotalSitesHarvested_sum * 180 * 180/10000

ggplot(data = harvest_summaries2_no_industrial[harvest_summaries2_no_industrial$climate == "Historical", ], 
       mapping = aes(x = Time, y = TotalBiomassHarvested_sum)) + 
  geom_point(color="steelblue") + 
  labs(title = "Biomass harvested (excluding industrial forest management)",
       subtitle = "by management scenario and climate scenario",
       y = "Biomass harvested (Mg)", x = "Timestep") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate)

ggplot(data = harvest_summaries2_no_industrial[harvest_summaries2_no_industrial$climate == "Historical", ], 
       mapping = aes(x = Time, y = HectaresHarvested)) + 
  geom_point(color="steelblue") + 
  labs(title = "Area harvested (excluding industrial forest management)",
       subtitle = "by management scenario and climate scenario",
       y = "Area harvested (acres)", x = "Timestep") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate)