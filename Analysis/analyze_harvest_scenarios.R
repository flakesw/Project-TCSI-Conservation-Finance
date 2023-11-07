# figure of area treated by each type of treatment
library("tidyverse")
ncells <- 275414

#some helper functions
read_plus <- function(flnm) {
  read_csv(flnm, show_col_types = FALSE) %>% 
    mutate(filename = as.character(flnm),
           run_name = strsplit(flnm, "/")[[1]][4])
  
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


#what folder do all the runs to be analyze live in?
scenario_folder <- "E:/TCSI LANDIS/LANDIS runs"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/Model runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) %>%
  `[`(grep("Scenario", .)) %>%
  `[`(!grepl("Scenario 11", .))




scenario_type <- data.frame(run_name = character(length(scenarios)), 
                            mgmt = character(length(scenarios)),
                            climate = character(length(scenarios)))

scenario_type <- scenario_type %>%
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(4, 1)))) %>%
  mutate(mgmt = unlist(map(scenarios, get_mgmt))) %>%
  mutate(climate = ifelse(grepl(pattern = "miroc", run_name), "MIROC", 
                          ifelse(grepl(pattern = "cnrm", run_name), "CNRM", "Historical"))) %>%
  filter(mgmt %in% c("Scenario1", "Scenario2", "Scenario3", "Scenario4", "Scenario5", "Scenario6"))


harvest_events <- paste0(scenarios, "/harvest/log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  right_join(scenario_type, c("run_name" = "run_name"))

# harv_bak <- harvest_events

crosswalk <- read.csv("./misc/original_scenarios_crosswalk.csv")

harvest_events <- left_join(harvest_events, crosswalk, by = c("Prescription" = "Prescription.name"))

harvest_sum <- harvest_events %>%
  group_by(run_name, mgmt, climate, Type, Time) %>%
  summarise(Sites = sum(HarvestedSites),
            Biomass = sum(MgBiomassRemoved)) %>%
  ungroup() %>%
  group_by(mgmt, climate, Type, Time) %>%
  summarise(Sites = mean(Sites),
            Biomass = mean(Biomass))

ggplot(filter(harvest_sum, climate == "Historical", mgmt == "Scenario6")) + 
  geom_point(aes(x = Time, y = Sites/ncells, colour = Type)) + 
  geom_smooth(aes(x = Time, y = Sites/ncells, colour = Type)) + 
  ylab("Proportion of landscape harvested")
  facet_wrap(facets = "mgmt")
ggplot(filter(harvest_sum, climate == "Historical")) + 
  geom_point(aes(x = Time, y = Biomass, colour = Type)) + 
  geom_smooth(aes(x = Time, y = Biomass, colour = Type)) + 
  facet_wrap(facets = "mgmt")

ggplot(filter(harvest_sum, climate == "MIROC")) + 
  geom_point(aes(x = Time, y = Sites, colour = Type)) + 
  geom_smooth(aes(x = Time, y = Sites, colour = Type)) + 
  facet_wrap(facets = "mgmt")
ggplot(filter(harvest_sum, climate == "MIROC")) + 
  geom_point(aes(x = Time, y = Biomass, colour = Type)) + 
  geom_smooth(aes(x = Time, y = Biomass, colour = Type)) + 
  facet_wrap(facets = "mgmt")



# test <- harvest_events %>%
#   filter(climate == "Historical") %>%
#   group_by(mgmt, run_name) %>%
#   summarise(rx = sum(HarvestedSites) / 275414 / 80) %>%
#   group_by(mgmt) %>%
#   summarise(rx = mean(rx))
