#compare species compositions

library("tidyverse")
library("terra")

# source("./Analysis/r_functions.R")
theme_set(theme_bw())
theme_update(panel.grid.minor = element_blank(),
             strip.background = element_rect(fill = "white"))

# Wrangle the NECN biomass tables

# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.

library("tidyverse")

#what folder do all the runs to be analyzed live in?
# scenario_folder <- "E:/TCSI LANDIS/LANDIS runs"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/Model templates"
scenario_folder <- "./Models/Model runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) %>%
  `[`(grep("Scenario", .))
scenarios <- scenarios[c(1:9)]
# scenarios <- scenarios[c(1,3,5,7,9,11,13)]
# scenarios <- scenarios[2:6]
# scenarios <- scenarios[c(88:91)]
# scenarios <- c(scenarios, "C:/Users/swflake/Documents/TCSI-conservation-finance/Models/Model runs/Scenario6 - miroc - test necnv7")
# scenarios <- scenarios[-74]
# scenarios <- scenarios[c(7,81,93,96,99)]

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
    pluck(1, 1)%>%
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
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(3, 1)))) %>% #change to fit scenario name
  mutate(mgmt = unlist(map(scenarios, get_mgmt))) %>%
  mutate(climate = ifelse(grepl(pattern = "miroc", run_name), "MIROC", 
                          ifelse(grepl(pattern = "cnrm", run_name), "CNRM", "Historical"))) 



#-----------------------------------------------
# Process tabular data
#-----------------------------------------------


regen_summaries <- paste0(scenarios, "/NECN-reproduction-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name")) %>%
  mutate(TotalCohorts = NumCohortsPlanting + NumCohortsSerotiny + NumCohortsResprout + NumCohortsSeed)


biomass_summaries <- paste0(scenarios, "/spp-biomass-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name")) %>%
  filter(EcoName == "eco1") %>%
  pivot_longer(cols =  starts_with("AboveGroundBiomass"),
               names_prefix = "AboveGroundBiomass_",
               names_to = "Species",
               values_to = "Biomass")


func_types <- data.frame(FunctionalGroupIndex = c(1,2,5,3,4),
                         Type = c("Other conifer",
                                  "Hardwood",
                                  "Dry Hardwood",
                                  "Shrub",
                                  "Fir"))
spp_table <- read.csv("./Models/Inputs/necn/NECN_Spp_Table.csv") %>%
  left_join(func_types) %>%
  select(c(SpeciesCode, FunctionalGroupIndex, Type))

biomass_summaries_sp <- biomass_summaries %>%
  left_join(spp_table %>% select(SpeciesCode, Type), by = c("Species" = "SpeciesCode")) %>%
  group_by(run_name, climate, Time, Species) %>%
  summarise(Biomass = mean(Biomass), #average over runs
            Type = Type[1])

biomass_summaries_means <- biomass_summaries_sp %>%
  group_by(climate, Time, Type) %>%
  summarise(Biomass = sum(Biomass)) #sum within functional type

biomass_summaries_points <- biomass_summaries_sp %>%
  group_by(run_name, climate, Time, Type) %>%
  summarise(Biomass = sum(Biomass)) #sum within functional type

biomass_summaries_all <- biomass_summaries %>%
  left_join(spp_table %>% select(SpeciesCode, Type), by = c("Species" = "SpeciesCode")) %>%
  group_by(run_name, climate, Time, Type) %>%
  summarise(Biomass = sum(Biomass)) #sum within functional type

biomass_summaries_total <- biomass_summaries_points %>%
  ungroup() %>%
  group_by(run_name, Time) %>%
  summarise(Biomass = sum(Biomass))
  

#----------------------------------
# Make figures of tabular data
#----------------------------------

sp_comp <- ggplot(data = biomass_summaries_points, 
                  mapping = aes(x = Time+2019, y = Biomass, color = Type)) + 
  # geom_area(position = "stack") +
  geom_point() +
  geom_smooth() +
  labs(y = "Aboveground biomass (g m-2)", x = "Simulation Year") +
  facet_wrap(facets = "run_name") + 
  guides(colour=guide_legend(title="Functional Group"))

# sp_comp <- tag_facet(sp_comp)
sp_comp <- shift_legend2(sp_comp)
plot(sp_comp)

sp_comp <- ggplot(data = biomass_summaries_total, 
                  mapping = aes(x = Time+2019, y = Biomass)) + 
  # geom_area(position = "stack") +
  geom_point() +
  geom_smooth() +
  labs(y = "Aboveground biomass (g m-2)", x = "Simulation Year") +
  facet_wrap(facets = "run_name") + 
  guides(colour=guide_legend(title="Functional Group"))

# sp_comp <- tag_facet(sp_comp)
sp_comp <- shift_legend2(sp_comp)
plot(sp_comp)



sp_comp <- ggplot(data = biomass_summaries_sp, 
                  mapping = aes(x = Time+2019, y = Biomass, col = Species)) + 
  geom_area(position = "stack") +
  # geom_point() +
  # geom_smooth() +
  labs(y = "Aboveground biomass (g m-2)", x = "Simulation Year") +
  facet_wrap(facets = "run_name") + 
  guides(colour=guide_legend(title="Functional Group"))

# sp_comp <- tag_facet(sp_comp)
sp_comp <- shift_legend2(sp_comp)
plot(sp_comp)
