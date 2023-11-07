# Wrangle the NECN biomass tables

# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.

library("tidyverse")

#what folder do all the runs to be analyzed live in?
scenario_folder <- "E:/TCSI LANDIS/LANDIS runs"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/Model templates"
# scenario_folder <- "./Models/Model runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) %>%
  `[`(grep("Scenario", .))
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
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(4, 1)))) %>% #change to fit scenario name
  mutate(mgmt = unlist(map(scenarios, get_mgmt))) %>%
  mutate(climate = ifelse(grepl(pattern = "miroc", run_name), "MIROC", 
                          ifelse(grepl(pattern = "cnrm", run_name), "CNRM", "Historical"))) 

# scenario_type$fire_model <- rep(c("fixed", "mixed"), each = 3)

necn_summaries <- paste0(scenarios, "/NECN-succession-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name"))

necn_summaries2 <- necn_summaries %>%
  group_by(run_name, Time) %>%
  summarise(TotalAGB = weighted.mean(AGB, NumSites),
            TotalC = weighted.mean(AGB + SOMTC, NumSites),
            SOMTC = weighted.mean(SOMTC, NumSites),
            NPP = weighted.mean(AG_NPPC, NumSites),
            mgmt = mgmt[1],
            climate = climate[1])
# necn_summaries2[69:85, ]$mgmt <- "new"
#-------------------------------------------------------------------------------
# Figures
#-------------------------------------------------------------------------------

#Biomass over time

ggplot(data = necn_summaries2[necn_summaries2$climate == "CNRM", ], 
       mapping = aes(x = Time+2020, y = TotalAGB)) + 
  geom_point(color="steelblue") + 
  labs(title = "Aboveground biomass",
       subtitle = "by management scenario and climate scenario",
       y = "Average AGB (g m-2)", x = "Simulation Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate, ncol = 3, dir = "v")


ggplot(data = necn_summaries2, mapping = aes(x = Time+2020, y = SOMTC)) + 
  geom_point(color="steelblue") + 
  labs(title = "SOMTC",
       subtitle = "by management scenario and climate scenario",
       y = "Average SOM C (g m-2)", x = "Simulation Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate, ncol = 3, dir = "v")

ggplot(data = necn_summaries2, mapping = aes(x = Time+2020, y = NPP)) + 
  geom_point(color="steelblue") + 
  labs(title = "NPP",
       subtitle = "by management scenario and climate scenario",
       y = "AG NPP (g m-2)", x = "Simulation Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate, ncol = 3, dir = "v")

