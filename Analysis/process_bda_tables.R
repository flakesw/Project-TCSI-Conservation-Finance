# Wrangle the BDA tables

# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.


library("tidyverse")

#what folder do all the runs to be analyzed live in?
scenario_folder <- "E:/TCSI LANDIS/LANDIS runs/"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/Model runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) %>%
  `[`(grep("Scenario", .))

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
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(5, 1)))) %>%
  mutate(mgmt = unlist(map(scenarios, get_mgmt))) %>%
  mutate(climate = ifelse(grepl(pattern = "miroc", run_name), "MIROC", 
                          ifelse(grepl(pattern = "cnrm", run_name), "CNRM", "Historical"))) 

bda_summaries <- paste0(scenarios, "/bda_log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name"))

bda_summaries2 <- bda_summaries %>%
  group_by(run_name, Time) %>%
  summarise(TotalBiomassKilled = sum(TotalBiomassMortality),
            TotalSitesAffected = sum(DamagedSites),
            mgmt = mgmt[1],
            climate = climate[1])

bda_summaries2 <- bda_summaries2 %>%
  filter(mgmt %in% c("Scenario1", "Scenario3", "Scenario6"))


# #---------------------
# #do it manually if needed
# scenarios <- c("./Analysis/Test/scen1/scrapple-summary-log.csv",
#                "./Analysis/Test/scen1/scrapple-summary-log (1).csv",
#                "./Analysis/Test/scen1/scrapple-summary-log (2).csv",
#                "./Analysis/Test/scen1/scrapple-summary-log (3).csv",
#                "./Analysis/Test/scen1/scrapple-summary-log (4).csv",
#                "./Analysis/Test/scen6/scrapple-summary-log.csv",
#                "./Analysis/Test/scen6/scrapple-summary-log (1).csv",
#                "./Analysis/Test/scen6/scrapple-summary-log (2).csv",
#                "./Analysis/Test/scen6/scrapple-summary-log (3).csv",
#                "./Analysis/Test/scen6/scrapple-summary-log (4).csv",
#                "./Analysis/Test/scen1miroc/scrapple-summary-log.csv",
#                "./Analysis/Test/scen1miroc/scrapple-summary-log (1).csv",
#                "./Analysis/Test/scen1miroc/scrapple-summary-log (2).csv",
#                "./Analysis/Test/scen1miroc/scrapple-summary-log (3).csv",
#                "./Analysis/Test/scen1miroc/scrapple-summary-log.csv",
#                "./Analysis/Test/scen6miroc/scrapple-summary-log.csv",
#                "./Analysis/Test/scen6miroc/scrapple-summary-log (1).csv",
#                "./Analysis/Test/scen6miroc/scrapple-summary-log (2).csv",
#                "./Analysis/Test/scen6miroc/scrapple-summary-log (3).csv",
#                "./Analysis/Test/scen6miroc/scrapple-summary-log (4).csv")
# 
# fire_summaries <- scenarios %>%
#   purrr::map_df(~read_plus(.))
# 
# scenario_type <- data.frame(filename = scenarios,
#                             mgmt = rep(c(1,1,1,1,1,6,6,6,6,6), times = 2),
#                             climate = rep(c("historical", "miroc"), each = 10))

#-------------------------------------------------------------------------------
# Figures
#-------------------------------------------------------------------------------

#Harvest over time
bda_summaries2$Year <- bda_summaries2$Time + 2020

harvest_summaries2$AcresHarvested <- harvest_summaries2$TotalSitesHarvested_sum * (180 * 180) / 4046

ggplot(data = bda_summaries2, mapping = aes(x = Time, y = TotalBiomassKilled)) + 
  geom_point(color="steelblue") + 
  labs(title = "Biomass killed by beetles",
       subtitle = "by management scenario and climate scenario",
       y = "Biomass killed (Mg)", x = "Timestep") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate)

ggplot(data = bda_summaries2, mapping = aes(x = Time, y = TotalSitesAffected)) + 
  geom_point(color="steelblue") + 
  labs(title = "Area affected by beetles",
       subtitle = "by management scenario and climate scenario",
       y = "Sites affected", x = "Timestep") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate, nrow = 3, ncol = 2)




