
maxyear <- 80

#what folder do all the runs to be analyze live in?
scenario_folder <- "E:/TCSI LANDIS/LANDIS runs"

# scenario_folder <- "C:/Users/swflake/Documents/TCSI-conservation-finance/Models/Model runs"
# scenario_folder <- "C:/Users/swflake/Documents/TCSI-conservation-finance/Models/Model runs/scrpplev2 testing"
#   `[`(grep("Scenario", .)) %>%
#   `[`(grep("historical", .))


scenarios <- list.dirs(scenario_folder, recursive = FALSE) %>%
  `[`(grep("Scenario", .))
# scenarios <- scenarios[-1]
# scenarios <- scenarios[4:8]
# scenarios <- scenarios[c(6:10, 16, 94:96)]
scenarios <- scenarios[19:110]
# scenarios <- scenarios[c(1,3,5)]

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

#------------------------------------
#SCRPPLE events
fire_events <- paste0(scenarios, "/scrapple-events-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name"))%>%
  dplyr::filter(SimulationYear <= maxyear) # %>%
#dplyr::filter(TotalSitesBurned > (1000/8)) #match MTBS criteria

write.csv(fire_events, "./fire_events_all_runs_for_Haojie.csv")

#------------------------------------
#BDA events
bda_events <- paste0(scenarios, "/bda_log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name")) #%>%
  # dplyr::filter(SimulationYear <= maxyear) # %>%
#dplyr::filter(TotalSitesBurned > (1000/8)) #match MTBS criteria

write.csv(bda_events, "./bda_events_all_runs_for_Haojie.csv")


