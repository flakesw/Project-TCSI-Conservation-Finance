library(tidyverse)

#cwhr model runs

cwhr_folder <- "E:/TCSI LANDIS/CWHR/CWHR_diversity_reprojected"
scenario_names_cwhr <- list.files(cwhr_folder) %>%
  `[`(grepl("beta", .)) %>%
  strsplit(., split = "_") %>%
  map(.f = function(x){paste(`[`(x, c(1:3)),collapse = "_")}) %>%
  unique() %>% 
  unlist()

#current models -- definitely have all the fire and harvest data
new_runs_folder <- "E:/TCSI LANDIS/LANDIS runs"
scenario_names_new_fire <- list.files(new_runs_folder, recursive = FALSE) %>%
  `[`(grepl("Scenario", .)) %>%
  map(.f = function(x){gsub(" - ", "_", x)}) %>%
  map(.f = function(x){gsub(" ", "", x)}) %>%
  unique() %>% 
  unlist()

#which scenarios don't have the good fire and harvest data?
scenarios_need <- scenario_names_cwhr[!(scenario_names_cwhr %in% scenario_names_new_fire)] %>%
  gsub("_", " - ", .) %>%
  gsub("([[:alpha:]])([[:digit:]])", "\\1 \\2", .) %>%
  sub("[[:space:]]", "", .)


#which of the old runs have good fire and harvest data?
scen_locs <- paste0("E:/TCSI LANDIS/bad runs/", scenarios_need)
fire_runs <- paste0(scen_locs, "/social-climate-fire")
list.files(fire_runs[5])

#4 has fire
#5 has fire

#which of the old runs have good fire and harvest data?
scen_locs <- paste0("E:/TCSI LANDIS/bad runs/", scenarios_need)
fire_runs <- paste0(scen_locs, "/harvest")
list.files(fire_runs[5])
#4 and 5 have harvest







