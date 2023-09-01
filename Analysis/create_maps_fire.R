#create maps of total fire mortality

#-------------------------------------------------------------------------------
# Import SCRPPLE data
#-------------------------------------------------------------------------------
# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.

library("terra")
library("tidyverse")

#what folder do all the runs to be analyze live in?
# scenario_folder <- "E:/TCSI LANDIS/LANDIS runs"
# scenario_folder <- "E:/equal fire"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/Model runs"
scenario_folder <- "./Models/Model runs"
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

scenarios <- scenarios[!grepl("miroc|cnrm", scenarios)]

fire_summaries <- paste0(scenarios, "/scrapple-summary-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name"))

get_burn_intensity <- function(raster, intensity){
  return(sum(terra::values(raster) >= intensity))
}


#read in SCRPPLE rasters
years <- 1:81
year_bins <- cut(years, breaks = seq(0,81, by = 5))

flaming_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "/flaming-consumptions-", years, ".img")
smolder_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "/smolder-consumption-", years, ".img")
rx_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "/ignition-type-", years, ".img")

fire_mortality <- rast()
rx_n_burned <- rast()

for(scen in scenarios){
  # flame_paths <- flaming_paths[grepl(scen, flaming_paths)]s
  # flame_stack <- terra::rast(flame_paths)
  # flame_sum <- sum(flame_stack)
  # smolder_stack <- terra::rast(flame_paths)
  # smolder_sum <- sum(smolder_stack)
  # fire_sum <- flame_sum + smolder_sum
  # names(fire_sum) <- basename(scen)
  
  rx_path <- rx_paths[grepl(scen, rx_paths)]
  rx_stack <- terra::rast(rx_path)
  rx_class <- terra::classify(rx_stack, rcl = matrix(c(4, 1), ncol = 2), other = 0)
  
  rx_sum <- sum(rx_class)
  names(rx_sum) <- basename(scen)
  
  # fire_mortality <- c(fire_mortality, fire_sum)
  rx_n_burned <- c(rx_n_burned, rx_sum)
}



test <- mean(rx_n_burned)
plot(test)

ignition_prob_map <- terra::rast("./Models/Inputs/input_rasters_reproject/scen6rx.tif")
plot(ignition_prob_map)


test_data <- data.frame(n_burned = values(test)[, 1],
                   ignition_weight = values(ignition_prob_map)[, 1])
ggplot(data = test_data, mapping = aes(y = n_burned, x = ignition_weight)) +
  geom_hex() + 
  geom_smooth(method = "lm")
