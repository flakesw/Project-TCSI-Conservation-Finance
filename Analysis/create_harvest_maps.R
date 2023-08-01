#create maps of total fire mortality

#-------------------------------------------------------------------------------
# Import SCRPPLE data
#-------------------------------------------------------------------------------
# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.

#what folder do all the runs to be analyze live in?
scenario_folder <- "E:/TCSI LANDIS/LANDIS runs"
# scenario_folder <- "E:/equal fire"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/Model runs"
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

fire_summaries <- paste0(scenarios, "/scrapple-summary-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name"))

get_burn_intensity <- function(raster, intensity){
  return(sum(terra::values(raster) >= intensity))
}


#read in SCRPPLE rasters
years <- 1:81
year_bins <- cut(years, breaks = seq(0,81, by = 5))

prescripts_paths <- paste0(rep(paste0(scenarios, "/harvest/"), each = length(years)), "/prescripts-", years, ".tif")
biomass_harvested_paths <- paste0(rep(paste0(scenarios, "/harvest/"), each = length(years)), "/biomass-removed-", years, ".tif")

biomass_removed_all <- rast()
rx_n_burned <- rast()
scen2 <- scenarios[103:105]

for(scen in scen2){
  
  biomass_harvested_path <- biomass_harvested_paths[grepl(scen, biomass_harvested_paths)]
  bio_stack <- terra::rast(biomass_harvested_path)
  
  bio_sum <- sum(bio_stack)
  names(bio_sum) <- basename(scen)
  
  # fire_mortality <- c(fire_mortality, fire_sum)
  biomass_removed_all <- c(biomass_removed_all, bio_sum)
}

test <- mean(biomass_removed_all[[42:44]])/100
plot(test, main = "Biomass removed by harvest, Mg/ha over 80 years")

ignition_prob_map <- terra::rast("./Models/Inputs/input_rasters_reproject/scen4rx.tif")
plot(ignition_prob_map)


test_data <- data.frame(n_burned = values(test)[, 1],
                   ignition_weight = values(ignition_prob_map)[, 1])
ggplot(data = test_data, mapping = aes(y = n_burned, x = ignition_weight)) +
  geom_hex() + 
  geom_smooth(method = "lm")
