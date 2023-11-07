#scrpple outputs to align with census grid
library("sf")
library("terra")
library("tidyverse")
library("gdalUtilities")
sf::sf_use_s2(FALSE)
setGDALconfig("OSR_USE_NON_DEPRECATED", value="NO") #so we can still use EPSG:2163

# sf::st_layers("./Analysis/EJ/TCSI_RevisedCensusBureau_BlockFeatures_10262023.gdb")
census <- sf::st_read("./Analysis/EJ/TCSI_tl2021_Block20Features_REVISED/TCSI_tl2021_Block20_Features_REVISED.shp")

ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  gdalUtilities::ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

census <- ensure_multipolygons(census) %>%
  sf::st_make_valid()

tcsi_mask_9311 <- rast("./Models/Inputs/masks_boundaries/mask_9311_new.tif")
crs(tcsi_mask_9311)


#------------------------------------
##rx fire frequency and mortality biomass

#what folder do all the runs to be analyze live in?
scenario_folder <- "E:/TCSI LANDIS/LANDIS runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) %>%
  `[`(grep("Scenario", .)) %>%
  `[`(!grepl("Scenario7|Scenario8|Scenario9|Scenario 10|Scenario 11", .))

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

# scenarios <- scenarios[!grepl("miroc|cnrm", scenarios)]

fire_summaries <- paste0(scenarios, "/scrapple-summary-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name"))

#read in SCRPPLE rasters
years <- 1:20
year_bins <- cut(years, breaks = seq(0,20, by = 5))

rx_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "/ignition-type-", years, ".img")

rx_n_burned <- rast()

for(scen in scenarios){
  rx_path <- rx_paths[grepl(scen, rx_paths)]
  rx_stack <- terra::rast(rx_path)
  rx_class <- terra::classify(rx_stack, rcl = matrix(c(4, 1), ncol = 2), other = 0)
  
  rx_sum <- sum(rx_class)
  
  rx_sum2 <- tcsi_mask_9311
  rx_sum2[] <- rx_sum[]
  
  names(rx_sum2) <- basename(scen) %>% gsub(" ", "", .)
  
  rx_n_burned <- c(rx_n_burned, rx_sum2)
}


sev_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "/fire-intensity-", years, ".img")

high_sev_n_burned <- rast()

for(scen in scenarios){
  sev_path <- sev_paths[grepl(scen, sev_paths)]
  sev_stack <- terra::rast(sev_path)
  sev_class <- terra::classify(sev_stack, rcl = matrix(c(4, 100, 1), ncol = 3), other = 0, right = FALSE)
  
  sev_sum <- sum(sev_class)
  
  sev_sum2 <- tcsi_mask_9311
  sev_sum2[] <- sev_sum[]
  
  names(sev_sum2) <- basename(scen)%>% gsub(" ", "", .)
  
  high_sev_n_burned <- c(high_sev_n_burned, sev_sum2)
}



#-----------------------
census <- sf::st_transform(census, crs(tcsi_mask_9311))
census_rx <- terra::extract(rx_n_burned, census, fun = function(x) mean(x, na.rm = TRUE), bind=T) %>%
  sf::st_as_sf()
census_rx2 <- census_rx %>%
  pivot_longer(starts_with("Scenario"), values_drop_na=TRUE) %>% 
  separate(name, into=c("Scenario", "Climate", "Replicate"), "[.]", extra = "drop", fill = "right") %>%
  group_by(Scenario, Climate, OBJECTID) %>%
  summarise(n_rx_fires = mean(value))
sf::st_write(census_rx2, "census_with_rx_fire.csv")


census_high_severity <- terra::extract(high_sev_n_burned, census, fun = function(x) mean(x, na.rm = TRUE), bind=T) %>%
  sf::st_as_sf() %>%
  pivot_longer(starts_with("Scenario"), values_drop_na=TRUE) %>% 
  separate(name, into=c("Scenario", "Climate", "Replicate"), "[.]", extra = "drop", fill = "right") %>%
  group_by(Scenario, Climate, OBJECTID) %>%
  summarise(n_severe_fires = mean(value))
sf::st_write(census_high_severity, "census_with_high_severity_fire.csv")



