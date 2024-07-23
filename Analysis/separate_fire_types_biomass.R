#separate rx and wildfire
library("terra")
library("tidyverse")

#what folder do all the runs to be analyze live in?
# scenario_folder <- "E:/TCSI LANDIS/LANDIS runs"
scenario_folder <- "./Models/Model runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) %>%
  `[`(grep("Scenario", .))
scenarios <- scenarios[c(2,4,6,8,10,12,14)]

years <- 1:81

biomass_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "/biomass-mortality-", years, ".img")
fire_type_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "/ignition-type-", years, ".img")

for(scen in scenarios){
  bio_paths <- biomass_paths[grepl(scen, biomass_paths)]
  type_paths <- fire_type_paths[grepl(scen, fire_type_paths)]
  
  bio_stack <- terra::rast(bio_paths)
  fire_type_stack <- terra::rast(type_paths)
  
  rx_fires <- terra::subst(fire_type_stack, c(0,1,2,3,4), c(0,0,0,0,1))
  wildfires <- terra::subst(fire_type_stack, c(0,1,2,3,4), c(0,0,1,1,0))
  
  rx_mort_stack <- terra::mask(bio_stack, rx_fires, maskvalues = 0, updatevalue = 0)
  wildfire_mort_stack  <- terra::mask(bio_stack, wildfires, maskvalues = 0, updatevalue = 0)

  writeRaster(rx_mort_stack, filename = paste0(scen,"/social-climate-fire/rx_fire_mortality-", years,".tif"), overwrite = TRUE)
  writeRaster(wildfire_mort_stack, filename = paste0(scen,"/social-climate-fire/wildfire_mortality-", years,".tif"), overwrite = TRUE)
  
}



##################################
# fire severity maps
#separate rx and wildfire


intensity_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "/fire-intensity-", years, ".img")
fire_type_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "/ignition-type-", years, ".img")

for(scen in scenarios){
  int_paths <- intensity_paths[grepl(scen, intensity_paths)]
  type_paths <- fire_type_paths[grepl(scen, fire_type_paths)]
  
  intensity_stack <- terra::rast(int_paths)
  fire_type_stack <- terra::rast(type_paths)
  
  rx_fires <- terra::subst(fire_type_stack, c(0,1,2,3,4), c(0,0,0,0,1))
  wildfires <- terra::subst(fire_type_stack, c(0,1,2,3,4), c(0,0,1,1,0))
  
  rx_int_stack <- terra::mask(intensity_stack, rx_fires, maskvalues = 0, updatevalue = 0)
  wildfire_int_stack  <- terra::mask(intensity_stack, wildfires, maskvalues = 0, updatevalue = 0)
  
  writeRaster(rx_int_stack, filename = paste0(scen,"/social-climate-fire/rx_fire_intensity-", years,".tif"), overwrite = TRUE)
  writeRaster(wildfire_int_stack, filename = paste0(scen,"/social-climate-fire/wildfire_intensity-", years,".tif"), overwrite = TRUE)
  
}

