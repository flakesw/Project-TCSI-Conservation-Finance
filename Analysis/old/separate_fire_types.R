#separate rx and wildfire
library("terra")
library("tidyverse")

#what folder do all the runs to be analyze live in?
scenario_folder <- "E:/TCSI LANDIS/LANDIS runs"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/Model runs"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) %>%
  `[`(grep("Scenario", .))
scenarios <- scenarios[38:52]

years <- 1:81

flaming_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "/flaming-consumptions-", years, ".img")
smolder_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "/smolder-consumption-", years, ".img")
fire_type_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "/ignition-type-", years, ".img")

for(scen in scenarios){
  flame_paths <- flaming_paths[grepl(scen, flaming_paths)]
  smold_paths <- smolder_paths[grepl(scen, smolder_paths)]
  type_paths <- fire_type_paths[grepl(scen, fire_type_paths)]
  
  flame_stack <- terra::rast(flame_paths)
  smolder_stack <- terra::rast(smold_paths)
  fire_stack <- flame_stack + smolder_stack
  fire_type_stack <- terra::rast(type_paths)
  
  rx_fires <- terra::subst(fire_type_stack, c(0,1,2,3,4), c(0,0,0,0,1))
  wildfires <- terra::subst(fire_type_stack, c(0,1,2,3,4), c(0,0,1,1,0))
  
  rx_mort_stack <- terra::mask(fire_stack, rx_fires, maskvalues = 0, updatevalue = 0)
  wildfire_mort_stack  <- terra::mask(fire_stack, wildfires, maskvalues = 0, updatevalue = 0)

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
