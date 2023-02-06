#which model runs did we do CWHR for?

cwhr_folder <- "E:/TCSI LANDIS/CWHR/CWHR_diversity_reprojected"
cwhr_files <- list.files(cwhr_folder) %>%
  `[`(grepl("Scenario", .)) %>%
  `[`(!grepl("xml", .))
scenario_names_cwhr <- map(strsplit(cwhr_files, split = "_"), .f = function(x){paste(`[`(x, c(1:3)), collapse = "_")}) %>%
  unique()



### which model runs are in the model runs folder?

runs_folder <- "E:/TCSI LANDIS/LANDIS runs"
runs <- list.files(runs_folder)
# runs <- runs[-c(1, 95:97)]

run_names <- gsub(" - ", "_", runs)
run_names <- gsub(" ", "", run_names)

scenario_names_cwhr[!(scenario_names_cwhr %in% run_names)]

### which runs should we copy over to the CWHR folder?

#fix epsg 2163 and 9311 issue
library(terra)
library(sf)
library("tidyverse")
setGDALconfig("OSR_USE_NON_DEPRECATED", value="NO") #so we can still use EPSG:2163


tcsi_mask_9311 <- rast("./Models/Inputs/masks_boundaries/mask_9311_new.tif")
crs(tcsi_mask_9311)

tcsi_mask_nad83 <- rast("./Models/Inputs/masks_boundaries/mask_nad83.tif")

tcsi_shape <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>% #is wgs84
  st_transform(crs = "EPSG:9311")
st_crs(tcsi_shape) #automatically updated to 9311


runs_to_copy <- runs[run_names %in% scenario_names_cwhr][39:40]


for(i in 1:length(runs_to_copy)){
  fire_folder <- paste0(runs_folder, "/", runs_to_copy[i], "/social-climate-fire")
  run_name_c <- gsub(" - ", "_", runs_to_copy[i])
  run_name_c <- gsub(" ", "", run_name_c)
  
  #which files to move?
  fire_files <- list.files(fire_folder, full.names = TRUE) %>%
    `[`(grepl("rx_fire_intensity|wildfire_intensity", .))
  
  out_fire_name <- paste0(run_name_c, "_", basename(fire_files))
  
  for(j in 1:length(fire_files)){
    in_rast <- rast(fire_files[j])
    in_rast_reprojected <- tcsi_mask_9311
    values(in_rast_reprojected) <- values(in_rast)
    #plot(in_rast_reprojected)
    
    in_rast_cwhr <- project(in_rast_reprojected, tcsi_mask_nad83, method = "near")
    
    terra::writeRaster(in_rast_cwhr, paste0("E:/TCSI LANDIS/CWHR/disturbance_layers_new/", out_fire_name[j]), overwrite = TRUE)
  }
}
