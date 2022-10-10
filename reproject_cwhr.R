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


test <- mask(tcsi_mask_9311, vect(tcsi_shape))
plot(test) #aligned!

#original DHSVM layers in EPSG:2163
in_folder <- "E:/TCSI LANDIS/CWHR/CWHR_diversity"
# in_folder <- "E:/tcsi backup/CWHR_diversity"
raster_list <- list.files(in_folder, full.names = FALSE)
raster_list <- raster_list[grepl(".tif$", raster_list)] #1600 vegetation layers, 405 year-run combos

raster_list_fixed <- gsub(" ", "", raster_list) #1600 vegetation layers, 405 year-run combos
raster_list_fixed <- gsub("-", "_", raster_list_fixed)
raster_list_fixed <- gsub("run", "Run", raster_list_fixed)
raster_list_fixed <- gsub("all_species", "allspecies", raster_list_fixed)
raster_list_fixed <- gsub("meanbeta", "beta", raster_list_fixed)


out_folder <- "E:/TCSI LANDIS/CWHR/CWHR_diversity_reprojected/"

for(i in 1:length(raster_list)){
  
  in_rast <- rast(paste0(in_folder, "/", raster_list[i]))
  #plot(in_rast)
  #in_rast
  
  in_rast_reprojected <- tcsi_mask_9311
  values(in_rast_reprojected) <- values(in_rast)
  plot(in_rast_reprojected)
  
  in_rast_cwhr <- project(in_rast_reprojected, tcsi_mask_nad83, method = "near")
 #plot(in_rast_cwhr)
 #in_rast_cwhr
  
  raster_name <- gsub(" ", "", basename(raster_list_fixed[i]))
  writeRaster(in_rast_cwhr, paste0(out_folder, raster_name), overwrite = TRUE)
}
