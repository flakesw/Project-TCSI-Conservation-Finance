#fix epsg 2163 and 9311 issue
library(terra)
library(sf)
setGDALconfig("OSR_USE_NON_DEPRECATED", value="NO") #so we can still use EPSG:2163


tcsi_mask_9311 <- rast("./Models/Inputs/masks_boundaries/mask_9311_new.tif")
crs(tcsi_mask_9311)


tcsi_shape <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>% #is wgs84
  st_transform(crs = "EPSG:9311")
st_crs(tcsi_shape) #automatically updated to 9311


test <- mask(tcsi_mask_9311, vect(tcsi_shape))
plot(test) #aligned!

dhsvm_grid <-  rast("./Models/Inputs/masks_boundaries/TCSIdomain_90mDEMfilled.tif")
crs(dhsvm_grid)

#original DHSVM layers in EPSG:2163
in_folder <- "E:/TCSI LANDIS/Outputs_to_DHSVM"
raster_list <- list.files(in_folder, full.names = TRUE)
raster_list <- raster_list[grepl(".tif", raster_list)]

out_folder <- "E:/TCSI LANDIS/Outputs_to_DHSVM_reprojected/"

for(i in 1:length(raster_list)){

in_rast <- rast(raster_list[i])
# plot(in_rast)
# in_rast

in_rast_reprojected <- tcsi_mask_9311
values(in_rast_reprojected) <- values(in_rast)
# plot(in_rast_reprojected)

in_rast_dhsvm <- project(in_rast_reprojected, dhsvm_grid, method = "near")
# plot(in_rast_dhsvm)
# in_rast_dhsvm

raster_name <- gsub(" ", "", basename(raster_list[i]))
writeRaster(in_rast_dhsvm, paste0(out_folder, raster_name), overwrite = TRUE)
}
