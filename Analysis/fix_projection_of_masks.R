#fix epsg 2163 and 9311 issue
library(terra)
library(sf)
setGDALconfig("OSR_USE_NON_DEPRECATED", value="NO") #so we can still use EPSG:2163


tcsi_mask_terra <- rast("./Models/Inputs/masks_boundaries/mask_old.tif") #supposedly in 9311, actually in 2163
crs(tcsi_mask_terra)

tcsi_shape <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  st_transform(crs = "EPSG:9311")
st_crs(tcsi_shape) #automatically updated to 9311

test <- mask(tcsi_mask_terra, vect(tcsi_shape))
plot(test) #misaligned

#We can use the setting above (setGDALconfig) to assign tcsi_mask_terra to its actual
# crs (2163), then reproject
tcsi_mask_terra <- rast("./Models/Inputs/masks_boundaries/mask_old.tif") #supposedly in 9311, actually in 2163
crs(tcsi_mask_terra) <- "EPSG:2163"

tcsi_mask_nad83 <- project(tcsi_mask_terra, "EPSG:26910", method = "near")
# nad83_blank <- rast(crs = "EPSG:26910", extent = ext(tcsi_mask_nad83), nrow = 800, ncol = 650)
# tcsi_mask_nad83 <- project(tcsi_mask_nad83, nad83_blank, method = "near")
writeRaster(tcsi_mask_nad83, "mask_nad83.tif", overwrite = TRUE)

tcsi_mask_9311 <- project(tcsi_mask_terra, "EPSG:9311", method = "near") #explicitly reproject from 2163 to 9311
r_9311_blank <- rast(crs = "EPSG:9311", extent = ext(tcsi_mask_9311), nrow = 800, ncol = 650)
tcsi_mask_9311 <- project(tcsi_mask_9311, r_9311_blank, method = "near")
tcsi_mask_9311[tcsi_mask_9311 == 0] <- NA
writeRaster(tcsi_mask_9311, "mask_9311.tif", overwrite = TRUE)

test <- rast("E:/TCSI LANDIS/Outputs_to_DHSVM/ Scenario1 - cnrm - Run 1 -  can-ht- 0 .tif")
crs(test) <- "EPSG:2163"
test_nad83 <- project(test, tcsi_mask_nad83)
writeRaster(test_nad83, "test_raster_nad83.tif")

test_nad83_v2 <- tcsi_mask_nad83
values(test_nad83_v2) <- values(test)
writeRaster(test_nad83_v2, "test_raster_nad83v2.tif")

#best method
test <- rast("E:/TCSI LANDIS/Outputs_to_DHSVM/ Scenario1 - cnrm - Run 1 -  can-ht- 0 .tif")
test_v3 <- tcsi_mask_9311
values(test_v3) <- values(test)
writeRaster(test_v3, "test_raster_9311.tif")

test <- rast("E:/TCSI LANDIS/Outputs_to_DHSVM/ Scenario1 - cnrm - Run 1 -  can-ht- 0 .tif")
crs(test) <- "EPSG:2163"
test_v4 <- project(test, "EPSG:9311")
writeRaster(test_v4, "test_raster_9311_v2.tif")
