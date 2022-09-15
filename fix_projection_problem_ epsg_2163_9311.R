#fix epsg 2163 and 9311 issue
library(terra)
library(sf)
setGDALconfig("OSR_USE_NON_DEPRECATED", value="NO") #so we can still use EPSG:2163


tcsi_mask_terra <- rast("./Models/Inputs/masks_boundaries/mask_old.tif") #supposedly in 9311, actually in 2163
crs(tcsi_mask_terra)

tcsi_shape <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  st_transform(crs = "EPSG:2163")
st_crs(tcsi_shape) #automatically updated to 2163

test <- mask(tcsi_mask_terra, vect(tcsi_shape))
plot(test) #misaligned


#We can use the setting above (setGDALconfig) to assign tcsi_mask_terra to its actual
# crs (2163), then reproject
tcsi_mask_terra <- rast("./Models/Inputs/masks_boundaries/mask_old.tif") #supposedly in 9311, actually in 2163
crs(tcsi_mask_terra) <- "EPSG:2163"
tcsi_mask_terra <- project(tcsi_mask_terra, "EPSG:9311") #explicitly reproject from 2163 to 9311

test <- mask(tcsi_mask_terra, vect(tcsi_shape))
plot(test) #aligned properly!



library("raster")

tcsi_mask_raster <- raster("./Models/Inputs/masks_boundaries/mask_2163.tif")
crs(tcsi_mask_raster)
test <- raster::crop(tcsi_mask_raster, tcsi_shape)

plot(test)


