#Create new management scenarios

library("stars")
library("sf")
library("tidyverse")
library("terra")
setGDALconfig("OSR_USE_NON_DEPRECATED", value="NO") #so we can still use EPSG:2163


project_to_template <- function(input_raster, template){
  #function to project landis input rasters with no CRS to an input file
  input_raster <- terra::rast(input_raster)
  #replace values of template with values from input raster
  out_raster <- template %>%
    `values<-`(values(input_raster))
  
  return(out_raster)
}

tcsi_mask_terra <- rast("./Models/Inputs/masks_boundaries/mask_9311.tif") 
empty_mask <- tcsi_mask_terra
empty_mask$mask.tif <- 0
tcsi_shape <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  st_transform(crs = "EPSG:9311")

test <- mask(tcsi_mask_terra, vect(tcsi_shape))
plot(test) #check alignment

#TODO do we need more stands? Weird gaps in WUI
stands <- terra::rast("./Models/Inputs/input_rasters_reproject/stands180_yr20.tif") %>%
  terra::set.crs("EPSG:2163") %>%
  terra::project("EPSG:9311")

test <- mask(stands,vect(tcsi_shape))
plot(test) #check alignment





