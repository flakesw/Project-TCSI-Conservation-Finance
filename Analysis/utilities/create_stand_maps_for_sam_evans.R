library(terra)
library(sf)
setGDALconfig("OSR_USE_NON_DEPRECATED", value="NO") #so we can still use EPSG:2163


project_to_template <- function(input_raster, template){
  #function to project landis input rasters with no CRS to an input file
  
  #replace values of template with values from input raster
  out_raster <- template
  terra::values(out_raster) <- values(input_raster)
  
  return(out_raster)
}

#import boundary data
tcsi_mask_terra <- rast("./Models/Inputs/masks_boundaries/mask_9311.tif") 


stands <- terra::rast("./Models/Inputs/input_rasters_reproject/stands180_yr20.tif") %>%
  project_to_template(tcsi_mask_terra)

stands_nad83 <- stands %>% project("EPSG:26910", method = "near")

tcsi_shape <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  st_transform(crs = "EPSG:9311")

test <- mask(stands, vect(tcsi_shape))
plot(test) #check alignment



terra::writeRaster(stands, "stands_equal_area.tif", overwrite = TRUE)
terra::writeRaster(stands_nad83, "stands_nad83.tif", overwrite = TRUE)

stands_shape <- terra::as.polygons(stands, dissolve = TRUE) %>%
  sf::st_as_sf()
plot(stands_shape)

sf::write_sf(stands_shape, "stands_poly_equal_area.shp")


