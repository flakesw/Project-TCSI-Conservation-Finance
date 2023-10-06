## raster of WUI area
library("stars")
library("sf")
library("tidyverse")
library("terra")
setGDALconfig("OSR_USE_NON_DEPRECATED", value="NO") #so we can still use EPSG:2163
options(warn = 0)

#TODO: download and use other no-disturbance model runs
#Compare final biomass values to targets

project_to_template <- function(input_raster, template){
  #function to project landis input rasters with no CRS to an input file
  
  #replace values of template with values from input raster
  out_raster <- template
  terra::values(out_raster) <- values(input_raster)
  
  return(out_raster)
}

#import boundary data
tcsi_mask_terra <- rast("./Models/Inputs/masks_boundaries/mask_9311.tif") 
tcsi_mask_nad83 <- rast("./Models/Inputs/masks_boundaries/mask_nad83.tif")
# empty_mask <- tcsi_mask
# empty_mask$mask.tif <- 0
tcsi_shape <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  st_transform(crs = "EPSG:9311")

tcsi_buffered <- sf::st_buffer(tcsi_shape, dist = 5000)

#wui map from SILVIS lab
ca_wui <- sf::st_read("D:/Data/WUI/ca_wui_cp12/ca_wui_cp12.shp") %>%
  sf::st_transform(crs = sf::st_crs(tcsi_buffered)) %>%
  sf::st_intersection(tcsi_buffered)
# plot(st_geometry(ca_wui))
nv_wui <- sf::st_read("D:/Data/WUI/nv_wui_cp12/nv_wui_cp12.shp")%>%
  sf::st_transform(crs = sf::st_crs(tcsi_buffered)) %>%
  sf::st_intersection(tcsi_buffered)
# plot(st_geometry(nv_wui))

wui <- dplyr::bind_rows(ca_wui, nv_wui)
# plot(sf::st_geometry(wui)) 
rm(ca_wui, nv_wui)

# plot(wui["WUIFLAG10"])

#buffer for wui zones, 1/4 mile and 1 1/2 mile 
#https://www.fs.usda.gov/detail/cleveland/landmanagement/planning/?cid=fsbdev7_016495

wui_threat <- wui[wui$WUIFLAG10 %in% c(1,2), ] %>%
  sf::st_buffer(dist = 400) %>%
  sf::st_union() %>%
  sf::st_intersection(tcsi_buffered)
wui_defense <- wui[wui$WUIFLAG10 %in% c(1,2), ] %>%
  sf::st_buffer(dist = 2400) %>%
  sf::st_union()%>%
  sf::st_intersection(tcsi_buffered)

plot(sf::st_geometry(wui_threat))
plot(sf::st_geometry(wui_defense), add = TRUE)

#PIF = 1-10
#PNIF = 11-20
#wui threat = 30 - 39
#wui defense = 20 - 29
#general forest = 30-39
#general forest only hand thinning = 40-48
#wilderness = 49
#general forest = 51-60

#even = steep slopes?


tz_scen1 <- terra::rast("./Models/Inputs/input_rasters_reproject/tz1_20_ssp2_v3.tif")
plot(tz_scen1)
