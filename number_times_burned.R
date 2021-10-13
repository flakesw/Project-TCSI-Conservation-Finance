#fire landscape times burned
library("RWmisc")
library("sf")
library("raster")
library("tidyverse")

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance")

project_to_template <- function(input_raster, template){
  #function to project landis input rasters with no CRS to an input file

  if(input_raster@ncols == template@ncols & input_raster@nrows == template@nrows){
    #replace values of template with values from input raster
    out_raster <- template %>%
      `values<-`(values(input_raster))
  } else print("Rasters have different numbers of rows or cols")

  # plot(out_raster)

  return(out_raster)
}

tcsi_poly <- st_read("./masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  st_make_valid() %>%
  st_set_crs("EPSG:3857") %>% #originally in EPSG:3857 
  st_transform("EPSG:2163")

mtbs_poly <- sf::st_read("./calibration data/mtbs/mtbs_perimeter_data/mtbs_perims_DD.shp") %>%
  sf::st_make_valid() %>%
  sf::st_transform(crs(tcsi_poly)) %>%
  sf::st_intersection(tcsi_poly)
# something is wrong with this shapefile geometry
# class(st_geometry(mtbs_poly))
# plot(st_geometry(mtbs_poly))

geomac_poly <- sf::st_read("./calibration data/geomac fire boundaries/Historic_GeoMAC_Perimeters_Combined_2000-2018-shp/US_HIST_FIRE_PERIMTRS_2000_2018_DD83.shp")%>%
  sf::st_make_valid() %>%
  sf::st_transform(crs(tcsi_poly)) %>%
  sf::st_intersection(tcsi_poly) %>%
  sf::st_cast("POLYGON")

grid <- raster("./TCSI_Scenario1/mask.tif") %>%
  projectRaster(crs = crs(geomac_poly))

overlap <- overlap.weight(grid, geomac_poly, count = TRUE, warn = TRUE)
writeRaster(overlap, "./test/geomac_overlap.tif", overwrite = TRUE)

grid <- raster("./TCSI_Scenario1/mask.tif")

#raster overlap
rast_names <- paste0("day-of-fire-", 1:20, ".img")
fire_stack <- stack(paste0("./TCSI_Scenario1/social-climate-fire/", rast_names))
fire_stack <- reclassify(fire_stack, c(0,0,NA,1,365,1,366,999,0))

fire_count <- sum(fire_stack)
plot(fire_count)

fire_reproject <- project_to_template(fire_count, grid)

writeRaster(fire_reproject, "./test/scenario1_overlap.tif", overwrite = TRUE)

#same for scenario 2
rast_names <- paste0("day-of-fire-", 1:20, ".img")
fire_stack <- stack(paste0("./TCSI_Scenario2/social-climate-fire/", rast_names))
fire_stack <- reclassify(fire_stack, c(0,0,NA,1,365,1,366,999,0))

fire_count <- sum(fire_stack)
plot(fire_count)

fire_reproject <- project_to_template(fire_count, grid)

writeRaster(fire_reproject, "./test/scenario2_overlap.tif", overwrite = TRUE)
