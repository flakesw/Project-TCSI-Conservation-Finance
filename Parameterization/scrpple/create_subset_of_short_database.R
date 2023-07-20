#create subsets of Short database

library("sf")
library("raster")
library("tidyverse")

# short dataset: https://www.fs.usda.gov/rds/archive/Catalog/RDS-2013-0009.5
# import the full dataset
short_full <- sf::st_read("./Parameterization/calibration data/short_ignitions/Data/FPA_FOD_20210617.gdb",
                          layer = "Fires") %>%
  sf::st_transform(crs = "EPSG:9311")

# raster map of TCSI
mask <- raster("./Models/Inputs/masks_boundaries/mask.tif")
crs(mask) <- "EPSG:9311" #replaces deprecated epsg:2163; US National Atlas Equal Area

tcsi_polygon <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  sf::st_transform(crs(short_full))

ca_region <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  summarise(do.union = TRUE) %>% 
  st_make_valid() %>%
  smoothr::fill_holes(threshold = 0.5) %>%
  st_transform(crs(short_full))

#create reduced dataset for sierra
short_ca <- short_full %>%
  st_intersection(ca_region)

sf::st_write(short_ca, "./Parameterization/calibration data/short_ignitions/short_sierra.gpkg")
saveRDS(short_ca, "./Parameterization/calibration data/short_ignitions/short_sierra.RDS")


#create reduced dataset for tcsi
short_tcsi <- short_full %>%
  st_intersection(tcsi_polygon) %>%
  st_zm()

sf::st_write(short_tcsi, "./Parameterization/calibration data/short_ignitions/short_tcsi.gpkg")
saveRDS(short_tcsi, "./Parameterization/calibration data/short_ignitions/short_tcsi.RDS")


#drop geometry and write csv
short_no_geom <- short_full %>%
  sf::st_drop_geometry()

write.csv(short_no_geom, "./Parameterization/calibration data/short_ignitions/short_drop_geometry.csv")
