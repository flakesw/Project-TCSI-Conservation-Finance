library(sf)
library(tidyverse)
library(data.table)

sf::sf_use_s2(FALSE)


#geomac data are in EPST:4269
sierra <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  sf::st_transform("EPSG:4269")

#merge GeoMAC boundaries into one file

# tcsi_poly <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
#   sf::st_zm() %>%
#   sf::st_transform(crs(template))

geomac_folder <- "D:/Data/geomac_daily/perimeters_2000-2019/"

# in this folder, there is a folder for each year 2000-2019, with the format
# "./calibration data/geomac fire boundaries/2000_perimeters_dd83" 

geomac_files <- list.files(geomac_folder, pattern = ".shp", full.names = TRUE) %>%
  `[`(!grepl("xml", .))

geomac_shape_list <- list()
cols <- c("incidentna", "complexnam", "uniquefire", "gisacres", "irwinid", "perimeterd", "mapmethod", "fireyear")
cols <- c(incidentna = NA_character_, complexnam= NA_character_, uniquefire= NA_character_, gisacres= NA_real_, irwinid = NA_character_, perimeterd= NA_character_, mapmethod= NA_character_, fireyear= NA_real_)

#Geomac data has really inconsistent structure -- different columns and names --
#between years. Really annoying, but we can clean it up in the loop below

for(i in 1:length(geomac_files)){
  geomac_file <- sf::st_read(geomac_files[i])
  geom <- sf::st_geometry(geomac_file)
  geomac_shape_list[[i]] <- geomac_file %>%
    sf::st_make_valid() %>%
    sf::st_transform(crs = sf::st_crs(sierra)) %>%
    sf::st_intersection(sierra) %>%
    add_column(., !!!cols[!(cols %in% names(.))], .name_repair = "minimal") %>%
    dplyr::select(c(incidentna, complexnam, irwinid, gisacres, perimeterd, mapmethod, fireyear, geometry)) %>%
    dplyr::rename(uniquefire = irwinid) %>%
    dplyr::mutate(across(c(incidentna, complexnam, uniquefire, gisacres, perimeterd, mapmethod, fireyear), as.character)) #%>%
  # sf::st_set_geometry(geom) #somehow the geometry gets lost -- re-add it from original file
}

combined <-sf::st_as_sf(dplyr::bind_rows(geomac_shape_list))

sf::st_write(combined, "./Parameterization/calibration data/geomac_all_years/combined_boundaries_2000-2019.gpkg", append = FALSE)

#-------------------------------------------------------------------
#For years 2020 - 2023, we have to use the public event archive

#import and clean up 2020 perimeter data
op_data_2020 <- sf::st_read("D:/Data/geomac_daily/Public_EventDataArchive_2020.gdb", layer = "EventPolygon")  %>%
  dplyr::filter(FeatureCategory == "Wildfire Daily Fire Perimeter") %>%
  sf::st_make_valid() %>%
  sf::st_transform(crs = sf::st_crs(sierra)) %>%
  sf::st_intersection(sierra)%>%
  dplyr::rename_with(tolower) %>%
  filter(featurestatus == "Approved" & isvisible == "Yes") %>%
  sf::st_set_geometry("shape") %>%
  sf::st_make_valid() %>%
  mutate(perimeterd = lubridate::ymd(as.Date(gdb_from_date))) %>%
  mutate(incidentna = stringr::str_to_lower(incidentname)) %>%
  group_by(incidentna, perimeterd) %>%
  mutate(size = st_area(shape)) %>%
  slice_max(size, with_ties = FALSE) %>%
  dplyr::filter(!is.na(perimeterd)) %>%
  dplyr::rename(uniquefire = irwinid) %>%
  dplyr::select(c("incidentna", "perimeterd", "size", "uniquefire")) %>%
  dplyr::mutate(fireyear = format(as.Date(perimeterd), "%Y"))

sf::write_sf(op_data_2020, "./Parameterization/calibration data/geomac_all_years/2020_perimeters_clean.gpkg", append = FALSE)
  
#2021 data doesn't have complex name or comments. Inconsistent data formatting between years for sure
op_data_2021 <- sf::st_read("D:/Data/geomac_daily/Public_EventDataArchive_2021.gdb", layer = "Event_Polygon_2021")   %>%
  dplyr::filter(FeatureCategory == "Wildfire Daily Fire Perimeter") %>%
  sf::st_make_valid() %>%
  sf::st_transform(crs = sf::st_crs(sierra)) %>%
  sf::st_intersection(sierra)%>%
  dplyr::rename_with(tolower) %>%
  filter(featurestatus == "Approved" & isvisible == "Yes") %>%
  sf::st_set_geometry("shape") %>%
  sf::st_make_valid() %>%
  mutate(perimeterd = lubridate::ymd(as.Date(gdb_from_date))) %>%
  mutate(incidentna = stringr::str_to_lower(incidentname)) %>%
  group_by(incidentna, perimeterd) %>%
  mutate(size = st_area(shape)) %>%
  slice_max(size, with_ties = FALSE) %>%
  dplyr::filter(!is.na(perimeterd)) %>%
  dplyr::rename(uniquefire = irwinid) %>%
  dplyr::select(c("incidentna", "perimeterd", "size", "uniquefire")) %>%
  dplyr::mutate(fireyear = format(as.Date(perimeterd), "%Y"))
                  
sf::write_sf(op_data_2021, "./Parameterization/calibration data/geomac_all_years/2021_perimeters_clean.gpkg", append = FALSE)


#2022 data
op_data_2022 <- sf::st_read("D:/Data/geomac_daily/Public_EventDataArchive_2022.gdb", layer = "Event_Polygon_2022")   %>%
  dplyr::filter(FeatureCategory == "Wildfire Daily Fire Perimeter") %>%
  sf::st_make_valid() %>%
  sf::st_transform(crs = sf::st_crs(sierra)) %>%
  sf::st_intersection(sierra)%>%
  dplyr::rename_with(tolower) %>%
  filter(featurestatus == "Approved" & isvisible == "Yes") %>%
  sf::st_set_geometry("shape") %>%
  sf::st_make_valid() %>%
  mutate(perimeterd = lubridate::ymd(as.Date(gdb_from_date))) %>%
  mutate(incidentna = stringr::str_to_lower(incidentname)) %>%
  group_by(incidentna, perimeterd) %>%
  mutate(size = st_area(shape)) %>%
  slice_max(size, with_ties = FALSE) %>%
  dplyr::filter(!is.na(perimeterd)) %>%
  dplyr::rename(uniquefire = irwinid) %>%
  dplyr::select(c("incidentna", "perimeterd", "size", "uniquefire")) %>%
  dplyr::mutate(fireyear = format(as.Date(perimeterd), "%Y"))

sf::write_sf(op_data_2022, "./Parameterization/calibration data/geomac_all_years/2022_perimeters_clean.gpkg", append = FALSE)

#2023 data
op_data_2023 <- sf::st_read("D:/Data/geomac_daily/Public_EventDataArchive_2023.gdb", layer = "EventPolygon2023")   %>%
  dplyr::filter(FeatureCategory == "Wildfire Daily Fire Perimeter") %>%
  sf::st_make_valid() %>%
  sf::st_transform(crs = sf::st_crs(sierra)) %>%
  sf::st_intersection(sierra)%>%
  dplyr::rename_with(tolower) %>%
  filter(featurestatus == "Approved" & isvisible == "Yes") %>%
  sf::st_set_geometry("shape") %>%
  sf::st_make_valid() %>%
  mutate(perimeterd = lubridate::ymd(as.Date(gdb_from_date))) %>%
  mutate(incidentna = stringr::str_to_lower(incidentname)) %>%
  group_by(incidentna, perimeterd) %>%
  mutate(size = st_area(shape)) %>%
  slice_max(size, with_ties = FALSE) %>%
  dplyr::filter(!is.na(perimeterd)) %>%
  dplyr::rename(uniquefire = irwinid) %>%
  dplyr::select(c("incidentna", "perimeterd", "size", "uniquefire")) %>%
  dplyr::mutate(fireyear = format(as.Date(perimeterd), "%Y"))
sf::write_sf(op_data_2023, "./Parameterization/calibration data/geomac_all_years/2023_perimeters_clean.gpkg", append = FALSE)


#-------------------------------------------------------------------------------
# Combine into one file
#-------------------------------------------------------------------------------
op_data_2020$perimeterd <- as.character(op_data_2020$perimeterd)
op_data_2021$perimeterd <- as.character(op_data_2021$perimeterd)
op_data_2022$perimeterd <- as.character(op_data_2022$perimeterd)
op_data_2023$perimeterd <- as.character(op_data_2023$perimeterd)
combined$shape <- combined$geometry
combined$size <- sf::st_area(combined)
combined <- combined %>% dplyr::select(c("incidentna", "perimeterd", "size", "uniquefire", "fireyear", "shape"))

combine_all <- bind_rows(combined, op_data_2020, op_data_2021, op_data_2022, op_data_2023)

combine_all <- combine_all %>%
  group_by(fireyear, incidentna) %>% 
  filter(n() >= 3) %>%
  st_set_geometry("shape") %>%
  dplyr::select(!c("geometry"))

sf::st_write(combine_all, "./Parameterization/calibration data/geomac_all_years/geomac_combined_2000-2023.gpkg")
