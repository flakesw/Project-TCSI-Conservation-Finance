library(sf)
library(tidyverse)
library(data.table)

sf::sf_use_s2(FALSE)

#geomac data are in EPST:4269
sierra <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  sf::st_transform("EPSG:4269")

#import and clean up 2020 perimeter data
# op_data_2020 <- sf::st_read("D:/Data/nifc_op_data/Public_EventDataArchive_2020.gdb", layer = "EventPolygon") %>%
#   dplyr::filter(FeatureCategory == "Wildfire Daily Fire Perimeter") %>%
    # dplyr::filter(!is.na(perimeterd)) %>%
    # dplyr::filter(isvisible %in% c("Yes", "YES")) %>%
#   sf::st_make_valid() %>%
#   sf::st_transform(crs = sf::st_crs(sierra)) %>%
#   sf::st_intersection(sierra) %>%
#   dplyr::rename_with(tolower)%>%
#   sf::st_set_geometry("shape") %>%
#   dplyr::rename(incidentna = incidentname, 
#                 perimeterd = polygondatetime,
#                 complexnam = complexname,
#                 uniquefire = irwinid
#                 ) %>%
#   dplyr::select(c("incidentna", "perimeterd", "gisacres", "complexnam", "uniquefire", "comments")) %>%
#   dplyr::mutate(fireyear = format(as.Date(perimeterd), "%Y")) %>%


# sf::write_sf(op_data_2020, "./Parameterization/calibration data/geomac fire boundaries/2020_perimeters/2020_perimeters_clean.shp")
  
#2021 data doesn't have complex name or comments. Inconsistent data formatting between years for sure
# but at least 2021 data has a lot more entries for polygondatetime
# op_data_2021 <- sf::st_read("D:/Data/nifc_op_data/Public_EventDataArchive_2021.gdb", layer = "Event_Polygon_2021") %>%
#     dplyr::filter(FeatureCategory == "Wildfire Daily Fire Perimeter") %>%
#     sf::st_make_valid() %>%
#     sf::st_transform(crs = sf::st_crs(sierra)) %>%
#     sf::st_intersection(sierra)%>%
#     dplyr::rename_with(tolower) %>%
#     sf::st_set_geometry("shape")%>%
#     dplyr::filter(!is.na(perimeterd)) %>%
#     dplyr::filter(isvisible %in% c("Yes", "YES")) %>%
#     dplyr::rename(incidentna = incidentname, 
#                   perimeterd = polygondatetime,
#                   uniquefire = irwinid
#                   ) %>%
#     dplyr::select(c("incidentna", "perimeterd", "gisacres", "uniquefire")) %>%
#     dplyr::mutate(fireyear = format(as.Date(perimeterd), "%Y")) %>%
                  
                  
                    
# sf::write_sf(op_data_2021, "./Parameterization/calibration data/geomac fire boundaries/2021_perimeters/2021_perimeters_clean.shp")


# daily_perims_2000_2018 <- sf::st_read("./Parameterization/calibration data/geomac fire boundaries/combined_boundaried_2000-2019.shp") %>%
#   sf::st_transform(crs = sf::st_crs(sierra)) %>%
#   sf::st_intersection(sierra) %>%
#   dplyr::rename_with(tolower) %>%
#   dplyr::mutate(gisacres = as.numeric(gisacres), perimeterd = as.Date(perimeterd))


#import all perimeter shapefiles in the folder, including cleaned up 2020 and 2021 data
perims_file_list <- list.files(path = "./Parameterization/calibration data/geomac fire boundaries", pattern = "*.shp$", #the dollar sign matches *.shp at the end of the string
             full.names = TRUE, recursive = TRUE)

shapefile_list <- lapply(perims_file_list, sf::st_read) %>%
  purrr::map(., ~ sf::st_set_precision(., 1000000)) %>%
  purrr::map(., ~ sf::st_make_valid(., NA_on_exception)) %>%
  purrr::map(., ~ sf::st_intersection(., sierra))

#The shapefiles have numbers of columns, names, column orders.
#just select important columns; all these are shared between the different shapefiles
shapefile_list_subset <- shapefile_list %>%
  purrr::map_if(., ~ "complexnam" %in% names(.), ~ dplyr::select(., c("incidentna", "perimeterd", "fireyear", "gisacres", "complexnam", "uniquefire", "comments"))) %>%
  purrr::map(., ~ dplyr::mutate(., perimeterd = as.character(perimeterd)))

perims_2000_2021 <- sf::st_as_sf(data.table::rbindlist(shapefile_list_subset, use.name = TRUE, fill = TRUE))

sf::write_sf(st_collection_extract(perims_2000_2021, "POLYGON"), "./Parameterization/calibration data/geomac_all_years/perims_2000_2021.shp")

