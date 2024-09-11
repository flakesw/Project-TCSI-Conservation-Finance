#merge GeoMAC boundaries into one file

# DEPRECATED -- use "process_geomac_files.R"


library("sf")

tcsi_poly <- sf::st_read("./masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  sf::st_zm() %>%
  sf::st_transform(crs(template))

getwd()

geomac_folder <- "./calibration data/geomac fire boundaries/"


# list.dirs(geomac_folder)

# in this folder, there is a folder for each year 2000-2019, with the format
# "./calibration data/geomac fire boundaries/2000_perimeters_dd83" 

geomac_files <- paste0(geomac_folder, 2000:2019,"_perimeters_dd83/",2000:2019, "_perimeters_dd83.shp")

geomac_shape_list <- list()
cols <- c("incidentna", "complexnam", "uniquefire", "gisacres", "perimeterd", "mapmethod", "fireyear")
cols <- c(incidentna = NA_character_, complexnam= NA_character_, uniquefire= NA_character_, gisacres= NA_real_, perimeterd= NA_character_, mapmethod= NA_character_, fireyear= NA_real_)

#Geomac data has really inconsistent structure -- different columns and names --
#between years. Really annoying, but we can clean it up in the loop below

for(i in 1:length(geomac_files)){
  geomac_file <- sf::st_read(geomac_files[i])
  geom <- sf::st_geometry(geomac_file)
  geomac_shape_list[[i]] <- geomac_file %>%
    sf::st_transform(crs = sf::st_crs(tcsi_poly)) %>%
    sf::st_intersection(tcsi_poly) %>%
    add_column(., !!!cols[!(cols %in% names(.))], .name_repair = "minimal") %>%
    dplyr::select(c(incidentna, complexnam, uniquefire, gisacres, perimeterd, mapmethod, fireyear, geometry)) %>%
    dplyr::mutate(across(c(incidentna, complexnam, uniquefire, gisacres, perimeterd, mapmethod, fireyear), as.character)) #%>%
    # sf::st_set_geometry(geom) #somehow the geometry gets lost -- re-add it from original file
}

combined <-sf::st_as_sf(dplyr::bind_rows(geomac_shape_list))

sf::st_write(combined, "./calibration data/geomac fire boundaries/combined_boundaried_2000-2019.shp")
