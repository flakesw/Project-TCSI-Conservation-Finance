#fire trends
library("sf")
library("tidyverse")
library("terra")

sf::sf_use_s2(FALSE)

sierra_poly <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  sf::st_zm() %>%
  sf::st_transform(crs = "EPSG:4326") %>%
  sf::st_make_valid() 

tcsi_poly <- sierra_poly <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  sf::st_zm() %>%
  sf::st_transform(crs = "EPSG:4326") %>%
  sf::st_make_valid() 

#fire size
nifc_perims <- sf::st_read("D:/Data/wfigs_incidents_final_fire_perims/Interagency_Fire_Perimeter_History_-_All_Years/InteragencyFirePerimeterHistory.shp") %>%
  sf::st_intersection(tcsi_poly)
plot(sf::st_geometry(nifc_perims))

min(nifc_perims$FIRE_YEAR)

yearly_area_burned <- nifc_perims %>%
  as.data.frame() %>%
  group_by(FIRE_YEAR) %>%
  summarise(total_area = sum(GIS_ACRES/2.47, na.rm = TRUE)) %>%
  filter(!is.na(total_area)) %>%
  mutate(FIRE_YEAR = as.numeric(FIRE_YEAR)) %>%
  filter(total_area > 0)

ggplot(yearly_area_burned, aes(y = total_area, x = FIRE_YEAR)) +
  geom_point()+
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")))
  
mean(yearly_area_burned[yearly_area_burned$FIRE_YEAR > 1999, ]$total_area)

#fire severity
mtbs_mosaics <- list.files("D:/Data/mtbs_mosaic/mtbs_mosaic/composite_data/MTBS_BSmosaics",
                           pattern = ".tif", full.names = TRUE, recursive = TRUE)

sierra_poly_mtbs <- sierra_poly %>% 
  sf::st_transform(crs = "ESRI:102039")

severity_data <- data.frame(year = 1984:2020,
                            all_cells = numeric(37),
                            high_severity_cells = numeric(37))

# assign a potential fire cell a value based on the severity of its upwind cell
# loop over years with fire spread data

#MTBS severity goes 0 = masked out; 1 = low intensity or unburned; 2 = low; 3 = moderate severity; 4 = high
#; 5 = increased greenness, 6 = non-processing mask
for(year in 1984:2020){
  mtbs_year <- terra::rast(mtbs_mosaics[year - 1983]) %>%
    terra::crop(x = ., y = sierra_poly_mtbs) 
  
  # plot(mtbs_year)
  
  severity_data[severity_data$year == year, "all_cells"] <- sum(values(mtbs_year) %in% c(1,2,3,4))
  severity_data[severity_data$year == year, "high_severity_cells"] <- sum(values(mtbs_year) %in% c(4))
  
}

severity_data$prop_high <- severity_data$high_severity_cells/severity_data$all_cells

ggplot(severity_data, aes(x = year, y = prop_high)) +
  geom_point() + 
  geom_smooth(method = "lm")


yearly_area_burned <- left_join(yearly_area_burned, severity_data, 
                            by = c("FIRE_YEAR" = "year"))
yearly_area_burned$area_high_severity <- yearly_area_burned$total_area * yearly_area_burned$prop_high


ggplot(filter(yearly_area_burned, FIRE_YEAR > 1984), aes(y = area_high_severity, x = FIRE_YEAR)) +
  geom_point()+
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")))
