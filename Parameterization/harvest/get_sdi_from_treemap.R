#create new initial communities
library("sf")
library("tidyverse")
library("rFIA")
library("terra")

#we'll use data from FIA for the initial communities, 
# https://www.fs.usda.gov/rmrs/publications/treemap-tree-level-model-conterminous-us-forests-circa-2014-produced-imputation-fia
# Riley et al. 2021

#treemap is in a weird CRS; probably a NAD83 UTM, EPSG:42303
treemap <- terra::rast("D:/Data/treemap/RDS-2019-0026_Data/Data/national_c2014_tree_list.tif")

boundary <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  sf::st_transform(crs = terra::crs(treemap))

treemap_area <- treemap %>% 
  terra::crop(vect(boundary))

#if needed, make a template raster from the boundary shapefile
# treemap_area <- terra::crop(treemap, boundary) %>%
#   terra::project("EPSG:26917", method = "near") #reproject to the project CRS
# 
# template <- terra::rast(extent = terra::ext(treemap_isro),
#                         resolution = 60,
#                         crs = "EPSG:26917")

template <- rast("./Models/Inputs/masks_boundaries/mask_9311.tif") 

tl_plots <- read.csv("D:/Data/treemap/RDS-2019-0026_Data/Data/TL_CN_Lookup.txt") %>%
  filter(tl_id %in% values(treemap_area))

tl_trees <- read.csv("D:/Data/treemap/RDS-2019-0026_Data/Data/Tree_table_CONUS.txt") %>%
  filter(tl_id %in% values(treemap_area)) %>%
  mutate(sdi_tree = TPA_UNADJ * ((DIA/10)^(-1.605)))

#TODO get biomass for treemap trees
all_fia_trees <- unique(tl_trees$State_Abbreviation)

tl_trees$BHAGE <- exp(predict(age_model, newdata = tl_trees, allow.new.levels = TRUE))
breaks <- seq(0, max(tl_trees$BHAGE+10, na.rm = TRUE) + (10 - max(tl_trees$BHAGE+10, na.rm = TRUE) %% 10), by = 10)

tl_cohorts <- tl_trees %>%
  filter(DIA > 5) %>%
  dplyr::mutate(AGE_BIN = as.numeric(base::cut(BHAGE+10, breaks)) * 10) %>%
  dplyr::group_by(tl_id, SPCD, AGE_BIN) %>%
  dplyr::reframe(cohort_sdi = sum(sdi_tree))

plot_sdi <- tl_trees %>%
  filter(DIA > 5) %>%
  group_by(tl_id) %>%
  summarise(SDI = sum(sdi_tree, na.rm = TRUE))
hist(plot_sdi$SDI)

sdi_map <- terra::classify(treemap_area, rcl = plot_sdi, others = NA)
plot(sdi_map)

#compare to LANDIS initial SDI
initial_landis_sdi <- terra::rast("./Parameterization/management scenario data/initial_LANDIS_sdi_10yr_no_correction.tif")
hist(initial_landis_sdi[initial_landis_sdi > 0])
mean(initial_landis_sdi[initial_landis_sdi > 0])
hist(sdi_map)
mean(sdi_map[], na.rm = TRUE)
treemap_scale_factor <- mean(sdi_map[], na.rm = TRUE) / mean(initial_landis_sdi[initial_landis_sdi > 0]) #0.59


#bring in MaxSDI map
bps_max_sdi <- terra::rast("./Parameterization/management scenario data/max_sdi_bps_dia5_reineke.tif")
bps_max_sdi[bps_max_sdi == 0] <- NA
bps_max_sdi2 <- bps_max_sdi %>% 
  terra::project(sdi_map, method = "near") %>%
  terra::crop(sdi_map) %>%
  terra::mask(sdi_map, maskvalues = c(NA, 0), updatevalue = NA)
comm_map2 <- terra::rast("./Models/Inputs/masks_boundaries/mask_9311.tif")
values(comm_map2) <- values(comm_map)
bps_max_sdi_landis_grid <- bps_max_sdi %>% 
  terra::project(template) %>%
  terra::crop(template) %>%
  terra::mask(template, maskvalues = c(NA, 0), updatevalue = NA)
plot(bps_max_sdi_landis_grid)

#$maxsdi from treemap
sdi_prop_map <- sdi_map / bps_max_sdi2
# sdi_prop_map[sdi_prop_map > 1] <- 1
sdi_prop_map <- sdi_prop_map*100
plot(sdi_prop_map)
hist(sdi_prop_map[])
mean(sdi_prop_map[], na.rm = TRUE)
sdi_prop_tcsi <- sdi_prop_map %>%
  project(template, method = "near") %>%
  crop(template) %>%
  mask(template)

percent_max_sdi_landis <- template
percent_max_sdi_landis[] <- initial_landis_sdi[] * treemap_scale_factor
percent_max_sdi_landis <- percent_max_sdi_landis/ bps_max_sdi_landis_grid * 100
percent_max_sdi_landis[percent_max_sdi_landis[] > 100] <- 100
plot(percent_max_sdi_landis) #landis
plot(sdi_prop_tcsi)
plot(sdi_prop_tcsi[] ~ percent_max_sdi[]) #this is from the LANDIS initial community file
treemap_coarsen <- terra::aggregate(sdi_prop_tcsi, 9, fun = mean, na.rm = TRUE)
landis_coarsen <- terra::aggregate(percent_max_sdi_landis, 9, fun = mean, na.rm = TRUE)#from convert_biomass_to_SDI.R
plot(treemap_coarsen, range = c(0,100))
plot(landis_coarsen, range = c(0,100))
summary(lm(treemap_coarsen[] ~ landis_coarsen[] + 0)) #get correction factor


sdi_tcsi <- sdi_map %>%
  project(template) %>%
  crop(template) %>%
  mask(template)
treemap_coarsen <- terra::aggregate(sdi_tcsi, 9, fun = mean, na.rm = TRUE)
comm_map2[comm_map2[] == 0] <- NA
landis_coarsen <- terra::aggregate(comm_map2, 9, fun = mean, na.rm = TRUE) #from convert_biomass_to_SDI.R
plot(treemap_coarsen)
plot(landis_coarsen)
summary(lm(treemap_coarsen[] ~ landis_coarsen[] + 0)) #get correction factor

test <- as.data.frame(cbind(treemap_coarsen[], landis_coarsen[]))
names(test) <- c("treemap", "landis")

ggplot(data = test, aes(x = treemap, y = landis * 0.52)) +
  geom_hex() +
  geom_abline(slope = 1, intercept = 0)

