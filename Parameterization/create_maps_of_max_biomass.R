#get maximum biomass maps from LANDIS runs
library("stars")
library("sf")
library("tidyverse")
library("terra")
setGDALconfig("OSR_USE_NON_DEPRECATED", value="NO") #so we can still use EPSG:2163


project_to_template <- function(input_raster, template){
  #if input is terra or raster
  
  #if input is stars
  
  
  #function to project landis input rasters with no CRS to an input file
  input_raster <- terra::rast(input_raster)
    #replace values of template with values from input raster
    out_raster <- template %>%
      `values<-`(values(input_raster))
  
  return(out_raster)
}

#import boundary data

tcsi_mask <- stars::read_stars("./Models/Inputs/masks_boundaries/mask_9311.tif") 
st_crs(tcsi_mask)
tcsi_mask_terra <- rast("./Models/Inputs/masks_boundaries/mask_9311.tif") 
empty_mask <- tcsi_mask
empty_mask$mask.tif <- 0
tcsi_shape <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  st_transform(crs = "EPSG:9311")

test <- mask(tcsi_mask_terra, vect(tcsi_shape))
plot(test) #check alignment

# 
stands <- terra::rast("./Models/Inputs/input_rasters_reproject/stands_ssp2_20_180_v5.tif") %>%
  terra::set.crs("EPSG:2163") %>%
  terra::project("EPSG:9311") %>%
  stars::st_as_stars()
stands <- stars::read_stars("./Models/Inputs/input_rasters_reproject/stands_ssp2_20_180_v5.tif")
st_crs(stands) <- "EPSG:2163"
stands
plot(stands)
table(stands)

test <- st_crop(stands, tcsi_shape)
plot(test) #check alignment


#import and process biomass data to make maximum biomass maps
initial_biomass <- stars::read_stars("C:/Users/Sam/Documents/GlobusEndpoint/TotalBiomass-0.img")
crs(initial_biomass)
# plot(initial_biomass)

hist(initial_biomass)

biomass_list <- list.files("C:/Users/Sam/Documents/GlobusEndpoint/Biomass", full.names = TRUE)

biomass_stack_all  <- c(stars::read_stars(biomass_list), initial_biomass)
biomass_stack_100 <- stars::read_stars(biomass_list)
biomass_stack_dist <- stars::read_stars(biomass_list)[-1]
biomass_nodist <- stars::read_stars(biomass_list)[1]

biomass_stack_all <- st_redimension(biomass_stack_all, new_dims = st_dimensions(biomass_stack_all), along = list(biomass = names(biomass_stack_all)))
names(biomass_stack_all) <- "biomass"
biomass_stack_100 <- st_redimension(biomass_stack_100, new_dims = st_dimensions(biomass_stack_100), along = list(biomass = names(biomass_stack_100)))
names(biomass_stack_100) <- "biomass"
biomass_stack_dist <- st_redimension(biomass_stack_dist, new_dims = st_dimensions(biomass_stack_dist), along = list(biomass = names(biomass_stack_dist)))
names(biomass_stack_dist) <- "biomass"
biomass_nodist <- st_redimension(biomass_nodist, new_dims = st_dimensions(biomass_nodist), 
                                 along = list(biomass = names(biomass_nodist)))
names(biomass_nodist) <- "biomass"

biomass_max <- stars::st_apply(biomass_nodist, 1:2, FUN = function(x) max(x, na.rm = TRUE))
biomass_max
st_crs(biomass_max)
plot(biomass_max)

test <- stars:::st_as_raster(biomass_max)

biomass_max_terra <- project_to_template(stars:::st_as_raster(biomass_max), tcsi_mask_terra)
plot(terra::rast(biomass_max_terra))
# write_stars(biomass_max, "biomass_max.tif")
writeRaster(biomass_max_terra, "biomass_max.tif", overwrite = TRUE)
test <- crop(tcsi_mask_terra, vect(tcsi_shape))
plot(test)

biomass_mean_100 <- stars::st_apply(biomass_stack_dist, 1:2, FUN = function(x) mean(x, na.rm = TRUE))

diff_fire_beetles <- (biomass_nodist - biomass_mean_100)/100
plot(diff_fire_beetles)
diff_fire_beetles <- project_to_template(diff_fire_beetles, tcsi_mask_terra)
NAflag(diff_fire_beetles) <- 0
plot(diff_fire_beetles,
     main = "Difference in biomass due to disturance and management")

biomass_35 <- biomass_max * 0.35
plot(biomass_35)
# write_stars(biomass_35, "biomass_35.tif")
biomass_35_terra <- project_to_template(biomass_35, tcsi_mask_terra)
# writeRaster(biomass_35_terra, "biomass_35.tif", overwrite=TRUE)

hist(biomass_35_terra[biomass_35_terra != 0])

#figure for initial biomass
initial_vals <- values(rast(initial_biomass))
initial_vals <- initial_vals[initial_vals != 0]
mean_35 <- mean(values(biomass_max_terra)[values(biomass_max_terra) != 0], na.rm = TRUE) * 0.35
mean_60 <- mean(values(biomass_max_terra)[values(biomass_max_terra) != 0], na.rm = TRUE) * 0.6

hist(initial_vals, breaks = seq(0, 80000, by = 1000))
abline(v = mean_35)
abline(v = mean_60)

initial_vals <- data.frame(biomass = initial_vals)
ggplot(initial_vals, aes(x = biomass/100)) +
  geom_histogram(aes(y = ..count..*3.24), binwidth = 10, color = "steelblue", fill = "steelblue") + 
  labs(title = "Initial biomass (year 2020)",
       y = "Ha", x = "Mg/ha") +
  geom_vline(xintercept = mean_35/100) + 
  geom_vline(xintercept = mean_60/100) + 
  annotate(geom="text", x=mean_35/100, y=30000, label="35% Max\nBiomass") + 
  annotate(geom="text", x=mean_60/100, y=35000, label="60% Max\nBiomass")

#figure for proportion of max
proportion_max <- initial_biomass/biomass_max
plot(proportion_max)
proportion_max <- values(rast(proportion_max))
proportion_max <- proportion_max[proportion_max != 0]
hist(proportion_max)
abline(v = 0.35)
abline(v = 0.6)
abline(v = 1)

proportion_max <- data.frame(biomass = proportion_max)
ggplot(proportion_max, aes(x = biomass)) +
  geom_histogram(aes(y = ..count..*3.24), binwidth = 0.05, color = "steelblue", fill = "steelblue") + 
  labs(title = "Initial biomass (year 2020) as a proportion of Max Biomass in year 2100",
       y = "Ha", x = "Proportion of Max Biomass in year 2100") +
  geom_vline(xintercept = 0.35) + 
  geom_vline(xintercept = 0.6) + 
  geom_vline(xintercept = 1) + 
  annotate(geom="text", 0.35, y=65000, label="35% Max\nBiomass") + 
  annotate(geom="text", 0.6, y=65000, label="60% Max\nBiomass") +
  annotate(geom="text", 1, y=65000, label="100% Max\nBiomass")


change_in_biomass <- biomass_max - initial_biomass
plot(change_in_biomass)

final_mean_vs_biomass_35 <- biomass_mean - biomass_35
# plot(final_mean_vs_biomass_35)

initial_vs_biomass_35 <- initial_biomass - biomass_35
plot(initial_vs_biomass_35)

biomass_terra <- biomass_max_terra
terra::NAflag(biomass_terra) <- 0
biomass_max_smooth <- focal(biomass_terra, w=3, fun="mean", na.policy="omit", na.rm = TRUE,
                            fillvalue=NA, 
                            expand=FALSE, silent=TRUE, filename="", overwrite=FALSE, wopt=list())

biomass_max_smooth <- st_as_stars(biomass_max_smooth)

biomass_max_rcl <- cut(biomass_max_smooth, breaks = c(0,18000,24000,30000,36000,42000,100000))
plot(biomass_max_rcl)

biomass_max_2 <- empty_mask
names(biomass_max_2) <- "biomass"
biomass_max_2$biomass <- biomass_max_rcl$values
 
# biomass_poly <- st_as_sf(biomass_max_2, as_points=FALSE, merge = TRUE) %>%
#   sf::st_cast("MULTIPOLYGON")
# 
# plot(biomass_poly["biomass"])


################################################################################
#bring in other data sources

#wui map from SILVIS lab
ca_wui <- sf::st_read("D:/Data/WUI/ca_wui_cp12/ca_wui_cp12.shp") %>%
  sf::st_transform(crs = sf::st_crs(tcsi_shape)) %>%
  sf::st_intersection(tcsi_shape)
plot(st_geometry(ca_wui))
nv_wui <- sf::st_read("D:/Data/WUI/nv_wui_cp12/nv_wui_cp12.shp")%>%
  sf::st_transform(crs = sf::st_crs(tcsi_shape)) %>%
  sf::st_intersection(tcsi_shape)
plot(st_geometry(nv_wui))

wui <- plyr::bind_rows(ca_wui, nv_wui)
plot(sf::st_geometry(wui)) 
rm(ca_wui, nv_wui)

# plot(wui["WUIFLAG10"])

wui_threat <- wui[wui$WUIFLAG10 %in% c(1,2), ] %>%
  sf::st_buffer(dist = 400) %>%
  sf::st_union()
wui_defense <- wui[wui$WUIFLAG10 %in% c(1,2), ] %>%
  sf::st_buffer(dist = 2400) %>%
  sf::st_union()
  
# plot(sf::st_geometry(wui_threat))
# plot(sf::st_geometry(wui_defense), add = TRUE)

# federal property
# from National Atlas 2006 https://geodata.lib.utexas.edu/catalog/stanford-jd927kd1521
# fed <- sf::st_read("./Parameterization/calibration data/landuse/fedland/fedlanp020.shp") %>%
#   sf::st_transform(crs = sf::st_crs(tcsi_shape)) %>%
#   sf::st_intersection(tcsi_shape)
# 
# plot(sf::st_geometry(fed))

# forest service actual ownership
fs <- sf::st_read("./Parameterization/calibration data/landuse/S_USA.BasicOwnership/S_USA.BasicOwnership.shp") %>%
  sf::st_transform(crs = sf::st_crs(tcsi_shape)) %>%
  sf::st_intersection(tcsi_shape)

# plot(fs["OWNERCLASS"])


#wilderness
# https://www.sciencebase.gov/catalog/item/4fc8f0e4e4b0bffa8ab259e7
wild <- sf::st_read("./Parameterization/calibration data/landuse/wilderness/wilderness.shp") %>%
  sf::st_transform(crs = sf::st_crs(tcsi_shape)) %>%
  sf::st_intersection(tcsi_shape)

# plot(sf::st_geometry(wild))

sum(tcsi_mask$mask.tif)
max(stands$stands_ssp2_20_180_v5.tif, na.rm = TRUE)

#########################
#pillars
fire_adapt <- stars::read_stars("D:/Data/pillars/fireDynamics/fire_severity/tif/adapt.tif") %>%
  st_warp(tcsi_mask)
# plot(fire_adapt)
fire_transform <- stars::read_stars("D:/Data/pillars/fireDynamics/fire_severity/tif/transform.tif") %>%
  st_warp(tcsi_mask)
# plot(fire_transform)

fire_adapt <- as(fire_adapt, "SpatRaster")
terra::NAflag(fire_adapt) <- 0
fire_adapt_smooth <- focal(fire_adapt, w=3, fun="mean", na.policy="omit", na.rm = TRUE,
                            fillvalue=NA, 
                            expand=FALSE, silent=TRUE, filename="", overwrite=FALSE, wopt=list())

fire_adapt_smooth <- st_as_stars(fire_adapt_smooth)

fire_adapt_rcl <- cut(fire_adapt_smooth, breaks = c(-1,0,0.3,0.6,1))
plot(fire_adapt_rcl)

fire_transform <- as(fire_transform, "SpatRaster")
terra::NAflag(fire_transform) <- 0
fire_transform_smooth <- focal(fire_transform, w=3, fun="mean", na.policy="omit", na.rm = TRUE,
                           fillvalue=NA, 
                           expand=FALSE, silent=TRUE, filename="", overwrite=FALSE, wopt=list())

fire_transform_smooth <- st_as_stars(fire_transform_smooth)

fire_transform_rcl <- cut(fire_transform_smooth, breaks = c(-1,0,0.3,0.6,1))
plot(fire_transform_rcl)


carbon_protect <- stars::read_stars("D:/Data/pillars/carbon/tif/protect.tif") %>%
  st_warp(tcsi_mask)

carbon_protect <- as(carbon_protect, "SpatRaster")
terra::NAflag(carbon_protect) <- 0
carbon_protect_smooth <- focal(carbon_protect, w=3, fun="mean", na.policy="omit", na.rm = TRUE,
                           fillvalue=NA, 
                           expand=FALSE, silent=TRUE, filename="", overwrite=FALSE, wopt=list())

carbon_protect_smooth <- st_as_stars(carbon_protect_smooth)

carbon_protect_rcl <- cut(carbon_protect_smooth, breaks = c(-1,0,0.3,0.6,1))
plot(carbon_protect_rcl)


####################
# Intersect layers

#subdivisions to use: 
#first digit: land type
  #--1: wilderness
  #--2: WUI threat
  #--3: WUI defense
  #--4: general forest
  #--5: private industrial
  #--6: private nonindustrial
#second digit: max biomass
  #--1: <10000
  #--2: 
  #--3:
#third digit:
  

# I can't figure out how to do this in stars; it's in terra instead, sorry

management_zones <- tcsi_mask_terra

#stars giving me trouble with this one
# fs_rast <- stars::st_rasterize(fs, tcsi_mask, options = c("MERGE_ALG=ADD", "ALL_TOUCHED=TRUE")) %>%
#   select("OWNERCLASS")

#rasterize land ownership map
fs_rast <- fs %>%
  mutate(OWNERCLASS = ifelse(OWNERCLASS == "NON-FS", 1, 2)) %>%
  sf::st_transform(crs = "EPSG:9311") %>%
  vect() %>%
  terra::rasterize(tcsi_mask_terra, field = "OWNERCLASS")

values(management_zones)[values(fs_rast) == 1] <- 2
plot(management_zones)
