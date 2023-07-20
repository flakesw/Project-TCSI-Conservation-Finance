#get maximum biomass maps from LANDIS runs
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
empty_mask <- tcsi_mask
empty_mask$mask.tif <- 0
tcsi_shape <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  st_transform(crs = "EPSG:9311")

test <- mask(tcsi_mask_terra, vect(tcsi_shape))
plot(test) #check alignment


#import and process biomass data to make maximum biomass maps
initial_biomass <- rast("C:/Users/Sam/Documents/GlobusEndpoint/TotalBiomass-0.img")

plot(initial_biomass)

hist(initial_biomass)



biomass_list <- list.files("C:/Users/Sam/Documents/GlobusEndpoint/Biomass", full.names = TRUE)

biomass_stack_all  <- rast((biomass_list))
biomass_stack_all <- c(biomass_stack_all, initial_biomass)

biomass_stack_100 <- rast(biomass_list)
biomass_stack_dist <- rast(biomass_list[-1])
biomass_nodist <- rast(biomass_list[1])

biomass_max <- max(biomass_stack_all, na.rm = TRUE)
biomass_max
st_crs(biomass_max)
plot(biomass_max)
biomass_max_terra <- project_to_template(biomass_max, tcsi_mask_terra)
plot(biomass_max_terra)
# write_stars(biomass_max, "biomass_max.tif")
writeRaster(biomass_max_terra, "biomass_max.tif", overwrite = TRUE)
test <- crop(tcsi_mask_terra, vect(tcsi_shape))
plot(test)

biomass_mean_100 <- mean(biomass_stack_100, na.rm = TRUE)

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
writeRaster(biomass_35_terra, "biomass_35.tif", overwrite=TRUE)

hist(biomass_35_terra[biomass_35_terra != 0])

#figure for initial biomass
initial_vals <- values(initial_biomass)
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
  annotate(geom="text", x=mean_35/100, y=30000, label="35% Max/nBiomass") + 
  annotate(geom="text", x=mean_60/100, y=35000, label="60% Max/nBiomass")

#figure for proportion of max
proportion_max_rast <- initial_biomass/biomass_max
plot(proportion_max_rast)
proportion_max <- values((proportion_max_rast))
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
  annotate(geom="text", 0.35, y=65000, label="35% Max/nBiomass") + 
  annotate(geom="text", 0.6, y=65000, label="60% Max/nBiomass") +
  annotate(geom="text", 1, y=65000, label="100% Max/nBiomass")


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

biomass_max_rcl <- terra::classify(biomass_max_smooth, rcl = c(0,18000,24000,30000,36000,100000),
                                   others = NA)

plot(biomass_max_rcl)
hist(values(biomass_max_rcl))  

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

wui <- dplyr::bind_rows(ca_wui, nv_wui)
plot(sf::st_geometry(wui)) 
rm(ca_wui, nv_wui)

# plot(wui["WUIFLAG10"])

#buffer for wui zones, 1/4 mile and 1 1/2 mile 
#https://www.fs.usda.gov/detail/cleveland/landmanagement/planning/?cid=fsbdev7_016495

wui_threat <- wui[wui$WUIFLAG10 %in% c(1,2), ] %>%
  sf::st_buffer(dist = 400) %>%
  sf::st_union()
wui_defense <- wui[wui$WUIFLAG10 %in% c(1,2), ] %>%
  sf::st_buffer(dist = 2400) %>%
  sf::st_union()
  
plot(sf::st_geometry(wui_threat))
plot(sf::st_geometry(wui_defense), add = TRUE)

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


#ownership layer from CalFire, frap.fire.ca.gov
ownership <- sf::st_read("./Parameterization/management scenario data/ownership22_1-gdb/ownership22_1.gdb",
                         layer = "ownership22_1") %>%
  sf::st_transform(crs = sf::st_crs(tcsi_shape)) %>%
  sf::st_intersection(tcsi_shape)
plot(st_geometry(ownership))

ownership2 <- terra::rast("D:/Data/ownership/Data/forest_own1/forest_own1.tif") %>%
  terra::project(tcsi_mask_terra, method = "near") %>%
  terra::mask(tcsi_mask_terra)
#1 = family
#2 = corporate
#3 = timber company
#4 = other private
#5 = federal
#6 = state
#7 = local
#8 = tribal


#wilderness
# https://www.sciencebase.gov/catalog/item/4fc8f0e4e4b0bffa8ab259e7
wild <- sf::st_read("./Parameterization/calibration data/landuse/wilderness/wilderness.shp") %>%
  sf::st_transform(crs = sf::st_crs(tcsi_shape)) %>%
  sf::st_intersection(tcsi_shape)

# plot(sf::st_geometry(wild))


#private industrial and nonindustiral land
# https://data.fs.usda.gov/geodata/state_private/nationaldata.php



#########################
#pillars
fire_adapt <- rast("D:/Data/pillars/fireDynamics/fire_severity/tif/adapt.tif") %>%
  project(tcsi_mask_terra)
plot(fire_adapt)
fire_transform <- rast("D:/Data/pillars/fireDynamics/fire_severity/tif/transform.tif") %>%
  project(tcsi_mask_terra)
plot(fire_transform)


terra::NAflag(fire_adapt) <- 0
fire_adapt_smooth <- focal(fire_adapt, w=3, fun="mean", na.policy="omit", na.rm = TRUE,
                            fillvalue=NA, 
                            expand=FALSE, silent=TRUE, filename="", overwrite=FALSE, wopt=list())

fire_adapt_rcl <- classify(fire_adapt_smooth, rcl = c(-1,0,0.3,0.6,1))
plot(fire_adapt_rcl)

terra::NAflag(fire_transform) <- 0
fire_transform_smooth <- focal(fire_transform, w=3, fun="mean", na.policy="omit", na.rm = TRUE,
                           fillvalue=NA, 
                           expand=FALSE, silent=TRUE, filename="", overwrite=FALSE, wopt=list())

fire_transform_rcl <- classify(fire_transform_smooth, rcl = c(-1,0,0.3,0.6,1))
plot(fire_transform_rcl)


carbon_protect <- rast("D:/Data/pillars/carbon/tif/protect.tif") %>%
  project(tcsi_mask_terra)


terra::NAflag(carbon_protect) <- 0
carbon_protect_smooth <- focal(carbon_protect, w=3, fun="mean", na.policy="omit", na.rm = TRUE,
                           fillvalue=NA, 
                           expand=FALSE, silent=TRUE, filename="", overwrite=FALSE, wopt=list())

carbon_protect_rcl <- classify(carbon_protect_smooth, rcl = c(-1,0,0.3,0.6,1))
plot(carbon_protect_rcl)


####################
# Intersect layers

#subdivisions to use: 
#first digit: land type
  #--1: nonclassified
  #--2: WUI threat
  #--3: WUI defense
  #--4: general forest
  #--5: private industrial
  #--6: private nonindustrial
  #--7: wilderness
#second digit: fire and carbon strategy
  #--1: conserve carbon
  #--2: high fire transform or adapt
  #--3: high fire adapt, not used
  #--4: low fire transform and adapt not used
#third digit: max biomass band c(0,18000,24000,30000,36000,42000,100000)
  #--0: <18000
  #--1: 180000 - 24000
  #--2: 24000 - 30000
  #--3: 30000-36000
  #--4: 36000-100000

# I can't figure out how to do this in stars; it's in terra instead, sorry



old_zones <- project_to_template(rast("./Parameterization/management scenario data/tz1_20_ssp2_v3_unprojected.tif"),
                                 tcsi_mask_terra)

#stars giving me trouble with this one
# fs_rast <- stars::st_rasterize(fs, tcsi_mask, options = c("MERGE_ALG=ADD", "ALL_TOUCHED=TRUE")) %>%
#   select("OWNERCLASS")

#rasterize land ownership map
fs_rast <- fs %>%
  mutate(OWNERCLASS = ifelse(OWNERCLASS == "NON-FS", 1, 2)) %>%
  sf::st_transform(crs = st_crs(tcsi_mask_terra)) %>%
  vect() %>%
  terra::rasterize(tcsi_mask_terra, field = "OWNERCLASS")

wild_rast <- wild %>%
  sf::st_transform(crs = st_crs(tcsi_mask_terra)) %>%
  vect() %>%
  terra::rasterize(tcsi_mask_terra)
plot(wild_rast)

wui_threat_rast <- wui_threat %>%
  sf::st_transform(crs = st_crs(tcsi_mask_terra)) %>%
  vect() %>%
  terra::rasterize(tcsi_mask_terra)
wui_defense_rast <- wui_defense %>%
  sf::st_transform(crs = st_crs(tcsi_mask_terra)) %>%
  vect() %>%
  terra::rasterize(tcsi_mask_terra)
nonprofit_rast <- ownership[ownership$Own_Level == "Non Profit", ] %>%
  sf::st_transform(crs = st_crs(tcsi_mask_terra)) %>%
  vect() %>%
  terra::rasterize(tcsi_mask_terra)
all_public_rast <- ownership[ownership$Own_Level != "Non Profit", ] %>%
  sf::st_transform(crs = st_crs(tcsi_mask_terra)) %>%
  vect() %>%
  terra::rasterize(tcsi_mask_terra)

private_indust_rast <- ownership2[ownership2 %in% c(2,3)]
plot(private_indust_rast)

#slope constraints -- 35% to 50% slopes, 40% as a general constraint



#make management zone map


management_zones <- tcsi_mask_terra

#ownership layers
values(management_zones) <- 500 #default -- PIF
# values(management_zones)[values(old_zones) %in% c(1:10)] <- 500 #PIF
# values(management_zones)[values(old_zones) %in% c(11:20)] <- 600 #PNIF
# values(management_zones)[values(fs_rast) == 2] <- 400 #general forest
values(management_zones)[values(all_public_rast) == 1] <- 400 #general forest -- state and federal land
values(management_zones)[values(nonprofit_rast) == 1] <- 400 #general forest -- conservation land
values(management_zones)[values(wui_defense_rast) == 1] <- 300
values(management_zones)[values(wui_threat_rast) == 1] <- 200
values(management_zones)[values(wild_rast) == 1] <- 700

management_zones <- management_zones %>%
  terra::mask(tcsi_mask_terra)

management_zones_stage_1 <- management_zones
#########
# Add carbon/fire regions
management_zones <- management_zones_stage_1

values(management_zones)[which(values(carbon_protect_smooth) >= 0.5 & !is.na(values(management_zones)))] <- 
  values(management_zones_stage_1)[which(values(carbon_protect_smooth) >= 0.5 & !is.na(values(management_zones)))] + 10
values(management_zones)[which(values(fire_transform_smooth) >= 0.5 & !is.na(values(management_zones)))] <- 
  values(management_zones_stage_1)[which(values(fire_transform_smooth) >= 0.5 & !is.na(values(management_zones)))] + 20
values(management_zones)[which(values(fire_adapt_smooth) >= 0.5 & !is.na(values(management_zones)))] <- 
  values(management_zones_stage_1)[which(values(fire_adapt_smooth) >= 0.5 & !is.na(values(management_zones)))] + 20
# values(management_zones)[which(values(fire_transform) < 0.5 &
#                          values(fire_transform) < 0.5 &
#                          values(fire_transform) < 0.5 & 
#                           !is.na(values(management_zones)))] <- values(management_zones)[which(values(fire_transform) < 0.5 &
#                                                                                                  values(fire_transform) < 0.5 &
#                                                                                                  values(fire_transform) < 0.5 & 
#                                                                                                  !is.na(values(management_zones)))] + 40

plot(management_zones)
table(values(management_zones))

management_zones_stage_2 <- management_zones
##################
# Add biomass band layers
hist(biomass_max_rcl)

management_zones <- management_zones_stage_2 + biomass_max_rcl
unique_zones <- unique(values(management_zones))
unique_zones <- unique_zones[order(unique_zones)]

rare_zones <- names(table(values(management_zones))[table(values(management_zones)) < 50])
common_zones <- names(table(values(management_zones))[table(values(management_zones)) >= 50])


zone_df <- data.frame(unique = unique_zones)
zone_df$rare <- ifelse(zone_df$unique %in% rare_zones, TRUE, FALSE)
zone_df$replacement <- NA

#for each rare zone, which is the next lowest option?
for(i in 1:nrow(zone_df)){
  if(zone_df$rare[i] == TRUE){
    chars <- str_split(zone_df$unique[i], pattern = "")[[1]]
    zone_df$replacement[i] <- paste0(chars[1], 0, chars[3])
  } else
    zone_df$replacement[i] <- zone_df$unique[i]
}

zone_df$replacement <- as.numeric(zone_df$replacement)

management_zones <- terra::classify(management_zones, rcl = zone_df[, c("unique", "replacement")])
plot(management_zones)
table(values(management_zones))
as.numeric(names(table(values(management_zones))))
values(management_zones)[is.na(values(management_zones))] <- 0

setGDALconfig("OSR_USE_NON_DEPRECATED", value="NO") #so we can still use EPSG:2163
template <- rast("C:/Users/Sam/Documents/Research/TCSI conservation finance/Models/Inputs/input_rasters_reproject/stands_ssp2_20_180_v5.tif")

management_zones2 <- template
values(management_zones2) <- values(management_zones)

writeRaster(management_zones2, "C:/Users/Sam/Documents/Research/TCSI conservation finance/Models/Inputs/input_rasters_reproject/new_treatment_zones_v1.tif", 
            overwrite = TRUE, datatype = "INT2S")

management_vector <- as.polygons(management_zones2)

tz <- management_zones2
values(tz)[values(management_zones2) > 0] <- 1:length(values(tz)[values(management_zones2) > 0])
plot(tz)

writeRaster(tz, "C:/Users/Sam/Documents/Research/TCSI conservation finance/Models/Inputs/input_rasters_reproject/new_stands_v1.tif", 
            overwrite = TRUE, datatype = "INT2S")



### make new stands
hu_layers <- sf::st_layers("D:/Data/hydro unit/WBD_18_HU2_GDB/WBD_18_HU2_GDB.gdb")
hu2_zone16 <- sf::st_read("D:/Data/hydro unit/WBD_16_HU2_GDB/WBD_16_HU2_GDB.gdb",
                          layer = "WBDHU12") %>%
  sf::st_transform(crs = st_crs(tcsi_shape)) %>%
  sf::st_intersection(tcsi_shape)
plot(sf::st_geometry(hu2_zone16))

hu2_zone18 <- sf::st_read("D:/Data/hydro unit/WBD_18_HU2_GDB/WBD_18_HU2_GDB.gdb",
                          layer = "WBDHU12") %>%
  sf::st_transform(crs = st_crs(tcsi_shape)) %>%
  sf::st_intersection(tcsi_shape)
plot(sf::st_geometry(hu2_zone18))

huc12 <- dplyr::bind_rows(hu2_zone16, hu2_zone18) %>%
  sf::st_make_valid()
plot(sf::st_geometry(huc12))

mgmt_vector <- terra::as.polygons(management_zones2)


