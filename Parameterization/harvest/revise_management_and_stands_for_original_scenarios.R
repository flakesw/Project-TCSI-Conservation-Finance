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
# empty_mask <- tcsi_mask
# empty_mask$mask.tif <- 0
tcsi_shape <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  st_transform(crs = "EPSG:9311")
tcsi_5070 <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  st_transform(crs = "EPSG:5070")

test <- mask(tcsi_mask_terra, vect(tcsi_shape))
plot(test) #check alignment


#import and process biomass data to make maximum biomass maps
mfri <- rast("D:/Data/landfire vegetation/US_140_MFRI/US_140_MFRI/Tif/us_140mfri.tif")
plot(mfri)

mfri_tcsi <- terra::crop(mfri, vect(tcsi_5070))
plot(mfri_tcsi)
mfri_tcsi <- terra::aggregate(mfri_tcsi, fact = 6, fun = "modal")
plot(mfri_tcsi)

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
  sf::st_union() %>%
  sf::st_intersection(tcsi_shape)
wui_defense <- wui[wui$WUIFLAG10 %in% c(1,2), ] %>%
  sf::st_buffer(dist = 2400) %>%
  sf::st_union()%>%
  sf::st_intersection(tcsi_shape)
  
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


### Make TCSI map
library("ggspatial")

tahoe <- sf::st_read("D:/Data/lake_tahoe_shape/Tahoe_Soils_and_Hydro_Data.shp") %>%
  sf::st_transform(crs(tcsi_shape))
public_land <- sf::st_union(ownership)

library("basemaps")


test <- ggplot() + 
  basemap_gglayer(tcsi_shape) +
  geom_sf(data = tcsi_shape, color = "white", bg = "white") +
  geom_sf(data = public_land, col = "#1f78b4", bg = "#1f78b4")  +
  geom_sf(data = wui_defense, color = "#fec44f", bg = "#fec44f") +
  geom_sf(data = wui_threat, color = "#fff7bc", bg = "#fff7bc")+
  geom_sf(data = wild, col = "#33a02c", bg = "#33a02c") + 
  geom_sf(data = tahoe, col = "gray", bg = "gray") + 
  theme_minimal()
plot(test)



####################
# Intersect layers

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
# plot(wild_rast)

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
# plot(private_indust_rast)

#slope constraints -- 35% to 50% slopes, 40% as a general constraint
slope_full <- rast("./Parameterization/calibration data/topography/sierra_slope.tif") %>%
  crop(vect(tcsi_shape %>% sf::st_transform("EPSG:4326")))
#Maxwell used 30% cutoff = 16.7 degrees
slope_class <- terra::classify(slope_full, rcl = matrix(c(0, 16.7, 1,
                                                          16.7, 90, 2), byrow = TRUE, ncol = 3))
plot(slope_full) 
plot(slope_class)

#make management zone map


management_zones <- tcsi_mask_terra

#ownership layers
values(management_zones) <- 5000 #default -- PIF
# values(management_zones)[values(old_zones) %in% c(1:10)] <- 500 #PIF
# values(management_zones)[values(old_zones) %in% c(11:20)] <- 600 #PNIF
# values(management_zones)[values(fs_rast) == 2] <- 400 #general forest
values(management_zones)[values(all_public_rast) == 1] <- 4000 #general forest -- state and federal land
values(management_zones)[values(nonprofit_rast) == 1] <- 4000 #general forest -- conservation land
values(management_zones)[values(wui_defense_rast) == 1] <- 3000
values(management_zones)[values(wui_threat_rast) == 1] <- 2000
values(management_zones)[values(wild_rast) == 1] <- 7000

management_zones <- management_zones %>%
  terra::mask(tcsi_mask_terra)

management_zones_stage_1 <- management_zones
#########
# Add carbon/fire regions
management_zones <- management_zones_stage_1

values(management_zones)[which(values(carbon_protect_smooth) >= 0.5 & !is.na(values(management_zones)))] <- 
  values(management_zones_stage_1)[which(values(carbon_protect_smooth) >= 0.5 & !is.na(values(management_zones)))] + 100
values(management_zones)[which(values(fire_transform_smooth) >= 0.5 & !is.na(values(management_zones)))] <- 
  values(management_zones_stage_1)[which(values(fire_transform_smooth) >= 0.5 & !is.na(values(management_zones)))] + 200
values(management_zones)[which(values(fire_adapt_smooth) >= 0.5 & !is.na(values(management_zones)))] <- 
  values(management_zones_stage_1)[which(values(fire_adapt_smooth) >= 0.5 & !is.na(values(management_zones)))] + 200
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

plot(management_zones)
# plot(management_zones2)
test <- mask(management_zones, tcsi_shape)
# test2 <- mask(management_zones2, tcsi_shape)

management_zones2 <- project_to_template(management_zones, tcsi_mask_terra)

writeRaster(management_zones2, "C:/Users/Sam/Documents/Research/TCSI conservation finance/Models/Inputs/input_rasters_reproject/new_treatment_zones_v1.tif", 
            overwrite = TRUE, datatype = "INT4S")

management_vector <- as.polygons(management_zones2)

#stand method 1:
#each cell is its own stand
#this makes too many stands for harvest to handle
stands1 <- management_zones2
values(stands1)[values(management_zones2) > 0] <- 1:length(values(stands1)[values(management_zones2) > 0])
plot(stands1)


writeRaster(stands1, "C:/Users/Sam/Documents/Research/TCSI conservation finance/Models/Inputs/input_rasters_reproject/stands_all_cells_v1.tif", 
            overwrite = TRUE, datatype = "INT4S")



#stand method 2:
#intersect TZ with huc12s to  make stands
hu_layers <- sf::st_layers("D:/Data/hydro unit/WBD_18_HU2_GDB/WBD_18_HU2_GDB.gdb")
hu2_zone16 <- sf::st_read("D:/Data/hydro unit/WBD_16_HU2_GDB/WBD_16_HU2_GDB.gdb",
                          layer = "WBDHU12") %>%
  sf::st_transform(crs = st_crs(tcsi_shape)) %>%
  sf::st_intersection(tcsi_shape)
# plot(sf::st_geometry(hu2_zone16))

hu2_zone18 <- sf::st_read("D:/Data/hydro unit/WBD_18_HU2_GDB/WBD_18_HU2_GDB.gdb",
                          layer = "WBDHU12") %>%
  sf::st_transform(crs = st_crs(tcsi_shape)) %>%
  sf::st_intersection(tcsi_shape)
# plot(sf::st_geometry(hu2_zone18))

huc12 <- dplyr::bind_rows(hu2_zone16, hu2_zone18) %>%
  sf::st_make_valid()
# plot(sf::st_geometry(huc12))

management_vector <- sf::st_as_sf(management_vector) %>%
  sf::st_make_valid()

#the geometry here gets wonky so we have to do some weird stuff to fix it
stands2_vect <-  sf::st_intersection(huc12, management_vector) %>%
  sf::st_as_sf() %>%
  sf::st_cast("MULTIPOLYGON") %>%
  sf::st_cast("POLYGON") %>%
  sf::st_make_valid() %>%
  sf::st_set_geometry("shape")
stands2_vect$id <- 0:(nrow(stands2_vect)-1)

# plot(st_geometry(stands2_vect))

stands2 <- terra::rasterize(vect(stands2_vect), tcsi_mask_terra, "id")
plot(stands2)
values(stands2)[is.na(values(stands2))] <- 0

test <- mask(stands2, tcsi_shape)
plot(test)



#the harvest extension seems to want an unsigned integer, but writing
#with unsigned integer doesn't work for some reason
#in fact this huc12 method doesn't work at all; not sure why but the intersection has invalid geometry
writeRaster(stands2, "C:/Users/Sam/Documents/Research/TCSI conservation finance/Models/Inputs/input_rasters_reproject/stands_huc12_v1.tif", 
            overwrite = TRUE, datatype = "INT4S", NAflag = 0)

#stands method 3:
#make a regular grid and intersect it with the treatment zones
grid_tcsi <- sf::st_make_grid(tcsi_shape, cellsize = 720)
# plot(grid_tcsi)

stands3_vect <- sf::st_intersection(grid_tcsi, management_vector)

stands3_vect_sf <- stands3_vect %>%
  sf::st_as_sf() %>%
  sf::st_cast("MULTIPOLYGON")

stands3_vect_sf$id <- 1:nrow(stands3_vect_sf)
stands3 <- terra::rasterize(stands3_vect_sf, tcsi_mask_terra, "id")
plot(stands3)
values(stands3)[is.na(values(stands3))] <- 0

test <- mask(stands3, tcsi_shape)
plot(test)

writeRaster(stands3, "C:/Users/Sam/Documents/Research/TCSI conservation finance/Models/Inputs/input_rasters_reproject/stands_grid_v1.tif", 
            overwrite = TRUE, datatype = "INT4S", NAflag = 0)
