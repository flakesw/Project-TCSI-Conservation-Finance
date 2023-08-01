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

test <- mask(tcsi_mask_terra, vect(tcsi_shape))
plot(test) #check alignment

#import sdimax map
sdimax <- terra::rast("./Parameterization/management scenario data/max_sdi_bps.tif") %>%
  terra::project(tcsi_mask_terra)
plot(sdimax)
hist(sdimax)
#looks like natural breakpoint at 325, and split at 500 

sdi_class <- terra::classify(sdimax, rcl = c(-Inf, 325, 500, Inf))
plot(sdi_class)

################################################################################
#bring in other data sources
tcsi_buffered <- sf::st_buffer(tcsi_shape, dist = 5000)
#wui map from SILVIS lab
ca_wui <- sf::st_read("D:/Data/WUI/ca_wui_cp12/ca_wui_cp12.shp") %>%
  sf::st_transform(crs = sf::st_crs(tcsi_shape_buffered)) %>%
  sf::st_intersection(tcsi_shape_buffered)
plot(st_geometry(ca_wui))
nv_wui <- sf::st_read("D:/Data/WUI/nv_wui_cp12/nv_wui_cp12.shp")%>%
  sf::st_transform(crs = sf::st_crs(tcsi_shape_buffered)) %>%
  sf::st_intersection(tcsi_shape_buffered)
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
  sf::st_intersection(tcsi_shape_buffered)
wui_defense <- wui[wui$WUIFLAG10 %in% c(1,2), ] %>%
  sf::st_buffer(dist = 2400) %>%
  sf::st_union()%>%
  sf::st_intersection(tcsi_shape_buffered)
  
plot(sf::st_geometry(wui_threat))
plot(sf::st_geometry(wui_defense), add = TRUE)

#Property ownership layers -------------------------------------------------------

# federal property
# from National Atlas 2006 https://geodata.lib.utexas.edu/catalog/stanford-jd927kd1521
# fed <- sf::st_read("./Parameterization/calibration data/landuse/fedland/fedlanp020.shp") %>%
#   sf::st_transform(crs = sf::st_crs(tcsi_shape)) %>%
#   sf::st_intersection(tcsi_shape)
# 
# plot(sf::st_geometry(fed))

# forest service actual ownership
# fs <- sf::st_read("./Parameterization/calibration data/landuse/S_USA.BasicOwnership/S_USA.BasicOwnership.shp") %>%
#   sf::st_transform(crs = sf::st_crs(tcsi_shape)) %>%
#   sf::st_intersection(tcsi_shape)

# plot(fs["OWNERCLASS"])


#ownership layer from CalFire, frap.fire.ca.gov
# ownership <- sf::st_read("./Parameterization/management scenario data/ownership22_1-gdb/ownership22_1.gdb",
#                          layer = "ownership22_1") %>%
#   sf::st_transform(crs = sf::st_crs(tcsi_shape)) %>%
#   sf::st_intersection(tcsi_shape)
# plot(st_geometry(ownership))

#ownership layer that includes family vs corporate and timber lands
# Sass, E.M.; Butler, B.J.; Markowski-Lindsay, M. 2020. Forest ownership in the 
# conterminous United States circa 2017: distribution of eight ownership types - 
#   geospatial dataset. Ft. Collins, CO: U.S. Department of Agriculture, 
# Forest Service, Research Data Archive. https://doi.org/10.2737/RDS-2020-004


#1 = family
#2 = corporate
#3 = timber company
#4 = other private
#5 = federal
#6 = state
#7 = local
#8 = tribal

#there's gaps because this was only made for "forested" area, so let's fill them in
#with the most common surrounding cell type
ownership2 <- terra::rast("D:/Data/ownership/Data/forest_own1/forest_own1.tif") %>%
  terra::project(tcsi_mask_terra, method = "near") %>%
  terra::crop(tcsi_mask_terra) %>%
  terra::focal(w=9, 
              fun= modal, 
              na.policy="only",
              na.rm = TRUE)



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

#slope map
dem <- terra::rast("D:/Data/EDNAUSORIGDEM/us_orig_dem/orig_dem/w001000.adf") %>%
  terra::project(tcsi_mask_nad83) %>%
  terra::crop(tcsi_mask_nad83)
plot(dem)
slope_percent <- terra::terrain(dem, v = "slope", unit = "degrees", neighbors = 8)
slope_percent <- tan(pi*slope_percent/180) * 100
slope_percent <- project(slope_percent, tcsi_mask_terra)
plot(slope_percent)

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
#third digit: sdi band
  

#fourth digit: slope > 30 or < 30
  #--0: < 30
  #--1: > 30


# 
# old_zones <- project_to_template(rast("./Parameterization/management scenario data/tz1_20_ssp2_v3_unprojected.tif"),
#                                  tcsi_mask_terra)

#rasterize land ownership map
pif_rast <- ownership2 %>%
  terra::classify(rcl = matrix(c(2, 1,
                                 3, 1), nrow = 2, byrow = TRUE),
                  others = 0)
fam_tribal_rast <- ownership2 %>%
  terra::classify(rcl = matrix(c(1, 1,
                                 4, 1,
                                 8, 1), nrow = 3, byrow = TRUE),
                  others = 0)

general_forest_rast <- ownership2 %>%
  terra::classify(rcl = matrix(c(5, 1,
                                 6, 1,
                                 7, 1), nrow = 3, byrow = TRUE),
                  others = 0)


wild_rast <- wild %>%
  sf::st_transform(crs = st_crs(tcsi_mask_terra)) %>%
  vect() %>%
  terra::rasterize(tcsi_mask_terra)
# plot(wild_rast)

wui_threat_rast <- wui_threat %>%
  sf::st_transform(crs = st_crs(tcsi_mask_terra)) %>%
  vect() %>%
  terra::rasterize(tcsi_mask_terra, touches = TRUE)
wui_defense_rast <- wui_defense %>%
  sf::st_transform(crs = st_crs(tcsi_mask_terra)) %>%
  vect() %>%
  terra::rasterize(tcsi_mask_terra)

# nonprofit_rast <- ownership[ownership$Own_Level == "Non Profit", ] %>%
#   sf::st_transform(crs = st_crs(tcsi_mask_terra)) %>%
#   vect() %>%
#   terra::rasterize(tcsi_mask_terra)
# all_public_rast <- ownership[ownership$Own_Level != "Non Profit", ] %>%
#   sf::st_transform(crs = st_crs(tcsi_mask_terra)) %>%
#   vect() %>%
#   terra::rasterize(tcsi_mask_terra)

#slope constraints -- 35% to 50% slopes, 40% as a general constraint



#make management zone map


management_zones <- tcsi_mask_terra

#ownership layers
values(management_zones)[values(general_forest_rast) == 1] <- 4000 #general forest -- state and federal land
values(management_zones)[values(fam_tribal_rast) == 1] <- 6000 #PNIF -- family and tribal lands
values(management_zones)[values(pif_rast) == 1] <- 5000 #private industrial forest
values(management_zones)[values(wui_defense_rast) == 1] <- 3000
values(management_zones)[values(wui_threat_rast) == 1] <- 2000
values(management_zones)[values(wild_rast) == 1] <- 7000

management_zones <- management_zones %>%
  terra::mask(tcsi_mask_terra)
plot(management_zones)

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
# Add sdi class layer
hist(sdi_class)

management_zones <- management_zones_stage_2 + sdi_class*10


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
    zone_df$replacement[i] <- paste0(chars[1], chars[2], 2, 0) #bump them to top SDI class
  } else
    zone_df$replacement[i] <- zone_df$unique[i]
}

zone_df$replacement <- as.numeric(zone_df$replacement)

mgmt_bak <- management_zones

management_zones <- terra::classify(management_zones, rcl = zone_df[, c("unique", "replacement")])
plot(management_zones)
table(values(management_zones))
as.numeric(names(table(values(management_zones))))
values(management_zones)[is.na(values(management_zones))] <- 0

plot(management_zones)
# plot(management_zones2)
test <- mask(management_zones, tcsi_shape)
plot(test)
# test2 <- mask(management_zones2, tcsi_shape)

management_zones2 <- project_to_template(management_zones, tcsi_mask_terra)

management_vector <- sf::st_as_sf(as.polygons(management_zones2))

#------------------------------------------------
# Make stands, and get slope values for management zones


#make a regular grid and intersect it with the treatment zones
grid_tcsi <- sf::st_make_grid(tcsi_shape, cellsize = 720)
# plot(grid_tcsi)

stands3_vect <- sf::st_intersection(management_vector, grid_tcsi) %>%
  dplyr::rename("MZ" = "mask_old")

#bring in slope and see how many cells in each stand are too steep.
# If a majority are too steep, then cannot be treated mechanically
stands3_vect$slope <- terra::extract(slope_class, 
                                        vect(stands3_vect), 
                                        fun = "mean") %>%
                          dplyr::select(slope)

stands3_vect <- sf::st_cast(stands3_vect, "POLYGON") #convert to polygon

stands3_vect$id <- 1:nrow(stands3_vect)
stands3 <- terra::rasterize(vect(stands3_vect), tcsi_mask_terra, "id")
plot(stands3)
values(stands3)[is.na(values(stands3))] <- 0
table(values(stands3))

test <- mask(stands3, tcsi_shape)
plot(test)

writeRaster(stands3, "C:/Users/Sam/Documents/Research/TCSI conservation finance/Models/Inputs/input_rasters_reproject/stands_grid_sdi.tif", 
            overwrite = TRUE, datatype = "INT4S", NAflag = 0)


### Make management zones
stands3_vect$MZ <- ifelse(stands3_vect$slope > 0.3, 
                             stands3_vect$MZ + 1, 
                             stands3_vect$MZ)
table(stands3_vect$MZ)

management_zone_raster <- terra::rasterize(stands3_vect, tcsi_mask_terra, "MZ")
plot(management_zone_raster)

terra::writeRaster(management_zone_raster, 
                   "C:/Users/Sam/Documents/Research/TCSI conservation finance/Models/Inputs/input_rasters_reproject/management_zones_sdi.tif")
