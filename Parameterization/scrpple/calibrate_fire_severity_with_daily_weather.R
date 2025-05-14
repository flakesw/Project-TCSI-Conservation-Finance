#download data needed to predict site-level severity from climate and fuels

#TODO see if we can parallelize this thing!!!

#TODO add: NDVI normals

#TODO add: time since fire; fire departure

#TODO add: Sierra Nevada region (extract when clipping raster to region)

#TODO add: monthly climate (CWD, PET, VPD) for all fires

#TODO add: year-of annual CWD and PET

library("geoknife")
library("sf")
library("terra")
library("tidyverse")
# library("raster")
library("FedData") #for downloading SSURGO data
library("stars")# using stars is so much faster, since the whole files aren't loaded into RAM, just the subsets
# it's really an amazing upgrade from raster
# library("MODIStsp") # for downloading MODIS data
library("archive")
library("lubridate")
library("cffdrs")
terraOptions(datatype="FLT8S")
options(warn = 0) #this gets reset sometimes for some reason, super annoying


# setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance")

#to fit the model, we need dNBR data, %clay, ET, CWD, effective windspeed, fine fuels, and ladder fuels
# There are a few data sources we could choose, but here I use MTBS for dNBR, 
# TerraClimate for ET and CWD, MACAv2 for windspeed, 
# and Landfire fuels models for fine and ladder fuels

# This script has a ton of downloading and processing going on. 
# response variable: dNBR
# predictors: %clay, ET, effective windspeed, CWD, fuels

#CRS -- EPSG 5070 or ESRI 102039





#*******************************************************************************
# LOAD DATA
#*******************************************************************************

#-------------------------------------------------------------------------------
# Load short database -- to get some other variables like number of days fire burned
sierra_shape <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  sf::st_transform("EPSG:5070") %>%
  sf::st_union()
sierra_poly_wgs <- sierra_shape %>%
  sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

short <- readRDS("./Parameterization/calibration data/short_ignitions/short_sierra.RDS") %>%
  sf::st_transform("EPSG:5070")

#-------------------------------------------------------------------------------
#LANDFIRE layers
# layers of fine fuels and ladder fuels, see create_fine_and_ladder_fuels_from_landfire_whole_sierra.R
landfire_2001_fine <- read_stars("D:/Data/Landfire fuels/sierra/landfire_fine_2001.tif")
landfire_2001_ladder <- read_stars("D:/Data/Landfire fuels/sierra/landfire_ladder_2001.tif")
landfire_2012_fine <- read_stars("D:/Data/Landfire fuels/sierra/landfire_fine_2012.tif")
landfire_2012_ladder <- read_stars("D:/Data/Landfire fuels/sierra/landfire_ladder_2012.tif")
landfire_2014_fine <- read_stars("D:/Data/Landfire fuels/sierra/landfire_fine_2014.tif")
landfire_2014_ladder <- read_stars("D:/Data/Landfire fuels/sierra/landfire_ladder_2014.tif")
landfire_2019_fine <- read_stars("D:/Data/Landfire fuels/sierra/landfire_fine_2019.tif")
landfire_2019_ladder <- read_stars("D:/Data/Landfire fuels/sierra/landfire_ladder_2019.tif")
landfire_2020_fine <- read_stars("D:/Data/Landfire fuels/sierra/landfire_fine_2020.tif")
landfire_2020_ladder <- read_stars("D:/Data/Landfire fuels/sierra/landfire_ladder_2020.tif")
landfire_2021_fine <- read_stars("D:/Data/Landfire fuels/sierra/landfire_fine_2021.tif")
landfire_2021_ladder <- read_stars("D:/Data/Landfire fuels/sierra/landfire_ladder_2021.tif")

dep <- stars::read_stars("D:/Data/landfire vegetation/veg_departure_sierra/US_105_VDEP/us_105vdep.tif")


#-------------------------------------------------------------------------------
# Treemap fuels

# treemap <- terra::rast("D:/Data/treemap/RDS-2019-0026_Data/Data/national_c2014_tree_list.tif")
# 
# sierra_temp <- sf::st_transform(sierra_shape, crs(treemap))
# 
# treemap <-  terra::crop(treemap, vect(sierra_temp)) %>%
#   terra::project("EPSG:5070", method = "near")
# #
# # cats(treemap)
# # levels(treemap)
# 
# treemap[] <- as.numeric(as.character(treemap[])) #convert factor to numeric
# 
# tl_plots <- read.csv("D:/Data/treemap/RDS-2019-0026_Data/Data/TL_CN_Lookup.txt") %>%
#   filter(tl_id %in% treemap[])
# 
# tl_trees <- read.csv("D:/Data/treemap/RDS-2019-0026_Data/Data/Tree_table_CONUS.txt") %>%
#   filter(tl_id %in% treemap[])
# 
# ladder_fuels <- tl_trees %>%
#   filter(DIA < 4) %>%
#   mutate(biomass = exp(-2.5356 + 2.4349*log(DIA*2.54))) %>% #Jenkins equation for pines -- all the equations are really similar for small diameter stems
#   mutate(biomass_density = biomass * TPA_UNADJ * 2.47 * 0.1) %>% #expand to kg/ac, convert to kg/ha, convert to g/m2
#   group_by(tl_id) %>%
#   summarize(ladder_fuels = sum(biomass_density))
# 
# tm_ladder_fuel <- terra::classify(treemap, rcl = ladder_fuels, others = 0)
# writeRaster(tm_ladder_fuel, "./Parameterization/calibration data/treemap_fuels.tif", overwrite = TRUE)


#already preprocessed
tm_ladder_fuel <- read_stars("./Parameterization/calibration data/treemap_fuels.tif")
 

#-------------------------------------------------------------------------------
# import slope and aspect map
#-------------------------------------------------------------------------------

# these rasters in the wrong CRS, but it takes a shit ton of RAM to reproject them.
# Instead, I'll project the fire boundary to match, and then crop and reproject.
# It's actually really friggin fast to do it this way.

slope_full <- read_stars("./Parameterization/calibration data/topography/sierra_slope.tif")

aspect_full <- read_stars("./Parameterization/calibration data/topography/sierra_aspect.tif")

#-------------------------------------------------------------------------------
#set up MTBS data
#-------------------------------------------------------------------------------

#folder with all the individual fire data (inside this folder are each folder for each year)
#everything is already unzipped, so there's a bunch of naked .tif files in here
mtbs_folder <- "D:/Data/mtbs_all_fires"

#these rasters are squares which extend past the fire boundary
# only using rdnbr rasters -- excludes some fires based on nbr
mtbs_dnbr <- list.files(path = mtbs_folder, pattern = "*_dnbr.tif", 
                        full.names = TRUE, recursive = TRUE)

mtbs_rdnbr <- list.files(path = mtbs_folder, pattern = "*_rdnbr.tif", 
                        full.names = TRUE, recursive = TRUE)

mtbs_sev <- list.files(path = mtbs_folder, pattern = "*_dnbr6.tif", 
                       full.names = TRUE, recursive = TRUE)

#fire boundaries to clip rasters to
mtbs_shape <- list.files(path = mtbs_folder, pattern = "*_burn_bndy.shp", 
                         full.names = TRUE, recursive = TRUE) 

#rasters and shapefiles have different lengths -- either there are duplicates,
#extra shapefiles for some fires, or something else like that
raster_firenumbers <- mtbs_dnbr %>%
  base::basename() %>%
  base::strsplit("_") %>%
  purrr::map(1) %>%
  base::unlist()

shape_firenumbers <- mtbs_shape %>%
  base::basename() %>%
  base::strsplit("_") %>%
  purrr::map(1) %>%
  base::unlist()

#some fires have shapes but not rasters
table(shape_firenumbers %in% raster_firenumbers)
table(raster_firenumbers %in% shape_firenumbers)

mtbs_shape <- subset(mtbs_shape, shape_firenumbers %in% raster_firenumbers)

mtbs_shape <- mtbs_shape[-769] #this is just to remove the Dixie fire which is too big to do in one go (!)
mtbs_dnbr <- mtbs_dnbr[-769]
mtbs_sev <- mtbs_sev[-769]
#-------------------------------------------------------------------------------
# Bring in daily perimeters to get dates that each cell burned
#-------------------------------------------------------------------------------
daily_perims_all <- sf::st_read("./Parameterization/calibration data/geomac_all_years/geomac_combined_2000-2023.gpkg") %>%
  sf::st_as_sf() %>%
  sf::st_transform(crs = sf::st_crs(sierra_shape)) %>%
  # sf::st_intersection(sierra_shape) %>%
  dplyr::filter(fireyear >= 2000) %>% 
  mutate(incidentna = paste0(incidentna, fireyear))

#recalculate area -- the gisacres column is a crazy mess, no idea what happened there
daily_perims_all$gisacres <- map(sf::st_geometry(daily_perims_all), ~ sf::st_area(.)) %>%
  unlist() %>%
  `/`(4046.86) #convert to acres

#fix dates
library("lubridate")
daily_perims_all$perimeterd
normal_format <- which((substr(daily_perims_all$perimeterd, 1, 4) %in% c(2000:2022)))
different_format <- which(!(substr(daily_perims_all$perimeterd, 1, 4) %in% c(2000:2022)))

dates_with_time <- mdy_hms(daily_perims_all[different_format, ]$perimeterd)
dates_mdy <- mdy(daily_perims_all[different_format, ]$perimeterd)

dates_fixed <- ifelse(!is.na(dates_with_time), 
                      as.character(dates_with_time, format = "%Y-%m-%d"), 
                      as.character(dates_mdy, format = "%Y-%m-%d"))

daily_perims_all[different_format, ]$perimeterd <- dates_fixed

daily_perims_all$perimeterd <- as.Date(daily_perims_all$perimeterd, format = "%Y-%m-%d")

#remove duplicates (polygons with same size on same day for same fire)
daily_perims_all <- daily_perims_all %>%
  group_by(incidentna) %>%
  group_by(perimeterd) %>%
  slice_max(gisacres) %>%
  distinct(gisacres, .keep_all= TRUE) %>%
  filter(fireyear %in% c(2000:2022)) #only have MTBS fires through 2022

## example fire growth
# king <- daily_perims_all[grep("King2014", daily_perims_all$incidentna), ] %>%
#   # sf::st_transform(crs = st_crs(boundary)) %>%
#   # dplyr::mutate(intersects = as.logical(sf::st_intersects(geometry, boundary, sparse = FALSE))) %>%
#   # filter(intersects) %>%
#   dplyr::arrange((perimeterd)) %>%
#   ungroup() %>%
#   mutate(growth = (gisacres-lag(gisacres, n = 1))/lag(gisacres, n = 1)) %>%
#   slice(c(which(growth > 0), 1)) %>% #include positive growth or the first day (which has no growth)
#   dplyr::arrange(desc(perimeterd)) %>%
#   dplyr::mutate(newdate = as.integer(format(perimeterd, "%Y%m%d"))) %>%
#   ungroup() %>%
#   dplyr::mutate(sequence = seq_len(n())) %>%
#   terra::rasterize(x = vect(.), y = rast(., res = 30), field = "sequence")
#   # terra::extend(dnbr_raster) %>% #no idea why I need to do all this finagling; crop(extend = TRUE) should do it on its own.
#   # terra::crop(dnbr_raster) %>%
#   # project(dnbr_raster, method = "near")
# diverging_color_ramp <- function(ras){
#   the_palette_fc <- leaflet::colorNumeric(palette = "RdBu",
#                                           domain = c(-max(abs(ras[]), na.rm = TRUE), max(abs(ras[]), na.rm = TRUE)),
#                                           reverse = TRUE)
#   the_colors <- the_palette_fc(seq(min(ras[], na.rm = TRUE), max(ras[], na.rm = TRUE), length.out = 50))
# 
# }
# king[] <- 22-king[]
# 
# terra::writeRaster(king, "king.tiff", overwrite = TRUE)
# 
# #King fire code is CA3878212060420140913
# 
# king_dnbr <- terra::rast(mtbs_dnbr[grep("3878212060420140913", mtbs_dnbr)])
# king_dnbr[king_dnbr < 0] <- 0
# plot(king_dnbr)
# terra::writeRaster(king_dnbr, "king_dnbr.tiff")

#*******************************************************************************
# DEFINE FUNCTIONS TO ACCESS DATA
#*******************************************************************************

get_raster_cell_date <- function(dnbr_raster, boundary){ 

  #this uses the daily perimeter data to figure out the first day that a cell burned.
  #This should allow us to use more precise climate information
  perims_sub <- daily_perims_all[daily_perims_all$fireyear == boundary$fireyear, ] %>%
    sf::st_transform(crs = st_crs(boundary)) %>%
    dplyr::mutate(intersects = as.logical(sf::st_intersects(geom, boundary, sparse = FALSE))) %>%
    filter(intersects) %>%
    dplyr::arrange((perimeterd)) %>%
    ungroup() %>%
    mutate(growth = (gisacres-lag(gisacres, n = 1))/lag(gisacres, n = 1),
           days_between = as.numeric(perimeterd - lag(perimeterd, n = 1))) %>%
    slice(c(which(growth > 0), 1))
  
  if(nrow(perims_sub) <= 1 | sum(perims_sub$days_between <= 2, na.rm = TRUE) <1){
    use_daily_perims <<- FALSE
    message("No daily perimeters found for fire")
    return(NA)
  }
  
  terraOptions(datatype="INT8U") #needed to stop terra from rounding large integers -- TODO make an issue on GitHub
  
  #TODO to make a nice figure
  date_raster <- perims_sub  %>%
    dplyr::arrange(desc(perimeterd)) %>%
    dplyr::mutate(newdate = as.integer(format(perimeterd, "%Y%m%d"))) %>%
    ungroup() %>%
    dplyr::mutate(sequence = seq_len(n())) %>%
    terra::rasterize(x = vect(.), y = dnbr_raster, field = "newdate") %>%
    terra::extend(dnbr_raster) %>% #no idea why I need to do all this finagling; crop(extend = TRUE) should do it on its own.
    terra::crop(dnbr_raster) %>%
    project(dnbr_raster, method = "near") #need to reproject to get things to line up exactly. This feels like I've done something wrong?
  NAflag(date_raster) <- 0
  # plot(date_raster)
  
  days_between_raster <-   perims_sub  %>%
    dplyr::arrange(desc(perimeterd)) %>%
    dplyr::mutate(newdate = as.integer(format(perimeterd, "%Y%m%d"))) %>%
    ungroup() %>%
    terra::rasterize(x = vect(.), y = dnbr_raster, field = "days_between") %>%
    terra::extend(dnbr_raster) %>% #no idea why I need to do all this finagling; crop(extend = TRUE) should do it on its own.
    terra::crop(dnbr_raster) %>%
    project(dnbr_raster, method = "near")
  # plot(days_between_raster)
  
  few_days <- terra::clamp(days_between_raster, lower = 1, upper = 2, values = FALSE)
  
  good_date_raster <- terra::mask(date_raster, mask = few_days, maskvalues = NA, updatevalue = NA)
  
  if(sum(!is.na(good_date_raster[])) < 2) {
    stop("Not enough dates available for the date raster calculation")
  }
  
  return(good_date_raster)
  
}

#-------------------------------------------------------------------------------
# Trim LANDFIRE fuels
#-------------------------------------------------------------------------------


get_landfire_fuel <- function(boundary, dnbr_raster){
  
  fire_year <- format(boundary$Ig_Date, "%Y")

  if(fire_year > 2001){ #because 2001 data includes 2001 fire scars, 
    #we can only use LANDFIRE for fires in 2002 or later
    if(fire_year<= 2012){
      fine_fuel <- landfire_2001_fine[boundary]
      ladder_fuel <- landfire_2001_ladder[boundary]
    }else if(fire_year <= 2014){ #for 2013 and 2014 fires, use 2012 fuels
      fine_fuel <- landfire_2012_fine[boundary]
      ladder_fuel <- landfire_2012_ladder[boundary]
    }else if(fire_year <= 2019){ #for fires in 2015 through 2019, use 2014 fuels
      fine_fuel <- landfire_2014_fine[boundary]
      ladder_fuel <- landfire_2014_ladder[boundary]
    }else if(fire_year == 2020){
      fine_fuel <- landfire_2019_fine[boundary]
      ladder_fuel <- landfire_2019_ladder[boundary]
    }else if(fire_year >= 2021){
      fine_fuel <- landfire_2020_fine[boundary]
      ladder_fuel <- landfire_2020_ladder[boundary]
    }
    
    fine_fuel <- as(fine_fuel, "SpatRaster") %>%
      terra::project(dnbr_raster) %>%
      terra::values()
    ladder_fuel <- as(ladder_fuel, "SpatRaster") %>%
      terra::project(dnbr_raster)%>%
      terra::values()
    
    combined <- data.frame(fine_fuel = fine_fuel,
                           ladder_fuel = ladder_fuel) #TODO why do the names stay unchaged?
    names(combined) <- c("fine_fuel", "ladder_fuel")
    
    return(combined)  
    
  }else return(NA)

  
}

#-------------------------------------------------------------------------------
# Trim sierra-wide rasters (slope, aspect, departure)
#-------------------------------------------------------------------------------
trim_stars_to_raster <- function(boundary, dnbr_raster, raster){
  boundary2 <- st_transform(boundary, crs = st_crs(raster))
  raster2_vals <- raster[boundary2] %>% #crop using stars
    as(., "SpatRaster") %>% #convert to terra
    terra::project(dnbr_raster) %>%
    terra::values()
  
  return(raster2_vals)
}

#-------------------------------------------------------------------------------
# get %clay from SSURGO
#-------------------------------------------------------------------------------

download_clay <- function(dnbr_raster, label){
  
  terraOptions(datatype="FLT4S")
  
  # This is pretty slow when first being run, because it downloads all the SSURGO data
  # for a grid. But it speeds up as you iterate through fires, because some fires
  # will share soils data. Not super efficient but not as bad as I thought.
  ssurgo_test <- get_ssurgo(template = raster::raster(dnbr_raster), 
                            label = label, 
                            raw.dir = "D:/Data/sierra_ssurgo/RAW",
                            extraction.dir = paste0("D:/Data/sierra_ssurgo/EXTRACTIONS/", label, "/SSURGO"),
                            force.redo = FALSE)
  
  # ssurgo_test$tabular$chorizon$cokey
  
  #get average clay for each component by averaging horizon %clay, weighted by horizon thickness
  component <- ssurgo_test$tabular$chorizon %>% 
    dplyr::mutate(hzthk_r = ifelse(!is.na(hzthk.r), hzthk.r, hzdepb.r - hzdept.r)) %>%  #there are a few NAs for thickness; replace with the depth to top minus depth to bottom
    dplyr::select(cokey, hzthk.r, claytotal.r) %>%
    dplyr::group_by(cokey) %>%
    dplyr::summarise(across(c(claytotal.r), 
                            ~stats::weighted.mean(., w = hzthk.r, na.rm = TRUE))) %>%
    dplyr::mutate(across(everything(), .fns = ~replace_na(.x, 1))) %>% #replace NAs where any are left -- just on rocky outcrops and beaches. Replace with 1 instead of 0 so that the sites can still be active
    dplyr::mutate(across(claytotal.r, .fns = ~ `*`(.x, 0.01))) %>% #multiply some columns by 0.01 to convert from percent to proportion
    dplyr::right_join(ssurgo_test$tabular$component, by = "cokey") %>%
    dplyr::select(cokey, claytotal.r, comppct.r, mukey) %>%
    dplyr::mutate(across(c("claytotal.r", "comppct.r"), as.double)) %>%
    dplyr::mutate(across(c("claytotal.r", "comppct.r"), ~replace_na(.x, 0.001))) %>%
    dplyr::mutate(MUKEY = as.character(mukey))
  
  #aggregate to mapunits by component
  mapunits_data <- component %>%
    dplyr::group_by(MUKEY) %>%
    dplyr::summarise(across(c(claytotal.r), 
                            ~stats::weighted.mean(., w = comppct.r, na.rm = TRUE))) 
  
  mapunits_spatial <- left_join(sf::st_as_sf(ssurgo_test$spatial), mapunits_data, by = c("MUKEY")) %>%
    sf::st_transform(crs(dnbr_raster))
  
  clay_map <- terra::rasterize(vect(mapunits_spatial), dnbr_raster, field = "claytotal.r", fun="mean")
  
  return(clay_map)
}  

# #-------------------------------------------------------------------------------
# # Download ET and CWD data
# # Monthly data from TerraClimate
# # Abatzoglou, J.T., S.Z. Dobrowski, S.A. Parks, K.C. Hegewisch, 2018, Terraclimate, 
# # a high-resolution global dataset of monthly climate and climatic water balance from 1958-2015, Scientific Data
# 
# This no longer works due to the death of geoknife, RIP
#
# download_pet_cwd <- function(boundary){  
#   #shapefile for fire
#   fire_boundary <- boundary %>%
#     sf::st_transform(crs = "+proj=longlat +datum=WGS84") #reproject to CRS that geoknife needs
#   
#   #"stencil" is what geoknife uses for the extent of the data
#   stencil <- simplegeom(as(fire_boundary, Class = "Spatial"))
#   
#   #download PET and AET to calculate CWD
#   vars_url <- c("pet", "aet")
#   year <- fire_boundary$fireyear
#   
#   urls <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/TERRACLIMATE_ALL/data/TerraClimate_", 
#                  vars_url, "_", year, ".nc")
#   
#   fabric <- webdata(url = urls[1])
#   
#   geoknife::query(fabric, 'variables')
#   
#   knife <- webprocess(wait = TRUE)
#   
#   # area grid statistics are the default, but we can change it if we  (we don't)
#   algorithm(knife) <- list('Area Grid Statistics (weighted)' = 
#                              "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")
#   
#   knife@processInputs$STATISTICS <- c("MEAN") #what statistics do we want?
#   
#   job_results <- list()
#   
#   for(i in 1:length(vars_url)){
#     #set the fabric for a new variable, but keep everything else the same (i.e. the stencil and knife)
#     fabric <- webdata(url = urls[i])
#     variables(fabric) <- vars_url[i]
#     print(vars_url[i])
#     job <- geoknife(stencil, fabric, knife)
#     
#     
#     if(error(job)){
#       break
#       check(job)
#     }
#     
#     job_results[[i]] <- result(job)
#   }
#   
#   # results <- result(job)
#   
#   month <- as.Date(fire_date, format = "%Y-%m-%d") %>%
#     format("%m")
#   
#   pet <- job_results[[1]][as.numeric(month), 2]
#   aet <- job_results[[2]][as.numeric(month), 2]
#   cwd <- pet - aet
#   
#   if(anyNA(c(pet,aet,cwd))){
#     return(NA)
#   } else return(c(pet, aet, cwd))
# }


#-------------------------------------------------------------------------------
#effective windspeed
# MACA downscaled data

get_daily_climate_rasters <- function(boundary, dates_of_fire){

  gridmet_dir <- "D:/Data/gridmet/"
  gridmet_files <- list.files(gridmet_dir)
  gridmet_crs <- "epsg:4326"
  boundary_wgs <- st_transform(boundary, gridmet_crs) %>%
    sf::st_buffer(dist = 10000)

  vs_rast <- rast(paste0(gridmet_dir, "vs_", boundary$fireyear, ".nc")) %>%
    terra::crop(vect(boundary_wgs)) 
  th_rast <- rast(paste0(gridmet_dir, "th_", boundary$fireyear, ".nc")) %>%
    terra::crop(vect(boundary_wgs)) 
  vpd_rast <- rast(paste0(gridmet_dir, "vpd_", boundary$fireyear, ".nc")) %>%
    terra::crop(vect(boundary_wgs)) 
  pet_rast <- rast(paste0(gridmet_dir, "pet_", boundary$fireyear, ".nc")) %>%
    terra::crop(vect(boundary_wgs)) 
  
  clim_names <- names(vs_rast) %>%
    gsub("\\D", "", .) %>%
    as.integer() %>%
    as.Date(origin = "1900-01-01")
  
  names(vs_rast) <- clim_names
  names(th_rast) <- clim_names
  names(vpd_rast) <- clim_names
  names(pet_rast) <- clim_names
  
  
  fire_dates_expanded <- as.Date(c((min(dates_of_fire) - 1) : (max(dates_of_fire) + 1)), origin = "1970-01-01")
  
  vs_rast <- vs_rast[[clim_names %in% fire_dates_expanded ]]
  th_rast <- th_rast[[clim_names %in% fire_dates_expanded ]]
  vpd_rast <- vpd_rast[[clim_names %in% fire_dates_expanded ]]
  pet_rast <- pet_rast[[clim_names %in% fire_dates_expanded ]]
  
  
  
  clim_rast <- list(vs_rast,
                    th_rast,
                    vpd_rast,
                    pet_rast)
  
  return(clim_rast)
}

#-------------------------------------------------------------------------------
# get relative windspeed and other daily climate data, extracted to the right
# location

get_daily_climate_mosaic <- function(dnbr_raster, date_raster, severity_raster, clim, fwi, cffdrs){ 
  terraOptions(datatype="FLT8S")
  
   for(i in 1:length(clim)){
    names(clim[i][[1]]) <-  names(clim[i][[1]]) %>%
      as.Date(format = "%Y-%m-%d") %>% #convert to date
      `+`(1) %>%  # add a day (to get previous day's weather for each day's burn perimeter)
      gsub("-", "", .) %>%
      as.character()
   }
  
  for(i in 1:length(cffdrs)){
    names(cffdrs[i][[1]]) <- names(cffdrs[i][[1]]) %>%
      as.Date(format = "%Y-%m-%d")
    `+`(1) %>%  # add a day (to get previous day's weather for each day's burn perimeter)
    gsub("-", "", .)
  }
  
  
  wind_speed <- clim[1][[1]] %>%
    terra::project(dnbr_raster, method = "bilinear")
  
  wind_dir <- clim[2][[1]] %>%
    terra::project(dnbr_raster, method = "near")
  
  vpd <- clim[3][[1]]%>%
    terra::project(dnbr_raster, method = "bilinear")
  
  pet <- clim[4][[1]]%>%
    terra::project(dnbr_raster, method = "bilinear")
  
  # eddi <- clim[5][[1]]%>%
  #   terra::project(dnbr_raster, method = "bilinear")
  # 
  # pdsi <- clim[6][[1]]%>%
  #   terra::project(dnbr_raster, method = "bilinear")
  
  FFMC_rast <-  cffdrs[1][[1]]%>%
    terra::project(dnbr_raster, method = "bilinear")
  DMC_rast <-  cffdrs[2][[1]]%>%
    terra::project(dnbr_raster, method = "bilinear")
  DC_rast <-  cffdrs[3][[1]]%>%
    terra::project(dnbr_raster, method = "bilinear")
  ISI_rast <-  cffdrs[4][[1]]%>%
    terra::project(dnbr_raster, method = "bilinear")
  BUI_rast <-  cffdrs[5][[1]]%>%
    terra::project(dnbr_raster, method = "bilinear")
  FWI_rast <-  cffdrs[6][[1]]%>%
    terra::project(dnbr_raster, method = "bilinear")
  DSR_rast <-  cffdrs[7][[1]]%>%
    terra::project(dnbr_raster, method = "bilinear")
  
  # 
  # fwi2 <- fwi %>%
  #   terra::project(dnbr_raster)
  
  wind_speed_raster <- dnbr_raster %>% terra::setValues(NA)
  wind_dir_raster <- dnbr_raster %>% terra::setValues(NA)
  vpd_raster <- dnbr_raster %>% terra::setValues(NA)
  pet_raster <- dnbr_raster %>% terra::setValues(NA)
  # eddi_raster <- dnbr_raster %>% terra::setValues(NA)
  # pdsi_raster <- dnbr_raster %>% terra::setValues(NA)
  fwi_raster <- dnbr_raster %>% terra::setValues(NA)
  
  FFMC_raster <-  dnbr_raster %>% terra::setValues(NA)
  DMC_raster <-  dnbr_raster %>% terra::setValues(NA)
  DC_raster <-  dnbr_raster %>% terra::setValues(NA)
  ISI_raster <-  dnbr_raster %>% terra::setValues(NA)
  BUI_raster <-  dnbr_raster %>% terra::setValues(NA)
  FWI_raster <-  dnbr_raster %>% terra::setValues(NA)
  DSR_raster <-  dnbr_raster %>% terra::setValues(NA)
  
  
  fire_dates <- unique(terra::values(date_raster)) %>%
    `[`(!is.na(.))
  for(date in fire_dates){
    
    #get cells burned on a particular day
    date_rast_temp <- terra::mask(date_raster, mask = date_raster, maskvalues = date, inverse = TRUE)
    
    wind_speed_temp <- wind_speed[[as.character(date)]]
    wind_speed_mask <- terra::mask(wind_speed_temp, date_rast_temp)
    wind_speed_raster <- sum(wind_speed_raster, wind_speed_mask, na.rm = TRUE)
    
    wind_dir_temp <- wind_dir[[as.character(date)]]
    wind_dir_mask <- terra::mask(wind_dir_temp, date_rast_temp)
    wind_dir_raster <- sum(wind_dir_raster, wind_dir_mask, na.rm = TRUE)
    
    vpd_temp <- vpd[[as.character(date)]]
    vpd_mask <- terra::mask(vpd_temp, date_rast_temp)
    vpd_raster <- sum(vpd_raster, vpd_mask, na.rm = TRUE)
    
    pet_temp <- pet[[as.character(date)]]
    pet_mask <- terra::mask(pet_temp, date_rast_temp)
    pet_raster <- sum(pet_raster, pet_mask, na.rm = TRUE)
    
    # eddi_temp <- eddi[[which.min(abs(as.numeric((as.Date(names(eddi), format = "%Y%m%d") - 
    #                                            as.Date(as.character(date), format = "%Y%m%d")))))]]
    # eddi_mask <- terra::mask(eddi_temp, date_rast_temp)
    # eddi_raster <- sum(eddi_raster, eddi_mask, na.rm = TRUE)
    # 
    # pdsi_temp <- pdsi[[which.min(abs(as.numeric((as.Date(names(pdsi), format = "%Y%m%d") - 
    #                                            as.Date(as.character(date), format = "%Y%m%d")))))]]
    # pdsi_mask <- terra::mask(pdsi_temp, date_rast_temp)
    # pdsi_raster <- sum(pdsi_raster, pdsi_mask, na.rm = TRUE)
    
    # fwi_temp <- fwi2[[which.min(abs(as.numeric((as.Date(sub("X", "", names(fwi2)), format = "%Y%j") -
    #                                               as.Date(as.character(date), format = "%Y%m%d")))))]]
    # fwi_mask <- terra::mask(fwi_temp, date_rast_temp)
    # fwi_raster <- sum(fwi_raster, fwi_mask, na.rm = TRUE)
    
    FFMC_temp <- FFMC_rast[[which.min(abs(as.numeric((as.Date(names(FFMC_rast), format = "%Y-%m-%d") - 
                                                   as.Date(as.character(date), format = "%Y%m%d")))))]]
    FFMC_mask <- terra::mask(FFMC_temp, date_rast_temp)
    FFMC_raster <- sum(FFMC_raster, FFMC_mask, na.rm = TRUE)
    
    DMC_temp <- DMC_rast[[which.min(abs(as.numeric((as.Date(names(DMC_rast), format = "%Y-%m-%d") - 
                                                        as.Date(as.character(date), format = "%Y%m%d")))))]]
    DMC_mask <- terra::mask(DMC_temp, date_rast_temp)
    DMC_raster <- sum(DMC_raster, DMC_mask, na.rm = TRUE)
    
    DC_temp <- DC_rast[[which.min(abs(as.numeric((as.Date(names(DC_rast), format = "%Y-%m-%d") - 
                                                        as.Date(as.character(date), format = "%Y%m%d")))))]]
    DC_mask <- terra::mask(DC_temp, date_rast_temp)
    DC_raster <- sum(DC_raster, DC_mask, na.rm = TRUE)
    
    ISI_temp <- ISI_rast[[which.min(abs(as.numeric((as.Date(names(ISI_rast), format = "%Y-%m-%d") - 
                                                        as.Date(as.character(date), format = "%Y%m%d")))))]]
    ISI_mask <- terra::mask(ISI_temp, date_rast_temp)
    ISI_raster <- sum(ISI_raster, ISI_mask, na.rm = TRUE)
    
    BUI_temp <- BUI_rast[[which.min(abs(as.numeric((as.Date(names(BUI_rast), format = "%Y-%m-%d") - 
                                                        as.Date(as.character(date), format = "%Y%m%d")))))]]
    BUI_mask <- terra::mask(BUI_temp, date_rast_temp)
    BUI_raster <- sum(BUI_raster, BUI_mask, na.rm = TRUE)
    
    FWI_temp <- FWI_rast[[which.min(abs(as.numeric((as.Date(names(FWI_rast), format = "%Y-%m-%d") - 
                                                        as.Date(as.character(date), format = "%Y%m%d")))))]]
    FWI_mask <- terra::mask(FWI_temp, date_rast_temp)
    FWI_raster <- sum(FWI_raster, FWI_mask, na.rm = TRUE)
    
    DSR_temp <- DSR_rast[[which.min(abs(as.numeric((as.Date(names(DSR_rast), format = "%Y-%m-%d") - 
                                                        as.Date(as.character(date), format = "%Y%m%d")))))]]
    DSR_mask <- terra::mask(DSR_temp, date_rast_temp)
    DSR_raster <- sum(DSR_raster, DSR_mask, na.rm = TRUE)
    
  }
  

  dnbr_val <- terra::values(dnbr_raster)
  
  
  aspect_val <- trim_stars_to_raster(boundary, dnbr_raster, aspect_full)
  slope_val <- trim_stars_to_raster(boundary, dnbr_raster, slope_full)
  
  #find what cell is upwind of the potential fire cell
  rose_breaks <- c(0, 45, 135, 225, 315, 360)
  rose_labs <- c(
    "North", "East", "South", "West", "North"
    # 1,2,3,4,1
  )

  wind_cut <- cut(
    terra::values(wind_dir_raster),
    breaks = rose_breaks,
    labels = rose_labs,
    right = FALSE,
    include.lowest = TRUE
  )

  
  #find which cell is upwind of each cell
  # there's probably an easier way to do this? Like, since we only have one 
  # wind direction here, we can just "slide" the raster over. Simpler than
  # the way we did it for fire spread
  rowcols <- rowColFromCell(dnbr_raster, c(1:ncell(dnbr_raster)))
  
  rowcol_new <- rowcols
  
  rowcol_new[, 1] <- rowcols[, 1] + ifelse(wind_cut =="North",
                                           -1,
                                           ifelse(wind_cut =="South",
                                                  1, 0))
  rowcol_new[, 2] <- rowcols[, 2] + ifelse(wind_cut =="East",
                                           -1,
                                           ifelse(wind_cut == "West",
                                                  1, 0))
  
  upwind_cells <- cellFromRowCol(dnbr_raster, rowcol_new[, 1], rowcol_new[, 2])
  
  # This changes based on fire severity. Combustion buoyancy.
  # severity of 4 = 50; severity of 3 = 25; severity of 2 = 10; 
  # severity of 1 = 5; unburned or increased greenness = 1
  sev_values <- raster::values(sev_raster)
  U_b <- ifelse(sev_values %in% c(0, 5), 1,
                ifelse(sev_values > 3 & sev_values < 5, 50,
                       ifelse(sev_values > 2 & sev_values <= 3, 25,
                              ifelse(sev_values > 1 & sev_values <= 2, 10,
                                     0))))
  U_b_upwind <- U_b[upwind_cells]
  
  U_b_raster <- dnbr_raster
  terra::values(U_b_raster) <- U_b_upwind
  
  ### Caculating windspeed in direction of spread 
  relative_wd <- terra::values(wind_dir_raster) - aspect_val
  Ua_Ub <- terra::values(wind_speed_raster) / U_b
  ### Calculating effective wind speed. 
  eff_wind <- U_b * ((Ua_Ub^2) + 2*(Ua_Ub) * sin(slope_val * (pi/180)) * 
                       cos(relative_wd * (pi/180)) + sin(slope_val * (pi/180))^2)^0.5
  
  
  eff_wind_raster <- dnbr_raster
  terra::values(eff_wind_raster) <- eff_wind
  
  return(list(eff_wind_raster, 
              wind_speed_raster, 
              vpd_raster, 
              pet_raster, 
              # eddi_raster, 
              # pdsi_raster, 
              fwi_raster,
              FFMC_raster,
              DMC_raster,
              DC_raster,
              ISI_raster,
              BUI_raster,
              FWI_raster,
              DSR_raster
              ))

  
  }


#-------------------------------------------------------------------------------
# Get Merra2 FWI
#-------------------------------------------------------------------------------

download_daily_fwi <- function(boundary, dates_of_fire){
  boundary_wgs <- boundary %>% sf::st_transform("+proj=longlat +datum=WGS84 +no_defs") %>%
    sf::st_buffer(dist = 10000)
  
  for(i in 1:length(dates_of_fire)){
    
    firedate <- dates_of_fire[i]
    
    
    error_flag <- FALSE
    yearday <- format(as.Date(paste(firedate), format = "%Y-%m-%d"),
                      format = "%Y%j")
    fwi_date <- format(as.Date(paste(firedate), format = "%Y-%m-%d"),
                       format = "%Y%m%d")
    fwi_year <- as.numeric(substr(yearday, 1, 4))
    fwi_day <- as.numeric(substr(yearday, 5, 7))
    tail <- paste0(fwi_date, ".nc")
    file <- paste0("FWI.MERRA2.CORRECTED.Daily.Default.", tail)
    
    # print(paste("Year = ", fwi_year))
    # print(paste("Day = ", fwi_day))
    
    fwi_raster_name <- paste0("D:/Data/fwi/", fwi_year,"/", tail)
    
    # day_data <- spread_data %>% dplyr::filter(year_days == yearday)
    
    if(file.exists(fwi_raster_name)){
      # message("FWI data already downloaded. Loading from disk.")
    } else{
      
      #create a directory if needed
      if(!(paste0("D:/Data/fwi/", fwi_year) %in% 
           list.dirs("D:/Data/fwi"))){
        dir.create(paste0("D:/Data/fwi/", fwi_year))
      }
      
      #download FWI raster, ~16 mb
      tryCatch(
        {
         
          fwi_url <- paste0("https://portal.nccs.nasa.gov/datashare/GlobalFWI/v2.0/fwiCalcs.MERRA2/Default/MERRA2.CORRECTED/",fwi_year,"/",file)
          download.file(url = fwi_url, 
                        destfile = paste0("D:/Data/fwi/", fwi_year,"/", tail), 
                        method = "curl", quiet = FALSE,
                        cacheOK = TRUE)
        },
        error=function(cond) {
          
          message(paste("Error downloading climate data for ", fwi_date))
          message("Here's the original error message:")
          message(cond)
          error_flag <<- TRUE
          
        }
      )
    }
    
    fwi_raster <- terra::rast(fwi_raster_name)[["MERRA2.CORRECTED_FWI"]]
    terra::set.crs(fwi_raster, "+proj=longlat +datum=WGS84")
    fwi_raster <- fwi_raster %>%
      terra::crop(vect(st_buffer(boundary_wgs, 10000))) %>%
      terra::project(crs(vect(boundary)))
    names(fwi_raster) <- yearday #I should use the "time" band but I couldn't figure it out
    #plot(fwi_raster)
    
    if(i == 1){ fwi_stack <- fwi_raster} else fwi_stack <- c(fwi_stack, fwi_raster)
    
  }
  return(fwi_stack)
}

#-------------------------------------------------------------------------------
# Calculate FWI from daily weather
#-------------------------------------------------------------------------------
calculate_cffdrs <- function(boundary, dates_of_fire){
  
  options(warn = 0)

  gridmet_dir <- "D:/Data/gridmet/"
  gridmet_files <- list.files(gridmet_dir)
  gridmet_crs <- "epsg:4326"
  boundary_wgs <- st_transform(boundary, gridmet_crs) %>%
    sf::st_buffer(dist = 10000)
  ppt_rast <- rast(paste0(gridmet_dir, "pr_", boundary$fireyear, ".nc")) %>%
    terra::crop(vect(boundary_wgs)) 
  rhmin_rast <- rast(paste0(gridmet_dir, "rmin_", boundary$fireyear, ".nc"))  %>%
    terra::crop(vect(boundary_wgs)) 
  rhmax_rast <- rast(paste0(gridmet_dir, "rmax_", boundary$fireyear, ".nc")) %>%
    terra::crop(vect(boundary_wgs))
  rh_rast <- (rhmin_rast + rhmax_rast)/2
  tmin_rast <- rast(paste0(gridmet_dir, "tmmn_", boundary$fireyear, ".nc")) %>%
    terra::crop(vect(boundary_wgs)) 
  tmax_rast <- rast(paste0(gridmet_dir, "tmmx_", boundary$fireyear, ".nc")) %>%
    terra::crop(vect(boundary_wgs)) 
  t_rast <- (tmin_rast + tmax_rast)/2
  vs_rast <- rast(paste0(gridmet_dir, "vs_", boundary$fireyear, ".nc")) %>%
    terra::crop(vect(boundary_wgs)) 
  
  last_date <- dates_of_fire %>% 
    as.Date() %>% 
    `[`(order(.)) %>% 
    `[`(length(.)) + 1 
  
  fire_dates_expanded <- as.Date(c((min(dates_of_fire) - 1) : (max(dates_of_fire) + 1)), origin = "1970-01-01")
  
  fwi_dates_all <- names(ppt_rast) %>%
    gsub("\\D", "", .) %>%
    as.integer() %>%
    as.Date(origin = "1900-01-01")
  fwi_dates <- fwi_dates_all %>%
    `[`(which(. <= last_date))
  
  #make a template to use to write the cffdrs variables
  template <- ppt_rast
  names(template) <- fwi_dates_all
  template <- template[[as.Date(names(template)) %in% dates_of_fire]]
  ncells <- nrow(template[])
  
  fwi_data <- data.frame(id = rep(1:ncells, each = length(fwi_dates)),
                         long = mean(ext(template)[c(1,2)]),
                         lat = mean(ext(template)[c(3,4)]),
                         yr = year(fwi_dates),
                         mon = month(fwi_dates),
                         day = day(fwi_dates),
                         temp = (t_rast[[fwi_dates_all %in% fwi_dates]][] %>% as.data.frame() %>% pivot_longer(cols = everything()))$value - 270.15,
                         rh = (rh_rast[[fwi_dates_all %in% fwi_dates]][] %>% as.data.frame() %>% pivot_longer(cols = everything()))$value,
                         ws = (vs_rast[[fwi_dates_all %in% fwi_dates]][] %>% as.data.frame() %>% pivot_longer(cols = everything()))$value,
                         prec = (ppt_rast[[fwi_dates_all %in% fwi_dates]][] %>% as.data.frame() %>% pivot_longer(cols = everything()))$value)
  
  fwi_out <- cffdrs::fwi(input = fwi_data,
                      batch = TRUE)
  fwi_out <- mutate(fwi_out,
                    date = as.Date(paste(YR, MON, DAY, sep = "-"), format = "%Y-%m-%d"))
  
  FFMC_rast <- template
  DMC_rast <- template
  DC_rast <- template
  ISI_rast <- template
  BUI_rast <- template
  FWI_rast <- template
  DSR_rast <- template
  
  
  for(i in 1:length(dates_of_fire)){
    rows_include <- which(fwi_out$date == dates_of_fire[i])
    FFMC_rast[[i]][] <- fwi_out[rows_include, "FFMC"]
    DMC_rast[[i]][] <- fwi_out[rows_include, "DMC"]
    DC_rast[[i]][] <- fwi_out[rows_include, "DC"]
    ISI_rast[[i]][] <- fwi_out[rows_include, "ISI"]
    BUI_rast[[i]][] <- fwi_out[rows_include, "BUI"]
    FWI_rast[[i]][] <- fwi_out[rows_include, "FWI"]
    DSR_rast[[i]][] <- fwi_out[rows_include, "DSR"]
  }
  
  cffdrs_rast <- list(FFMC_rast,
                      DMC_rast,
                      DC_rast,
                      ISI_rast,
                      BUI_rast,
                      FWI_rast,
                      DSR_rast)
  
  return(cffdrs_rast)
}

#-------------------------------------------------------------------------------
# Import MODIS NDVI data
#-------------------------------------------------------------------------------

modis_list <- list.files("D:/Data/modis/downloaded_appEARS", full.names = TRUE)
modis_rasters <- data.frame(file = modis_list[grep("NDVI", modis_list)])

modis_rasters$date <- str_match(modis_rasters$file, "(?:_doy)(\\d+)")[, 2] %>%
  as.Date("%Y%j")

get_ndvi_before <- function(boundary, dnbr_raster){
  
  first_date <- boundary$Ig_Date
  
  modis_rasters$distance_ig <- (first_date - modis_rasters$date) 
  modis_file <- modis_rasters[which.min(modis_rasters[modis_rasters$distance_ig>0, "distance_ig"]), "file"]
  
  modis_before <- stars::read_stars(modis_file) * 0.0001
  
  boundary_reproj <- boundary %>%
    sf::st_transform(crs = st_crs(modis_before))
  
  modis_before <- st_crop(modis_before, boundary_reproj) %>%
    stars::st_warp(dest = stars::st_as_stars(dnbr_raster), method = "near")
  
  return(modis_before)
}


get_ndvi_anomaly <- function(boundary, dnbr_raster, modis_before){
  
  first_date <- boundary$Ig_Date
  hist_date <- lubridate::ymd(first_date) - lubridate::years(10)
  
  modis_rasters$distance_ig <- (first_date - modis_rasters$date)
  modis_rasters$year <- lubridate::year(lubridate::ymd(modis_rasters$date))
  modis_rasters$jday <- lubridate::yday(lubridate::ymd(modis_rasters$date))
  
  jday_range <- c((yday(first_date) - 30) : (yday(first_date) + 30))
  year_range <- c((year(first_date) - 11) : (year(first_date) - 1))
  
  rasters_hist <- modis_rasters[modis_rasters$jday %in% jday_range & modis_rasters$year %in% year_range, ]
  
  modis_stack <- stars::read_stars(rasters_hist$file) * 0.0001
  
  boundary_reproj <- boundary %>%
    sf::st_transform(crs = st_crs(modis_stack))
  
  modis_stack <- st_crop(modis_stack, boundary_reproj) %>%
    stars::st_warp(dest = stars::st_as_stars(dnbr_raster), method = "near")
  
  historical_median <- st_apply(modis_stack, 1:2, median, na.rm = TRUE, FUTURE = TRUE)
  
  anomaly <- historical_median - modis_before
  
  return(anomaly)
}


#*******************************************************************************
# Download and process data for all fires
#*******************************************************************************

# This took 21 hours to do for the whole Sierra (for 701 fires). 
# Your mileage may vary.

#How many rows to collect before writing data to disk
data_length <- 1000000

#TODO add cell locations to data catcher!

create_data_catcher <- function(data_length){
  return(data.frame(fire_code = character(data_length),
                    fire_name = character(data_length),
                    mtbs_filename = character(data_length),
                    year = character(data_length),
                    x = numeric(data_length),
                    y = numeric(data_length),
                    dnbr = numeric(data_length),
                    rdnbr = numeric(data_length),
                    mtbs_severity = numeric(data_length),
                    departure = numeric(data_length),
                    clay = numeric(data_length),
                    pet = numeric(data_length),
                    cwd = numeric(data_length),
                    normal_cwd = numeric(data_length),
                    ews = numeric(data_length),
                    windspeed = numeric(data_length),
                    vpd = numeric(data_length),
                    # eddi = numeric(data_length),
                    # pdsi = numeric(data_length),
                    # fwi = numeric(data_length),
                    fine_fuel = numeric(data_length),
                    ladder_fuel = numeric(data_length),
                    tm_ladder_fuel = numeric(data_length),
                    FFMC = numeric(data_length),
                    DMC = numeric(data_length),
                    DC = numeric(data_length),
                    ISI = numeric(data_length),
                    BUI = numeric(data_length),
                    FWI = numeric(data_length),
                    DSR = numeric(data_length),
                    ndvi = numeric(data_length),
                    ndvi_anomaly = numeric(data_length)))
}

data_catcher <- create_data_catcher(data_length)
#row to start on when adding data
row_tracker <- 1

start_time <- Sys.time()

#fires with fuels and daily progressions (year 2001) start at 303
for(i in 1:length(mtbs_shape)){
  
  error_flag <- FALSE
  try_again <- FALSE
  use_daily_perims <- TRUE

  message(paste("Beginning processing fire", i))
  print(paste("Fire code", mtbs_shape[i]))
  
  boundary <- sf::st_read(mtbs_shape[i])%>%
    sf::st_make_valid() %>%
    sf::st_transform("EPSG:5070") %>%
    sf::st_cast(to = "MULTIPOLYGON") %>%
    dplyr::mutate(fireyear = substr(Ig_Date, 1, 4)) %>%
    dplyr::summarise(fireyear = fireyear[1],
                     Event_ID = Event_ID[1],
                     Ig_Date = Ig_Date[1],
                     Incid_Name = Incid_Name[1])

  

  if(boundary$fireyear[1] <= 2000 | is.na(boundary$fireyear[1])) next()
  
  #check that fire is inside boundary
  if(length(as.vector(sf::st_area(sf::st_intersection(boundary, sierra_shape)))) == 0){
    message("Fire outside of sierra border; moving to next fire")
    next()
  }else if(sum(sf::st_area(sf::st_intersection(boundary, sierra_shape))) < sum(sf::st_area(boundary))){
    message("Fire boundary partially outside Sierra border; setting boundary to intersection")
    boundary$geometry <- tryCatch(
      {
         suppressWarnings(sf::st_intersection(boundary, sierra_shape) %>% st_union()) #in case it lies on a border between sierra subregions
      },
      error = function(cond) {
        message("Error in geometry of boundary")
        error_flag <<- TRUE
      }
    )
  }
    
    
  
  dnbr_raster <-  tryCatch(
    {
    test <- terra::rast(mtbs_dnbr[i]) %>%
        terra::project(y = "EPSG:5070") %>%
        terra::crop(boundary) %>%
        terra::mask(boundary) 
    
    },
    error=function(cond) {
      
      message(paste("Error loading raster", mtbs_dnbr[i]))
      message("Here's the original error message:")
      message(cond)
      error_flag <<- TRUE
      
    }
  )
  
  if(error_flag){
    message("Moving to next fire record")
    next()
  } 
  

  # if(ncell(dnbr_raster) > data_length){
  #   message("Fire too large")
  #   message(ncell(dnbr_raster))
  #   next()
  # }

  terra::values(dnbr_raster) <- ifelse(terra::values(dnbr_raster) < -2000 | terra::values(dnbr_raster) > 2000, 
                                NA, 
                                terra::values(dnbr_raster))
  
  plot(dnbr_raster, main = paste0(boundary$Incid_Name, " fire, ignited ", boundary$Ig_Date))
  
  rdnbr_raster <-  tryCatch(
    {
      test2 <- terra::rast(mtbs_rdnbr[i]) %>%
        terra::project(y = "EPSG:5070") %>%
        terra::crop(boundary) %>%
        terra::mask(boundary) 
      
    },
    error=function(cond) {
      
      message(paste("Error loading raster", mtbs_rdnbr[i]))
      message("Here's the original error message:")
      message(cond)
      error_flag <<- TRUE
      
    }
  )
  
  terra::values(rdnbr_raster) <- ifelse(terra::values(rdnbr_raster) < -2000 | terra::values(rdnbr_raster) > 2000, 
                                       NA, 
                                       terra::values(rdnbr_raster))

  
  #severity goes 0 = masked out; 1 = low intensity or unburned; 2 = low; 3 = moderate severity; 4 = high
  #; 5 = increased greenness, 6 = non-processing mask
  sev_raster <- tryCatch(
    { terra::rast(mtbs_sev[i]) %>%
        terra::project(y = "EPSG:5070") %>%
        terra::crop(boundary) %>%
        terra::mask(boundary)
        
    },
    error=function(cond) {
      
      message(paste("Error loading raster", mtbs_dnbr[i]))
      message("Here's the original error message:")
      message(cond)
      error_flag <<- TRUE
      
    }
  )
  
  
  if(error_flag){
    message("Moving to next fire record")
    next()
  } 

 # plot(sev_raster)
  
  cells_burned <- !is.na(dnbr_raster[]) & 
    !is.na(sev_raster[]) &
    terra::values(sev_raster) > 1 & 
    terra::values(sev_raster) != 5 & 
    terra::values(sev_raster) != 6
  
  #if there's too much data loaded, write it to disk and make a new data catcher
  if(I(row_tracker + length(cells_burned)) > data_length){
    #TODO add a check to see if the data_catcher is empty, in which case don't write it
    message("Writing data to disk")
    write.csv(data_catcher, paste0("./Parameterization/calibration data/fire severity/data_catcher ", 
                                   format(Sys.time(), format = "%y-%m-%d %H-%M-%S"), ".csv"))
    data_catcher <- create_data_catcher(data_length)
    #row to start on when adding data
    row_tracker <- 1
  }
  
  
  #mean dNBR of cells that burned -- equivalent to SCRPPLE output
  mean_dnbr <- mean(terra::values(dnbr_raster)[cells_burned])
  
  fire_date <- boundary$Ig_Date
  
  label = raster_firenumbers[i]
  
  #get dates for each cell -- used for getting daily climate data
  date_raster <-  tryCatch(
    {
      get_raster_cell_date(dnbr_raster, boundary)
    },
    error = function(cond){
      message(paste("Error getting dates for fire:", label))
      message(cond)
      message("\nProceeding without daily climate data")
      use_daily_perims <<- FALSE

      # error_flag <<- TRUE
    }
  )
  
  if(error_flag){
    message("Moving to next fire record")
    next()
  } 
  

  if(use_daily_perims){                   
    dates_of_fire <-as.character(unique(terra::values(date_raster))) %>%
                            as.Date(., format = "%Y%m%d") %>%
                            `[`(!is.na(.))
    
    
    # fwi <- tryCatch(
    #   {
    #     download_daily_fwi(boundary, dates_of_fire) #FWI from MERRA-2 satellite
    #   },
    #   error = function(cond){
    #     message(paste("Error getting MERRA-2 FWI data:", label))
    #     message(cond)
    #   return(NA)
    #   }
    # )
    fwi <- NA
    
    cffdrs <- calculate_cffdrs(boundary, dates_of_fire) #FFMC, DMC, DC, ISI, BUI, FWI, DSR
    
    dailyclim <- NA
    dailyclim <- tryCatch(
      {
        get_daily_climate_rasters(boundary, dates_of_fire) #daily
      },
      error=function(cond) {
        message(paste("Error downloading daily climatedata for:", label))
        message("Here's the original error message:")
        message(cond)
        
        return(NA)
      }
    )
    
    if(!anyNA(dailyclim)){
      message("Processing effective windspeed and weather mosaic")
      
      clim_mosaic <- tryCatch(
        {
          get_daily_climate_mosaic(dnbr_raster, date_raster, sev_raster, clim = dailyclim, fwi = fwi, cffdrs)
        },
        error=function(cond) {
          message(paste("Error processing daily climate data combined mosaic for:", label))
          message("Here's the original error message:")
          message(cond)
          
          return(NA)
        }
      )
    }
  }else{
      message("No daily climate data; setting climate  mosaic to NA")
      clim_mosaic <- rep(dnbr_raster,12) %>% terra::setValues(NA) 
      }
  
  
  #download soil data
  clay_map <- tryCatch(
      {
        download_clay(dnbr_raster, label)
      },
      error=function(cond) {
        message(paste("Error downloading SSURGO data for:", label))
        message("Here's the original error message:")
        message(cond)
        message("Trying again")

        try_again <<- TRUE
      }
    )

    if(try_again){
      try_again <<- FALSE
      Sys.sleep(5)
      clay_map <- tryCatch(
        {
          download_clay(dnbr_raster, label)
        },
        error=function(cond) {
          message(paste("Error downloading SSURGO data for:", label))
          message("Here's the original error message:")
          message(cond)
          message("Aborting clay map, returing NA")

          na_raster <- dnbr_raster %>% setValues(NA)

          return(na_raster)
        }
      )
    }
  
  clim_annual <- NA
  clim_annual <- tryCatch(
    {
      # download_pet_cwd(boundary) #this doesn't work anymore
      message("Getting AET/PET/CWD annual values")
      
      aet_rast <- terra::rast(paste0("D:/Data/terraclimate/aet_", as.numeric(boundary$fireyear)-1, ".nc"))
      aet_annual <- sum(aet_rast)
      pet_rast <- terra::rast(paste0("D:/Data/terraclimate/pet_", as.numeric(boundary$fireyear)-1, ".nc"))
      pet_annual <- sum(pet_rast)
      cwd_rast <- pet_annual - aet_annual
      
      clim_stack <- c(cwd_rast,  #previous-year CWD
                      pet_annual) #previous-year PET
      
      temp_boundary <- boundary %>% 
        st_buffer(10000) %>%
        sf::st_transform(crs(aet_rast))
      clim_annual <- terra::crop(clim_stack, vect(temp_boundary)) %>%
        terra::project(dnbr_raster) %>%
        crop(dnbr_raster) %>% mask(dnbr_raster)
    },
    error=function(cond) {
      message(paste("Error processing aet/pet/cwd data for:", label))
      message("Here's the original error message:")
      message(cond)
      
      return(NA)
    }
  )
  
  clim_normal<- NA
  clim_normal <- tryCatch(
    {
      
      cwd_rast <- sum(terra::rast("D:/Data/terraclimate/TerraClimate19912020_def.nc"))
      
      temp_boundary <- boundary %>% 
        st_buffer(10000) %>%
        sf::st_transform(crs(cwd_rast))
      clim_normal<- terra::crop(cwd_rast, vect(temp_boundary)) %>%
        terra::project(dnbr_raster) %>%
        crop(dnbr_raster) %>% mask(dnbr_raster)
      
    },
    error=function(cond) {
      message(paste("Error downloading normal CWD data for:", label))
      message("Here's the original error message:")
      message(cond)
      
      return(NA)
    }
  )
  
  departure <- trim_stars_to_raster(boundary, dnbr_raster, dep)
  
  fuel <- tryCatch(
    {
      message("Extracting LANDFIRE fuels")
      if(as.numeric(format(fire_date, "%Y")) > 2001){
        get_landfire_fuel(boundary, dnbr_raster)
      } else(rep(dnbr_raster %>% setValues(NA), 2))
      
    },
    error=function(cond) {
      message(paste("Error extracting fuels for:", label))
      message("Here's the original error message:")
      message(cond)
      
      return(rep(dnbr_raster %>% setValues(NA), 2))
    }
  ) 
  
  tm_fuel <-  tryCatch(
    {
      message("Extracting TreeMap ladder fuels")
    
      if(as.numeric(format(fire_date, "%Y")) > 2014){
      tm_fuel <- tm_ladder_fuel[boundary]
      tm_fuel <- as(tm_fuel, "SpatRaster") %>%
        terra::project(dnbr_raster)%>%
        terra::values()
  } else(NA)
  
  },
  error=function(cond) {
    message(paste("Error extracting fuels for:", label))
    message("Here's the original error message:")
    message(cond)
    
    return(NA)
  }
) 
  
  ndvi <- NA
  # ndvi <- tryCatch(
  #   {
  #     if(as.numeric(format(fire_date, "%Y")) > 2002){
  #       get_ndvi_before(boundary, dnbr_raster)
  #     } else(NA)
  #   },
  #   error=function(cond) {
  #     message(paste("Error extracting ndvi for:", label))
  #     message("Here's the original error message:")
  #     message(cond)
  #     
  #     return(NA)
  #   }
  # ) 
  
  #skip for now
  
  # 
  # ndvi_anomaly <- tryCatch(
  #   {
  #     if(as.numeric(format(fire_date, "%Y")) > 2002){
  #       get_ndvi_anomaly(boundary, dnbr_raster, ndvi)
  #     } else(NA)
  #   },
  #   error=function(cond) {
  #     message(paste("Error extracting ndvi anomaly for:", label))
  #     message("Here's the original error message:")
  #     message(cond)
  #     
  #     return(NA)
  #   }
  # ) 
  
  ncells <- sum(cells_burned)
  
  data <- data.frame(fire_code = rep(label, sum(cells_burned)),
                     fire_name = rep(boundary$Incid_Name[1], sum(cells_burned)),
                     mtbs_filename = rep(mtbs_shape[i], sum(cells_burned)),
                     year = rep(boundary$fireyear[1], sum(cells_burned)),
                     x = xFromCell(dnbr_raster, which(cells_burned)),
                     y = yFromCell(dnbr_raster, which(cells_burned)),
                     dnbr = terra::values(dnbr_raster)[cells_burned],
                     rdnbr = terra::values(rdnbr_raster)[cells_burned],
                     mtbs_sev = terra::values(sev_raster)[cells_burned],
                     departure = departure[cells_burned],
                     clay = terra::values(clay_map)[cells_burned],
                     pet = terra::values(clim_annual[[2]])[cells_burned],
                     cwd = terra::values(clim_annual[[1]])[cells_burned],
                     normal_cwd = terra::values(clim_normal)[cells_burned],
                     ews = terra::values(clim_mosaic[[1]])[cells_burned],
                     windspeed = terra::values(clim_mosaic[[2]])[cells_burned],
                     vpd = terra::values(clim_mosaic[[3]])[cells_burned],
                     # eddi = terra::values(clim_mosaic[[5]])[cells_burned],
                     # pdsi = terra::values(clim_mosaic[[6]])[cells_burned],
                     # fwi = terra::values(clim_mosaic[[5]])[cells_burned],
                     fine_fuel = fuel[[1]][cells_burned],
                     ladder_fuel = fuel[[2]][cells_burned],
                     tm_ladder_fuel = tm_fuel[cells_burned],
                     FFMC = clim_mosaic[[6]][][cells_burned],
                     DMC = clim_mosaic[[7]][][cells_burned], 
                     DC = clim_mosaic[[8]][][cells_burned],  
                     ISI = clim_mosaic[[9]][][cells_burned], 
                     BUI = clim_mosaic[[10]][][cells_burned], 
                     FWI = clim_mosaic[[11]][][cells_burned],  
                     DSR = clim_mosaic[[12]][][cells_burned], 
                     ndvi = ifelse(is.null(nrow(ndvi)), NA, ndvi[[1]][cells_burned]),
                     ndvi_anomaly = NA #ifelse(is.null(nrow(ndvi_anomaly)), NA, 
                                         #  ndvi_anomaly[[1]][cells_burned])
                     )
  
  
  data_catcher[c(row_tracker:(row_tracker+nrow(data)-1)), ] <- data
  row_tracker <- row_tracker + nrow(data)
   
  message(paste("Finished processing fire", i))
  
}

#write what's leftover after the loop
write.csv(data_catcher, paste0("./Parameterization/calibration data/fire severity/data_catcher ", 
                               format(Sys.time(), format = "%y-%m-%d %H-%M-%S"), ".csv"))

end_time <- Sys.time()
end_time - start_time


