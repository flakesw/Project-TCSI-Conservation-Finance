#download data needed to predict site-level severity from climate and fuels

#TODO change to previous-year PET and CWD
#TODO change to RDBNR

library("geoknife")
library("sf")
library("raster")
library("tidyverse")
library("FedData")
library("stars")
# using stars is so much faster, since the whole files aren't loaded into RAM, just the subsets
# it's really an amazing upgrade from raster

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance")

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

# short_all <- sf::st_read("./Parameterization/calibration data/short/Data/FPA_FOD_20210617.gdb")%>%
#   sf::st_transform("EPSG:5070")
short_all <- read.csv("./Parameterization/calibration data/short/short_drop_geometry.csv")
short <- readRDS("./Parameterization/calibration data/short_tcsi/short_ca.RDS") %>%
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
mtbs_dnbr <- list.files(path = mtbs_folder, pattern = "*_rdnbr.tif", 
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


#*******************************************************************************
# DEFINE FUNCTIONS TO ACCESS DATA
#*******************************************************************************

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
    }else if(fire_year == 2021){
      fine_fuel <- landfire_2020_fine[boundary]
      ladder_fuel <- landfire_2020_ladder[boundary]
    }
    
    fine_fuel <- as(fine_fuel, "Raster") %>%
      projectRaster(dnbr_raster) %>%
      raster::values()
    ladder_fuel <- as(ladder_fuel, "Raster") %>%
      projectRaster(dnbr_raster) %>%
      raster::values()
    
    return(data.frame(fine_fuel = fine_fuel,
                      ladder_fuel = ladder_fuel))  
    
  }else return(NA)

  
}

#-------------------------------------------------------------------------------
# Trim aspect and slope
#-------------------------------------------------------------------------------
get_slope_or_aspect <- function(boundary, dnbr_raster, raster){
  boundary2 <- st_transform(boundary, crs = st_crs(raster))
  raster2_vals <- raster[boundary2] %>%
    as(., "Raster") %>%
    projectRaster(dnbr_raster) %>%
    raster::values()
  
  
  return(raster2_vals)
}

#-------------------------------------------------------------------------------
# get %clay from SSURGO
#-------------------------------------------------------------------------------
download_clay <- function(dnbr_raster, label){
  
  # This is pretty slow when first being run, because it downloads all the SSURGO data
  # for a grid. But it speeds up as you iterate through fires, because some fires
  # will share soils data. Not super efficient but not as bad as I thought.
  ssurgo_test <- get_ssurgo(template = dnbr_raster, label = label, raw.dir = "D:/Data/sierra_ssurgo/RAW",
                            extraction.dir = paste0("D:/Data/sierra_ssurgo/EXTRACTIONS/", label, "/SSURGO"),
                            force.redo = FALSE)
  
  ssurgo_test$tabular$chorizon$cokey
  
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
    dplyr::mutate(across(everything(), ~replace_na(.x, 0.001))) %>%
    dplyr::select(cokey, claytotal.r, comppct.r, mukey) %>%
    dplyr::mutate(MUKEY = as.character(mukey))
  
  #aggregate to mapunits by component
  mapunits_data <- component %>%
    dplyr::group_by(MUKEY) %>%
    dplyr::summarise(across(c(claytotal.r), 
                            ~stats::weighted.mean(., w = comppct.r, na.rm = TRUE))) 
  
  mapunits_spatial <- left_join(ssurgo_test$spatial, mapunits_data, by = c("MUKEY")) %>%
    sf::st_transform(crs(dnbr_raster))
  
  clay_map <- raster::rasterize(mapunits_spatial, dnbr_raster, field = "claytotal.r", fun="mean")
  
  return(clay_map)
}  

#-------------------------------------------------------------------------------
# Download ET and CWD data
# Monthly data from TerraClimate
# Abatzoglou, J.T., S.Z. Dobrowski, S.A. Parks, K.C. Hegewisch, 2018, Terraclimate, 
# a high-resolution global dataset of monthly climate and climatic water balance from 1958-2015, Scientific Data

download_pet_cwd <- function(boundary){  
  #shapefile for fire
  fire_boundary <- boundary %>%
    sf::st_transform(crs = "+proj=longlat +datum=WGS84") #reproject to CRS that geoknife needs
  
  #"stencil" is what geoknife uses for the extent of the data
  stencil <- simplegeom(as(fire_boundary, Class = "Spatial"))
  
  #download PET and AET to calculate CWD
  vars_url <- c("pet", "aet")
  year <- substr(fire_boundary$Ig_Date, 1, 4)
  
  urls <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/TERRACLIMATE_ALL/data/TerraClimate_", 
                 vars_url, "_", year, ".nc")
  
  fabric <- webdata(url = urls[1])
  
  query(fabric, 'variables')
  
  knife <- webprocess(wait = TRUE)
  
  # area grid statistics are the default, but we can change it if we  (we don't)
  algorithm(knife) <- list('Area Grid Statistics (weighted)' = 
                             "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")
  
  knife@processInputs$STATISTICS <- c("MEAN") #what statistics do we want?
  
  job_results <- list()
  
  for(i in 1:length(vars_url)){
    #set the fabric for a new variable, but keep everything else the same (i.e. the stencil and knife)
    fabric <- webdata(url = urls[i])
    variables(fabric) <- vars_url[i]
    print(vars_url[i])
    job <- geoknife(stencil, fabric, knife)
    
    
    if(error(job)){
      break
      check(job)
    }
    
    job_results[[i]] <- result(job)
  }
  
  results <- result(job)
  
  month <- as.Date(fire_date, format = "%Y-%m-%d") %>%
    format("%m")
  
  pet <- job_results[[1]][as.numeric(month), 2]
  aet <- job_results[[2]][as.numeric(month), 2]
  cwd <- pet - aet
  
  return(c(pet, aet, cwd))
}


#-------------------------------------------------------------------------------
#effective windspeed
# MACA downscaled data

download_windspeed <- function(boundary){  
  
  #shapefile for fire
  fire_boundary <- boundary %>%
    sf::st_transform(crs = "+proj=longlat +datum=WGS84") #reproject to CRS that geoknife needs
  
  fire_date <- fire_boundary$Ig_Date
  
  #"stencil" is what geoknife uses for the extent of the data
  stencil <- simplegeom(as(fire_boundary, Class = "Spatial"))
  
  #download PET and AET to calculate CWD
  vars_url <- c("pet", "aet")
  year <- substr(fire_date, 1, 4)
  first_day <- fire_date
  last_day <- ifelse(boundary$Event_ID %in% short_all$MTBS_ID, 
                     as.Date(subset(short_all, MTBS_ID == boundary$Event_ID)$CONT_DATE), first_day + 14)
  
  # windspeed and wind direction
  vars_url <- c("vs", "th")
  urls <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_", vars_url, "_1979_CurrentYear_CONUS.nc")
  fabric <- webdata(url = urls[1], times = c(as.POSIXct.Date(first_day), as.POSIXct.Date(last_day)))
  query(fabric, 'variables')
  vars_long <- c("daily_mean_wind_speed", 
                 "daily_mean_wind_direction")
  
  knife <- webprocess(wait = TRUE)
  
  # area grid statistics are the default, but we can change it if we  (we don't)
  algorithm(knife) <- list('Area Grid Statistics (weighted)' = 
                             "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")
  
  #I think this is the best way to set it? The geoknife documentation isn't clear on
  # if there's a better way; might be a feature in development
  knife@processInputs$STATISTICS <- "MEAN" #what statistics do we want?
  
  job_results <- list()
  
  for(i in 1:length(vars_long)){
    #set the fabric for a new variable, but keep everything else the same (i.e. the stencil and knife)
    fabric <- webdata(url = urls[i], times = c(as.POSIXct.Date(first_day), as.POSIXct.Date(last_day)))
    variables(fabric) <- vars_long[i]
    print(vars_long[i])
    job <- geoknife(stencil, fabric, knife)
    if(error(job)){
      break
      check(job)
    }
    
    job_results[[i]] <- result(job)
  }
  
  #TODO maybe replace with max, or 90% percentile or something
  ws <- mean(job_results[[1]][, 2])
  ws_max <- max(job_results[[1]][, 2])
  wd <- mean(job_results[[2]][, 2])
  
  
  return(c(ws, ws_max, wd))
}



#-------------------------------------------------------------------------------
# get relative wind direction

get_effective_windspeed <- function(dnbr_raster, severity_raster, wind_speed, wind_direction){ 
  
  dnbr_raster <- dnbr_raster
  severity_raster <- sev_raster
  dnbr_val <- raster::values(dnbr_raster)
  
  aspect_val <- get_slope_or_aspect(boundary, dnbr_raster, aspect_full)
  slope_val <- get_slope_or_aspect(boundary, dnbr_raster, slope_full)
  
  #find what cell is upwind of the potential fire cell
  rose_breaks <- c(0, 45, 135, 225, 315, 360)
  rose_labs <- c(
    "North", "East", "South", "West", "North"
  )
  
  wind_cut <- cut(
    wind_direction,
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
  #severity of 4 = 50; severity of 3 = 25; severity of 2 = 10; severity of 1 = 5; unburned or increased greenness = 1
  sev_values <- raster::values(sev_raster)
  U_b <- ifelse(sev_values %in% c(0, 5), 1,
                ifelse(sev_values > 3 & sev_values < 5, 50,
                       ifelse(sev_values > 2 & sev_values <= 3, 25,
                              ifelse(sev_values > 1 & sev_values <= 2, 10,
                                     0))))
  U_b_upwind <- U_b[upwind_cells]
  
  U_b_raster <- dnbr_raster
  raster::values(U_b_raster) <- U_b_upwind
  
  ### Caculating windspeed in direction of spread 
  relative_wd <- wind_direction - aspect_val
  Ua_Ub <- wind_speed / U_b
  ### Calculating effective wind speed. 
  eff_wind <- U_b * ((Ua_Ub^2) + 2*(Ua_Ub) * sin(slope_val * (pi/180)) * 
                       cos(relative_wd * (pi/180)) + sin(slope_val * (pi/180))^2)^0.5
}


#*******************************************************************************
# Download and process data for all fires
#*******************************************************************************

# This took 21 hours to do for the whole Sierra (for 701 fires). Your mileage may vary.

#How many rows to collect before writing data to disk
data_length <- 1000000

create_data_catcher <- function(data_length){
  return(data.frame(fire_name = character(data_length),
                    dnbr = numeric(data_length),
                    clay = numeric(data_length),
                    pet = numeric(data_length), 
                    cwd = numeric(data_length),
                    ews = numeric(data_length),
                    fine_fuel = numeric(data_length),
                    ladder_fuel = numeric(data_length)))
}

data_catcher <- create_data_catcher(data_length)
#row to start on when adding data
row_tracker <- 1

start_time <- Sys.time()

#restart with i = 332

for(i in 1:length(mtbs_shape)){
  
  error_flag <- FALSE
  try_again <- FALSE

  message(paste("Beginning processing fire", i))
  print(paste("Fire code", mtbs_shape[i]))
  
  boundary <- sf::st_read(mtbs_shape[i])%>%
    sf::st_make_valid() %>%
    sf::st_transform("EPSG:5070")
  
  
  #TODO remove; just for fuels
  # if(as.numeric(format(boundary$Ig_Date, "%Y")) <= 2001){
  #   next()
  # }

  #check that fire is inside boundary
  if(length(as.vector(sf::st_area(sf::st_intersection(boundary, sierra_shape)))) == 0){
    message("Fire outside of sierra border; moving to next fire")
    next()
  }else if(sum(sf::st_area(sf::st_intersection(boundary, sierra_shape))) < sum(sf::st_area(boundary))){
    message("Fire boundary partially outside Sierra border; setting boundary to intersection")
    boundary <- tryCatch(
      {
        sf::st_intersection(boundary, sierra_shape)
      },
      error = function(cond) {
        message("Error in geometry of boundary")
        error_flag <<- TRUE
      }
    )
  }
    
    
  
  dnbr_raster <-  tryCatch(
    { raster(mtbs_dnbr[i]) %>%
    crop(boundary) %>%
    mask(boundary) %>% 
    raster::projectRaster(crs = "EPSG:5070")
    
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
  
  if(ncell(dnbr_raster) > data_length){
    message("Fire too large")
    message(ncell(dnbr_raster))
    next()
  }
  
  #plot(dnbr_raster)
  
  #severity goes 0 = masked out; 1 = low intensity or unburned; 2 = low; 3 = moderate severity; 4 = high
  #; 5 = increased greenness, 6 = non-processing mask
  #what are decimal layers for?
  #make sure errors aren't introduced by the projection process!
  sev_raster <- tryCatch(
    { raster(mtbs_sev[i]) %>%
        crop(boundary) %>%
        mask(boundary) %>% 
        raster::projectRaster(crs = "EPSG:5070")
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
  

  
  #if there's too much data loaded, write it to disk and make a new data catcher
  if(I(row_tracker + ncell(dnbr_raster)) > data_length){
    message("Writing data to disk")
    write.csv(data_catcher, paste0("./Parameterization/calibration data/fire severity/data_catcher ", 
                                   format(Sys.time(), format = "%y-%m-%d %H-%M-%S"), ".csv"))
    data_catcher <- create_data_catcher(data_length)
    #row to start on when adding data
    row_tracker <- 1
  }
  
  cells_burned <- !is.na(raster::values(dnbr_raster)) & 
    raster::values(sev_raster) > 1 & 
    raster::values(sev_raster) != 5 & 
    raster::values(sev_raster) != 6
  
  #mean dNBR of cells that burned -- equivalent to SCRPPLE output
  mean_dnbr <- mean(raster::values(dnbr_raster)[cells_burned])
  
  fire_date <- boundary$Ig_Date
  
  label = raster_firenumbers[i]
  

  #get data
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


  clim <- tryCatch(
    {
      download_pet_cwd(boundary)
    },
    error=function(cond) {
      message(paste("Error downloading climate data data for:", label))
      message("Here's the original error message:")
      message(cond)


      return(NA)
    }
  )


  wind <- tryCatch(
    {
      download_windspeed(boundary)
    },
    error=function(cond) {
      message(paste("Error downloading wind data for:", label))
      message("Here's the original error message:")
      message(cond)

      return(NA)
    }
  )

  eff_ws <- tryCatch(
    {
      get_effective_windspeed(dnbr_raster, sev_raster, wind[1], wind[3])
    },
    error=function(cond) {
      message(paste("Error downloading SSURGO data for:", label))
      message("Here's the original error message:")
      message(cond)

      return(NA)
    }
  )
  
  fuel <- tryCatch(
    {
      if(as.numeric(format(fire_date, "%Y")) > 2001){
        get_landfire_fuel(boundary, dnbr_raster)
      } else(NA)
    },
    error=function(cond) {
      message(paste("Error extracting fuels for:", label))
      message("Here's the original error message:")
      message(cond)
      
      return(NA)
    }
  ) 
  
  ncells <- sum(cells_burned)
  
  data <- data.frame(fire_name = rep(label, sum(cells_burned)),
                     dnbr = raster::values(dnbr_raster)[cells_burned],
                     clay = raster::values(clay_map)[cells_burned],
                     pet = rep(clim[1], ncells)[cells_burned],
                     cwd = rep(clim[3], ncells)[cells_burned],
                     ews = eff_ws[cells_burned],
                     fine_fuel = ifelse(is.null(nrow(fuel)), NA, fuel$fine_fuel[cells_burned]),
                     ladder_fuel = ifelse(is.null(nrow(fuel)), NA, fuel$ladder_fuel[cells_burned])
  )
  
 
  
  data_catcher[c(row_tracker:(row_tracker+nrow(data)-1)), ] <- data
  row_tracker <- row_tracker + nrow(data)
   
  message(paste("Finished processing fire", i))
  
}

write.csv(data_catcher, paste0("./Parameterization/calibration data/fire severity/data_catcher", 
                               format(Sys.time(), format = "%y-%m-%d %H-%M-%S"), ".csv"))

end_time <- Sys.time()
end_time - start_time

#*******************************************************************************
#* Analyze the data
#*******************************************************************************
library("tidyverse")
library("lme4")
library("effects")
#import data
fire_severity_data <- list.files("./Parameterization/calibration data/fire severity/",
                                 full.names = TRUE)

col_types <- list(
  fire_name = col_character(),
  dnbr = col_double(),
  clay = col_double(),
  pet = col_double(),
  cwd = col_double(),
  ews = col_double(),
  fine_fuel = col_double(),
  ladder_fuel = col_double()
)

#readr is incredible
data_all <- fire_severity_data %>% 
  purrr::map_df(~read_csv(., col_types = col_types)) %>%
  dplyr::filter(dnbr > 0) %>%
  dplyr::mutate(fine_fuel = ifelse(fine_fuel > 1000, 1, fine_fuel/1000)) %>%
  dplyr::mutate(dnbr = ifelse(dnbr < 100, 100, dnbr)) %>%
  dplyr::mutate(dnbr = ifelse(dnbr > 1000, 1000, dnbr))

data_with_fuel <- data_all %>%
  dplyr::filter(!is.na(fine_fuel))

# plot(dnbr ~ ladder_fuel, data = data_with_fuel)

with(gamma("inverse"), {
  print(valideta)
  print(validmu)
})

test <- lm(dnbr ~ clay + cwd + fine_fuel + ews + 0, data = data_all)
summary(test)

test_gamma <- glm(I(1/dnbr) ~ clay + cwd + pet + fine_fuel + ews + 0, data = data_all, family = Gamma(link = "inverse"))
summary(test_gamma)

plot(allEffects(test_gamma, partial.residuals = FALSE))



# 
# test_lmm <- lmer(dnbr ~ scale(clay) + scale(pet) + scale(cwd) + scale(ews) + scale(fine_fuel) + scale(ladder_fuel) + 0 + (1|fire_name), data = data_all)
# summary(test_lmm)
# 
# table(data_all$fire_name)

#visualize raw data
plot(data_all$dnbr ~ data_all$cwd)
abline(lm(data_all$dnbr ~ data_all$cwd))

plot(data_all$dnbr ~ data_all$pet)
abline(lm(data_all$dnbr ~ data_all$pet))

plot(data_all$dnbr ~ data_all$ews)
abline(lm(data_all$dnbr ~ data_all$ews))

plot(data_all$dnbr ~ data_all$clay)
abline(lm(data_all$dnbr ~ data_all$clay))

plot(data_all$dnbr ~ data_all$ladder_fuel)
abline(lm(data_all$dnbr ~ data_all$ladder_fuel))

plot(data_all$dnbr ~ data_all$fine_fuel)
abline(lm(data_all$dnbr ~ data_all$fine_fuel))

plot(data$pet ~ data$ews)
abline(lm(data$pet ~ data$ews))



install.packages("ggExtra")
library("ggExtra")

#
p1 <- ggplot(data = data,
             mapping = aes(x = ews, y = dnbr)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "Weekly Learning Time", y = "Science Scores") +
  theme_bw()

# Replace "histogram" with "boxplot" or "density" for other types
ggMarginal(p1, type = "histogram")
