library("sf")
library("tidyverse")
library("raster")
library("stars")

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance/")

template <- raster("./Models/Inputs/masks_boundaries/mask.tif")
raster::values(template) <- 1
sierra_poly <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  sf::st_zm() %>%
  sf::st_transform(crs(template))
tcsi_poly <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  sf::st_zm() %>%
  sf::st_transform(crs(template))

sierra_template <- template
extent(sierra_template) <- extent(sierra_poly)
sierra_template <- raster::mask(sierra_template, sierra_poly, update.value = 1)

daily_perims_all <- sf::st_read("./Parameterization/calibration data/geomac_all_years/perims_2000_2021.shp") %>%
  sf::st_transform(crs = sf::st_crs(sierra_poly)) %>%
  sf::st_intersection(sierra_poly) %>%
  dplyr::filter(fireyear >= 2000)

#recalculate area -- the gisacres column is a crazy mess, no idea what happened there
test<- map(sf::st_geometry(daily_perims_all), ~ sf::st_area(.)) %>%
  unlist() %>%
  `/`(4046.86) #convert to acres

plot(test ~ daily_perims_all$gisacres,
     xlim = c(0, 4e+05))

#fix dates
library(lubridate)
daily_perims_all$perimeterd
normal_format <- which((substr(daily_perims_all$perimeterd, 1, 4) %in% c(2000:2022)))
different_format <- which(!(substr(daily_perims_all$perimeterd, 1, 4) %in% c(2000:2022)))

dates_with_time <- mdy_hms(daily_perims_all[different_format, ]$perimeterd)
dates_mdy <- mdy(daily_perims_all[different_format, ]$perimeterd)

dates_fixed <- ifelse(!is.na(dates_with_time), 
                      as.character(dates_with_time, format = "%Y-%m-%d"), 
                      as.character(dates_mdy, format = "%Y-%m-%d"))

daily_perims_all[different_format, ]$perimeterd <- dates_fixed

#explore data
#396 fires with more than 10 daily perimeters!
length(table(daily_perims_all$incidentna)[table(daily_perims_all$incidentna) > 10])

test <- daily_perims_all %>% 
  group_by(incidentna) %>%
  slice_max(gisacres) %>%
  group_by(fireyear) %>%
  summarise(area_burned = sum(gisacres), .groups = "keep")

plot(test$area_burned ~ test$fireyear)

#-------------------------------------------------------------------------------
# get FWI data
# download data
# check NCAR or FWI grid data

# http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_catalog.html

# for now, use landis output -- prevents us from extrapolating outside of the study area, though
# in future, find a gridded product (e.g. daymet) that covers whole sierra

# climate <- read.csv("./calibration data/climate/Climate_10_regions_historical.csv")
# ecoregions <- raster("./calibration data/climate/TCSI_ecoregions_10.tif")


#-------------------------------------------------------------------------------
# Import slope and aspect from DEM
#-------------------------------------------------------------------------------
# these rasters in the wrong CRS, but it takes a  ton of RAM to reproject them.
# Instead, I'll project the fire boundary to match, and then crop and reproject.
# It's actually really  fast to do it this way.

slope_full <- read_stars("./Parameterization/calibration data/topography/sierra_slope.tif")

aspect_full <- read_stars("./Parameterization/calibration data/topography/sierra_aspect.tif")

#-------------------------------------------------------------------------------
#LANDFIRE layers
#-------------------------------------------------------------------------------
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


#-----------------------------------------------------------------------------------
# MTBS severity mosaics
#-------------------------------------------------------------------------------
#used for combustion buoyancy. We'll import them one year at a time as needed
mtbs_folder <- "C:/Users/Sam/Documents/Research/TCSI conservation finance/Parameterization/calibration data/mtbs/severity_mosaic/composite_data/MTBS_BSmosaics"
mtbs_list <- list.files(mtbs_folder, pattern = "*.tif", recursive = TRUE, full.names = TRUE)


#*******************************************************************************
# Extract fire spread information
#*******************************************************************************

# initialize dataframe to catch the data
# we just need to know the date and cell location,
# and then we can do another loop to extract the fwi, fuels, and windspeed

spread_data <- data.frame(fire_name = character(1000000),
                               year = numeric(1000000),
                               day = numeric(1000000),
                               cell = numeric(1000000),
                               success = logical(1000000),
                               days_between = numeric(1000000)
                               )

years <- 2000:2021

# this little guy keeps track of what row we're on, and gets incremented
# as data gets added to the dataframe, so we can avoid recursively binding rows
rowtracker <- 1

error_flag <- FALSE
#-------------------------------------------------------------------------------
# nested loop goes through each year, finds fires in that year,
# then loops through fires, then through days per fire. 
# The loop extracts the identity of potential fire cells and whether or not 
# fire spread successfully to those cells

for(k in 19:22){
  year <- years[k]

  daily_perims <- daily_perims_all %>%
    filter(fireyear == year)
  
  # TODO find potential fires
  # look through years and extract fires which lasted a certain # days, or spread to a certain size?
  fires_current_year <- daily_perims %>%
    dplyr::group_by(incidentna) %>%
    dplyr::summarise(incidentna = dplyr::first(incidentna),
                     n_days = n_distinct(perimeterd),
                     max_size = max(gisacres)) %>%
    filter(n_days >= 3) #TODO add other criteria?
  
  if(nrow(fires_current_year) == 0){
    message(paste0("No fires that lasted 2 or more days in year ", year))
    next()
  }

  for(fire in fires_current_year$incidentna){
    
    current_fire <- tryCatch(
      {
        daily_perims[daily_perims$incidentna == fire, ] %>%
        arrange(perimeterd) %>%
        group_by(perimeterd) %>%
        slice_max(gisacres) %>%
        distinct(gisacres, .keep_all = TRUE) %>% #in case there's duplicate polygons with the same gisacres and date
        ungroup() %>%
        mutate(spread = gisacres - dplyr::lag(gisacres, default = NA),
               days_between = as.Date(perimeterd) - dplyr::lag(as.Date(perimeterd), default = NA)) %>%
        sf::st_cast("MULTIPOLYGON")
      },
      error=function(cond) {
        
        message(paste("Error processing fire", fire))
        message("Here's the original error message:")
        message(cond)
        error_flag <<- TRUE
        
      }
    )
    if(error_flag) next()
    
    print(fire)
    plot(st_geometry(current_fire))
    plot(current_fire$gisacres ~ as.Date(current_fire$perimeterd))
    plot(current_fire$spread ~ as.Date(current_fire$perimeterd)) #all spread is > 0
    
    if(sum(current_fire$days_between == 1, na.rm = TRUE) < 1){
      message("Skipping fire due to lack of successive daily perimeters")
      next()
    }
    
    #if fire decreases in size, use previous fire polygon
    for(i in 2:nrow(current_fire)){
      if(!is.na(current_fire$spread[i]) & current_fire$spread[i] < 0){
        sf::st_geometry(current_fire)[i] <-  sf::st_geometry(current_fire[i-1, ])
      }
    }
    
    #if data looks good, then do the loop to get potential/burned cells
    for(i in 1:length(unique(current_fire$perimeterd))){
      print(i)
      #TODO check if data for burn day
      #if day is burning
      if(i == 1){
        burning <- raster::mask(sierra_template, current_fire[i, ], updatevalue = NA) # 1 means "burning"
        burned <- burning
      }else{
        # cells within the polygon this timestep, minus the ones that were burned as of
        # the previous timestep
        burning <- raster::mask(sierra_template, current_fire[i, ], updatevalue = NA) %>%
          mask(previous_burned, inverse = TRUE) #burned from last timestep
        burned <- raster::mask(sierra_template, current_fire[i, ], updatevalue = NA)
        
        
        #for the potential cells added last timestep, which ones ended up burning this timestep?
        # previous-year's first_row and last_row
        spread_data$success[first_row:last_row] <- ifelse(spread_data$cell[first_row:last_row] %in% which(values(burning) == 1),
                                                               TRUE,
                                                               FALSE)
      }
      
      #calculate potential burn based on currently burning cells
      potential_burn <- adjacent(burning, cells = which(values(burning) == 1),
                                 directions = 4, pairs = FALSE, include = FALSE) %>%
        `[`(!(. %in% which(values(burned) == 1))) #remove cells that already burned
      
      
      previous_burned <- burned #update for next timestep
      
      plot(burned, ext = st_bbox(current_fire))
      
      n_potential <- length(potential_burn)
      if(n_potential == 0) next()
      
      print(rowtracker)
      
      first_row <- rowtracker
      last_row <- first_row + n_potential -1
      rowtracker <- last_row + 1
      
      #add daily fire data to dataframe
      spread_data$fire_name[first_row:last_row] <- current_fire$incidentna[i]
      spread_data$year[first_row:last_row] <- current_fire$fireyear[i]
      spread_data$day[first_row:last_row] <- format(as.Date(current_fire$perimeterd[i]), "%j")
      spread_data$cell[first_row:last_row] <- potential_burn
      spread_data$success[first_row:last_row] <- F
      spread_data$days_between[first_row:last_row] <- current_fire$days_between[i]
      
      #the loop ends on the last day of the fire, so spread success is left as FALSE
    }
      
  }
 
} 

#*******************************************************************************
# extract climate variables for each cell
#*******************************************************************************

spread_data <- spread_data[1:last_row, ]

spread_data$day <- as.integer(spread_data$day)
spread_data$year <- as.integer(spread_data$year)

spread_data$slope <- slope[spread_data$cell]
spread_data$aspect <- uphill[spread_data$cell]

#extract fuels for each cell, from the proper fuel layer
spread_data$fuel <- NA

for(i in 1:19){
  year <- c(2001:2019)[i]
  #fuels go from 2001 to 2021, different from mtbs. TODO fix this; check on data sources
  spread_data[spread_data$year == year, "fuel"] <- fuels[[i]][spread_data[spread_data$year == year, "cell"]]
}

#extract ecoregion for each cell, to match to climate
spread_data$ecoregion <- ecoregions[spread_data$cell]

spread_data <- left_join(spread_data, 
                                   select(climate, Year, Timestep, EcoregionIndex, FWI, winddirection, windspeed),
                                   by = c("year" = "Year", 
                                          "day" = "Timestep", 
                                          "ecoregion" = "EcoregionIndex"))

#find what cell is upwind of the potential fire cell
rose_breaks <- c(0, 45, 135, 225, 315, 360)
rose_labs <- c(
  "North", "East", "South", "West", "North"
)

spread_data <- spread_data %>%
  mutate(
    rose = cut(
      winddirection,
      breaks = rose_breaks,
      labels = rose_labs,
      right = FALSE,
      include.lowest = TRUE
    )
  )
    
#find which cell is upwind of each cell
rowcols <- rowColFromCell(sierra_template, spread_data$cell)
rowcol_new <- rowcols

rowcol_new[, 1] <- rowcols[, 1] + ifelse(spread_data$rose =="North",
                                            -1,
                                            ifelse(spread_data$rose =="South",
                                                   1, 0))
rowcol_new[, 2] <- rowcols[, 2] + ifelse(spread_data$rose =="East",
                                         -1,
                                         ifelse(spread_data$rose == "West",
                                                1, 0))

spread_data$cell_mtbs <- cellFromRowCol(sierra_template, rowcol_new[, 1], rowcol_new[, 2])

# assign a potential fire cell a value based on the severity of its upwind cell
# loop over years with fire spread data
for(year in unique(spread_data$year)){
  spread_data[spread_data$year == year, "mtbs"] <- mtbs[[year - 1999]][spread_data[spread_data$year == year, "cell_mtbs"]]
}

# This changes based on fire severity. Combustion buoyancy.
# TODO Check on MTBS severity and converting to LANDIS severity
spread_data$U_b <- ifelse(is.nan(spread_data$mtbs) | is.na(spread_data$mtbs), 5,
                               ifelse(spread_data$mtbs > 3, 50,
                                      ifelse(spread_data$mtbs > 2, 25,
                                            10)))

### Caculating windspeed in direction of spread 
spread_data$relative_wd <- spread_data$winddirection - spread_data$aspect
spread_data$Ua_Ub <- spread_data$windspeed / spread_data$U_b
### Calculating effective wind speed. 
spread_data<- spread_data %>%
  mutate(eff_wspd = 
           U_b * ((Ua_Ub^2) + 2*(Ua_Ub) * sin(slope * (pi/180)) * cos(relative_wd * (pi/180)) + sin(slope * (pi/180))^2)^0.5
  )
                  
library("vioplot")
boxplot(spread_data$FWI ~ spread_data$success)
vioplot(spread_data$FWI ~ spread_data$success)
t.test(spread_data$FWI ~ spread_data$success)
boxplot(spread_data$fuel ~ spread_data$success)
t.test(spread_data$fuel ~ spread_data$success)
boxplot(spread_data$mtbs ~ spread_data$success)
t.test(spread_data$mtbs ~ spread_data$success)
boxplot(spread_data$eff_wspd ~ spread_data$success)
t.test(spread_data$eff_wspd ~ spread_data$success)


#relativize fuels
max_fuel <- 1000
spread_data$fuel <- ifelse(spread_data$fuel < max_fuel, 
                           spread_data$fuel / max_fuel,
                           1)


#-------------------------------------------------------------------------------
# fit model to fire spread
library("lme4")
model <- glm(success ~ FWI + fuel + eff_wspd,
             data = spread_data,
             family = "binomial")
summary(model)
plot(effects::allEffects(model))

#-------------------------------------------------------------------------------
# recycling can
# 
# #calculate FWI
# library(cffdrs)
# library(raster)
# # daily noon temp, rh, ws, and prec (we recommend tif format):
# # The test data is a stack with four input variables including 
# day01src <- system.file("extdata","test_rast_day01.tif",package="cffdrs")
# day01 <- stack(day01src)
# day01 <- crop(day01,c(250,255,47,51))
# # assign variable names:
# names(day01)<-c("temp","rh","ws","prec")
# # (1) use the initial values
# foo<-fwiRaster(day01)
# plot(foo)
# ### Additional, longer running examples ###
# # (2) use initial values with larger raster
# day01 <- stack(day01src)
# names(day01)<-c("temp","rh","ws","prec")
# foo<-fwiRaster(day01)
# plot(foo)



# 
# north_fire <- daily_perims_2018[daily_perims_2018$incidentna == daily_perims_2018$incidentna[1], ] %>%
# # North fire; started 2018-09-03, first perimeter drawn on 09-04-281
# # strongly suppressed with several crews and helicopters https://www.sacbee.com/news/california/fires/article217783940.html
# # much satellite evidence of dozer lines, staging areas
#   arrange(perimeterd) %>%
#   group_by(perimeterd) %>%
#   slice_max(gisacres) %>% #if there are multiple values for a day, pick the largest polygon
#   ungroup() %>%
#   mutate(spread = gisacres - dplyr::lag(gisacres, default = NA))
#   #this fire is a bad example -- the initial drawing is a bad sketch, and then all the polygons are more or less the same size.
#   # fire was quickly suppressed and did not grow after initial growth to maximum boundary