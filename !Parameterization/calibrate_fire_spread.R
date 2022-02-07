library("sf")
library("tidyverse")
library("raster")
# library("zoom")

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance/")

#TODO expand to sierra region
#TODO check what happens when there are gaps in spread data -- missing days
#TODO check what happens when fire boundary doesn't increase each timestep
#TODO add in 2020 and 2021 fires

template <- raster("./masks_boundaries/mask.tif")
values(template) <- 1
sierra_poly <- sf::st_read("./masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  sf::st_zm() %>%
  sf::st_transform(crs(template))
tcsi_poly <- sf::st_read("./masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  sf::st_zm() %>%
  sf::st_transform(crs(template))

# mtbs <- 


# final_perims <- sf::st_read("./calibration data/geomac fire boundaries/Historic_GeoMAC_Perimeters_Combined_2000-2018-shp/US_HIST_FIRE_PERIMTRS_2000_2018_DD83.shp") %>%
#   sf::st_transform(crs = sf::st_crs(tcsi_poly)) %>%
#   sf::st_intersection(tcsi_poly)
# 
# plot(st_geometry(final_perims))


#TODO make polygons enclosing fires to compare with other datasets, see https://gis.stackexchange.com/questions/345823/identify-spatially-contiguous-clusters-in-raster-data-using-kmeans


#2019 -- just Caples fire, which was an escaped prescribed burn (data collection starts with 1080 acre fire, when it was transitioned
#to a wildfire
# daily_perims_2019 <- sf::st_read("./calibration data/geomac fire boundaries/2019_perimeters_dd83/2019_perimeters_dd83.shp") %>%
#   sf::st_transform(crs = sf::st_crs(tcsi_poly)) %>%
#   sf::st_intersection(tcsi_poly)
# 
# daily_perims_2018 <- sf::st_read("./calibration data/geomac fire boundaries/2018_perimeters_dd83/2018_perimeters_dd83.shp") %>%
#   sf::st_transform(crs = sf::st_crs(tcsi_poly)) %>%
#   sf::st_intersection(tcsi_poly)

daily_perims_all <- sf::st_read("./calibration data/geomac fire boundaries/combined_boundaried_2000-2019.shp") %>%
  sf::st_transform(crs = sf::st_crs(tcsi_poly)) %>%
  sf::st_intersection(tcsi_poly) %>%
  dplyr::mutate(gisacres = as.numeric(gisacres), perimeterd = as.Date(perimeterd))


#-------------------------------------------------------------------------------
# get FWI data
# download data
# check NCAR or FWI grid data

# http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_catalog.html

# for now, use landis output -- prevents us from extrapolating outside of the study area, though
# in future, find a gridded product (e.g. daymet) that covers whole sierra

climate <- read.csv("./calibration data/climate/Climate_10_regions_historical.csv")
ecoregions <- raster("./calibration data/climate/TCSI_ecoregions_10.tif")

#topography layers (needed for relative windspeed)
# TODO: get continuous uphill and slope rasters for whole sierra, from DEM 
uphill <- raster("./calibration data/climate/Upslope.tif")
slope <- raster("./calibration data/climate/slope.tif")

# import fuels data -- see create_fine_fuels_from_LANDFIRE.R script
fuels <- stack("./calibration data/landfire/landfire_fuels_all_years.tif")

# import annual mtbs fire severity -- see combine_mtbs_mosaics.R
mtbs <- stack("./calibration data/mtbs/severity_mosaic/combined_mosaic.tif")

# initialize dataframe to catch the data
# we just need to know the date and cell location,
# and then we can do another loop to extract the fwi, fuels, and windspeed

spread_data <- data.frame(fire_name = character(100000),
                               year = numeric(100000),
                               day = numeric(100000),
                               cell = numeric(100000),
                               success = logical(100000)
                               )

# this little guy keeps track of what row we're on, and gets incremented
# as data gets added to the dataframe, so we can avoid recursively binding rows
rowtracker <- 1
#-------------------------------------------------------------------------------
# nested loop goes through each year, finds fires in that year,
# then loops through fires, then through days per fire. 
# The loop extracts the identity of potential fire cells and whether or not 
# fire spread successfully to those cells

k <- 7
years <- 2000:2019
for(k in 1:20){
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
    filter(n_days >= 2) #TODO add other criteria?
  
  if(nrow(fires_current_year) == 0){
    print(paste0("No fires that lasted 2 or more days in year ", year))
    next()
  }
  #fire <- "Ralston"
  for(fire in fires_current_year$incidentna){
    
    current_fire <- daily_perims[daily_perims$incidentna == fire, ] %>%
        arrange(perimeterd) %>%
        group_by(perimeterd) %>%
        slice_max(gisacres) %>%
        ungroup() %>%
        mutate(spread = gisacres - dplyr::lag(gisacres, default = NA))
      # plot(st_geometry(current_fire))
      # plot(current_fire$gisacres ~ current_fire$perimeterd)
      # plot(current_fire$spread ~ current_fire$perimeterd) #all spread is > 0
      
    # burn_days <- min(current_fire$perimeterd):max(current_fire$perimeterd)
      
    for(i in 1:length(unique(current_fire$perimeterd))){
      
      #TODO check if data for burn day
      #if day is burning
      if(i == 1){
        burning <- raster::mask(template, current_fire[i, ], updatevalue = NA) # 1 means "burning"
        burned <- burning
      }else{
        # cells within the polygon this timestep, minus the ones that were burned as of
        # the previous timestep
        burning <- raster::mask(template, current_fire[i, ], updatevalue = NA) %>%
          mask(previous_burned, inverse = TRUE) #burned from last timestep
        burned <- raster::mask(template, current_fire[i, ], updatevalue = NA)
        
        
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
      
      n_potential <- length(potential_burn)
      if(n_potential == 0) next()
      
      first_row <- rowtracker
      last_row <- first_row + n_potential -1
      rowtracker <- last_row + 1
      
      #add daily fire data to dataframe
      spread_data$fire_name[first_row:last_row] <- current_fire$incidentna[i]
      spread_data$year[first_row:last_row] <- current_fire$fireyear[i]
      spread_data$day[first_row:last_row] <- format(current_fire$perimeterd[i], "%j")
      spread_data$cell[first_row:last_row] <- potential_burn
      spread_data$success[first_row:last_row] <- F
      
      #the loop ends on the last day of the fire, so spread success is left as FALSE
    }
      
  }
 
} 

#-------------------------------------------------------------------------------
# extract climate variables for each cell

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
rowcols <- rowColFromCell(template, spread_data$cell)
rowcol_new <- rowcols

rowcol_new[, 1] <- rowcols[, 1] + ifelse(spread_data$rose =="North",
                                            -1,
                                            ifelse(spread_data$rose =="South",
                                                   1, 0))
rowcol_new[, 2] <- rowcols[, 2] + ifelse(spread_data$rose =="East",
                                         -1,
                                         ifelse(spread_data$rose == "West",
                                                1, 0))

spread_data$cell_mtbs <- cellFromRowCol(template, rowcol_new[, 1], rowcol_new[, 2])

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
           U_b * ((Ua_Ub^2) + 2*(Ua_Ub) * sin(slope) * cos(relative_wd) + sin(slope)^2)^0.5
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