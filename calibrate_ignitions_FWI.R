# parameterize SCRPPLE ignitions
library(sf)
library(raster)
library(tidyverse)

setwd("C:/Users/Sam/Documents/Research/California forest economics")

ecoregions <- raster("./input_rasters_tcsi/categorical/TCSI_ecoregions.tif")

#read polygon of study area and reproject to match raster
study_area <- st_read("./masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  st_make_valid() %>%
  st_transform(crs(ecoregions))

mtbs <- st_read("./calibration data/mtbs/mtbs_fod_pts_data/mtbs_FODpoints_DD.shp") %>%
  st_transform(crs(ecoregions)) %>%
  st_intersection(study_area)


# read Karen Short ignition data and reproject to match raster
# this data was already intersected with the shapefile, just has ignitions from
# the study area
short <- readRDS("./calibration data/short_tcsi/short_tcsi.RDS") %>%
  st_transform(crs(ecoregions))
plot(st_geometry(short))

#import climate data, result of climate library spinup for a previous model run
clim_data <- read.csv("./calibration data/Climate-future-input-log_for_calibration.csv")

short$eco <- raster::extract(ecoregions, short)
plot_sf(short["eco"])


l_fire_dat <- filter(short, NWCG_CAUSE_CLASSIFICATION == "Human") %>%
  filter(FIRE_SIZE > 3.24)

### Subset 
### Order data
l_fire_days <- as.data.frame(cbind(l_fire_dat$FIRE_YEAR, l_fire_dat$DISCOVER_1)) #Extracting year and julian day
colnames(l_fire_days) <- c("YEAR", "J_DAY")
l_fire_days_sort <- l_fire_days[order(l_fire_days[,1]),] #sorting by year
#plot no of fires/y
l_fire_days_sort_count<- cbind(l_fire_days_sort, rep(1, nrow(l_fire_days_sort)))
l_fires_count <- cbind.data.frame(unique(l_fire_days_sort_count$YEAR) , 
                                  tapply(l_fire_days_sort_count$`rep(1, nrow(l_fire_days_sort))`,l_fire_days_sort_count$YEAR, sum))
colnames(l_fires_count) <- c("YEAR", "COUNT")

barplot(l_fires_count$COUNT, main ="No of ign/yr Lightning Observed",col=red,names.arg=l_fires_count$YEAR)


##---------------------------------------------------------------------------------
# Attempt #2
library("tidyverse")
library("sf")
library("cffdrs")
library("lubridate")
library("smoothr")
# library("ncdf4")

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance")

#TODO
# extract Short to a larger region
# 

short_full <- sf::st_read("./calibration data/short/Data/FPA_FOD_20210617.gdb",
                          layer = "Fires") %>%
              sf::st_transform(crs = "EPSG:4326")


#import a shapefile of Sierra Nevada regions, merge them, remove holes
ca_region <- sf::st_read("./masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  summarise(do.union = TRUE) %>% 
  st_make_valid() %>%
  smoothr::fill_holes(threshold = 0.5)
crs(ca_region) <- "EPSG:4326"
  
# julianday <- xx
year <- "2014"
month <- "05"
day <- "01"
tail <- paste0(year, month, day, ".nc")
file <- paste0("FWI.GEOS-5.Daily.Default.", tail)

#download FWI record
download.file(url = paste0("https://portal.nccs.nasa.gov/datashare/GlobalFWI/v2.0/fwiCalcs.GEOS-5/Default/GEOS-5/2014/", file), 
              destfile = paste0("./calibration data/fwi/", year,"/", tail), method = "curl", quiet = FALSE,
              cacheOK = TRUE,
              extra = getOption("download.file.extra"))

#raster package is the only one that really works for this, because of errors in the file
fwi <- raster(paste0("./calibration data/fwi/", year,"/", tail), varname = "GEOS-5_FWI")
crs(fwi)



