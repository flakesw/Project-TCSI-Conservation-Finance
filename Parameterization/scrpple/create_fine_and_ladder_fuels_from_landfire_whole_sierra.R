# create rasters of fine fuels and ladder fuels for entire sierra
# writes a bunch of rasters for fine fuel and ladder fuel loads, for each
# timestep that Landfire data is available
library(sf)
library(raster)
library(tidyverse)

#landfire is in EPSG:5070

#sierra shapefile
sierra <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  sf::st_transform("EPSG:5070")

#parameters calculated earliers; to translate LANDFIRE fuels to LANDIS fuels
fine_fuels_coef <- 0.76
ladder_fuels_coef <- 1.02

mapcode <- read.csv("D:/Data/Landfire fuels/conus/US_105fbfm40_09122104/US_105fbfm40/CSV_Data/US_105FBFM40.csv")

#table to translate from fuel model to fuel load
fuel_lookup <- read.csv("./Parameterization/calibration data/landfire/fuel_loads_scott_burgan.csv") %>%
  dplyr::mutate(X10hr = ifelse(X10hr > 0, X10hr, 10/224.17),
                Live_woody = ifelse(Live_woody > 0, Live_woody, 10/224.17)) %>%
  right_join(mapcode, by = (c("Fuel_model_code" = "FBFM40")))
  # mutate(total_fuels = ifelse(is.na(total_fuels), 0, total_fuels))

#import rasters of landfire fuel models and process to fine/ladder fuels
landfire_2001 <- raster("D:/Data/Landfire fuels/conus/US_105fbfm40_09122104/US_105fbfm40/grid1/us_105fbfm40") %>%
  raster::crop(sierra) %>%
  raster::mask(sierra)
landfire_fine_2001 <- raster::match(landfire_2001, fuel_lookup$VALUE)
landfire_ladder_2001 <- landfire_fine_2001
raster::values(landfire_fine_2001) <- fuel_lookup$X10hr[raster::values(landfire_fine_2001)] * 224.17 * fine_fuels_coef
raster::values(landfire_ladder_2001) <- fuel_lookup$Live_woody[raster::values(landfire_ladder_2001)] * 224.17 * ladder_fuels_coef 
rm(landfire_2001)
writeRaster(landfire_fine_2001, "D:/Data/Landfire fuels/sierra/landfire_fine_2001.tif")
rm(landfire_fine_2001)
writeRaster(landfire_ladder_2001, "D:/Data/Landfire fuels/sierra/landfire_ladder_2001.tif")
rm(landfire_ladder_2001)
gc()


landfire_2012 <- raster("D:/Data/Landfire fuels/conus/US_130FBFM40_05042015/US_130FBFM40/Grid2/us_130fbfm40") %>%
  raster::crop(sierra) %>%
  raster::mask(sierra)
landfire_fine_2012 <- raster::match(landfire_2012, fuel_lookup$VALUE)
landfire_ladder_2012 <- landfire_fine_2012
raster::values(landfire_fine_2012) <- fuel_lookup$X10hr[raster::values(landfire_fine_2012)] * 224.17 * fine_fuels_coef
raster::values(landfire_ladder_2012) <- fuel_lookup$Live_woody[raster::values(landfire_ladder_2012)] * 224.17 * ladder_fuels_coef 
rm(landfire_2012)
writeRaster(landfire_fine_2012, "D:/Data/Landfire fuels/sierra/landfire_fine_2012.tif")
rm(landfire_fine_2012)
writeRaster(landfire_ladder_2012, "D:/Data/Landfire fuels/sierra/landfire_ladder_2012.tif")
rm(landfire_ladder_2012)
gc()


landfire_2014 <- raster("D:/Data/Landfire fuels/conus/US_140FBFM40_20180618/Grid/us_140fbfm40") %>%
  raster::crop(sierra) %>%
  raster::mask(sierra)
landfire_fine_2014 <- raster::match(landfire_2014, fuel_lookup$VALUE)
landfire_ladder_2014 <- landfire_fine_2014
raster::values(landfire_fine_2014) <- fuel_lookup$X10hr[raster::values(landfire_fine_2014)] * 224.17 * fine_fuels_coef
raster::values(landfire_ladder_2014) <- fuel_lookup$Live_woody[raster::values(landfire_ladder_2014)] * 224.17 * ladder_fuels_coef 
rm(landfire_2014)
writeRaster(landfire_fine_2014, "D:/Data/Landfire fuels/sierra/landfire_fine_2014.tif")
rm(landfire_fine_2014)
writeRaster(landfire_ladder_2014, "D:/Data/Landfire fuels/sierra/landfire_ladder_2014.tif")
rm(landfire_ladder_2014)
gc()

landfire_2019 <- raster("D:/Data/Landfire fuels/conus/LF2019_FBFM40_200_CONUS/Tif/LC19_F40_200.tif") %>%
  raster::crop(sierra) %>%
  raster::mask(sierra)
landfire_fine_2019 <- raster::match(landfire_2019, fuel_lookup$VALUE)
landfire_ladder_2019 <- landfire_fine_2019
raster::values(landfire_fine_2019) <- fuel_lookup$X10hr[raster::values(landfire_fine_2019)] * 224.17 * fine_fuels_coef
raster::values(landfire_ladder_2019) <- fuel_lookup$Live_woody[raster::values(landfire_ladder_2019)] * 224.17 * ladder_fuels_coef 
rm(landfire_2019)
writeRaster(landfire_fine_2019, "D:/Data/Landfire fuels/sierra/landfire_fine_2019.tif")
rm(landfire_fine_2019)
writeRaster(landfire_ladder_2019, "D:/Data/Landfire fuels/sierra/landfire_ladder_2019.tif")
rm(landfire_ladder_2019)
gc()

landfire_2020 <- raster("D:/Data/Landfire fuels/conus/LF2020_FBFM40_200_CONUS/Tif/LC20_F40_200.tif") %>%
  raster::crop(sierra) %>%
  raster::mask(sierra)
landfire_fine_2020 <- raster::match(landfire_2020, fuel_lookup$VALUE)
landfire_ladder_2020 <- landfire_fine_2020
raster::values(landfire_fine_2020) <- fuel_lookup$X10hr[raster::values(landfire_fine_2020)] * 224.17 * fine_fuels_coef
raster::values(landfire_ladder_2020) <- fuel_lookup$Live_woody[raster::values(landfire_ladder_2020)] * 224.17 * ladder_fuels_coef 
rm(landfire_2020)
writeRaster(landfire_fine_2020, "D:/Data/Landfire fuels/sierra/landfire_fine_2020.tif")
rm(landfire_fine_2020)
writeRaster(landfire_ladder_2020, "D:/Data/Landfire fuels/sierra/landfire_ladder_2020.tif")
rm(landfire_ladder_2020)
gc()

landfire_2021 <- raster("D:/Data/Landfire fuels/conus/LF2021_FBFM40_210_CONUS/Tif/LC21_F40_210.tif") %>%
  raster::crop(sierra) %>%
  raster::mask(sierra)
landfire_fine_2021 <- raster::match(landfire_2021, fuel_lookup$VALUE)
landfire_ladder_2021 <- landfire_fine_2021
raster::values(landfire_fine_2021) <- fuel_lookup$X10hr[raster::values(landfire_fine_2021)] * 224.17 * fine_fuels_coef
raster::values(landfire_ladder_2021) <- fuel_lookup$Live_woody[raster::values(landfire_ladder_2021)] * 224.17 * ladder_fuels_coef 
rm(landfire_2021)
writeRaster(landfire_fine_2021, "D:/Data/Landfire fuels/sierra/landfire_fine_2021.tif")
rm(landfire_fine_2021)
writeRaster(landfire_ladder_2021, "D:/Data/Landfire fuels/sierra/landfire_ladder_2021.tif")
rm(landfire_ladder_2021)
gc()

# library("stars")
# library("raster")
# temp_rast <- raster("D:/Data/Landfire fuels/sierra/landfire_ladder_2001.tif") %>%
#   setValues(NA)
# landfire_fine_stack <- stack(replicate(21, temp_rast))
# landfire_fine_stack[[1]] <- raster("D:/Data/Landfire fuels/sierra/landfire_fine_2001.tif")
# landfire_fine_stack[[12]] <- raster("D:/Data/Landfire fuels/sierra/landfire_fine_2012.tif")
# landfire_fine_stack[[14]] <- raster("D:/Data/Landfire fuels/sierra/landfire_fine_2014.tif")
# landfire_fine_stack[[19]] <- raster("D:/Data/Landfire fuels/sierra/landfire_fine_2019.tif")
# landfire_fine_stack[[20]] <- raster("D:/Data/Landfire fuels/sierra/landfire_fine_2020.tif")
# landfire_fine_stack[[21]] <- raster("D:/Data/Landfire fuels/sierra/landfire_fine_2021.tif")
# landfire_fine_all_years <- approxNA(landfire_fine_stack, method = "constant", f = 0) #maps are static until updated with new landfire data -- safer choice
# 

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




