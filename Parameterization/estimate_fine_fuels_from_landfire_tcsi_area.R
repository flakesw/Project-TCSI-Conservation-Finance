# make fine fuels map for parameterizing fine fuels in fire spread
library("raster")
library("tidyverse")
# library("mblm")

# TODO compare with other fuel map data sources or other methods

# step 1:
# LANDIS fine fuels for 2019
# Use LANDIS vs Landfire to adjust LANDFIRE fuels to Landis
# from an NECN model run
landis_fuels <- raster("./Parameterization/calibration data/landfire/fine-fuels-6.tif")

# LANDFIRE fine fuels for 2019
landfire_2019 <- raster("D:/Data/Landfire fuels/lf89339302_US_200F40_19/US_200F40_19/us_200f40_19/w001001.adf")

#mapcodes to translate from raster values to fuel model type
mapcode <- landfire_2019$w001001@data@attributes[[1]]

# fuel load that corresponds with each model type
# extracted from Scott and Burgan 2005
# loads are in tons/acre
fuel_lookup <- read.csv("./Parameterization/calibration data/landfire/fuel_loads_scott_burgan.csv") %>%
  mutate(total_fuels = X10hr) %>% # think about which fuel variables to use
  right_join(mapcode, by = (c("Fuel_model_code" = "FBFM40"))) %>%
  mutate(total_fuels = ifelse(is.na(total_fuels), 0, total_fuels))

# translate categorical raster to fuel density
fuel_2019 <- raster::match(landfire_2019, fuel_lookup$ID)
raster::values(fuel_2019) <- fuel_lookup$total_fuels[raster::values(fuel_2019)]
raster::values(fuel_2019) <- raster::values(fuel_2019) * 224.17 #convert to g m-2

# fit regression to relate LANDFIRE to LANDIS
fuel_2019 <- raster::aggregate(fuel_2019, fact = 6, fun = mean, expand = FALSE)
fuel_2019 <- projectRaster(fuel_2019, landis_fuels)
fuel_2019 <- raster::crop(fuel_2019, landis_fuels)
raster::values(fuel_2019) <- ifelse(raster::values(landis_fuels) == 0, NA, raster::values(fuel_2019))
raster::values(landis_fuels) <- ifelse(raster::values(landis_fuels) == 0, NA, raster::values(landis_fuels))

plot(raster::values(landis_fuels) ~ raster::values(fuel_2019))
mod <- lm(raster::values(landis_fuels)~ raster::values(fuel_2019)  + 0)
summary(mod)
abline(a = 0, b = coef(mod))


#-------------------------------------------------------------------------------
# step 2:
# import LANDFIRE fine fuels for 2001, 2014, 2019, 2020, 2021

landfire_2001 <- raster("D:/Data/Landfire fuels/lf06186785_US_105FBFM40/US_105FBFM40/us_105fbfm40/w001001.adf")
landfire_mod_2001 <- raster::match(landfire_2001, fuel_lookup$ID)
values(landfire_mod_2001) <- fuel_lookup$total_fuels[values(landfire_mod_2001)]
values(landfire_mod_2001) <- values(landfire_mod_2001) * 224.17 #convert to g m-2

landfire_2014 <- raster("D:/Data/Landfire fuels/lf23669231_US_140FBFM40/US_140FBFM40/us_140fbfm40/w001001.adf")
landfire_mod_2014 <- raster::match(landfire_2014, fuel_lookup$ID)
values(landfire_mod_2014) <- fuel_lookup$total_fuels[values(landfire_mod_2014)]
values(landfire_mod_2014) <- values(landfire_mod_2014) * 224.17 #convert to g m-2

landfire_2020 <- raster("D:/Data/Landfire fuels/lf34933400_US_200F40_20/US_200F40_20/us_200f40_20/w001001.adf")
landfire_mod_2020 <- raster::match(landfire_2020, fuel_lookup$ID)
values(landfire_mod_2020) <- fuel_lookup$total_fuels[values(landfire_mod_2020)]
values(landfire_mod_2020) <- values(landfire_mod_2020) * 224.17 #convert to g m-2

landfire_2021 <- raster("D:/Data/Landfire fuels/lf65319260_US_210F40_21/US_210F40_21/us_210f40_21/w001001.adf")
landfire_mod_2021 <- raster::match(landfire_2021, fuel_lookup$ID)
values(landfire_mod_2021) <- fuel_lookup$total_fuels[values(landfire_mod_2021)]
values(landfire_mod_2021) <- values(landfire_mod_2021) * 224.17 #convert to g m-2

# use predictions from model fit above to translate landfire fuel model to fine fuel biomass
landfire_mod_2019 <- raster::aggregate(landfire_mod_2019, fact = 6, fun = mean, expand = FALSE)
landfire_mod_2019 <- landfire_mod_2019 * coef(mod)
plot(landfire_mod_2019)

landfire_mod_2001 <- raster::aggregate(landfire_mod_2001, fact = 6, fun = mean, expand = FALSE)
landfire_mod_2001 <- landfire_mod_2001 * coef(mod)
plot(landfire_mod_2001)

landfire_mod_2014 <- raster::aggregate(landfire_mod_2014, fact = 6, fun = mean, expand = FALSE)
landfire_mod_2014 <- landfire_mod_2014 * coef(mod)
plot(landfire_mod_2014)

landfire_mod_2020 <- raster::aggregate(landfire_mod_2020, fact = 6, fun = mean, expand = FALSE)
landfire_mod_2020 <- landfire_mod_2020 * coef(mod)
plot(landfire_mod_2020)

landfire_mod_2021 <- raster::aggregate(landfire_mod_2021, fact = 6, fun = mean, expand = FALSE)
landfire_mod_2021 <- landfire_mod_2021 * coef(mod)
plot(landfire_mod_2021)

# step 3: 
# interpolate between fuel layers to make fuel layer for each year
# TODO: something more sophisticated -- maybe mask out areas which burned, 
# and don't allow them to be interpolated (prevent influence of future fires on past fuels)
# but let fuels elsewhere develop.
# Especially a big deal for 2014 (King fire)


#make an empty raster layer
temp <- setValues(landfire_mod_2001, NA)

#initialize an empty raster stack, one layer per year
# and fill with landfire data for appropriate years
# it seems like landfire data apply to the beginning of the next year (e.g., fires in 2014
# are reflected in the 2014 landfire data. So fuels need to be offset by a year)
landfire_all_years <- stack(replicate(22, temp))
landfire_all_years[[1]] <- landfire_mod_2001
landfire_all_years[[15]] <- landfire_mod_2014
landfire_all_years[[20]] <- landfire_mod_2019
landfire_all_years[[21]] <- landfire_mod_2020
landfire_all_years[[22]] <- landfire_mod_2021

# landfire_test <- approxNA(landfire_all_years) # linear interpolation -- allows influence of fire to affect pre-fire fuels
landfire_all_years <- approxNA(landfire_all_years, method = "constant", f = 0) #maps are static until updated with new landfire data -- safer choice

writeRaster(landfire_all_years, "./calibration data/landfire/landfire_fuels_all_years.tif", overwrite = TRUE)
