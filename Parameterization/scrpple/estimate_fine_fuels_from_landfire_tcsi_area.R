# make fine fuels map for parameterizing fine fuels in fire spread
library("raster")
library("tidyverse")
# library("mblm")

# step 1:
# LANDIS fine fuels for 2019
# Use LANDIS vs Landfire to adjust LANDFIRE fuels to Landis
# from an NECN model run
landis_fuels <- terra::rast("./Parameterization/calibration data/landfire/fine-fuels-6.tif")

# LANDFIRE fine fuels for 2019
landfire_2019 <- rast("D:/Data/Landfire fuels/lf89339302_US_200F40_19/US_200F40_19/us_200f40_19/w001001.adf")
# fuel load that corresponds with each model type
# extracted from Scott and Burgan 2005
# loads are in tons/acre
fuel_attr <- read.csv("D:/Data/Landfire fuels/LF23_F40_240.csv")
fuel_lookup <- read.csv("./Parameterization/calibration data/landfire/fuel_loads_scott_burgan.csv") %>%
  mutate(total_fuels = X10hr + X1hr) %>% # think about which fuel variables to use
  right_join(fuel_attr, by = (c("Fuel_model_code" = "FBFM40"))) %>%
  mutate(total_fuels = ifelse(is.na(total_fuels), 0, total_fuels))

# translate categorical raster to fuel density
fuel_2019 <- terra::classify(landfire_2019, rcl = fuel_lookup[, c("VALUE", "total_fuels")]) * 224.17 #convert to g m-2

# fit regression to relate LANDFIRE to LANDIS
fuel_2019 <- terra::aggregate(fuel_2019, fact = 6, fun = mean, expand = FALSE)
fuel_2019 <- terra::project(fuel_2019, landis_fuels)
fuel_2019 <- terra::crop(fuel_2019, landis_fuels)
fuel_2019[] <- ifelse(landis_fuels[] == 0, NA, fuel_2019[])
landis_fuels[] <- ifelse(fuel_2019[] == 0, NA, landis_fuels[])

plot(raster::values(landis_fuels) ~ raster::values(fuel_2019))
mod <- lm(raster::values(landis_fuels)~ raster::values(fuel_2019)  + 0)
summary(mod)
abline(a = 0, b = coef(mod))


#-------------------------------------------------------------------------------
# step 2:
# import LANDFIRE fine fuels for 2001, 2014, 2019, 2020, 2021

landfire_2001 <- rast("D:/Data/Landfire fuels/lf06186785_US_105FBFM40/US_105FBFM40/us_105fbfm40/w001001.adf")
landfire_mod_2001 <- terra::classify(landfire_2001, rcl = fuel_lookup[, c("VALUE", "total_fuels")]) * 224.17 #convert to g m-2
NAflag(landfire_mod_2001) <- NA

landfire_2014 <- rast("D:/Data/Landfire fuels/lf23669231_US_140FBFM40/US_140FBFM40/us_140fbfm40/w001001.adf")
landfire_mod_2014 <- terra::classify(landfire_2014, rcl = fuel_lookup[, c("VALUE", "total_fuels")]) * 224.17 #convert to g m-2
NAflag(landfire_mod_2014) <- NA

landfire_2019 <- rast("D:/Data/Landfire fuels/lf89339302_US_200F40_19/US_200F40_19/us_200f40_19/w001001.adf")
landfire_mod_2019 <- terra::classify(landfire_2019, rcl = fuel_lookup[, c("VALUE", "total_fuels")]) * 224.17 #convert to g m-2
NAflag(landfire_mod_2019) <- NA

landfire_2020 <- rast("D:/Data/Landfire fuels/lf34933400_US_200F40_20/US_200F40_20/us_200f40_20/w001001.adf")
landfire_mod_2020 <- terra::classify(landfire_2020, rcl = fuel_lookup[, c("VALUE", "total_fuels")]) * 224.17 #convert to g m-2
NAflag(landfire_mod_2020) <- NA

landfire_2021 <- rast("D:/Data/Landfire fuels/lf65319260_US_210F40_21/US_210F40_21/us_210f40_21/w001001.adf")
landfire_mod_2021 <- terra::classify(landfire_2021, rcl = fuel_lookup[, c("VALUE", "total_fuels")]) * 224.17 #convert to g m-2
NAflag(landfire_mod_2021) <- NA

# use predictions from model fit above to translate landfire fuel model to fine fuel biomass
landfire_mod_2019 <- terra::aggregate(landfire_mod_2019, fact = 6, fun = mean, expand = FALSE, na.rm = TRUE)
landfire_mod_2019 <- landfire_mod_2019 * coef(mod)
plot(landfire_mod_2019)

landfire_mod_2001 <- terra::aggregate(landfire_mod_2001, fact = 6, fun = mean, expand = FALSE, na.rm = TRUE)
landfire_mod_2001 <- landfire_mod_2001 * coef(mod)
plot(landfire_mod_2001)

landfire_mod_2014 <- raster::aggregate(landfire_mod_2014, fact = 6, fun = mean, expand = FALSE, na.rm = TRUE)
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
temp <- setValues(raster(landfire_mod_2001), NA)

#initialize an empty raster stack, one layer per year
# and fill with landfire data for appropriate years
# it seems like landfire data apply to the beginning of the next year (e.g., fires in 2014
# are reflected in the 2014 landfire data. So fuels need to be offset by a year)
landfire_all_years <- stack(replicate(22, temp))
landfire_all_years[[1]] <- raster(landfire_mod_2001)
landfire_all_years[[15]] <- raster(landfire_mod_2014)
landfire_all_years[[20]] <- raster(landfire_mod_2019)
landfire_all_years[[21]] <- raster(landfire_mod_2020)
landfire_all_years[[22]] <- raster(landfire_mod_2021)

# landfire_test <- approxNA(landfire_all_years) # linear interpolation -- allows influence of fire to affect pre-fire fuels
landfire_all_years <- approxNA(landfire_all_years, method = "constant", f = 0) #maps are static until updated with new landfire data -- safer choice

raster::writeRaster(landfire_all_years, "./calibration data/landfire/landfire_fuels_all_years.tif", overwrite = TRUE)
