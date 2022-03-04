#get ladder fuels

initial_communities <- read.csv("./Models/Inputs/climate_IC/TCSI_IC6.csv")

ladder_spp <- c("AbieConc", "CaloDecu", "PinuCont", "PinuJeff", "PinuLamb", 
                "PinuPond", "PinuMono", "PinuSabi", "PinuWash", "PseuMenz", 
                "FX_R_SEED", "NOFX_R_SEED", "NOFX_NOR_SEED")
ladder_age <- 28

ladder_fuels <- initial_communities %>%
  filter(SpeciesName %in% ladder_spp) %>%
  filter(CohortAge <= 28) %>%
  group_by(MapCode) %>%
  summarise(ladder_biomass = sum(CohortBiomass))

ic_raster <- raster("./Models/Inputs/input_rasters_reproject/ic_mapcode5.tif")

ladder_raster <- raster::match(ic_raster, ladder_fuels$MapCode)
raster::values(ladder_raster) <- ladder_fuels$ladder_biomass[raster::values(ladder_raster)]

#------------
#fill in gaps using landfire
landfire_2019 <- raster("D:/Data/Landfire fuels/lf89339302_US_200F40_19/US_200F40_19/us_200f40_19/w001001.adf")

#mapcodes to translate from raster values to fuel model type
mapcode <- landfire_2019$w001001@data@attributes[[1]]

# fuel load that corresponds with each model type
# extracted from Scott and Burgan 2005
# loads are in tons/acre
fuel_lookup <- read.csv("./Parameterization/calibration data/landfire/fuel_loads_scott_burgan.csv") %>%
  mutate(total_fuels = Live_woody) %>% # think about which fuel variables to use
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

#pretty much a 1:1 relationship, so that's nice!


#TODO make a stack of ladder fuels rasters (like we did for fine fuels)
#TODO expand to whole sierra

