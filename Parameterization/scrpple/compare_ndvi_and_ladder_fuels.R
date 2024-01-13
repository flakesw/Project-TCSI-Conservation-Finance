#ladder fuels and ndvi relationship
library(stars)
library(terra)
library(tidyverse)

ladder <- read_stars("D:/Data/Landfire fuels/sierra/landfire_ladder_2019.tif")
plot(ladder)

modis_list <- list.files("D:/Data/modis/downloaded_appEARS", full.names = TRUE)
modis_rasters <- data.frame(file = modis_list[grep("NDVI", modis_list)])

modis_rasters$date <- str_match(modis_rasters$file, "(?:_doy)(\\d+)")[, 2] %>%
  as.Date("%Y%j")

first_date <- as.Date("2019-07-01")

modis_rasters$distance_ig <- (first_date - modis_rasters$date) 
modis_file <- modis_rasters[which.min(modis_rasters[modis_rasters$distance_ig>0, "distance_ig"]), "file"]

modis_before <- stars::read_stars(modis_file) * 0.0001

modis_before <- stars::st_warp(modis_before, dest = ladder, method = "near")
plot(modis_before)

modis_before <- terra::rast(modis_before)
ladder <- terra::rast(ladder)

both <- c(modis_before, ladder)

ncell(both)

data <- data.frame(ndvi = modis_before[],
                   ladder = ladder[])

data_samp <- both[sample(ncell(both), 20000)]
names(data_samp) <- c("NDVI", "ladder")

plot(data_samp$ladder ~ data_samp$NDVI)
summary(lm(data_samp$ladder ~ data_samp$NDVI))
abline(coef(lm(data_samp$ladder ~ data_samp$NDVI)))



#--------------------------------------------

initial_communities <- read.csv("./Models/Inputs/climate_IC/TCSI_IC6.csv")

ladder_spp <- c("AbieConc", "AbieMagn", "CaloDecu", 
                "PinuCont", "PinuJeff", "PinuLamb", 
                "PinuPond", "PinuMono", "PinuSabi", 
                "PinuWash", "PseuMenz", "PinuMont",
                "JuniOcci", "PinuAtte", "PinuSabi",
                "PinuWash", 
                "FX_R_SEED", "NOFX_R_SEED", "NOFX_NOR_SEED")
ladder_age1 <- 20
ladder_age2 <- 28
slope = -1/(ladder_age2 - ladder_age1)
int = 1 - (ladder_age1*slope)

tree_fuels <- initial_communities %>%
  filter(SpeciesName %in% ladder_spp) %>%
  filter(CohortAge <= ladder_age2) %>%
  mutate(CohortBiomass = ifelse(CohortBiomass > 150, 150, CohortBiomass))


ladder_fuels <- tree_fuels %>%
  group_by(MapCode) %>%
  summarise(ladder_biomass = sum(CohortBiomass))

library(raster)
ic_raster <- raster("./Models/Inputs/input_rasters_reproject/ic_mapcode5.tif")

ladder_raster <- raster::match(ic_raster, ladder_fuels$MapCode)
raster::values(ladder_raster) <- ladder_fuels$ladder_biomass[raster::values(ladder_raster)]
hist(ladder_raster)


modis_list <- list.files("D:/Data/modis/downloaded_appEARS", full.names = TRUE)
modis_rasters <- data.frame(file = modis_list[grep("NDVI", modis_list)])

modis_rasters$date <- str_match(modis_rasters$file, "(?:_doy)(\\d+)")[, 2] %>%
  as.Date("%Y%j")

first_date <- as.Date("2015-09-01")

modis_rasters$distance_ig <- (first_date - modis_rasters$date) 
modis_file <- modis_rasters[which.min(modis_rasters[modis_rasters$distance_ig>0, "distance_ig"]), "file"]

modis_before <- stars::read_stars(modis_file) * 0.0001

modis_before <- stars::st_warp(modis_before, dest = st_as_stars(ladder_raster), method = "near")
plot(modis_before)

modis_before <- terra::rast(modis_before)
modis_before[modis_before < 0.2] <- NA
modis_before[modis_before > 0.9] <- NA
ladder_raster <- terra::rast(ladder_raster)
ladder_raster[ladder_raster < 1] <- NA

both <- c(modis_before, ladder_raster)

ncell(both)

data <- data.frame(ndvi = modis_before[],
                   ladder = ladder_raster[])

data_samp <- both[sample(ncell(both), 20000)]
names(data_samp) <- c("NDVI", "ladder")

plot((data_samp$ladder) ~ (data_samp$NDVI))
summary(lm((data_samp$ladder) ~ (data_samp$NDVI)))
abline(coef(lm((data_samp$ladder) ~ (data_samp$NDVI))))
