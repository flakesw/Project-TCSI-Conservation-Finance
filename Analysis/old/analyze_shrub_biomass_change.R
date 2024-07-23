#track shrubs over time
library(tidyverse)
library(terra)

biomass_table <- read.csv("E:/TCSI LANDIS/LANDIS runs/Scenario6 - miroc - Run 7/spp-biomass-log.csv")

shrub_bio <- biomass_table %>%
  select(Time, EcoName, NumActiveSites, AboveGroundBiomass_FX_R_SEED, 
         AboveGroundBiomass_NOFX_NOR_SEED, AboveGroundBiomass_NOFX_R_SEED)
shrub_bio$shrub_biomass <- shrub_bio$AboveGroundBiomass_FX_R_SEED+shrub_bio$AboveGroundBiomass_NOFX_NOR_SEED + shrub_bio$AboveGroundBiomass_NOFX_R_SEED

ggplot(data = shrub_bio, mapping = aes(x = Time, y = shrub_biomass, group = EcoName)) +
  geom_line()




shrub_rasters <- list.files("E:/TCSI LANDIS/LANDIS runs/Scenario6 - miroc - Run 7/biomass", full.names = TRUE) %>%
  `[`(grepl("SEED", .))
sh_ra <- rast(shrub_rasters)

shrub_combined <- sh_ra[[1:9]] + sh_ra[[10:18]] + sh_ra[[19:27]]
plot(shrub_combined[[1]])
plot(shrub_combined[[9]])
plot(shrub_combined[[9]] - shrub_combined[[1]])
