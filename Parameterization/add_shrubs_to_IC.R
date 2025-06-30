library(terra)
library(tidyverse)

comm_map <- rast("./Parameterization/calibration data/output-community-0.img")
shrub_cover <- read.csv("./Parameterization/calibration data/initial_shrub_cover.csv")
original_map <- rast("./Models/Inputs/input_rasters_reproject/ic_mapcode5.tif")
original_ic <- read.csv("./Models/Inputs/climate_ic/TCSI_IC6.csv")

shrub_map <- terra::classify(comm_map, select(shrub_cover, MapCode, shrub_cover))
plot(shrub_map)
shrub_map[shrub_map[] > 1] <- 0
plot(shrub_map)
shrub_map <- shrub_map*3500 #(mean shrub biomass = 3500 g m-2) Patton and Underwood 2021 https://www.mdpi.com/2072-4292/13/8/1581#

orig_shrub_bio <- filter(original_ic, SpeciesName %in% c("NOFX_R_SEED", "NOFX_NOR_SEED", "FX_R_SEED")) %>%
  group_by(MapCode) %>%
  summarize(shrub_biomass = sum(CohortBiomass))
orig_shrub_map <- terra::classify(original_map, orig_shrub_bio, others = NA)
plot(orig_shrub_map)

plot_shrub_bio <- data.frame(MapCode = original_map[][,],
                             CohortBiomass = shrub_map[][,]) %>%
  filter(!is.na(MapCode))

plot_shrub_bio2 <- rbind(plot_shrub_bio, plot_shrub_bio, plot_shrub_bio)
plot_shrub_bio2$SpeciesName <- rep(c("NOFX_R_SEED", "NOFX_NOR_SEED", "FX_R_SEED"), each = nrow(plot_shrub_bio))
plot_shrub_bio2$CohortAge <- 10
plot_shrub_bio2$CohortBiomass <- plot_shrub_bio2$CohortBiomass/3
plot_shrub_bio2 <- plot_shrub_bio2[plot_shrub_bio2$CohortBiomass > 0, ]

new_ic <- bind_rows(original_ic, plot_shrub_bio2)


new_ic <- new_ic %>% 
  group_by(MapCode, SpeciesName, CohortAge) %>%
  summarize(CohortBiomass = sum(CohortBiomass))
new_ic <- as.data.frame(new_ic)
new_ic$CohortBiomass <- as.integer(new_ic$CohortBiomass)
new_ic$MapCode <- as.integer(new_ic$MapCode)
new_ic$CohortAge <- as.integer(new_ic$CohortAge)
new_ic <- new_ic[!is.na(new_ic$MapCode), ]

# write.csv(new_ic, "ic_with_shrubs.csv")


#--------------------------------
# add spunup shrubs to ic


new_comm_map <- rast("./Models/Model runs/Scenario2 - shrub test/output-community-30.img")
new_ic <- read.csv("./Models/Model runs/Scenario2 - shrub test/community-input-file-30.csv")

new_ic_shrubs <- filter(new_ic, SpeciesName %in% c("NOFX_R_SEED", "NOFX_NOR_SEED", "FX_R_SEED"))

new_ic_shrub_bio <- new_ic_shrubs %>%
  group_by(MapCode) %>%
  summarize(shrub_biomass = sum(CohortBiomass))
new_shrub_map <- terra::classify(new_comm_map, new_ic_shrub_bio, others = NA)
plot(new_shrub_map)

crosswalk_new_mapcodes <- data.frame(new = new_comm_map[] %>% unname(),
                                     old = original_map[] %>% unname()) %>%
  filter(new != 0)

# orig_ic2 <- original_ic %>% left_join(crosswalk_new_mapcodes, by = c("MapCode" = "old"))
# orig_ic_update <- orig_ic2 %>%
#   select("new", "SpeciesName", "CohortAge", "CohortBiomass") %>%
#   rename(MapCode = new) 
#%>%
  # filter(if_all(.cols = everything(), 
                # .fns = ~ !is.na(.x))) 

new_ic_shrubs2 <- left_join(new_ic_shrubs, crosswalk_new_mapcodes, by = c("MapCode" = "new"))
new_ic_shrubs3 <- new_ic_shrubs2 %>% select("old", "SpeciesName", "CohortAge", "CohortBiomass") %>%
  rename(MapCode = old) %>%
  filter(if_all(.cols = everything(),
                .fns = ~ !is.na(.x)))

# ic_with_new_shrubs <- orig_ic_update %>%
#   filter(!(SpeciesName %in% c("NOFX_R_SEED", "NOFX_NOR_SEED", "FX_R_SEED"))) %>%
#   rbind(new_ic_shrubs)
# new_ic_shrub_bio2 <- ic_with_new_shrubs %>%
#   filter(SpeciesName %in% c("NOFX_R_SEED", "NOFX_NOR_SEED", "FX_R_SEED")) %>%
#   group_by(MapCode) %>%
#   summarize(shrub_biomass = sum(CohortBiomass))
#check new map
# new_shrub_map2 <- terra::classify(new_comm_map, new_ic_shrub_bio2, others = NA)
# plot(new_shrub_map2)
# plot(new_shrub_map)


ic_with_new_shrubs <- rbind(select(original_ic, !X), new_ic_shrubs3)

ic_with_new_shrubs$MapCode <- as.integer(ic_with_new_shrubs$MapCode)
# ic_with_new_shrubs$MapCode[1] <- 0
write.csv(ic_with_new_shrubs, "./Models/Inputs/climate_IC/ic_spunup_shrubs.csv",
          row.names = FALSE)
# ic_with_new_shrubs <- read.csv("./Models/Inputs/climate_IC/ic_spunup_shrubs.csv")

