## example biomass layers
tcsi_mask_9311 <- rast("./Models/Inputs/masks_boundaries/mask_9311.tif")
crs(tcsi_mask_9311)

target_grid <- rast("./Models/Inputs/masks_boundaries/TCSIforesttype.tif")

raster_list <- list.files("./Analysis/example biomass layers/", full.names = TRUE)

carbon_list <- raster_list[c(3,4,5,6)]

carbon_init <- rast(carbon_list) #actually in EPSG: 2163, not 9311
carbon_init <- sum(carbon_init)
plot(carbon_init)
carbon_init_proj <- tcsi_mask_9311
values(carbon_init_proj) <- values(carbon_init)/100
carbon_init_proj[is.na(tcsi_mask_9311)] <- 0
plot(carbon_init_proj)

carbon_1 <- rast(raster_list[7])
plot(carbon_1)
carbon_1_proj <- carbon_init
values(carbon_1_proj) <- values(carbon_1)/100
carbon_1_proj[is.na(tcsi_mask_9311)] <- 0
plot(carbon_1_proj)

# test <- carbon_init_proj - carbon_1_proj
test <- carbon_init - carbon_1_proj
test[is.na(tcsi_mask_9311)] <- 0
plot(test)
hist(test)

abg <- rast(raster_list[8])
plot(abg)
abg_proj <- tcsi_mask_9311
values(abg_proj) <- values(abg)/2/100
abg_proj[tcsi_mask_9311 == 0] <- NA
plot(abg_proj)

totalc_init <- abg_proj*1.5 + carbon_init_proj
plot(totalc_init)
totalc_init[tcsi_mask_9311 == 0] <- 0

totalc <- rast(raster_list[9])
totalc_proj <- tcsi_mask_9311
values(totalc_proj) <- values(totalc)/100
plot(totalc_proj)
test <- totalc_proj - totalc_init
test[totalc_proj == 0] <- 0
plot(test)
hist(test)

abg_out <- abg_proj %>%
  project(target_grid, method = "near")
plot(abg_out)
totalc_out <- totalc_init %>%
  project(target_grid, method = "near")
plot(totalc_out)
somtc_out <- carbon_init_proj %>%
  project(target_grid, method = "near")
plot(somtc_out)
