#draft analysis for management effectiveness

# general idea: use scenario 2 as a BAU "baseline" to compare against
# for each model run for other scenarios, compare area treated and area Rx burned
# to the average for scenario 2 to get an estimate of treatment intensity. 
# For fire risk, use either total DNBR, number of severe fires, or some other metric.
# Subtract for each model run from the mean of scenario 2. Plot, for each model run,
# the treatment against risk of fire. 

# TODO: explore whether aggregating fire helps -- treatments may affect nearby cells, for example
# TODO: explore other metrics of fire severity


library("terra")
library("tidyverse")
library("sf")
library("vioplot")

subset_landscape <- FALSE
maxyear <- 81

get_raster_stack <- function(direct, pattern){
  raster_list <- list.files(direct, pattern = pattern)
  
  matches <- regmatches(list.files(direct, pattern = pattern), 
                        gregexpr("[[:digit:]]+", list.files(direct, pattern = pattern))) %>%
    unlist() %>% 
    as.numeric()
  raster_list <- raster_list[order(matches)]
  
  raster_stack <- terra::rast(paste0(direct, raster_list))
  
  return(raster_stack)
}

collapse_list_to_layers <- function(raster_list){
  stack <- raster_list[[1]][[1]]
  for(i in 2:length(raster_list)){
    add(stack) <- raster_list[[i]][[1]]
  }
  return(stack)
}

select_severe_fire <- function(raster, sev_raster){
  raster2 <- terra::mask(raster, sev_raster)
  return(raster2)
}

diverging_color_ramp <- function(ras){
  the_palette_fc <- leaflet::colorNumeric(palette = "RdBu", 
                                          domain = c(-max(abs(ras[]), na.rm = TRUE), max(abs(ras[]), na.rm = TRUE)),
                                          reverse = TRUE)
  the_colors <- the_palette_fc(seq(min(ras[], na.rm = TRUE), max(ras[], na.rm = TRUE), length.out = 50))
  
}

color_ramp <- function(min, max, palette){
  the_palette_fc <- leaflet::colorNumeric(palette = palette, 
                                          domain = c(min, max))
  the_colors <- the_palette_fc(seq(min, max, length.out = 50))
  
}


project_to_template <- function(input_raster, template){
  #function to project landis input rasters with no CRS to an input file
  
  #replace values of template with values from input raster
  out_raster <- template
  terra::values(out_raster) <- values(input_raster)
  
  return(out_raster)
}


#-------------------------------------------------------------------------------
# mgmt_dirs <- paste0("E:/TCSI LANDIS/LANDIS Runs/Scenario2 - miroc - Run ", c(1:5),"/harvest/")
mgmt_dirs <- paste0("C:/Users/swflake/Documents/TCSI-conservation-finance/Models/Model runs/fire_test_runs/Scenario1 - historical - Run ", c(1:2), "/harvest/")
# mgmt_dirs <- paste0("C:/Users/swflake/Documents/LANDIS inputs/Model runs/Scenario1_equal_fire - miroc - Run ", c(1:2),"/harvest/")

#read in all the harvest biomass-removed layers
mgmt_biomass_runs_list <- mgmt_dirs %>%
  purrr::map(~get_raster_stack(.x, "biomass"))

#for each model run (top-level list element), sum all the rasters to get total
# biomass removed
mgmt_biomass_sums_baseline <- mgmt_biomass_runs_list %>%
  purrr::modify(.f = ~ terra::app(.x, fun=sum))

#convert the list to a raster with layers, and take the average across all the 
# model runs
mgmt_total_biomass_baseline <- collapse_list_to_layers(mgmt_biomass_sums_baseline) %>%
  terra::app(fun=sum)%>%
  `/`(length(mgmt_biomass_sums_baseline))
plot(mgmt_total_biomass_baseline)


####
# mgmt_dirs <- paste0("E:/TCSI LANDIS/LANDIS runs/Scenario6 - miroc - Run ", c(4,7,8,9,10),"/harvest/")
mgmt_dirs <- paste0("C:/Users/swflake/Documents/TCSI-conservation-finance/Models/Model runs/fire_test_runs/Scenario6 - historical - Run ", c(1:2), "/harvest/")
#mgmt_dirs <- paste0("C:/Users/swflake/Documents/LANDIS inputs/Model runs/Scenario6_equal_fire - miroc - Run ", c(1:2),"/harvest/")

biomass_runs_list <- mgmt_dirs %>%
  purrr::map(~get_raster_stack(.x, "biomass"))

mgmt_biomass_sums6 <- biomass_runs_list %>%
  purrr::modify(.f = ~ terra::app(.x, fun=sum))

mgmt_total_biomass6 <- collapse_list_to_layers(mgmt_biomass_sums6) %>%
  terra::app(fun = sum) %>%
  `/`(length(mgmt_biomass_sums6))

### Difference in biomass from average of scenario1
#
# biomass_diff <- biomass_sums %>%
#   purrr::modify(.f = ~ .x - total_biomass_baseline)
#
# biomass_diff_mean6 <- collapse_list_to_layers(biomass_diff) %>%
#   terra::app(fun = sum) %>%
#   `/`(length(biomass_diff))

plot(mgmt_total_biomass6)
#-------------------------------------------------------------------------------
## fire
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Scenario 1 -- reference
#-------------------------------------------------------------------------------

# fire_dirs <- paste0("E:/TCSI LANDIS/LANDIS runs/Scenario2 - miroc - Run ", c(1:5),"/social-climate-fire/")
#fire_dirs <- paste0("C:/Users/swflake/Documents/LANDIS inputs/Model runs/Scenario1_equal_fire - miroc - Run ", c(1:2),"/social-climate-fire/")
fire_dirs <- paste0("C:/Users/swflake/Documents/TCSI-conservation-finance/Models/Model runs/fire_test_runs/Scenario1 - historical - Run ", c(1:2), "/social-climate-fire/")

biomass_fire_runs_list <- fire_dirs %>% 
  purrr::map(.f = ~get_raster_stack(.x, "biomass-mortality"))

#get a stack of rasters identifying where wildfire happened each year
wildfire_mask_stack_list <- fire_dirs %>%
  purrr::map(.f = ~get_raster_stack(.x, "ignition")) %>%
  purrr::map(.f = ~ lapply(.x, FUN = function(x){terra::classify(x, rcl = matrix(data = c(2, 3, 1), 
                                                                                 byrow = TRUE, 
                                                                                 ncol = 3),
                                                            right = TRUE, include.lowest = TRUE, 
                                                            others = NA)}))

#get a stack of rasters identifying where rx fire happened each year
rx_mask_stack_list <- fire_dirs %>%
  purrr::map(.f = ~get_raster_stack(.x, "ignition")) %>%
  purrr::map(.f = ~ lapply(.x, FUN = function(x){terra::classify(x, rcl = matrix(data = c(1, 0, 2, 0, 3, 0, 4, 1), 
                                                                                 byrow = TRUE,
                                                                                 ncol = 2),
                                                                 others = NA)}))

#get a stack of high severity fires to mask out beneficial fire
mat <- matrix(c(0, 450, 0, 450, 2001, 1), ncol = 3, byrow = TRUE)
severe_mask_stack_list <- fire_dirs %>%
  purrr::map(.f = ~get_raster_stack(.x, "dnbr")) %>%
  purrr::map(.f = ~ lapply(.x, FUN = function(x){terra::classify(x, rcl = mat,
                                                                 right = TRUE, include.lowest = TRUE, 
                                                                 others = NA)}))

#mask out rx fires, leaving just wildfire. Maskvalue = 1 (assigned above) with inverse = TRUE 
# will assign everything else to NA
wildfire_biomass_stack <- biomass_fire_runs_list

for(j in 1:length(biomass_fire_runs_list)){
  for(i in c(1:maxyear)){
    wildfire_biomass_stack[[j]][[i]] <- terra::mask(biomass_fire_runs_list[[j]][[i]], 
                                                    wildfire_mask_stack_list[[j]][[i]], 
                                                    maskvalue = 1, 
                                                    inverse = TRUE)
  }
}

#mask out wildfire, leaving just rx fires
rx_biomass_stack <- biomass_fire_runs_list

for(j in 1:length(biomass_fire_runs_list)){
  for(i in c(1:81)){
    rx_biomass_stack[[j]][[i]] <- terra::mask(biomass_fire_runs_list[[j]][[i]], 
                                              rx_mask_stack_list[[j]][[i]], 
                                              maskvalue = 1, 
                                              inverse = TRUE)
  }
}

#mask out wildfire, leaving just rx fires
high_sev_biomass_stack <- biomass_fire_runs_list

for(j in 1:length(biomass_fire_runs_list)){
  for(i in c(1:81)){
    high_sev_biomass_stack[[j]][[i]] <- terra::mask(biomass_fire_runs_list[[j]][[i]], 
                                              severe_mask_stack_list[[j]][[i]], 
                                              maskvalue = 1, 
                                              inverse = TRUE)
  }
}

## calculate totals for each model run and across all model runs

wildfire_totals <- wildfire_biomass_stack %>%
  purrr::modify(.f = ~ terra::app(.x, fun=function(x){sum(x, na.rm = TRUE)}))

total_wildfire_baseline <- collapse_list_to_layers(wildfire_totals) %>%
  terra::app(fun=mean)
plot(total_wildfire_baseline)

severe_totals_baseline <- high_sev_biomass_stack %>%
  purrr::modify(.f = ~ terra::app(.x, fun=function(x){sum(x, na.rm = TRUE)}))

total_severe_baseline <- collapse_list_to_layers(severe_totals_baseline) %>%
  terra::app(fun=mean) 
plot(total_severe_baseline)

plot(total_wildfire_baseline - total_severe_baseline)


rx_biomass_totals1 <- rx_biomass_stack %>%
  purrr::modify(.f = ~ terra::app(.x, fun=function(x){sum(x, na.rm = TRUE)}))

total_rx1 <- collapse_list_to_layers(rx_biomass_totals1) %>%
  terra::app(fun=mean)
plot(total_rx1)



## update biomass removed to add in Rx fire
biomass_with_rx_sums_baseline <- mgmt_biomass_sums_baseline %>%
  purrr::map2(.x = ., .y = rx_biomass_totals1, .f = ~ .x + .y)
biomass_with_rx_mean1 <- biomass_with_rx_sums_baseline %>%
  collapse_list_to_layers() %>%
  terra::app(fun = sum) %>%
  `/`(5)
plot(rx_biomass_totals1[[1]])
plot(mgmt_biomass_sums_baseline[[1]])
plot(biomass_with_rx_sums_baseline[[1]]/100)

#-------------------------------------------------------------------------------
# Scenario 6 -- comparison
#-------------------------------------------------------------------------------

# fire_dirs <- paste0("E:/TCSI LANDIS/LANDIS runs/Scenario6 - miroc - Run ", c(4,7,8,9,10),"/social-climate-fire/")
mgmt_dirs <- paste0("C:/Users/swflake/Documents/TCSI-conservation-finance/Models/Model runs/fire_test_runs/Scenario6 - historical - Run ", c(1:2), "/social-climate-fire/")
#fire_dirs <- paste0("C:/Users/swflake/Documents/LANDIS inputs/Model runs/Scenario6_equal_fire - miroc - Run ", c(1:2),"/social-climate-fire/")


#TODO
#this isn't all of the biomass, but pretty close -- run new models with biomass outputs in SCRPPLE
biomass_fire_runs_list <- fire_dirs %>% 
  purrr::map(.f = ~get_raster_stack(.x, "biomass-mortality"))

#get a stack of rasters identifying where wildfire happened each year
wildfire_mask_stack_list <- fire_dirs %>%
  purrr::map(.f = ~get_raster_stack(.x, "ignition")) %>%
  purrr::map(.f = ~ lapply(.x, FUN = function(x){terra::classify(x, rcl = matrix(data = c(2, 3, 1), 
                                                                                 byrow = TRUE, 
                                                                                 ncol = 3),
                                                                 right = TRUE, include.lowest = TRUE, 
                                                                 others = NA)}))

#get a stack of rasters identifying where rx fire happened each year
rx_mask_stack_list <- fire_dirs %>%
  purrr::map(.f = ~get_raster_stack(.x, "ignition")) %>%
  purrr::map(.f = ~ lapply(.x, FUN = function(x){terra::classify(x, rcl = matrix(data = c(1, 0, 2, 0, 3, 0, 4, 1), 
                                                                                 byrow = TRUE,
                                                                                 ncol = 2),
                                                                 others = NA)}))



#get a stack of high severity fires to mask out beneficial fire
mat <- matrix(c(0, 450, 0, 450, 2001, 1), ncol = 3, byrow = TRUE)
severe_mask_stack_list <- fire_dirs %>%
  purrr::map(.f = ~get_raster_stack(.x, "dnbr")) %>%
  purrr::map(.f = ~ lapply(.x, FUN = function(x){terra::classify(x, rcl = mat,
                                                                 right = TRUE, include.lowest = TRUE, 
                                                                 others = NA)}))

#mask out rx fires, leaving just wildfire. Maskvalue = 1 (assigned above) with inverse = TRUE 
# will assign everything else to NA
wildfire_biomass_stack <- biomass_fire_runs_list

for(j in 1:length(biomass_fire_runs_list)){
  for(i in c(1:81)){
    wildfire_biomass_stack[[j]][[i]] <- terra::mask(biomass_fire_runs_list[[j]][[i]], 
                                                    wildfire_mask_stack_list[[j]][[i]], 
                                                    maskvalue = 1, 
                                                    inverse = TRUE)
  }
}

#mask out wildfire, leaving just rx fires
rx_biomass_stack <- biomass_fire_runs_list

for(j in 1:length(biomass_fire_runs_list)){
  for(i in c(1:81)){
    rx_biomass_stack[[j]][[i]] <- terra::mask(biomass_fire_runs_list[[j]][[i]], 
                                              rx_mask_stack_list[[j]][[i]], 
                                              maskvalue = 1, 
                                              inverse = TRUE)
  }
}

#mask out wildfire, leaving just rx fires
high_sev_biomass_stack <- biomass_fire_runs_list

for(j in 1:length(biomass_fire_runs_list)){
  for(i in c(1:81)){
    high_sev_biomass_stack[[j]][[i]] <- terra::mask(biomass_fire_runs_list[[j]][[i]], 
                                                    severe_mask_stack_list[[j]][[i]], 
                                                    maskvalue = 1, 
                                                    inverse = TRUE)
  }
}


## calculate totals for each model run and across all model runs

wildfire_totals <- wildfire_biomass_stack %>%
  purrr::modify(.f = ~ terra::app(.x, fun=function(x){sum(x, na.rm = TRUE)}))

total_wildfire6 <- collapse_list_to_layers(wildfire_totals) %>%
  terra::app(fun=mean)
plot(total_wildfire6)

severe_frequency <- high_sev_biomass_stack %>%
  purrr::modify(.f = ~ terra::app(.x, fun=function(x){sum(x, na.rm = TRUE)}))

total_severe6 <- collapse_list_to_layers(severe_frequency) %>%
  terra::app(fun=mean)
plot(total_severe6)


rx_biomass_totals6 <- rx_biomass_stack %>%
  purrr::modify(.f = ~ terra::app(.x, fun=function(x){sum(x, na.rm = TRUE)}))

total_rx6 <- collapse_list_to_layers(rx_biomass_totals6) %>%
  terra::app(fun=mean)
plot(total_rx6)


## update biomass removed to add in Rx fire
biomass_with_rx_sums6 <- mgmt_biomass_sums6 %>%
  purrr::map2(.x = ., .y = rx_biomass_totals6, .f = ~ .x + .y)
biomass_with_rx_mean6 <- biomass_with_rx_sums6 %>%
  collapse_list_to_layers() %>%
  terra::app(fun = sum) %>%
  `/`(5)

#make sure things got added up right
plot(rx_biomass_totals6[[1]])
plot(mgmt_biomass_sums6[[1]])
plot(biomass_with_rx_sums6[[1]]/100)

#-------------------------------------------------------------------------------
# Calculate diffrences in treatment and wildfire
#-------------------------------------------------------------------------------

### Difference in biomass from average of scenario1

biomass_diff <- mgmt_biomass_sums6 %>%
  purrr::modify(.f = ~ .x - mgmt_total_biomass_baseline)

biomass_diff_mean6 <- collapse_list_to_layers(biomass_diff) %>%
  terra::app(fun = sum) %>%
  `/`(length(biomass_diff))

plot(total_wildfire1)
plot(total_wildfire6)

wildfire_diff <- total_wildfire6 %>%
  purrr::modify(.f = ~ .x - total_wildfire1)

plot(wildfire_diff, col = diverging_color_ramp(wildfire_diff))

# wildfire_diff_mean6 <- collapse_list_to_layers(wildfire_diff) %>%
#   terra::app(fun = sum) %>%
#   `/`(length(wildfire_diff))





#-------------------------------------------------------------------------------
# Analyze differences in fire regimes on a per-scenario average basis
#-------------------------------------------------------------------------------
diff_data <- data.frame(wildfire = values(wildfire_diff),
                        harvest = values(biomass_diff_mean6)) %>%
  rename(wildfire = `mean`,
         harvest = `sum`) %>%
  filter(harvest != 0 &
           wildfire != 0)

effectiveness_diagram <- ggplot(data = diff_data, mapping = aes(x = harvest, y = wildfire)) +
  geom_hex(bins = 100) + 
  scale_y_reverse()
plot(effectiveness_diagram)

plot(values(wildfire_diff) ~ values(biomass_diff_mean6))

severe_ref <- total_severe1
# severe_ref <- clamp(total_severe1, 0, 7)

severe_diff <- severe_frequency %>%
  purrr::modify(.f = ~ .x - severe_ref)

severe_diff_sum <- collapse_list_to_layers(severe_diff) %>%
  terra::app(fun=mean)
values(severe_diff_sum)[values(severe_diff_sum) > 0] <- 0
NAflag(severe_diff_sum) <- 0
plot(severe_diff_sum)

effective_sites <- wildfire_diff
values(effective_sites) <- NA
values(effective_sites) <- ifelse(values(wildfire_diff) < quantile(values(wildfire_diff), 0.25) & 
                                    values(biomass_diff_mean6 < quantile(values(wildfire_diff), 0.25)), 1, NA)
values(effective_sites) <- ifelse(values(wildfire_diff) < quantile(values(wildfire_diff), 0.5) & 
                                    values(biomass_diff_mean6 < quantile(values(wildfire_diff), 0.5)), 1, NA)
values(effective_sites) <- ifelse(values(wildfire_diff) < -2000 & 
                                    values(biomass_diff_mean6) < 10000, 1, NA)
plot(effective_sites, col = "blue")

effective_sites <- wildfire_diff
values(effective_sites) <- NA
values(effective_sites) <- ifelse(values(wildfire_diff) > quantile(values(wildfire_diff), 0.75) & 
                                    values(biomass_diff_mean6 < quantile(values(wildfire_diff), 0.25)), 1, NA)
values(effective_sites) <- ifelse(values(wildfire_diff) > quantile(values(wildfire_diff), 0.75) & 
                                    values(biomass_diff_mean6 < quantile(values(wildfire_diff), 0.5)), 1, NA)
plot(effective_sites, col = "blue")

#-------------------------------------------------------------------------------
# Effect of spatial scale/aggregation with moving window
#-------------------------------------------------------------------------------

#TODO what to do about places where the biomass removed is the same for scenarios?

smoothed_fire_diff_mean6 <- terra::focal(wildfire_diff, 
                              w = 15,
                              fun = "mean", 
                              na.rm = TRUE, na.policy = "omit")

plot(smoothed_fire_diff_mean6)

smoothed_biomass <- terra::focal(biomass_diff[[1]], 
                                 w = 3,
                                 fun = "mean", 
                                 na.rm = TRUE, na.policy = "omit")
  
plot(values(smoothed_fire_diff_mean6)*-1 ~ values(smoothed_biomass),
     xlab = "Biomass harvested or Rx burned",
     ylab = "Biomass burned by wildfire")  

smoothed_fire[smoothed_fire_diff_mean6 == 0] <- NA
smoothed_biomass[smoothed_biomass_diff_mean6 == 0] <- NA

test <- (smoothed_fire_diff_mean6 * -1) / smoothed_biomass

test <- terra::clamp(test, lower = -2, upper = 2, values = TRUE)

plot(test)

test2 <- (wildfire_diff[[1]] * -1) / biomass_diff[[1]] 
test2 <- terra::clamp(test2, lower = -2, upper = 2, values = TRUE)
plot(test2)


severe_diff_sum_smooth <- terra::focal(severe_diff_sum, w = 3, fun = "mean",
                                       na.rm = TRUE, na.policy = "omit")

severe_diff_sum_smooth <- terra::subst(severe_diff_sum_smooth, 0, NA)

plot(severe_diff_sum_smooth)

#---------
# smoothed fire ~ average diffrence in biomass removed (not smoothed)
plot(smoothed_fire_diff_mean6)
plot(biomass_diff_mean6, col = diverging_color_ramp(biomass_diff_mean6))
plot(values(smoothed_fire_diff_mean6) ~ values(biomass_diff_mean6))


#-------------- Ratio of prevented fire / treated area
accidental_ignitions <- terra::rast("./Models/Inputs/input_rasters_tcsi/accidental_tcsi.tif")
lightning_ignitions <- terra::rast("./Models/Inputs/input_rasters_tcsi/lightning_tcsi.tif")


ratio <- -severe_diff_sum / biomass_diff_mean6
values(ratio)[values(biomass_diff_mean6) < 0] <- NA
values(ratio)[values(severe_diff_sum) > 0] <- NA
ratio <- clamp(ratio, -5, 10)
ratio <- project_to_template(ratio, tcsi_mask_terra)
plot(ratio, col = diverging_color_ramp(ratio))


ratio_vals <- values(ratio)
accidental_vals <- values(accidental_ignitions)
lightning_vals <- values(lightning_ignitions)

ignition_df <- data.frame(ratio = as.numeric(values(severe_diff_sum))/100,
                          accidental = as.numeric(accidental_vals),
                          lightning = as.numeric(lightning_vals))

ggplot(ignition_df) +
  geom_hex(aes(x = accidental, y = ratio)) + 
  geom_smooth(aes(x = accidental, y = ratio), method = "lm") + 
  xlab("Accidental ignition index") + 
  ylab("Prevented severe fire (Mg ha-1)")
ggplot(ignition_df) +
  geom_hex(aes(x = lightning, y = ratio))+ 
  geom_smooth(aes(x = lightning, y = ratio), method = "lm")+ 
  xlab("Lightning ignition index")+ 
  ylab("Prevented severe fire (Mg ha-1)")

plot(values(severe_diff_sum) ~ values(accidental_ignitions))
abline(lm(values(severe_diff_sum) ~ values(accidental_ignitions)))
plot(values(severe_diff_sum) ~ values(lightning_ignitions))
abline(lm(values(severe_diff_sum) ~ values(lightning_ignitions)))

wui_threat <- sf::st_read("./wui/wui_thread.shp")
wui_defense <- sf::st_read("./wui/wui_def.shp")

wui_threat_ratio <- terra::extract(ratio, vect(wui_threat))
boxplot(wui_threat_ratio)

wui_defense_ratio <- terra::extract(ratio, vect(wui_defense))
boxplot(wui_defense_ratio)



################################################################################
### Figures --------------------------------------------------------------------
################################################################################

#terra options: https://www.rdocumentation.org/packages/terra/versions/0.5-2/topics/plot

library("basemaps")
library("tidyterra")

#import boundary data
tcsi_mask_terra <- rast("./Models/Inputs/masks_boundaries/mask_9311.tif") 
tcsi_mask_nad83 <- rast("./Models/Inputs/masks_boundaries/mask_nad83.tif")
# empty_mask <- tcsi_mask
# empty_mask$mask.tif <- 0
tcsi_shape <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  st_transform(crs = "EPSG:9311")
tahoe_shape <- sf::st_read("./Parameterization/calibration data/Tahoe_Soils_and_Hydro_Data/Tahoe_Soils_and_Hydro_Data.shp") %>%
  st_transform(crs = "EPSG:9311")

#treatment intensity -------

scenario1_biomass_treated <- project_to_template(biomass_with_rx_sums_baseline[[1]]/100, tcsi_mask_terra)
NAflag(scenario1_biomass_treated) <- 0
plot(scenario1_biomass_treated, 
     range = c(0,500), 
     col = color_ramp(0, 500, "viridis")
     )
plot(vect(tcsi_shape), add = TRUE)
plot(vect(tahoe_shape), add = TRUE)

scenario6_biomass_treated <- project_to_template(biomass_with_rx_sums6[[1]]/100, tcsi_mask_terra)
NAflag(scenario6_biomass_treated) <- 0
plot(scenario6_biomass_treated, range = c(0, 500), col = color_ramp(0, 500, "viridis"))
plot(vect(tcsi_shape), add = TRUE)
plot(vect(tahoe_shape), add = TRUE)


difference_biomass_treated <- scenario6_biomass_treated - scenario1_biomass_treated
plot(scenario6_biomass_treated, col = diverging_color_ramp(scenario6_biomass_treated))
plot(vect(tcsi_shape), add = TRUE)
plot(vect(tahoe_shape), add = TRUE)

#Wildfire mortality -----------------------
scenario1_wildfire <- project_to_template(total_severe1[[1]]/100, tcsi_mask_terra)
NAflag(scenario1_wildfire ) <- 0
plot(scenario1_wildfire , range = c(0, 200), col = color_ramp(0, 200, "viridis"))
plot(vect(tcsi_shape), add = TRUE)
plot(vect(tahoe_shape), add = TRUE)

scenario6_wildfire <- project_to_template(total_severe6[[1]]/100, tcsi_mask_terra)
NAflag(scenario6_wildfire ) <- 0
plot(scenario6_wildfire , range = c(0, 200), col = color_ramp(0, 200, "viridis"))
plot(vect(tcsi_shape), add = TRUE)
plot(vect(tahoe_shape), add = TRUE)


wildfire_mort_diff <- project_to_template(severe_diff_sum_smooth/100, tcsi_mask_terra)
plot(wildfire_mort_diff, col = diverging_color_ramp(wildfire_mort_diff))
plot(vect(tcsi_shape), add = TRUE)
plot(vect(tahoe_shape), add = TRUE)

#------ Effectiveness diagram----------------
diff_data <- data.frame(wildfire = values(severe_diff_sum)/100,
                        harvest = values(biomass_diff_mean6)/100) %>%
  rename(wildfire = `mean`,
         harvest = `sum`) %>%
  filter(harvest != 0 &
           wildfire != 0)

median(diff_data[diff_data$harvest >= 0, ]$wildfire)
median(diff_data[diff_data$harvest >= 0, ]$harvest)
quantile(diff_data[diff_data$harvest >= 0, ]$wildfire, 0.75)
quantile(diff_data[diff_data$harvest >= 0, ]$harvest, 0.75)

effectiveness_diagram <- ggplot(data = diff_data[diff_data$harvest >= 0, ], 
                                mapping = aes(x = harvest, y = wildfire,
                                              color= I(ifelse(harvest < 95.26 & wildfire < -8.54 , '#1b9e77', 
                                                              ifelse(harvest > 95.26 & wildfire > -8.54, '#d95f02', "#7570b3"))),
                                              fill = I(ifelse(harvest < 95.26 & wildfire < -8.54 , '#1b9e77', 
                                                              ifelse(harvest > 95.26 & wildfire > -8.54, '#d95f02', "#7570b3"))))) +
  geom_hex(bins = 60) + 
  scale_y_reverse() + 
  xlab("Biomass killed by harvest and Rx fire (Mg ha-1)") +
  ylab("Biomass killed by severe wildfire (Mg ha-1)")
plot(effectiveness_diagram)

#---------- Effectiveness map ------------------
#TODO add a real mask instead of just setting NAflag = 0 (some might be real zeroes)
severe_diff_sum2 <- severe_diff_sum/100
biomass_diff_mean62 <- biomass_diff_mean6/100
NAflag(biomass_diff_mean62) <- 0
effective_sites <- severe_diff_sum
values(effective_sites) <- NA
values(effective_sites) <- ifelse(values(severe_diff_sum2) < quantile(values(severe_diff_sum2), 0.5, na.rm = TRUE) & 
                                    values(biomass_diff_mean62 < quantile(values(biomass_diff_mean62), 0.5, na.rm = TRUE)), 1, NA)
values(effective_sites) <- ifelse(values(severe_diff_sum2) > quantile(values(severe_diff_sum2), 0.5, na.rm = TRUE) & 
                                    values(biomass_diff_mean62 > quantile(values(biomass_diff_mean62), 0.5, na.rm = TRUE)), 2, values(effective_sites))
effective_sites <- project_to_template(effective_sites, tcsi_mask_terra)
plot(effective_sites, col = data.frame(value = c(1,2),
                                       col = c('#1b9e77', '#d95f02')))
plot(vect(tcsi_shape), add = TRUE)
plot(vect(tahoe_shape), add = TRUE)


#------- make hex ----------------
effective_sites[][effective_sites[] == 2] <- -1
plot(effective_sites)
hexgrid <- sf::st_make_grid(tcsi_shape, cellsize = 5000, square = FALSE) %>%
  st_as_sf()
plot(hexgrid, add = TRUE)
hexgrid$extract <- terra::extract(effective_sites, vect(hexgrid), fun = sum, na.rm = TRUE, ID = FALSE)$mask_old
plot(hexgrid["extract"])

hexgrid <- hexgrid %>%
  sf::st_intersection(tcsi_shape)
ggplot() +
  geom_sf(data = hexgrid, aes(col = extract, fill = extract)) +
  colorspace::scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0)+
  colorspace::scale_color_continuous_divergingx(palette = 'RdBu', mid = 0) +
  geom_sf(data = tahoe_shape, fill = "white") +
  geom_sf(data = tcsi_shape, fill = NA) +
  labs(fill='Cost-effectiveness') +
  guides(color = "none")

#------ calculate effectiveness differently
cont_eff <- -severe_diff_sum2/biomass_diff_mean62
cont_eff2 <-  cont_eff %>%
  terra::clamp(0, 1) %>%
  project_to_template(tcsi_mask_terra) %>%
  focal(w=matrix(1, 3, 3), mean, na.rm = TRUE) %>%
  # focal(w=matrix(1, 13, 13), mean, na.rm = TRUE) %>%
  # terra::aggregate(2, fun = mean, na.rm = TRUE) %>%
  mask(tcsi_shape)
plot(cont_eff2, col = color_ramp(0, 1, palette = "Greens"))
plot(vect(tcsi_shape), add = TRUE)
plot(vect(tahoe_shape), add = TRUE, col = "white")

writeRaster(cont_eff2, "management_effectiveness_continuous.tif")

# Trash can --------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Analyze difference in fire regimes between treatments, all replicates
#-------------------------------------------------------------------------------

#quadrant map for scenario 6 averages
plot(values(smoothed_fire_diff_mean6) * -1 ~ values(biomass_diff_mean6))

#quadrant map for each individual run, scenario 6
plot(values(wildfire_diff[[1]]) * -1 ~ values(biomass_diff[[1]]))
plot(values(severe_diff[[4]]) * -1 ~ values(biomass_diff[[4]]))


wildfire_all <- wildfire_diff %>%
  purrr::map(.f = terra::values) %>%
  unlist

wildfire_all_smooth3 <- wildfire_diff %>%
  purrr::modify(.f = ~ terra::focal(.x, w = 3, fun = "mean",
                                   na.rm = TRUE, na.policy = "omit")) %>%
  purrr::map(.f = terra::values) %>%
  unlist

wildfire_all_smooth9 <- wildfire_diff %>%
  purrr::modify(.f = ~ terra::focal(.x, w = 9, fun = "mean",
                                    na.rm = TRUE, na.policy = "omit")) %>%
  purrr::map(.f = terra::values) %>%
  unlist

wildfire_all_smooth15 <- wildfire_diff %>%
  purrr::modify(.f = ~ terra::focal(.x, w = 15, fun = "mean",
                                    na.rm = TRUE, na.policy = "omit")) %>%
  purrr::map(.f = terra::values) %>%
  unlist


biomass_all <- biomass_diff %>%
  purrr::map(.f = terra::values) %>%
  unlist

all_quadrant_data <- data.frame(
                                wildfire = wildfire_all_smooth15 * -1,
                                biomass = biomass_all
                              )

#remove to plot values; keep zeros if you want to back-transform to raster
all_quadrant_data <- all_quadrant_data %>%
  filter(wildfire != 0 & biomass!= 0)

ggplot(data = all_quadrant_data, mapping = aes(x = biomass, y = wildfire)) +
  geom_hex()
  # geom_bin_2d()


summary(lm(wildfire ~ biomass, data = all_quadrant_data))


######
all_quadrant_data <- data.frame(
  wildfire = wildfire_all * -1,
  biomass = biomass_all
)

all_quadrant_data <- all_quadrant_data %>%
  filter(wildfire != 0 & biomass!= 0)

ggplot(data = all_quadrant_data) +
  geom_hex(mapping = aes(x = biomass, y = wildfire))
summary(lm(wildfire ~ biomass, data = all_quadrant_data))

#-------------------------------------------------------------------------------
# Analyze difference in fire regimes between treatments, averages

# fire_diff <- total_wildfire6 - total_wildfire1
fire_diff <- total_severe6 - total_severe1

fire_diff3 <- terra::aggregate(fire_diff, fact = 3, fun = "mean")

plot(fire_diff)
fire_diff2 <- terra::subst(fire_diff, 0, NA)
plot(fire_diff2, col = diverging_color_ramp(fire_diff2)) #treatment "moves" fire to higher elevation

##

total_biomass6 <- subst(total_biomass6, 0, NA)
total_biomass6_2 <- terra::aggregate(total_biomass6, fact = 3, fun = "median")
plot(values(fire_diff2) * -1 ~ values(total_biomass6))
plot(values(fire_diff3) * -1 ~ values(total_biomass6_2))
abline(h = 0)
abline(v = 20000*5)

##

#map of ratio of prevented fire to biomass removed
total_biomass6 <- subst(total_biomass6, 0, NA)
test <- (fire_diff3 * -1) / (total_biomass6_2)

test[test < -2] <- NA
test[test > 2] <- NA
plot(test)

test2 <- test
test2[test2 < 0] <- NA

test2 <- sqrt(test2)
plot(test2)

