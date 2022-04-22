#draft analysis for management effectiveness

# general idea: use scenario 1 as a "baseline" to compare against
# for each model run for other scenarios, compare area treated and area Rx burned
# to the average for scenario 1 to get an estimate of treatment intensity. 
# For fire risk, use either total DNBR, number of severe fires, or some other metric.
# Subtract for each model run from the mean of scenario 1. Plot, for each model run,
# the treatment against risk of fire. 

# TODO: explore whether aggregating fire helps -- treatments may affect nearby cells, for example
# TODO: explore other metrics of fire severity


library("terra")
library("tidyverse")
library("sf")
library("vioplot")

subset_landscape <- TRUE

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

select_severe_fire <- function(raster){
  mat <- matrix(c(0, 400, 0, 400, 2001, 1), ncol = 3, byrow = TRUE)
  raster2 <- terra::classify(raster, rcl = mat)
  return(raster2)
}

#-------------------------------------------------------------------------------
mgmt_dirs <- paste0("E:/tcsi_for_nick/Scenario1 - historical - Run ", c(1:5),"/harvest/")

biomass_runs_list <- mgmt_dirs %>%
  purrr::map(~get_raster_stack(.x, "biomass"))

biomass_sums1 <- biomass_runs_list %>%
  purrr::modify(.f = ~ terra::app(.x, fun=sum))

total_biomass1 <- collapse_list_to_layers(biomass_sums1) %>%
  terra::app(fun=sum)%>%
  `/`(length(biomass_sums1))


####
mgmt_dirs <- paste0("E:/tcsi_for_nick/Scenario6 - historical - Run ", c(1:5),"/harvest/")

biomass_runs_list <- mgmt_dirs %>%
  purrr::map(~get_raster_stack(.x, "biomass"))

biomass_sums6 <- biomass_runs_list %>%
  purrr::modify(.f = ~ terra::app(.x, fun=sum))

total_biomass6 <- collapse_list_to_layers(biomass_sums6) %>%
  terra::app(fun = sum) %>%
  `/`(length(biomass_sums6))

### Difference in biomass from average of scenario1
# 
# biomass_diff <- biomass_sums %>%
#   purrr::modify(.f = ~ .x - total_biomass1)
# 
# biomass_diff_mean6 <- collapse_list_to_layers(biomass_diff) %>%
#   terra::app(fun = sum) %>%
#   `/`(length(biomass_diff))


#-------------------------------------------------------------------------------
## fire
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Scenario 1 -- reference
#-------------------------------------------------------------------------------

fire_dirs <- paste0("E:/tcsi_for_nick/Scenario1 - historical - Run ", c(1:5),"/social-climate-fire/")

dnbr_runs_list <- fire_dirs %>% 
  purrr::map(.f = ~get_raster_stack(.x, "dnbr"))

#TODO
#this isn't all of the biomass, but pretty close -- run new models with biomass outputs in SCRPPLE
biomass_fire_runs_list <- fire_dirs %>% 
  purrr::map(.f = ~get_raster_stack(.x, "special-dead-wood"))

wildfire_mask_stack_list <- fire_dirs %>%
  purrr::map(.f = ~get_raster_stack(.x, "ignition")) %>%
  purrr::map(.f = ~ lapply(.x, FUN = function(x){terra::classify(x, rcl = c(2, 3), 
                                                            right = TRUE, include.lowest = TRUE, 
                                                            othersNA = TRUE)}))
rx_mask_stack_list <- fire_dirs %>%
  purrr::map(.f = ~get_raster_stack(.x, "ignition")) %>%
  purrr::map(.f = ~ lapply(.x, FUN = function(x){terra::classify(x, rcl = matrix(data = c(1, 0, 2, 0, 3, 0, 4, 1), 
                                                                                 byrow = TRUE,
                                                                                 ncol = 2),
                                                                 othersNA = TRUE)}))

wildfire_dnbr_stack <- dnbr_runs_list

for(j in 1:length(dnbr_runs_list)){
  for(i in c(1:81)){
    wildfire_dnbr_stack[[j]][[i]] <- terra::mask(dnbr_runs_list[[j]][[i]], wildfire_mask_stack_list[[j]][[i]])
  }
}

rx_biomass_stack <- biomass_fire_runs_list

for(j in 1:length(biomass_fire_runs_list)){
  for(i in c(1:81)){
    rx_biomass_stack[[j]][[i]] <- terra::mask(biomass_fire_runs_list[[j]][[i]], rx_mask_stack_list[[j]][[i]])
  }
}

## calculate totals for each model run and across all model runs

wildfire_totals <- wildfire_dnbr_stack %>%
  purrr::modify(.f = ~ terra::app(.x, fun=function(x){sum(x, na.rm = TRUE)}))

total_wildfire1 <- collapse_list_to_layers(wildfire_totals) %>%
  terra::app(fun=sum)
plot(total_wildfire1)

severe_frequency <- wildfire_dnbr_stack %>%
  purrr::lmap(.f = ~ lapply(.x, FUN = select_severe_fire)) %>% #reclassify each year
  purrr::modify(.f = ~ terra::app(.x, fun=function(x){sum(x, na.rm = TRUE)}))

total_severe1 <- collapse_list_to_layers(severe_frequency) %>%
  terra::app(fun=sum)
plot(total_severe1)


rx_biomass_totals1 <- rx_biomass_stack %>%
  purrr::modify(.f = ~ terra::app(.x, fun=function(x){sum(x, na.rm = TRUE)}))

total_rx1 <- collapse_list_to_layers(rx_biomass_totals1) %>%
  terra::app(fun=sum)
plot(total_rx1)


fire_summary1 <- read.csv("E:/tcsi_for_nick/Scenario1 - historical - Run 1/scrapple-summary-log.csv")


## update biomass removed to add in Rx fire
biomass_with_rx_sums1 <- biomass_sums1 %>%
  purrr::map2(.x = ., .y = rx_biomass_totals1, .f = ~ .x + .y)
biomass_with_rx_mean1 <- biomass_with_rx_sums1 %>%
  collapse_list_to_layers() %>%
  terra::app(fun = sum) %>%
  `/`(5)
plot(rx_biomass_totals1[[1]])
plot(biomass_sums1[[1]])
plot(biomass_with_rx_sums1[[1]])

#-------------------------------------------------------------------------------
# Scenario 6 -- comparison
#-------------------------------------------------------------------------------

fire_dirs <- paste0("E:/tcsi_for_nick/Scenario6 - historical - Run ", c(1:5),"/social-climate-fire/")

dnbr_runs_list <- fire_dirs %>% 
  purrr::map(.f = ~get_raster_stack(.x, "dnbr"))

#TODO
#this isn't all of the biomass, but pretty close -- run new models with biomass outputs in SCRPPLE
biomass_fire_runs_list <- fire_dirs %>% 
  purrr::map(.f = ~get_raster_stack(.x, "special-dead-wood"))

wildfire_mask_stack_list <- fire_dirs %>%
  purrr::map(.f = ~get_raster_stack(.x, "ignition")) %>%
  purrr::map(.f = ~ lapply(.x, FUN = function(x){terra::classify(x, rcl = c(2, 3), 
                                                                 right = TRUE, include.lowest = TRUE, 
                                                                 othersNA = TRUE)}))
rx_mask_stack_list <- fire_dirs %>%
  purrr::map(.f = ~get_raster_stack(.x, "ignition")) %>%
  purrr::map(.f = ~ lapply(.x, FUN = function(x){terra::classify(x, rcl = matrix(data = c(1, 0, 2, 0, 3, 0, 4, 1), 
                                                                                 byrow = TRUE,
                                                                                 ncol = 2),
                                                                 othersNA = TRUE)}))

wildfire_dnbr_stack <- dnbr_runs_list

for(j in 1:length(dnbr_runs_list)){
  for(i in c(1:81)){
    wildfire_dnbr_stack[[j]][[i]] <- terra::mask(dnbr_runs_list[[j]][[i]], wildfire_mask_stack_list[[j]][[i]])
  }
}

rx_biomass_stack <- biomass_fire_runs_list

for(j in 1:length(biomass_fire_runs_list)){
  for(i in c(1:81)){
    rx_biomass_stack[[j]][[i]] <- terra::mask(biomass_fire_runs_list[[j]][[i]], rx_mask_stack_list[[j]][[i]])
  }
}

## calculate totals for each model run and across all model runs

wildfire_totals6 <- wildfire_dnbr_stack %>%
  purrr::modify(.f = ~ terra::app(.x, fun=function(x){sum(x, na.rm = TRUE)}))

total_wildfire6 <- collapse_list_to_layers(wildfire_totals6) %>%
  terra::app(fun=sum)
plot(total_wildfire6)

severe_frequency <- wildfire_dnbr_stack %>%
  purrr::lmap(.f = ~ lapply(.x, FUN = select_severe_fire)) %>% #reclassify each year
  purrr::modify(.f = ~ terra::app(.x, fun=function(x){sum(x, na.rm = TRUE)}))

total_severe6 <- collapse_list_to_layers(severe_frequency) %>%
  terra::app(fun=sum)

rx_biomass_totals6 <- rx_biomass_stack %>%
  purrr::modify(.f = ~ terra::app(.x, fun=function(x){sum(x, na.rm = TRUE)}))

total_rx6 <- collapse_list_to_layers(rx_biomass_totals6) %>%
  terra::app(fun=sum)
plot(total_rx6)


fire_summary6 <- read.csv("E:/tcsi_for_nick/Scenario1 - historical - Run 1/scrapple-summary-log.csv")


## update biomass removed to add in Rx fire
biomass_with_rx_sums6 <- biomass_sums6 %>%
  purrr::map2(.x = ., .y = rx_biomass_totals6, .f = ~ .x + .y)
biomass_with_rx_mean6 <- biomass_with_rx_sums6 %>%
  collapse_list_to_layers() %>%
  terra::app(fun = sum) %>%
  `/`(5)
plot(rx_biomass_totals6[[1]])
plot(biomass_sums6[[1]])
plot(biomass_with_rx_sums6[[1]])

#-------------------------------------------------------------------------------
# Calculate diffrences in treatment and wildfire
#-------------------------------------------------------------------------------

### Difference in biomass from average of scenario1

biomass_diff <- biomass_sums6 %>%
  purrr::modify(.f = ~ .x - total_biomass1)

biomass_diff_mean6 <- collapse_list_to_layers(biomass_diff) %>%
  terra::app(fun = sum) %>%
  `/`(length(biomass_diff))

wildfire_diff <- wildfire_totals6 %>%
  purrr::modify(.f = ~ .x - total_wildfire1)

wildfire_diff_mean6 <- collapse_list_to_layers(wildfire_diff) %>%
  terra::app(fun = sum) %>%
  `/`(length(wildfire_diff))





#-------------------------------------------------------------------------------
# Analyze differences in fire regimes on a per-scenario average basis
#-------------------------------------------------------------------------------

plot(values(wildfire_diff_mean6) ~ values(biomass_diff_mean6))

severe_ref <- clamp(total_severe1, 0, 7)

severe_diff <- severe_frequency %>%
  purrr::modify(.f = ~ .x - severe_ref)

severe_diff_sum <- collapse_list_to_layers(severe_diff) %>%
  terra::app(fun=mean)

plot(severe_diff_sum)


#-------------------------------------------------------------------------------
# Effect of spatial scale/aggregation with moving window
#-------------------------------------------------------------------------------

#TODO what to do about places where the biomass removed is the same for scenarios?

smoothed_fire_diff_mean6 <- terra::focal(wildfire_diff_mean6, 
                              w = 15,
                              fun = "mean", 
                              na.rm = TRUE, na.policy = "omit")

plot(smoothed_fire_diff_mean6)

smoothed_biomass <- terra::focal(biomass_diff[[1]], 
                                 w = 3,
                                 fun = "mean", 
                                 na.rm = TRUE, na.policy = "omit")
  
plot(values(smoothed_fire_diff_mean6)*-1 ~ values(smoothed_biomass))  

smoothed_fire[smoothed_fire == 0] <- NA
smoothed_biomass[smoothed_biomass == 0] <- NA

test <- (smoothed_fire * -1) / smoothed_biomass

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
plot(smoothed_fire_mean6)
plot(biomass_diff_mean6)
plot(values(smoothed_fire_mean6) ~ values(biomass_diff_mean6))


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
plot(fire_diff2) #treatment "moves" fire to higher elevation

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
