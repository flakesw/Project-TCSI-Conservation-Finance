mgmt_dir <- "E:/TCSI LANDIS/Scenario3 - historical - run1/social-climate-fire/"

harvest_list <- list.files(mgmt_dir, pattern = "ignition")

matches <- regmatches(list.files(mgmt_dir, pattern = "ignition"), 
                      gregexpr("[[:digit:]]+", list.files(mgmt_dir, pattern = "ignition"))) %>%
  unlist() %>% 
  as.numeric()
harvest_list <- harvest_list[order(matches)]

harvest_stack <- terra::rast(paste0(mgmt_dir, harvest_list))

total_biomass_1 <- terra::tapp(harvest_stack, index=1, fun=sum)

plot(total_biomass_1)