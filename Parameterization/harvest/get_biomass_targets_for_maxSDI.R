#This script creates regressions to predict biomass from SDI, and then creates biomass targets
#for managing for a particular %maxSDI level


#-------------------------------------------------------------------------------
#get biomass targets for each age/SDI zone
#--------------------------------------------------------------------------
#Fit models to calculate SDI from biomass and age information
#These are used to generate biomass targets for harvest, but not for 
#estimating SDI
tree_summary2 <- tree_summary %>%
  mutate(bps_code = as.integer(as.character(bps_code))) %>%
  filter(plot_sdi < 800) %>%
  left_join(max_sdi, by = "bps_code") %>%
  mutate(percent_max_sdi = plot_sdi / sdi_max) %>% 
  filter(percent_max_sdi < 1) 
hist(tree_summary2$percent_max_sdi)

#instead let's estimate %maxSDI
# mod_biomass_age_mixed <- (lmer(log10(sum_sdi) ~ log10(biomass) + log10(high_age) + (1|bps_code), data = tree_summary[!is.na(tree_summary$bps_code) & !is.na(tree_summary$mean_age), ]))
mod_biomass_age_mixed <- lmer(logit(percent_max_sdi) ~ log10(biomass) + 
                                log10(high_age) + (1|bps_code), 
                              data = tree_summary2[!is.na(tree_summary2$bps_code) & 
                                                     !is.na(tree_summary2$mean_age), ])

summary(mod_biomass_age_mixed)
plot((predict(mod_biomass_age_mixed) ~ mod_biomass_age_mixed@frame$`logit(percent_max_sdi)`),
     xlab = "observed SDI",
     ylab = "Predicted SDI")
abline(0,1)
plot(I(10^(predict(mod_biomass_age_mixed))) ~ I(10^(mod_biomass_age_mixed@frame$`log10(percent_max_sdi)`)))
abline(0,1)
MuMIn::r.squaredGLMM(mod_biomass_age_mixed)
plot(effects::allEffects(mod_biomass_age_mixed))
ranef(mod_biomass_age_mixed)

#Get SDI for combination of biomass and age
tree_summary$bps_code[tree_summary$bps_name == "Mediterranean California Mixed Oak Woodland"]
test <- invlogit(predict(mod_biomass_age_mixed, 
                         newdata = data.frame(biomass = 8000, high_age = 50, bps_code = 10290)))


newdat = expand.grid(biomass = seq(0, 40000, 100),
                     high_age = c(20, 50, 100),
                     bps_code = unique(tree_summary2$bps_code))
newdat$percent_max_sdi <- invlogit(predict(mod_biomass_age_mixed, 
                                           newdata = newdat,
                                           allow.new.levels = TRUE))
newdat$bps_code <- as.factor(newdat$bps_code)

ggplot(data = newdat, aes(x = biomass, y = percent_max_sdi)) + 
  geom_line(aes(color = bps_code, group = bps_code)) +
  geom_hline(yintercept = c(0.35, 0.6)) +
  facet_wrap(facets = ~ high_age)

biomass_vals_by_bps_35 <- newdat %>%
  group_by(bps_code, high_age) %>%
  slice(which.min(abs(percent_max_sdi - 0.35)))%>%
  mutate(bps_code = as.integer(as.character(bps_code)))
hist(biomass_vals_by_bps_35$biomass)

biomass_vals_by_bps_60 <- newdat %>%
  group_by(bps_code, high_age) %>%
  slice(which.min(abs(percent_max_sdi - 0.6))) %>%
  mutate(bps_code = as.integer(as.character(bps_code)))
hist(biomass_vals_by_bps_60$biomass)

biomass_vals_by_bps_60 <- left_join(biomass_vals_by_bps_60, 
                                    bps_codes %>% dplyr::select(BPS_CODE, BPS_NAME) %>%
                                      group_by(BPS_CODE) %>%
                                      slice(1),
                                    by = c("bps_code" = "BPS_CODE"))

#-------
#make a map for biomass targets for 35% SDI
unique(values(sierra_bps)) %in% biomass_vals_by_bps_35$bps_code

biomass_target_rast_35_20 <- sierra_bps_unfactor %>% 
  terra::crop(terra::vect(tcsi_shape)) %>% 
  terra::classify(rcl = biomass_vals_by_bps_35[biomass_vals_by_bps_35$high_age == 20,
                                               c("bps_code", "biomass")], 
                  others = 2000)
plot(biomass_target_rast_35_20)

biomass_target_rast_35_50 <- sierra_bps_unfactor %>% 
  terra::crop(terra::vect(tcsi_shape)) %>% 
  terra::classify(rcl = biomass_vals_by_bps_35[biomass_vals_by_bps_35$high_age == 50,
                                               c("bps_code", "biomass")])
plot(biomass_target_rast_35_50)

biomass_target_rast_35_100 <- sierra_bps_unfactor %>% 
  terra::crop(terra::vect(tcsi_shape)) %>% 
  terra::mask(terra::vect(tcsi_shape)) %>% 
  terra::classify(rcl = biomass_vals_by_bps_35[biomass_vals_by_bps_35$high_age == 100,
                                               c("bps_code", "biomass")])
plot(biomass_target_rast_35_100)

#-------
#make a map for biomass targets for 60% SDI
unique(values(sierra_bps)) %in% biomass_vals_by_bps_60$bps_code

biomass_target_rast_60_20 <- sierra_bps_unfactor %>% 
  terra::crop(terra::vect(tcsi_shape)) %>% 
  terra::classify(rcl = biomass_vals_by_bps_60[biomass_vals_by_bps_60$high_age == 20,
                                               c("bps_code", "biomass")], 
                  others = 2000)
plot(biomass_target_rast_60_20)

biomass_target_rast_60_50 <- sierra_bps_unfactor %>% 
  terra::crop(terra::vect(tcsi_shape)) %>% 
  terra::classify(rcl = biomass_vals_by_bps_60[biomass_vals_by_bps_60$high_age == 50,
                                               c("bps_code", "biomass")])
plot(biomass_target_rast_60_50)

biomass_target_rast_60_100 <- sierra_bps_unfactor %>% 
  terra::crop(terra::vect(tcsi_shape)) %>% 
  terra::mask(terra::vect(tcsi_shape)) %>% 
  terra::classify(rcl = biomass_vals_by_bps_60[biomass_vals_by_bps_60$high_age == 100,
                                               c("bps_code", "biomass")])
plot(biomass_target_rast_60_100)