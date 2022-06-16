#model fire spread

library("sf")
library("tidyverse")
library("raster")
library("stars")
library("sp")
library("geoknife")
library("archive")
sf::sf_use_s2(FALSE)

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance/")

template <- raster("./Models/Inputs/masks_boundaries/mask.tif")
raster::values(template) <- 1
sierra_poly <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  sf::st_zm() %>%
  sf::st_transform(crs(template))
tcsi_poly <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  sf::st_zm() %>%
  sf::st_transform(crs(template))

sierra_poly_wgs <- sierra_poly %>% sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

#make a template for the whole sierra
sierra_template <- template
extent(sierra_template) <- extent(sierra_poly)
res(sierra_template) <- res(template)
sierra_template2 <- raster::rasterize(sierra_poly, sierra_template, field = 1)
sierra_template <- sierra_template2
rm(sierra_template2)

daily_perims_all <- sf::st_read("./Parameterization/calibration data/geomac_all_years/perims_2000_2021.shp") %>%
  sf::st_transform(crs = sf::st_crs(sierra_poly)) %>%
  sf::st_intersection(sierra_poly) %>%
  dplyr::filter(fireyear >= 2000) %>%
  mutate(incidentna = paste0(incidentna, fireyear))

#recalculate area -- the gisacres column is a crazy mess, no idea what happened there
area <- map(sf::st_geometry(daily_perims_all), ~ sf::st_area(.)) %>%
  unlist() %>%
  `/`(4046.86) #convert to acres

# plot(area ~ daily_perims_all$gisacres,
#   xlim = c(0, 4e+05))

daily_perims_all$gisacres <- area



#fix dates
library("lubridate")
daily_perims_all$perimeterd
normal_format <- which((substr(daily_perims_all$perimeterd, 1, 4) %in% c(2000:2022)))
different_format <- which(!(substr(daily_perims_all$perimeterd, 1, 4) %in% c(2000:2022)))

dates_with_time <- mdy_hms(daily_perims_all[different_format, ]$perimeterd)
dates_mdy <- mdy(daily_perims_all[different_format, ]$perimeterd)

dates_fixed <- ifelse(!is.na(dates_with_time), 
                      as.character(dates_with_time, format = "%Y-%m-%d"), 
                      as.character(dates_mdy, format = "%Y-%m-%d"))

daily_perims_all[different_format, ]$perimeterd <- dates_fixed

daily_perims_all$perimeterd <- as.Date(daily_perims_all$perimeterd, format = "%Y-%m-%d")

#remove duplicates (polygons with same size on same day for same fire)
daily_perims_all <- daily_perims_all %>%
  group_by(incidentna) %>%
  group_by(perimeterd) %>%
  slice_max(gisacres) %>%
  distinct(gisacres, .keep_all= TRUE) %>%
  filter(fireyear %in% c(2000:2021))




#-------------------------------------------------------------------------------
# fit model to fire spread

#original data from may 2022, used for SCRPPLE parameterization
spread_data <- read.csv("./Parameterization/calibration data/processed_fire_spread_data_new_days_between.csv")

library("lme4")
model <- glm(success ~ scale(fwi) + scale(fuel) + scale(eff_wspd),
             data = spread_data,
             family = "binomial")
summary(model)
MuMIn::r.squaredGLMM(model)
# plot(effects::allEffects(model))

model_reduced <- glm(success ~ scale(fwi) + scale(fuel) + scale(eff_wspd),
                     data = spread_data[spread_data$days_between == 1, ],
                     family = "binomial")
summary(model_reduced)
MuMIn::r.squaredGLMM(model)
# plot(effects::allEffects(model))

#the SCRPPLE model:
model_reduced <- glm(success ~ fwi + fuel + eff_wspd,
                     data = spread_data[spread_data$days_between == 1, ],
                     family = "binomial")

model2 <- glm(success ~ scale(fwi)*scale(fuel)*scale(eff_wspd),
              data = spread_data,
              family = "binomial")
summary(model2)
MuMIn::r.squaredGLMM(model2)
# plot(effects::allEffects(model2))

model3 <- glmer(success ~ scale(fwi) + scale(fuel) + scale(eff_wspd) + (1|fire_name),
                data = spread_data,
                family = "binomial")
summary(model3)
MuMIn::r.squaredGLMM(model3)
# plot(effects::allEffects(model3))

model3_reduced <- lme4::glmer(success ~ scale(fwi) + scale(fuel) + scale(eff_wspd) + (1|fire_name),
                              data = spread_data[spread_data$days_between == 1, ],
                              family = "binomial")
model3_reduced <- lme4::glmer(success ~ fwi + fuel + eff_wspd + (1|fire_name),
                              data = spread_data[spread_data$days_between == 1, ],
                              family = "binomial")
summary(model3_reduced)
MuMIn::r.squaredGLMM(model3_reduced)
plot(effects::allEffects(model3_reduced))
hist(ranef(model3_reduced)$fire_name[[1]])


model4 <- glmer(success ~ scale(fwi)*scale(fuel)*scale(eff_wspd) + (1|fire_name),
                data = spread_data,
                family = "binomial")
summary(model4)
MuMIn::r.squaredGLMM(model4)
plot(effects::allEffects(model4))


test <- lmer(fire_severity ~ scale(fwi)*scale(fuel)*scale(eff_wspd) + year + (1|fire_name),
                data = spread_data)
summary(test)
MuMIn::r.squaredGLMM(model4)
plot(effects::allEffects(model4))

#-------------------------------------------------------------------------------
# temporal model
#-------------------------------------------------------------------------------

year_model <- glm(success ~ scale(fwi) + scale(fuel) + scale(eff_wspd) + scale(year),
                    data = spread_data,
                    family = "binomial")
summary(year_model)
MuMIn::r.squaredGLMM(year_model)
plot(effects::allEffects(year_model))


year_model_glmm <- glmer(success ~ scale(fwi) + scale(fuel) + scale(eff_wspd) + scale(year) + (1|fire_name),
                    data = spread_data,
                    family = "binomial")
summary(year_model_glmm)
MuMIn::r.squaredGLMM(year_model_glmm)
plot(effects::allEffects(year_model_glmm))


year_model_interaction <- glmer(success ~ scale(fwi)*scale(year) + scale(fuel)*scale(year) + 
                      scale(eff_wspd)*scale(year) + scale(year) + (1|fire_name),
                    data = spread_data,
                    family = "binomial")
summary(year_model_interaction)
MuMIn::r.squaredGLMM(year_model_interaction)
plot(effects::allEffects(year_model_interaction))

#-------------------------------------------------------------------------------
# geographic model
#-------------------------------------------------------------------------------
library("spgwr")

fire_centers  <- daily_perims_all %>% 
  group_by(incidentna) %>%
  slice_max(gisacres) %>%
  slice_max(perimeterd) %>%
  filter(incidentna %in% spread_data$fire_name)

spread_data_loc <- spread_data %>%
  left_join(fire_centers, by = c("fire_name" = "incidentna")) %>%
  sf::st_sf()

spread_data_sp <- sf::st_cast(spread_data_loc, "POINT") %>%
  as(., "Spatial")

wip_model <- glm(success ~ scale(fwi) + scale(fuel) + scale(eff_wspd) + WIPGeoNam.1,
                         data = spread_data_sp,
                         family = "binomial")
summary(wip_model)
MuMIn::r.squaredGLMM(wip_model)

interaction_model <- glm(success ~ scale(fwi)*WIPGeoNam.1 + scale(fuel)*WIPGeoNam.1 + scale(eff_wspd)*WIPGeoNam.1,
                         data = spread_data_sp,
                         family = "binomial")
summary(interaction_model)
MuMIn::r.squaredGLMM(interaction_model)

random_model <- glmer(success ~ scale(fwi) + scale(fuel)+ scale(eff_wspd) + (1|WIPGeoNam.1),
                      data = as.data.frame(spread_data_sp),
                      family = "binomial")
summary(random_model)
MuMIn::r.squaredGLMM(random_model)

random_model <- glmer(success ~ (scale(fwi)|WIPGeoNam.1) + scale(fuel) + scale(eff_wspd) + (1|WIPGeoNam.1),
                      data = spread_data_loc,
                      family = "binomial")
summary(random_model)
MuMIn::r.squaredGLMM(random_model)


#GWR
#this takes a really long time

GWRbandwidth <- gwr.sel(success ~ scale(fwi) + scale(fuel)+ scale(eff_wspd),
                        data = spread_data_sp,
                        adapt = T)

test_model <- gwr(success ~ scale(fwi) + scale(fuel)+ scale(eff_wspd),
                  data = as_Spatial(st_as_sf(spread_data_loc)),
                  adapt=GWRbandwidth,
                  hatmatrix=TRUE,
                  se.fit=TRUE) 




#-----------------------------------------------------------------------------
# Effects plot
#-----------------------------------------------------------------------------

newdata <- expand.grid(fwi = seq(0, 100, length.out = 200), 
                       fuel = 0.5,
                       eff_wspd = 5)

preds1 <- predict(model3_reduced, newdata = newdata, re.form=NA, type = "response")%>%
  cbind(newdata)

newdata <- expand.grid(fwi = seq(0, 100, length.out = 200), 
                       fuel = 0.5,
                       eff_wspd = 20)

preds2 <- predict(model3_reduced, newdata = newdata, re.form=NA, type = "response")%>%
  cbind(newdata)

newdata <- expand.grid(fwi = seq(0, 100, length.out = 200), 
                       fuel = 0.5,
                       eff_wspd = 40)
preds3 <- predict(model3_reduced, newdata = newdata, re.form=NA, type = "response") %>%
  cbind(newdata)

ggplot() + geom_line(data=preds1, aes(x=fwi, y = .), color = "black") + 
  geom_line(data=preds2, aes(x=fwi, y = .), color = "black") +
  geom_line(data=preds3, aes(x=fwi, y = .), color = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  ylab("P(Spread)") + 
  xlab("Fire Weather Index")



#Fuels plot

newdata <- expand.grid(fwi = 30, 
                       fuel = seq(0, 1, length.out = 400),
                       eff_wspd = 5)

preds1 <- predict(model3_reduced, newdata = newdata, re.form=NA, type = "response") %>%
  cbind(newdata) %>%
  mutate(fuel = fuel*1000)

newdata <- expand.grid(fwi = 30, 
                       fuel = seq(0, 1, length.out = 400),
                       eff_wspd = 20)

preds2 <- predict(model3_reduced, newdata = newdata, re.form=NA, type = "response") %>%
  cbind(newdata)%>%
  mutate(fuel = fuel*1000)

newdata <- expand.grid(fwi = 30, 
                       fuel = seq(0, 1, length.out = 400),
                       eff_wspd = 40)
preds3 <- predict(model3_reduced, newdata = newdata, re.form=NA, type = "response") %>%
  cbind(newdata)%>%
  mutate(fuel = fuel*1000)

ggplot() + geom_line(data=preds1, aes(x=fuel, y = .), color = "black") + 
  geom_line(data=preds2, aes(x=fuel, y = .), color = "black") +
  geom_line(data=preds3, aes(x=fuel, y = .), color = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  ylab("P(Spread)") + 
  xlab(expression(Fine~fuels~(g~m^{"-2"})))


#-------------------------------------------------------------------------------
#Spaghetti plot
#-------------------------------------------------------------------------------

model <- random_model

# newdata <- expand.grid(fwi = seq(0, 100, length.out = 200),
#                       fuel = 0.5,
#                       eff_wspd = 10,
#                       fire_name = rownames(ranef(model)$fire_name))

newdata <- expand.grid(fwi = seq(0, 100, length.out = 200),
                       fuel = 0.5,
                       eff_wspd = 10,
                       WIPGeoNam.1 = rownames(ranef(random_model)$WIPGeoNam.1))

# preds <- predict(model3_reduced, newdata = newdata[1:200, ], re.form=NA)
# preds_re <- predict(model3_reduced, newdata = newdata)

preds <- predict(model, newdata = newdata[1:200, ], re.form=NA)
preds_re <- predict(model, newdata = newdata)

plot(boot::inv.logit(preds) ~ newdata$fwi[1:200], type = "l", lwd = 5,
     ylim = c(0, 1), 
     col = "blue",
     xlab = "FWI",
     ylab = "P(Spread)")
# for(i in 1:length(unique(newdata$fire_name))){
#   lines(boot::inv.logit(preds_re)[newdata$fire_name == unique(newdata$fire_name)[i]] ~ newdata$fwi[newdata$fire_name == unique(newdata$fire_name)[i]])
# }

for(i in 1:length(unique(newdata$WIPGeoNam.1))){
  lines(boot::inv.logit(preds_re)[newdata$WIPGeoNam.1 == unique(newdata$WIPGeoNam.1)[i]] ~ newdata$fwi[newdata$WIPGeoNam.1 == unique(newdata$WIPGeoNam.1)[i]])
}




#-------------------------------------------------------------------------------
# nonlinear models

#boosted regression tree

# gbm_data <- spread_data %>%
#   mutate(success = as.numeric(success)) %>%
#   filter(days_between == 1 | is.na(days_between))
# 
# library("dismo")
# 
# test <- gbm.step(data = gbm_data,
#                  gbm.y = "success",
#                  gbm.x = c("fwi", "fuel", "eff_wspd"),
#             distribution = "bernoulli",
#             n.trees = 1000)
# summary(test)
# 
# gbm.plot(test, smooth = TRUE)
# interaction.plot(test)
# gbm.interactions(test)
