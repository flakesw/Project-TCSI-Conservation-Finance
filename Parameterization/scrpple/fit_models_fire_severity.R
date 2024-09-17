
#*******************************************************************************
#* Analyze the data
#*******************************************************************************
library("tidyverse")
library("sf")
library("terra")
library("lme4")
library("effects")
library("glm2")
library("spaMM")
library("sp")
#import data
fire_severity_data <- list.files("./Parameterization/calibration data/fire severity/",
                                 pattern = "catcher",
                                 full.names = TRUE)[1:40]

col_types <- list(
  fire_code = col_character(),
  fire_name = col_character(),
  mtbs_filename = col_character(),
  year = col_integer(),
  x = col_double(),
  y = col_double(),
  dnbr = col_double(),
  rdnbr = col_double(),
  mtbs_severity = col_integer(),
  departure = col_double(),
  clay = col_double(),
  pet = col_double(),
  cwd = col_double(),
  normal_cwd = col_double(),
  ews = col_double(),
  windspeed = col_double(),
  vpd = col_double(),
  # eddi = col_double(),
  # pdsi = col_double(),
  fwi = col_double(),
  fine_fuel = col_double(),
  ladder_fuel = col_double(),
  tm_ladder_fuel = col_double(),
  FFMC = col_double(),
  DMC = col_double(),
  DC = col_double(),
  ISI = col_double(),
  BUI = col_double(),
  FWI = col_double(),
  DSR = col_double()
  # ndvi = col_double(),
  # ndvi_anomaly = col_double()
)

short <- readRDS("./Parameterization/calibration data/short_ignitions/short_sierra.RDS")%>%
  filter(FIRE_SIZE >= 8)

short_annual <- short %>%
  group_by(FIRE_YEAR) %>%
  summarize(number = n())
plot(short_annual$number ~ short_annual$FIRE_YEAR)


#readr is incredible
data_all2 <- fire_severity_data %>%
  purrr::map_df(~read_csv(., col_types = col_types) %>%
                  group_by(fire_name) %>%
                  slice_sample(n = 5000)) %>%
  dplyr::filter(dnbr > 0) %>%
  dplyr::mutate(fine_fuel = ifelse(fine_fuel > 1000, 1, fine_fuel/1000)) %>%
  dplyr::mutate(fwi = fwi * 0.658, #calibration to move from MERRA2 to LANDIS internal calculation
                ews = ews * 1.9409,
                FWI = FWI * 3.751713) %>% #calibration to match LANDIS internal calculations
  # dplyr::mutate(dnbr = ifelse(dnbr < 100, 100, dnbr)) %>%
  # dplyr::mutate(dnbr = ifelse(dnbr > 1000, 1000, dnbr)) %>%
  dplyr::filter(!is.na(dnbr)) #%>%
  # dplyr::filter(Year>=2000) %>%
  # group_by(fire_name) %>%
  # slice_sample(n = 5000)

data_all2$departure <- ifelse(data_all2$departure >100, NA, data_all2$departure)

mean(data_all2$dnbr)
hist(data_all2$dnbr)
abline(v = quantile(data_all2$dnbr, 0.45))
abline(v = quantile(data_all2$dnbr, 0.75))

data_all <- data_all2 %>%
  # group_by(fire_name) %>%
  # slice_sample(n = 100)
  sample_frac(0.1)


#assign to Sierra regions
sierra_shape <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  sf::st_transform("EPSG:5070")

data_all_with_loc <- data_all %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = "EPSG:5070") %>%
  sf::st_join(sierra_shape, join = st_within)

data_tcsi <- data_all_with_loc[data_all_with_loc$WIPGeoNam == "TCSI Plus", ]

data_all <- data_tcsi


plot(dnbr ~ ladder_fuel, data = data_all2[data_all2$FWI <20, ])
summary(lm(dnbr~ladder_fuel, data = data_all2[data_all2$FWI <20, ]))
abline(coef(lm(dnbr~ladder_fuel, data = data_all2[data_all2$FWI <20, ])))
plot(dnbr ~ tm_ladder_fuel, data = data_all2[data_all2$FWI <20, ])
summary(lm(dnbr~tm_ladder_fuel, data = data_all2[data_all2$FWI <20, ]))
abline(coef(lm(dnbr~tm_ladder_fuel, data = data_all2[data_all2$FWI <20, ])))
plot(dnbr ~ fine_fuel, data = data_all2[data_all2$FWI <20, ])
summary(lm(dnbr~fine_fuel, data = data_all2[data_all2$FWI <20, ]))
abline(coef(lm(dnbr~fine_fuel, data = data_all2[data_all2$FWI <20, ])))

plot(rdnbr ~ ladder_fuel, data = data_all)
summary(lm(rdnbr~ladder_fuel, data = data_all))
abline(coef(lm(rdnbr~ladder_fuel, data = data_all)))
plot(rdnbr ~ tm_ladder_fuel, data = data_all)
summary(lm(rdnbr~tm_ladder_fuel, data = data_all))
abline(coef(lm(rdnbr~tm_ladder_fuel, data = data_all)))
plot(rdnbr ~ fine_fuel, data = data_all)
summary(lm(rdnbr~fine_fuel, data = data_all))
abline(coef(lm(rdnbr~fine_fuel, data = data_all)))


plot(dnbr ~ fwi, data = data_all)
test <- lmer(dnbr ~ windspeed+fwi+fine_fuel + (1|fire_name),
             data = data_all)
plot(effects::allEffects(test))

abline(coef(lm(data_all$rdnbr~data_all$fwi)))

hist(data_all$fwi)
hist(data_all$ews)
hist(data_all$ladder_fuel)
hist(test$mean_ews)
mean(test$mean_ews)
hist(test$mean_fwi)
mean(test$mean_fwi)
mean(data_all$ews, na.rm = TRUE)

hist(data_all$departure)
summary(lm(rdnbr ~ departure*fwi + fine_fuel + ews, data = data_all))
plot(allEffects(lm(rdnbr ~ departure*fwi + fine_fuel + ews, data = data_all)))

ggplot(data = data_all, mapping = aes(x = fine_fuel, y = dnbr)) +
  geom_jitter(width = 0.02) +
  geom_smooth(method = "lm", formula = y ~ exp(x))
ggplot(data = data_all, mapping = aes(x = ladder_fuel, y = dnbr)) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ x)
ggplot(data = data_all, mapping = aes(x = tm_ladder_fuel, y = dnbr)) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ x)
ggplot(data = data_all, mapping = aes(x = ndvi, y = dnbr)) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ x)
ggplot(data = data_all, mapping = aes(x = ews, y = dnbr)) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ x) +
  xlab("Effective windspeed")
ggplot(data = data_all, mapping = aes(x = fine_fuel, y = log(dnbr))) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ x)
ggplot(data = data_all, mapping = aes(x = cwd, y = dnbr)) +
  geom_jitter() +
  geom_smooth(method = "lm")
ggplot(data = data_all, mapping = aes(x = pet, y = dnbr)) +
  geom_jitter() +
  geom_smooth(method = "lm")
ggplot(data = data_all, mapping = aes(x = fwi, y = dnbr)) +
  geom_jitter() +
  geom_smooth(method = "lm")
ggplot(data = data_all, mapping = aes(x = ndvi, y = dnbr)) +
  geom_jitter() +
  geom_smooth(method = "lm")

hist(data_all$dnbr)

test <- lm(dnbr ~ ews + ladder_fuel + BUI, 
           data = data_all)
summary(test)
MuMIn::r.squaredGLMM(test)
plot(predict(test) ~ test$model$dnbr)
abline(0,1)
plot(allEffects(test))
hist(predict(test), xlim = c(0,1200))
hist(test$model$dnbr, xlim = c(0,1200))

plot(test)
hist(resid(test))

test <- glm(dnbr ~ scale(ews) + scale(ladder_fuel) + scale(BUI) + scale(pet) + scale(clay), 
            family = gaussian(link = "log"),
            data = data_all)
summary(test)
plot(test)
hist(resid(test))

test <- glm(dnbr ~ scale(ews) + scale(ladder_fuel) + scale(BUI), 
            family = Gamma(link = "inverse"),
            data = data_all)
summary(test)
plot(test)
hist(resid(test))
plot(allEffects(test))

test <- glm(dnbr ~ scale(ews) + scale(ladder_fuel) + scale(BUI), 
            family = gaussian(link = "sqrt"),
            data = data_all)
summary(test)
plot(test)
hist(resid(test))
plot(allEffects(test))

test_lmer <- glmer(dnbr ~  scale(ews) + 
                    scale(ladder_fuel) +
                    scale(fine_fuel) +
                    # scale(FFMC) +
                    # scale(DMC) +
                    # scale(DC) +
                    # scale(ISI) +
                    scale(FWI) *
                     scale(ladder_fuel) +
                    # scale(FWI)  +
                    # scale(DSR) +
                    (1|fire_name), data = data_all, 
                   family = gaussian(link = "log"))
summary(test_lmer)
plot(allEffects(test_lmer))
MuMIn::r.squaredGLMM(test_lmer)

test_lmer <- lmer(dnbr ~  #scale(windspeed) +
                    (ews) +
                    # scale(cwd) +
                    # scale(normal_cwd) +
                    # scale(pet) +
                    # scale(vpd) +
                    # scale(eddi)  +
                    scale(fine_fuel) * scale(FFMC) +
                    (tm_ladder_fuel) *
                    (BUI) + 
                    (1|fire_name), data = data_all)

summary(test_lmer)
plot(allEffects(test_lmer))
MuMIn::r.squaredGLMM(test_lmer)
# plot(Effect(focal.predictors = c("pdsi", "normal_cwd"), mod = test_lmer))

test_lmer <- glmer(dnbr ~  ews + 
                     ladder_fuel + 
                     FWI + 
                    (1|fire_name), 
                   data = data_all,
                   family = Gamma(link = "log"))

summary(test_lmer)
plot(test_lmer)
plot(effects::allEffects(test_lmer))
plot(residuals(test_lmer) ~ log(fitted(test_lmer)))
qqplot(scale(residuals(test_lmer)), scale(fitted(test_lmer)))
abline(0,1)

plot(exp(predict(test_lmer)) ~ test_lmer@frame$dnbr)
abline(0,1)

# plot(Effect(test_lmer, focal.predictors = c("ndvi", "ews")))

test_lmer <- lmer(I(1/dnbr) ~  fwi + ews + ladder_fuel + fine_fuel +
                    (1|fire_name), data = sample_frac(data_all, 0.02))
summary(test_lmer)

test_lmer2 <- lmer(log(dnbr) ~  fwi + ews + ladder_fuel +
                    (1|fire_name), data = sample_frac(data_all, 0.02))
summary(test_lmer2)

test_lmer2 <- lmer(boot::logit(dnbr/2000) ~  ews + ladder_fuel*fwi + fine_fuel+
                     (1|fire_name), data = sample_frac(data_all, 0.02))
summary(test_lmer2)
plot(effects::allEffects(test_lmer2))

test_lmer <- lmer(I(1/dnbr) ~  ews + 
                    (1|fire_name), data = data_all)

nrow(data_all)

length(unique(data_all$fire_name))

plot(data_all$ladder_fuel ~ data_all$ndvi)

#TODO: combine all shapefiles into one file for plotting, getting centroids

library("spaMM")

reduced <- sample_frac(data_all, 0.01)
reduced2 <- sample_frac(data_all, 0.05)
# 
test_gamma <- spaMM::fitme(dnbr ~ ladder_fuel + ews + fwi, 
                           data = reduced,
                           family = Gamma(link = "inverse"))
summary(test_gamma)

test_gamma <- spaMM::fitme(dnbr ~ scale(cwd) + scale(ladder_fuel) + scale(ews), 
                           data = reduced,
                           family = Gamma(link = "log"))
summary(test_gamma)

test_gamma2 <- spaMM::fitme(dnbr ~ ladder_fuel + ews + fwi + (1|fire_name), 
                            data = data_all,
                            family = Gamma(link = "inverse"))
summary(test_gamma2)

#this crashes R
# test_gamma_lmer <- glmer(dnbr ~ clay + cwd + pet + fine_fuel + ews + (1|fire_name), 
#                    data = data_all,
#                    start = coef(lm(I(1/dnbr) ~ clay + cwd + pet + fine_fuel + ews, data = data_all)),
#                    family = Gamma(link = "inverse"))
# summary(test_gamma_lmer)

#make effects plots
newdata <- expand.grid(clay = mean(data_all$clay, na.rm = TRUE), 
                       cwd = seq(min(data_all$cwd, na.rm = TRUE), max(data_all$cwd, na.rm = TRUE), length.out = 1000),
                       ews =  mean(data_all$ews, na.rm = TRUE),
                       ladder_fuel =  mean(data_all$ladder_fuel, na.rm = TRUE),
                       ndvi_anomaly =  mean(data_all$ndvi_anomaly, na.rm = TRUE),
                       ndvi =  mean(data_all$ndvi, na.rm = TRUE)
)

preds1 <- predict(test, newdata = newdata, type = "response")

plot(preds1 ~ newdata$cwd, type = "l",
     xlab = "CWD",
     ylab = "DNBR")

plot(preds1 ~ newdata$ndvi_anomaly, type = "l",
     xlab = "NDVI Anomaly",
     ylab = "DNBR")

plot(preds1 ~ newdata$ews, type = "l",
     xlab = "Effective Windspeed",
     ylab = "DNBR")

length(unique(data_all$fire_name))
nrow(data_all)



#---------------------------------------------
#boosted regression tree
library("dismo")
bak <- data_all

data_all <- data_all[, c(7:20)]
# data_all <- as.data.frame(data_all[complete.cases(data_all), ]) #dismo needs data frames
data_all <-as.data.frame(data_all[!is.na(data_all$dnbr), ])
data_all$dnbr <- data_all$dnbr/1000 #dismo needs small values for response variable
gbm_mod <- gbm.step(data = data_all,
                    gbm.x = c(10,11,12,13,14,15,16,17,18,20),
                    gbm.y = 6,
                    family = "gaussian",
                    learning.rate = 0.15,
                    max.trees = 5000)

gbm.plot(gbm_mod, n.plots = 9, plot.layout = c(3,3))
gbm_simp <- gbm.simplify(gbm_mod)
gbm.plot(gbm_simp)

#----------------------------------------------------------------------------
# GWR

library("spgwr")
library(parallel)

sierra_grid <- sf::st_make_grid(sierra_shape, square = F, cellsize = c(10 * 1000, 10 * 1000))
plot(sierra_grid)
plot(sierra_shape, add = TRUE)
plot(sf::st_geometry(data_all_with_loc[which(!duplicated(data_all_with_loc$fire_name)), ]),
     add = TRUE)

grid_points <- sf::st_centroid(sierra_grid) %>%
  st_as_sf() %>%
  st_filter(sierra_shape) %>%
  sf::as_Spatial() %>% 
  as("SpatialPointsDataFrame") 
  
grid_points@data <- data.frame(ID=1:nrow(grid_points),
                               ews = 0,
                               ndvi = 0,
                               pet = 0,
                               pdsi = 0)

data_all_reduced <- data_all_with_loc %>%
  group_by(fire_name) %>%
  slice_sample(n = 300) %>%
  dplyr::select(c(dnbr, ews, ndvi, pet, pdsi, vpd, eddi)) %>%
  tidyr::drop_na()

# find optimal ADAPTIVE kernel bandwidth using cross validation
# abw <- gwr.sel(dnbr ~ ews + ndvi + pet + pdsi,
#                data = data_all_reduced,
#                coords = sf::st_coordinates(data_all_reduced),
#                adapt = FALSE, #test this out
#                verbose = TRUE,
#                gweight = gwr.Gauss,
#                show.error.messages = TRUE,
#                longlat = FALSE)

# fit a gwr based on adaptive bandwidth
cl <- makeCluster(detectCores())
xx_no_fit <- gwr(dnbr ~ scale(ews) + scale(ndvi) + scale(pet) + scale(pdsi), 
          data = data_all_reduced,
          coords = sf::st_coordinates(data_all_reduced),
          # adapt = abw, 
          bandwidth = 20000,
          gweight = gwr.Gauss,
          hatmatrix = TRUE,
          se.fit = FALSE,
          predictions = FALSE,
          # fit.points = grid_points,
          longlat = FALSE,
          cl = cl)
stopCluster(cl)


# test <- readRDS("geographic_regression.RDS)")
saveRDS(xx, "geographic_regression_v4_nofit.RDS")

### Fit surface to model
cl <- makeCluster(detectCores())
xx_fit_grid <-  gwr(dnbr ~ scale(ews) + scale(ndvi) + scale(pet) + scale(pdsi), 
               data = data_all_reduced,
               coords = sf::st_coordinates(data_all_reduced),
               # adapt = abw, 
               bandwidth = 10000,
               gweight = gwr.Gauss,
               hatmatrix = FALSE,
               se.fit = FALSE,
               predictions = TRUE,
               fit.points = grid_points,
               longlat = FALSE,
               cl = cl)
stopCluster(cl)

grid_poly <- sierra_grid %>%
  st_as_sf() %>%
  st_filter(sierra_shape)


# gwr.map <- cbind(grid_poly, as.data.frame(xx_fit_grid$SDF)) %>%
#   sf::st_as_sf()

gwr.map <- xx_fit_grid$SDF %>%
  sf::st_as_sf() %>%
  st_set_crs("epsg:5070") %>%
  sf::st_cast("POINT") %>%
  sf::st_join(grid_poly, ., st_contains)

#collapse to grid
library(colorspace)

sierra_poly <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  sf::st_zm() %>%
  sf::st_transform("epsg:5070")

ggplot(gwr.map) +
  geom_sf(mapping = aes(fill = `scale(ews)`)) + 
  geom_sf(data = sierra_poly, fill = NA, col = "white") +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0, na.value = NA)

gwr.map$`scale(ndvi)` <- ifelse(gwr.map$`scale(ndvi)`>200, 200, gwr.map$`scale(ndvi)`)
gwr.map$`scale(ndvi)` <- ifelse(gwr.map$`scale(ndvi)`< -200, -200, gwr.map$`scale(ndvi)`)
ggplot(gwr.map) +
  geom_sf(mapping = aes( fill = `scale(ndvi)`)) + 
  geom_sf(data = sierra_poly, fill = NA, col = "white") + 
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0, na.value = NA)

hist(gwr.map$`scale(pdsi)`)
gwr.map$`scale(pdsi)` <- ifelse(gwr.map$`scale(pdsi)`>200, 200, gwr.map$`scale(pdsi)`)
gwr.map$`scale(pdsi)` <- ifelse(gwr.map$`scale(pdsi)`< -200, -200, gwr.map$`scale(pdsi)`)

ggplot(gwr.map) +
  geom_sf(mapping = aes(fill = `scale(pdsi)`)) + 
  geom_sf(data = sierra_poly, fill = NA, col = "white") + 
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0, na.value = NA)
