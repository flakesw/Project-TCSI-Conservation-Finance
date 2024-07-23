
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
                                 full.names = TRUE)[10:17]

col_types <- list(
  fire_name = col_character(),
  x = col_double(),
  y = col_double(),
  dnbr = col_double(),
  rdnbr = col_double(),
  clay = col_double(),
  pet = col_double(),
  cwd = col_double(),
  cwd_normal = col_double(),
  ews = col_double(),
  vpd = col_double(),
  eddi = col_double(),
  pdsi = col_double(),
  fwi = col_double(),
  fine_fuel = col_double(),
  ladder_fuel = col_double(),
  ndvi = col_double(),
  ndvi_anomaly = col_double()
)

short <- readRDS("./Parameterization/calibration data/short_ignitions/short_sierra.RDS")%>%
  filter(FIRE_SIZE >= 8)

short_annual <- short %>%
  group_by(FIRE_YEAR) %>%
  summarize(number = n())
plot(short_annual$number ~ short_annual$FIRE_YEAR)


#readr is incredible
data_all2 <- fire_severity_data %>%
  purrr::map_df(~read_csv(., col_types = col_types)) %>%
  dplyr::filter(dnbr > 0) %>%
  dplyr::mutate(fine_fuel = ifelse(fine_fuel > 1000, 1, fine_fuel/1000)) %>%
  dplyr::mutate(fwi = fwi * 0.658, #calibration to move from MERRA2 to LANDIS internal calculation
                ews = ews * 1.9409) %>% #calibration to match LANDIS internal calculations
  # dplyr::mutate(dnbr = ifelse(dnbr < 100, 100, dnbr)) %>%
  # dplyr::mutate(dnbr = ifelse(dnbr > 1000, 1000, dnbr)) %>%
  dplyr::filter(!is.na(dnbr)) #%>%
  # dplyr::filter(Year>=2000) %>%
  # group_by(fire_name) %>%
  # slice_sample(n = 5000)

mean(data_all2$dnbr)
hist(data_all2$dnbr)

data_all <- data_all2 %>%
  group_by(fire_name) %>%
  slice_sample(n = 1000)

test <- data_all %>%
  group_by(fire_name) %>%
  summarize(mean_dnbr = mean(dnbr, na.rm = TRUE),
            mean_ews = mean(ews, na.rm = TRUE),
            mean_fwi = mean(fwi, na.rm = TRUE),
            mean_ladder = mean(ladder_fuel, na.rm = TRUE))

summary(lm(mean_dnbr ~ mean_ews + mean_fwi + mean_ladder, data = test))

mean(test$mean_dnbr)
mean(data_all2$dnbr)
hist(data_all2$dnbr)
median(data_all$dnbr)
sum(data_all$dnbr > 300)/nrow(data_all)
sum(data_all2$dnbr > 500)/nrow(data_all2)
hist(test$mean_dnbr)
hist(data_all$dnbr)
plot(test$mean_dnbr ~ test$mean_ladder)
summary(lm(test$mean_dnbr ~ test$mean_ladder))
plot(rdnbr ~ ladder_fuel, data = sample_frac(data_all, 0.01))
summary(lm(data_all$rdnbr~data_all$ladder_fuel))
abline(coef(lm(data_all$rdnbr~data_all$ladder_fuel)))

plot(test$mean_dnbr ~ test$mean_ews)
summary(lm(test$mean_dnbr ~ test$mean_ews))
plot(dnbr ~ ews, data = sample_frac(data_all, 0.01))
summary(lm(data_all$dnbr~data_all$ews))

plot(test$mean_dnbr ~ test$mean_fwi)
summary(lm(test$mean_dnbr ~ test$mean_fwi))
plot(rdnbr ~ fwi, data = sample_frac(data_all, 0.01))
summary(lm(data_all$rdnbr~data_all$fwi))
abline(coef(lm(data_all$rdnbr~data_all$fwi)))

hist(data_all$fwi)
hist(data_all$ews)
hist(data_all$ladder_fuel)
hist(test$mean_ews)
mean(test$mean_ews)
hist(test$mean_fwi)
mean(test$mean_fwi)
mean(data_all$ews, na.rm = TRUE)



#add in landfire 
#TODO put this in data-extraction script
# library("stars")
# mtbs_rast <- stars::read_stars("D:/Data/mtbs_all_fires/2020/nv3957611991320200627_20190715_20200701_dnbr.tif")
# dep <- stars::read_stars("D:/Data/landfire vegetation/veg_departure_sierra/US_105_VDEP/us_105vdep.tif")
# sierra_shape <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
#   sf::st_transform(st_crs(dep)) %>%
#   sf::st_union()
# test <- st_crop(dep, sierra_shape) %>%
#   st_warp(mtbs_rast)
# plot(test)



# test <- terra::crop(dep, vect(sierra_shape)) %>% terra::mask(vect(sierra_shape))
# dep <- terra::project(dep, crs(mtbs_rast))
hist(values(dep))
dep[dep[]>100] <- NA
plot(dep)


# tcsi_poly <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
#   sf::st_zm() %>%
#   sf::st_transform(crs = "EPSG:5070") %>%
#   sf::st_make_valid() 
# 
# data_all_with_loc <- data_all %>%
#   mutate(fire_name = toupper(fire_name)) %>%
#   left_join(dplyr::select(short, "MTBS_ID", "MTBS_FIRE_NAME", "LATITUDE", "LONGITUDE") %>%
#               filter(!duplicated(MTBS_ID)),
#             by = c("fire_name" = "MTBS_ID")  ) %>%
#   filter(!is.na(LATITUDE)) %>%
#   sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>%
#   sf::st_set_crs("EPSG:5070") %>%
#   ungroup() 
# 
# tcsi_data <- data_all_with_loc %>%
#   st_filter(st_union(tcsi_poly), .predicate = st_within)




# head(data_all_with_loc)

# plot(sf::st_geometry(data_all_with_loc[which(!duplicated(data_all_with_loc$fire_name)), ]))

# data_all$ndvi_normal <- data_all$ndvi + data_all$ndvi_anomaly
# 
# data_with_fuel <- data_all %>%
#   dplyr::filter(!is.na(fine_fuel))

# plot(dnbr ~ fine_fuel, data = data_with_fuel[sample(x = nrow(data_with_fuel), size = 1000), ])

ggplot(data = sample_frac(data_all, 0.01), mapping = aes(x = fine_fuel, y = rdnbr)) +
  geom_jitter(width = 0.02) +
  geom_smooth(method = "lm", formula = y ~ exp(x))
ggplot(data = sample_frac(data_all[!is.na(data_all$ladder_fuel), ], 0.01), mapping = aes(x = ladder_fuel, y = rdnbr)) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ x)
ggplot(data = sample_frac(data_all, 0.01), mapping = aes(x = ndvi, y = rdnbr)) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ x)
ggplot(data = sample_frac(data_with_fuel, 0.01), mapping = aes(x = ews, y = dnbr)) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ x) +
  xlab("Effective windspeed")
ggplot(data = sample_frac(data_with_fuel, 0.01), mapping = aes(x = ladder_fuel, y = log(dnbr))) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ x)
ggplot(data = sample_frac(data_with_fuel, 0.01), mapping = aes(x = cwd, y = dnbr)) +
  geom_jitter() +
  geom_smooth(method = "lm")
ggplot(data = sample_frac(data_with_fuel, 0.01), mapping = aes(x = pet, y = dnbr)) +
  geom_jitter() +
  geom_smooth(method = "lm")
ggplot(data = sample_frac(data_with_fuel, 0.01), mapping = aes(x = fwi, y = dnbr)) +
  geom_jitter() +
  geom_smooth(method = "lm")
ggplot(data = sample_frac(data_with_fuel, 0.01), mapping = aes(x = ndvi, y = dnbr)) +
  geom_jitter() +
  geom_smooth(method = "lm")

hist(data_all$dnbr)

test <- lm(dnbr ~ scale(ews) + scale(ladder_fuel) + scale(fwi), 
           data = sample_frac(data_all, 0.01))
plot(predict(test) ~ test$model$dnbr)
abline(0,1)
hist(predict(test), xlim = c(0,1200))
hist(test$model$dnbr, xlim = c(0,1200))

summary(test)
plot(test)
hist(resid(test))

test <- glm(dnbr ~ scale(ews) + scale(ladder_fuel) + scale(fwi), 
            family = gaussian(link = "log"),
            data = sample_frac(data_all, 0.01))
summary(test)
plot(test)
hist(resid(test))

test <- glm(dnbr ~ scale(ews) + scale(ladder_fuel) + scale(fwi), 
            family = Gamma(link = "inverse"),
            data = sample_frac(data_all, 0.01))
summary(test)
plot(test)
hist(resid(test))

test <- glm(dnbr ~ scale(ews) + scale(ladder_fuel) + scale(fwi), 
            family = gaussian(link = "sqrt"),
            data = sample_frac(data_all, 0.01))
summary(test)
plot(test)
hist(resid(test))

test_lmer <- lmer(rdnbr ~  scale(ews) + 
                    scale(fwi) *
                    # scale(cwd) +
                    scale(normal_cwd) +
                    # scale(pet) +
                    # scale(vpd) +
                    # scale(eddi)  +
                    scale(ladder_fuel) +
                    scale(pdsi) +
                   (1|fire_name), data = sample_frac(data_all, 0.01))

summary(test_lmer)
plot(allEffects(test_lmer))
plot(Effect(focal.predictors = c("pdsi", "normal_cwd"), mod = test_lmer))

test_lmer <- glmer(dnbr ~  scale(ews) + 
                    scale(fwi) +
                    # scale(pet) +
                    # scale(vpd) +
                    # scale(eddi)  +
                    scale(ndvi) +
                     # scale(ladder_fuel) +
                    # scale(pdsi) +
                    + (1|fire_name), 
                   data = sample_frac(data_all, 0.01),
                   family = Gamma(link = "log"))

test_lmer <- lmer(dnbr ~  ews + 
                    fwi +
                    # scale(pet) +
                    # scale(vpd) +
                    # scale(eddi)  +
                    ndvi +
                    # scale(pdsi) +
                    + (1|fire_name), data = sample_frac(data_all, 0.01))

summary(test_lmer)
plot(test_lmer)
plot(effects::allEffects(test_lmer))
plot(residuals(test_lmer) ~ log(fitted(test_lmer)))
qqplot(scale(residuals(test_lmer)), scale(fitted(test_lmer)))
abline(0,1)

plot(predict(test_lmer) ~ test_lmer@frame$dnbr)
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
data_all <-as.data.frame(data_all[!is.na(data_all$rdnbr), ])
data_all$rdnbr <- data_all$rdnbr/1000 #dismo needs small values for response variable
gbm_mod <- gbm.step(data = data_all,
                    gbm.x = c(4,5,6,9,10,11,13, 14),
                    gbm.y = 1,
                    family = "gaussian",
                    learning.rate = 0.15,
                    max.trees = 5000)

plot(gbm_mod, 1)
plot(gbm_mod, 2)
plot(gbm_mod, 3)
plot(gbm_mod, 4)
plot(gbm_mod, 5)
plot(gbm_mod, 6)
plot(gbm_mod, 7)
plot(gbm_mod, 8)
plot(gbm_mod, 9)

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
