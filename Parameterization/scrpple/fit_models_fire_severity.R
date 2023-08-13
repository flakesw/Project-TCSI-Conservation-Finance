
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
                                 full.names = TRUE)

col_types <- list(
  fire_name = col_character(),
  dnbr = col_double(),
  clay = col_double(),
  pet = col_double(),
  cwd = col_double(),
  ews = col_double(),
  vpd = col_double(),
  eddi = col_double(),
  pdsi = col_double(),
  fine_fuel = col_double(),
  ladder_fuel = col_double(),
  ndvi = col_double()
  # ndvi_anomaly = col_double()
)



#readr is incredible
data_all <- fire_severity_data %>%
  purrr::map_df(~read_csv(., col_types = col_types)) %>%
  dplyr::filter(dnbr > 0) %>%
  dplyr::mutate(fine_fuel = ifelse(fine_fuel > 1000, 1, fine_fuel/1000)) %>%
  # dplyr::mutate(dnbr = ifelse(dnbr < 100, 100, dnbr)) %>%
  # dplyr::mutate(dnbr = ifelse(dnbr > 1000, 1000, dnbr)) %>%
  dplyr::filter(!is.na(dnbr)) %>%
  group_by(fire_name) %>%
  slice_sample(n = 300)

data_all_with_loc <- data_all %>%
  mutate(fire_name = toupper(fire_name)) %>%
  left_join(dplyr::select(short, "MTBS_ID", "MTBS_FIRE_NAME", "LATITUDE", "LONGITUDE") %>%
              filter(!duplicated(MTBS_ID)),
            by = c("fire_name" = "MTBS_ID")  ) %>%
  filter(!is.na(LATITUDE)) %>%
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>%
  sf::st_set_crs("EPSG:5070") %>%
  ungroup()

head(data_all_with_loc)

plot(sf::st_geometry(data_all_with_loc[which(!duplicated(data_all_with_loc$fire_name)), ]))

# data_all$ndvi_normal <- data_all$ndvi + data_all$ndvi_anomaly

data_with_fuel <- data_all %>%
  dplyr::filter(!is.na(fine_fuel))

# plot(dnbr ~ fine_fuel, data = data_with_fuel[sample(x = nrow(data_with_fuel), size = 1000), ])
# 
# ggplot(data = sample_frac(data_with_fuel, 0.01), mapping = aes(x = fine_fuel, y = dnbr)) + 
#   geom_jitter(width = 0.02) + 
#   geom_smooth(method = "lm", formula = y ~ exp(x))
# ggplot(data = sample_frac(data_with_fuel[!is.na(data_with_fuel$ladder_fuel), ], 0.01), mapping = aes(x = ladder_fuel, y = dnbr)) + 
#   geom_jitter() + 
#   geom_smooth(method = "lm", formula = y ~ x)
# ggplot(data = sample_frac(data_with_fuel, 0.01), mapping = aes(x = ews, y = dnbr)) + 
#   geom_jitter() + 
#   geom_smooth(method = "lm", formula = y ~ x) + 
#   xlab("Effective windspeed")
# ggplot(data = sample_frac(data_with_fuel, 0.01), mapping = aes(x = ladder_fuel, y = dnbr)) + 
#   geom_jitter() + 
#   geom_smooth(method = "lm", formula = y ~ x)
# ggplot(data = sample_frac(data_with_fuel, 0.01), mapping = aes(x = cwd, y = dnbr)) + 
#   geom_jitter() + 
#   geom_smooth(method = "lm")
# ggplot(data = sample_frac(data_with_fuel, 0.01), mapping = aes(x = pet, y = dnbr)) + 
#   geom_jitter() + 
#   geom_smooth(method = "lm")
# ggplot(data = sample_frac(data_with_fuel, 0.01), mapping = aes(x = ndvi_anomaly, y = dnbr)) + 
#   geom_jitter() + 
#   geom_smooth(method = "lm")
# ggplot(data = sample_frac(data_with_fuel, 0.01), mapping = aes(x = ndvi_normal, y = dnbr)) + 
#   geom_jitter() + 
#   geom_smooth(method = "lm")

hist(data_all$dnbr)

test <- lm(dnbr ~ scale(cwd) + scale(ews)  +  
             scale(ndvi) + scale(pet) +
             scale(vpd) + scale(eddi) + scale(pdsi), data = data_all)
summary(test)


test_lmer <- lmer(dnbr ~  scale(ews) + 
                    scale(pet) +
                    # scale(vpd) +
                    # scale(eddi)  +
                    scale(ndvi) +
                    scale(pdsi) +
                  + (1|fire_name), data = data_all)

summary(test_lmer)

plot(residuals(test_lmer) ~ fitted(test_lmer))

# plot(Effect(test_lmer, focal.predictors = c("ndvi", "ews")))

test_lmer <- lmer(I(1/dnbr) ~  ews + ladder_fuel + 
                    ndvi_anomaly + ndvi + pet + clay +
                    (1|fire_name), data = data_all)
test_lmer <- lmer(I(1/dnbr) ~  ews + 
                    (1|fire_name), data = data_all)

nrow(data_all)

length(unique(data_all$fire_name))

plot(data_all$ladder_fuel ~ data_all$ndvi)


#TODO: combine all shapefiles into one file for plotting, getting centroids


# 
test_gamma <- spaMM::fitme(dnbr ~ clay + cwd + fine_fuel + ews, 
                           data = data_all,
                           family = Gamma(link = "inverse"))
summary(test_gamma)

test_gamma <- spaMM::fitme(dnbr ~ scale(cwd) + scale(fine_fuel) + scale(ews), 
                           data = data_all,
                           family = Gamma(link = "log"))
summary(test_gamma)

test_gamma2 <- spaMM::fitme(dnbr ~ clay + cwd + pet + fine_fuel + ews + (1|fire_name), 
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

data_all <- data_all[, c(3:13)]
data_all <- as.data.frame(data_all[complete.cases(data_all), ]) #dismo needs data frames
data_all$dnbr <- data_all$dnbr #dismo needs small values for response variable
gbm_mod <- gbm.step(data = data_all,
                    gbm.x = c(3,5,6,7,8,9,11),
                    gbm.y = 1,
                    family = "gaussian",
                    learning.rate = 0.01,
                    tree.complexity = 10)

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
