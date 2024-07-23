# This script creates a model of SDI ~ biomass + age for LANDIS-II cohorts

#TODO refactor!!

library(tidyverse)
library(sf)
library(terra)
library(lme4)

theme_set(theme_bw())

logit <- function(x){
  log(x/(1-x))
}

invlogit <- function(x){
  exp(x)/(1 + exp(x))
}

# crosswalk biomass/age to SDI
ca_fia_plot <- read.csv("D:/data/fia/rFIA_downloads/CA_PLOT.csv") %>%
  dplyr::filter(!is.na(LAT) & !is.na(LON)) %>%
  sf::st_as_sf(coords = c("LON", "LAT"), remove = TRUE, crs = "EPSG:4269") %>%
  sf::st_transform(crs = "EPSG:5070")

nv_fia_plot <- read.csv("D:/data/fia/rFIA_downloads/NV_PLOT.csv")%>%
  dplyr::filter(!is.na(LAT) & !is.na(LON)) %>%
  sf::st_as_sf(coords = c("LON", "LAT"), remove = TRUE, crs = "EPSG:4269") %>%
  sf::st_transform(crs = "EPSG:5070")

all_fia_plot <- rbind(ca_fia_plot, nv_fia_plot) %>%
  sf::st_transform(crs = "EPSG:5070")

ca_fia_cond <- read.csv("D:/Data/fia/rFIA_downloads/CA_COND.csv")
nv_fia_cond <- read.csv("D:/data/fia/rFIA_downloads/NV_COND.csv")

forest_ref <- read.csv("D:/Data/fia/FIADB_REFERENCE/REF_FOREST_TYPE.csv")

all_fia_cond <- rbind(ca_fia_cond, nv_fia_cond)



sierra_shape <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  summarise(do.union = TRUE) %>% 
  st_make_valid() %>%
  smoothr::fill_holes(threshold = 0.5) %>%
  st_transform("EPSG:5070")

tcsi_shape <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  st_transform("EPSG:5070")

tcsi_fia_plot <- sf::st_join(all_fia_plot, tcsi_shape, join = st_within, left = FALSE)

sierra_fia_plot <- sf::st_join(all_fia_plot, sierra_shape, join = st_within, left = FALSE) %>%
  filter(do.union == TRUE)

plot(sf::st_geometry(sierra_shape))
plot(sf::st_geometry(sierra_fia_plot), add = TRUE)

sierra_fia_cond <- all_fia_cond %>%
  filter(PLT_CN %in% sierra_fia_plot$CN) %>%
  filter(OWNCD != 46) %>%
  mutate(IS_FOREST = ifelse(FORTYPCD %in%(c(1:998)), 1, 0)) %>%
  group_by(PLT_CN) %>%
  summarise(total_cond = sum(CONDPROP_UNADJ),
            natural = sum(STDORGCD, na.rm = TRUE),
            treatment = sum(TRTCD1, na.rm = TRUE),
            proportion_forest = sum(CONDPROP_UNADJ * IS_FOREST),
            cc = sum(CONDPROP_UNADJ * LIVE_CANOPY_CVR_PCT, na.rm = TRUE)) %>%
  filter(total_cond > 0.95 )

plot_forest_type <- all_fia_cond %>%
  group_by(PLT_CN, FORTYPCD) %>%
  summarise(total_fortypcd = sum(CONDPROP_UNADJ)) %>%
  slice_max(total_fortypcd)

sierra_fia_cond <- left_join(sierra_fia_cond, plot_forest_type, by = "PLT_CN")
sierra_fia_cond$forest_group <- forest_ref[match(sierra_fia_cond$FORTYPCD, forest_ref$VALUE), "TYPGRPCD"]

#-------------------------------------------------------------------------------
# Match FIA data to LANDFIRE 
#-------------------------------------------------------------------------------
bps <- terra::rast("D:/Data/landfire vegetation/BPS/landfire_bps_200_sierra/LF2016_BPS_200_CONUS/LC16_BPS_200.tif")
# plot(bps)
bps_shape <- sierra_shape %>%
  sf::st_transform(crs= st_crs(bps))
sierra_bps <- terra::crop(x = bps, y = terra::vect(bps_shape))
tcsi_bps <- terra::crop(x = bps, y = terra::vect(tcsi_shape))
# test <- catalyze(sierra_bps) #catalyze isn't working
plot(sierra_bps$BPS_CODE)
#replace values with the factor levels
sierra_bps_unfactor <- terra::classify(sierra_bps, rcl = levels(sierra_bps)[[1]])

sierra_fia_plot <- sierra_fia_plot %>%
  sf::st_transform(crs= st_crs(bps))

plot(sf::st_geometry(sierra_fia_plot), add = TRUE)

sierra_fia_plot$bps_code <- terra::extract(bps, vect(sierra_fia_plot), layer = "BPS_NAME")$BPS_CODE


#----
#Trees
ca_trees <- read.csv("D:/data/fia/rFIA_downloads/CA_TREE.csv") %>%
  dplyr::filter(PLT_CN %in% sierra_fia_plot$CN)

nv_trees <- read.csv("D:/data/fia/rFIA_downloads/NV_TREE.csv") %>%
  dplyr::filter(PLT_CN %in% sierra_fia_plot$CN)

sierra_trees <- rbind(ca_trees, nv_trees)

sierra_trees <- filter(sierra_trees, PLT_CN %in% sierra_fia_plot$CN)

breaks <- seq(0, max(sierra_trees$BHAGE+10, na.rm = TRUE) + (10 - max(sierra_trees$BHAGE+10, na.rm = TRUE) %% 10), by = 10)

age_model <- lmer(log(BHAGE) ~ log(DIA) + (log(DIA)|SPCD) + 0, data = sierra_trees)
plot(effects::allEffects(age_model))
sierra_trees$BHAGE_pred <- predict(age_model, sierra_trees, allow.new.levels = TRUE)

sierra_trees <- sierra_trees%>%
  filter(STATUSCD == 1) %>%
  mutate(DRYBIO_AG = CARBON_AG * 2,
         AGE_BIN = ifelse(is.na(BHAGE), as.numeric(base::cut(BHAGE_pred+10, breaks)) * 10,
                          as.numeric(base::cut(BHAGE+10, breaks)) * 10),
         SPCD = as.character(SPCD))

tcsi_trees <- sierra_trees %>% filter(PLT_CN %in% tcsi_fia_plot$CN)

plot(log(TPA_UNADJ) ~ log(AGE_BIN), data = sierra_trees) #sort of linear, but really variable
plot(log(TPA_UNADJ) ~ log(DRYBIO_AG), data = sierra_trees) #nice linear relationship!

test_mod <- lm(log(TPA_UNADJ) ~ log(AGE_BIN)*log(DRYBIO_AG), data = sierra_trees[sierra_trees$TPA_UNADJ > 0, ])
summary(test_mod)

## MAke plot-level summary variables

tree_summary <- sierra_trees %>% 
  dplyr::group_by(PLT_CN) %>%
  dplyr::filter(DIA > 5) %>% #use DIA>12.7 cm = 5 inches  to match North et al. 2022
  # dplyr::filter(n() > 10) %>%
  dplyr::filter(STATUSCD == 1) %>%
  dplyr::summarise(total_trees = n(),
                   trees_with_age = sum(!is.na(BHAGE+10)),
                   plot_bapa = sum(0.005454*(DIA^2)*TPA_UNADJ), 
                   plot_tpa = sum(TPA_UNADJ), 
                   plot_qmd = sqrt((plot_bapa/plot_tpa)/0.005454),
                   plot_sdi = plot_tpa * ((plot_qmd/10)^(-1.605)),
                   sum_sdi = sum(TPA_UNADJ * ((DIA/10)^(-1.605))),
                   biomass = sum(DRYBIO_AG * TPA_UNADJ) / 892 * 100, #convert to grams per meter squared
                   mean_age = mean(BHAGE+10, na.rm = TRUE),
                   high_age = quantile(BHAGE+10, 0.9, na.rm = TRUE),
                   .groups = "keep") %>%
  # filter(!is.na(high_age)) %>%
  droplevels() %>%
  left_join(., sierra_fia_plot, by = c("PLT_CN" = "CN")) %>%
  left_join(., sierra_fia_cond, by = c("PLT_CN" = "PLT_CN")) #%>%
  # filter(cc > 0)  %>%
  # group_by(forest_group) %>%
  # filter(n() >= 20)


plot(log10(tree_summary$sum_sdi) ~ log10(tree_summary$biomass),
     xlab = "Plot biomass",
     ylab = "Plot SDI")
summary(lm(log10(tree_summary$sum_sdi) ~ log10(tree_summary$biomass)))
abline(coef(lm(log10(tree_summary$sum_sdi) ~ log10(tree_summary$biomass))))

plot(log(tree_summary$sum_sdi) ~ tree_summary$mean_age)
plot(tree_summary$sum_sdi ~ tree_summary$mean_age)
plot(log(tree_summary$sum_sdi) ~ tree_summary$high_age)
plot(biomass ~ log(plot_tpa), data = tree_summary)
plot(log(plot_qmd) ~ log(plot_tpa), data = tree_summary)


fortypcd_enough <- names(which(table(tree_summary$FORTYPCD) > 50))
bps_enough <- names(which(table(tree_summary$bps_code) > 100))

#------------------------------------------------------------------------------
#cohort-level data analysis
age_cohort_summary <- sierra_trees %>%
  dplyr::filter(DIA > 5) %>% #minimum diameter from North et al. 2022
  dplyr::filter(STATUSCD == 1) %>%
  dplyr::group_by(PLT_CN, SPCD, AGE_BIN) %>%
  dplyr::summarize(cohort_biomass = sum(DRYBIO_AG * TPA_UNADJ) / 892 * 100,
                 cohort_tpa = sum(TPA_UNADJ),
                 cohort_ba = sum(0.005454*(DIA^2)*TPA_UNADJ),
                 # cohort_ba2 = pi*sum((DIA/2/12)^2 * TPA_UNADJ), #equivalent
                 cohort_d = sqrt((cohort_ba/cohort_tpa)/0.005454),
                 cohort_sdi = sum(TPA_UNADJ*((DIA/10)^1.6)))  %>%
  group_by(SPCD) %>%
  filter(n() > 50) %>%
  dplyr::mutate(SPCD = as.factor(SPCD),
                AGE_BIN = as.numeric(AGE_BIN)) %>%
  filter(cohort_d > 0,
         cohort_sdi > 0)
# ,
#          cohort_biomass < 2000,
#          AGE_BIN > 0,
#          AGE_BIN < 300)


# %>%
  # dplyr::left_join(tree_summary %>% filter(!duplicated(PLT_CN)) %>% dplyr::select(PLT_CN, biomass),
  #                  by = "PLT_CN", relationship = "many-to-one") %>%
  # drop_na() 

age_cohort_summary <- age_cohort_summary %>%
  left_join(sierra_fia_plot, by = c("PLT_CN" = "CN")) %>%
  left_join(., sierra_fia_cond[!duplicated(sierra_fia_cond$PLT_CN), ], by = c("PLT_CN" = "PLT_CN"))

#fit cohort SDI model
plot(log(cohort_sdi) ~ log(cohort_biomass), data = age_cohort_summary)
plot(log(cohort_sdi) ~ log(AGE_BIN), data = age_cohort_summary)
plot(log(cohort_sdi) ~ log(cohort_d), data = age_cohort_summary)


sdi_cohort_model <- (lmer(log(cohort_sdi) ~ poly(log(cohort_biomass), 3) + (AGE_BIN) + (1|SPCD), data = age_cohort_summary)) #(TOTAGE) SDI too high, correction factor 0.61
summary(sdi_cohort_model)
# ranef(sdi_cohort_model)
MuMIn::r.squaredGLMM(sdi_cohort_model)
plot(residuals(sdi_cohort_model) ~ fitted(sdi_cohort_model))
plot(predict(sdi_cohort_model) ~ log(age_cohort_summary$cohort_sdi))
abline(0,1)
plot(effects::allEffects(sdi_cohort_model))

saveRDS(sdi_cohort_model, "./Parameterization/management scenario data/sdi_cohort_model.RDS")

sdi_cohort_model_no_sp <- (lm(log(cohort_sdi) ~ poly(log(cohort_biomass), 3) + (AGE_BIN), data = age_cohort_summary)) #(TOTAGE) SDI too high, correction factor 0.61
summary(sdi_cohort_model_no_sp )
# ranef(sdi_cohort_model)
MuMIn::r.squaredGLMM(sdi_cohort_model_no_sp )
plot(residuals(sdi_cohort_model_no_sp ) ~ fitted(sdi_cohort_model_no_sp ))
plot(predict(sdi_cohort_model_no_sp) ~ log(age_cohort_summary$cohort_sdi))
abline(0,1)
plot(effects::allEffects(sdi_cohort_model_no_sp))

saveRDS(sdi_cohort_model, "./Parameterization/management scenario data/sdi_cohort_model_no_sp.RDS")

age_cohort_summary$preds <- exp(predict(sdi_cohort_model, newdata = age_cohort_summary))
age_cohort_plot <- age_cohort_summary %>%
  group_by(PLT_CN) %>%
  reframe(SDI = sum(cohort_sdi),
          SDI_pred = sum(preds))

# sdi_plot_correction <- lm(log(SDI) ~ log(SDI_pred), data = age_cohort_plot)
sdi_plot_correction <- lm(log(SDI) ~ log(SDI_pred) + 0, data = age_cohort_plot)
summary(sdi_plot_correction)
plot(log(age_cohort_plot$SDI) ~ log(age_cohort_plot$SDI_pred),
     xlab = "Predicted SDI", 
     ylab = "Observed SDI")
abline(0,1)
abline(0, coef(sdi_plot_correction)[1])

# saveRDS(sdi_plot_correction, "./Parameterization/management scenario data/sdi_plot_correction_model.RDS")

hist(age_cohort_plot$SDI)