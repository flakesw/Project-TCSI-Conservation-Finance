library(tidyverse)
library(sf)
library(terra)
library(lme4)

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

sierra_fia_plot <- sf::st_join(all_fia_plot, sierra_shape, join = st_within) %>%
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
plot(sierra_bps$BPS_CODE)

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

breaks <- seq(0, max(sierra_trees$TOTAGE, na.rm = TRUE) + (10 - max(sierra_trees$TOTAGE, na.rm = TRUE) %% 10), by = 5)

sierra_trees <- sierra_trees%>%
  filter(STATUSCD == 1) %>%
  mutate(DRYBIO_TOTAL = CARBON_AG * 2,
         AGE_BIN = as.numeric(base::cut(TOTAGE, breaks)),
         SPCD = as.character(SPCD))

plot(log(TPA_UNADJ) ~ log(AGE_BIN), data = sierra_trees) #sort of linear, but really variable
plot(log(TPA_UNADJ) ~ log(DRYBIO_TOTAL), data = sierra_trees) #nice linear relationship!

test_mod <- lm(log(TPA_UNADJ) ~ log(AGE_BIN)*log(DRYBIO_TOTAL), data = sierra_trees)
summary(test_mod)

## MAke plot-level summary variables

tree_summary <- sierra_trees %>% 
  dplyr::group_by(PLT_CN) %>%
  dplyr::filter(DIA >5) %>%
  # dplyr::filter(n() > 10) %>%
  dplyr::filter(STATUSCD == 1) %>%
  dplyr::summarise(total_trees = n(),
                   trees_with_age = sum(!is.na(TOTAGE)),
                   plot_bapa = sum(0.005454*(DIA^2)*TPA_UNADJ), 
                   plot_tpa = sum(TPA_UNADJ), 
                   plot_qmd = sqrt((plot_bapa/plot_tpa)/0.005454),
                   plot_sdi = plot_tpa * ((plot_qmd/10)^(-1.605)),
                   sum_sdi = sum(TPA_UNADJ * ((DIA/10)^(-1.605))),
                   biomass = sum(DRYBIO_TOTAL * TPA_UNADJ) / 892, #convert to megagrams per ha
                   mean_age = mean(TOTAGE, na.rm = TRUE),
                   high_age = quantile(TOTAGE, 0.9, na.rm = TRUE),
                   .groups = "keep") %>%
  # filter(!is.na(high_age)) %>%
  droplevels() %>%
  left_join(., sierra_fia_plot, by = c("PLT_CN" = "CN")) %>%
  left_join(., sierra_fia_cond, by = c("PLT_CN" = "PLT_CN"))# %>%
  # filter(cc > 0)  %>%
  # group_by(forest_group) %>%
  # filter(n() >= 20)


plot(log10(tree_summary$sum_sdi) ~ log10(tree_summary$biomass),
     xlab = "Plot biomass",
     ylab = "Plot SDI")
summary(lm(log10(tree_summary$sum_sdi) ~ log10(tree_summary$biomass)))
abline(coef(lm(log10(tree_summary$sum_sdi) ~ log10(tree_summary$biomass))))

plot(log(tree_summary$sum_sdi) ~ tree_summary$mean_age)
plot(log(tree_summary$sum_sdi) ~ tree_summary$high_age)
plot(biomass ~ log(plot_tpa), data = tree_summary)
plot(log(plot_qmd) ~ log(plot_tpa), data = tree_summary)


fortypcd_enough <- names(which(table(tree_summary$FORTYPCD) > 50))
bps_enough <- names(which(table(tree_summary$bps_code) > 50))

#stand management diagram
ggplot(tree_summary[as.character(tree_summary$FORTYPCD) %in% fortypcd_enough, ]) + 
  geom_point(aes(y = plot_tpa, x = plot_qmd, col = as.factor(FORTYPCD))) + 
  scale_x_log10() + 
  scale_y_log10() + 
  geom_quantile(aes(y = plot_tpa, x = plot_qmd, col = as.factor(FORTYPCD)),
              stat = "quantile", 
              quantiles = 0.9)

bps_codes <- read.csv("D:/Data/landfire vegetation/BPS/LF16_BPS_200.csv")
tree_summary$bps_name <- bps_codes[match(tree_summary$bps_code, bps_codes$BPS_CODE), ]$BPS_NAME

ggplot(tree_summary[as.character(tree_summary$bps_code) %in% bps_enough, ]) + 
  geom_point(aes(y = plot_tpa, x = plot_qmd, col = as.factor(bps_name))) + 
  scale_x_log10() + 
  scale_y_log10() + 
  geom_quantile(aes(y = plot_tpa, x = plot_qmd, col = as.factor(bps_name)),
                stat = "quantile",
                method = "rq",
                quantiles = 0.95) + 
  geom_vline(xintercept = 10) +
  geom_abline(intercept= log10(10^4.2),
              slope=-1.54, colour="blue", size = 1.5)+
  geom_abline(intercept= log10(10^4.2 * 0.65),
            slope=-1.54, colour="blue", size = 1.5)+
  geom_abline(intercept= log10(10^4.2 * 0.35),
              slope=-1.54, colour="blue", size = 1.5)

max_sdi <- data.frame(bps_code = as.double(unique(bps_enough)),
                      sdi_max = NA,
                      k_mean = NA)

for(i in c(1:nrow(max_sdi))){
  model <- tryCatch(
    {
      quantreg::rq(log10(plot_tpa) ~ log10(plot_qmd), tau = 0.95, 
         data = tree_summary[tree_summary$bps_code == max_sdi[i, "bps_code"], ])
    },
    error = function(cond) {
    }
  )
  # summary(model)
  print(coef(model))
  
  # print(max_sdi)
  max_sdi$sdi_max[i] <- 10^((log10(10)*coef(model)[[2]] + coef(model)[[1]]))
  max_sdi$k_mean[i] <- quantile(tree_summary[tree_summary$bps_code == max_sdi[i, "bps_code"], ]$plot_tpa /
    ((tree_summary[tree_summary$bps_code == max_sdi[i, "bps_code"], ]$plot_qmd)^-1.605), 0.95, na.rm = TRUE)
  
  max_sdi$sdi_max_reineke[i] <- 10^(log10(10)*-1.605 + log10(max_sdi$k_mean[i]))
}

max_sdi$sdi_max
max_sdi$sdi_max_reineke
max_sdi[max_sdi$bps_code %in% c(11, 31), c("sdi_max", "sdi_max_reineke")] <- 0

#map of max SDI

bps_max_sdi <- sierra_bps %>% terra::crop(terra::vect(tcsi_shape)) %>% as.factor() %>%
  terra::classify(rcl = bps_data[, c("VALUE", "BPS_CODE")])
plot(bps_max_sdi)
# click(bps_max_sdi)

bps_max_sdi <- terra::classify(bps_max_sdi, rcl = max_sdi[, c("bps_code", "sdi_max")], others = NA)
plot(bps_max_sdi)




#--------------------------------------------------------------------------
#Fit models to calculate SDI from biomass and age information
mod <- (lmer(log(sum_sdi) ~ log(biomass)*mean_age + (1|bps_code), data = tree_summary[!is.na(tree_summary$bps_code) & !is.na(tree_summary$mean_age), ]))
summary(mod)
plot((predict(mod)) ~ log(tree_summary[!is.na(tree_summary$bps_code)& !is.na(tree_summary$mean_age), ]$sum_sdi),
     xlab = "observed SDI",
     ylab = "Predicted SDI")
abline(0,1)
plot(exp(predict(mod)) ~ (tree_summary[!is.na(tree_summary$bps_code)& !is.na(tree_summary$mean_age), ]$sum_sdi))
abline(0,1)
MuMIn::r.squaredGLMM(mod)

mod <- (lmer(log(sum_sdi) ~ log(biomass) + log(mean_age) + (1|bps_code), data = tree_summary[!is.na(tree_summary$bps_code) & !is.na(tree_summary$mean_age), ]))
summary(mod)
plot((predict(mod)) ~ log(tree_summary[!is.na(tree_summary$bps_code)& !is.na(tree_summary$mean_age), ]$sum_sdi),
     xlab = "observed SDI",
     ylab = "Predicted SDI")
abline(0,1)
plot(exp(predict(mod)) ~ (tree_summary[!is.na(tree_summary$bps_code)& !is.na(tree_summary$mean_age), ]$sum_sdi))
abline(0,1)
MuMIn::r.squaredGLMM(mod)

# mod <- (lm(log(sum_sdi) ~ log(biomass) + log(high_age) + bps_code, data = tree_summary[!is.na(tree_summary$bps_code) & !is.na(tree_summary$mean_age), ]))
# summary(mod)
# plot((predict(mod)) ~ log(tree_summary[!is.na(tree_summary$bps_code)& !is.na(tree_summary$mean_age), ]$sum_sdi),
#      xlab = "observed SDI",
#      ylab = "Predicted SDI")
# abline(0,1)
# plot(exp(predict(mod)) ~ (tree_summary[!is.na(tree_summary$bps_code)& !is.na(tree_summary$mean_age), ]$sum_sdi))
# abline(0,1)
# MuMIn::r.squaredGLMM(mod)

tree_summary$Plot_level_pred <- predict(mod, newdata = tree_summary)


#------------------------------------------------------------------------------
#cohort-level data analysis

age_cohort_summary <- sierra_trees %>%
  dplyr::filter(DIA > 1) %>%
  dplyr::group_by(PLT_CN, SPCD, AGE_BIN) %>%
  dplyr::reframe(cohort_biomass = sum(DRYBIO_TOTAL * TPA_UNADJ),
                   cohort_tpa = sum(TPA_UNADJ),
                   cohort_ba = sum(0.005454*(DIA^2)*TPA_UNADJ),
                   # cohort_ba2 = pi*sum((DIA/2/12)^2 * TPA_UNADJ), #equivalent
                   cohort_d = sqrt((cohort_ba/cohort_tpa)/0.005454),
                   cohort_sdi = TPA_UNADJ*(DIA/10)^1.6)  %>%
  # dplyr::filter(cohort_biomass > 3000) %>%
  group_by(SPCD) %>%
  filter(n() > 100) %>%
  dplyr::mutate(SPCD = as.factor(SPCD),
                AGE_BIN = as.numeric(AGE_BIN)) %>%
  dplyr::left_join(tree_summary %>% filter(!duplicated(PLT_CN)) %>% dplyr::select(PLT_CN, biomass),
                   by = "PLT_CN", relationship = "many-to-one") %>%
  drop_na() 

age_cohort_summary <- age_cohort_summary %>%
  left_join(sierra_fia_plot, by = c("PLT_CN" = "CN")) %>%
  left_join(., sierra_fia_cond, by = c("PLT_CN" = "PLT_CN")) %>%
  dplyr::filter(!is.na(FORTYPCD))

#fit cohort SDI model
plot(log(cohort_sdi) ~ log(cohort_biomass), data = age_cohort_summary)
plot(log(cohort_sdi) ~ log(AGE_BIN), data = age_cohort_summary)
plot(log(cohort_sdi) ~ log(cohort_d), data = age_cohort_summary)


sdi_cohort_model <- (lmer(log(cohort_sdi) ~ poly(log(cohort_biomass), 3) + poly(log(AGE_BIN), 3) + (1|SPCD), data = age_cohort_summary))
summary(sdi_cohort_model)
r.squaredGLMM(sdi_cohort_model)
plot(residuals(sdi_cohort_model) ~ fitted(sdi_cohort_model))
plot(predict(sdi_cohort_model) ~ log(age_cohort_summary$cohort_sdi))
abline(0,1)
plot(effects::allEffects(sdi_cohort_model))

age_cohort_summary$preds <- exp(predict(sdi_cohort_model, newdata = age_cohort_summary))
age_cohort_plot <- age_cohort_summary %>%
  group_by(PLT_CN) %>%
  reframe(SDI = sum(cohort_sdi),
          SDI_pred = sum(preds))

sdi_plot_correction <- lm(log(SDI) ~ log(SDI_pred), data = age_cohort_plot)
summary(sdi_plot_correction)
plot(log(age_cohort_plot$SDI) ~ log(age_cohort_plot$SDI_pred))
abline(0,1)
abline(coef(sdi_plot_correction))
abline(0, coef(lm(log(SDI) ~ log(SDI_pred) + 0, data = age_cohort_plot)))


#---------------------------------------------
# Bring in LANDIS layers
sp_ref <- read.csv("D:/Data/fia/FIADB_REFERENCE/REF_SPECIES.csv") %>%
  mutate(SpeciesLandis = paste0(substr(GENUS, 1, 4), stringr::str_to_title(substr(SPECIES, 1, 4)))) %>%
  filter(SPCD != 143) #subspecies we don't want

comm_input <- read.csv("./Parameterization/management scenario data/community-input-file-80.csv")

comm_input = left_join(comm_input, sp_ref %>% select(SPCD, SpeciesLandis),
                       by = c("SpeciesName" = "SpeciesLandis"))

comm_input <- comm_input %>%
  dplyr::rename(cohort_biomass = CohortBiomass,
                AGE_BIN = CohortAge)

#this takes a very long time, since there are about 10 million rows
comm_input$SDI_cohort = exp(predict(sdi_cohort_model, 
                             newdata = comm_input, 
                             allow.new.levels = TRUE))

plot_comm <- comm_input %>%
  group_by(MapCode) %>%
  summarise(SDI_pred = sum(SDI_cohort, na.rm = TRUE))
plot_comm$SDI_plot = exp(predict(sdi_plot_correction, newdata = plot_comm))

hist(plot_comm$SDI_plot)

#link to map
comm_map <- terra::rast("./Parameterization/management scenario data/output-community-80.img")
table(values(comm_map) %in% plot_comm$MapCode)
table(values(comm_map)[!(values(comm_map) %in% comm_input$MapCode)])

comm_map <- terra::classify(comm_map, 
                            rcl = dplyr::select(plot_comm, MapCode, SDI_plot),
                            others = 0)

plot(comm_map)


comm_map2 <- terra::rast("./Models/Inputs/masks_boundaries/mask_9311.tif")
values(comm_map2) <- values(comm_map)
bps_max_sdi2 <- bps_max_sdi %>% 
  terra::project(comm_map2) %>%
  terra::crop(comm_map2) %>%
  terra::mask(comm_map2, maskvalues = c(NA, 0), updatevalue = NA)

plot(bps_max_sdi2)

percent_max_sdi <- comm_map2 / bps_max_sdi2 * 100 %>%
  terra::clamp(., lower = 0, upper = 100, values = TRUE) #why isn't clamp working?
values(percent_max_sdi)[values(percent_max_sdi)>100] <- 100
plot(percent_max_sdi)
hist(percent_max_sdi)

terra::writeRaster(percent_max_sdi, "percent_max_sdi_initial.tif")

