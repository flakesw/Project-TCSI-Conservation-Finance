# This script calculates maxSDI for different ecoregions, creates a density-management diagram,
# makes a map of maxSDI

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

#stand management diagram using FORTYPCD
ggplot(tree_summary[as.character(tree_summary$FORTYPCD) %in% fortypcd_enough, ]) + 
  geom_point(aes(x = plot_tpa, y = plot_qmd, col = as.factor(FORTYPCD))) + 
  scale_x_log10() + 
  scale_y_log10() 
# geom_quantile(aes(x = plot_tpa, y = plot_qmd, col = as.factor(FORTYPCD)),
#             stat = "quantile", 
#             quantiles = 0.9)

bps_codes <- read.csv("D:/Data/landfire vegetation/BPS/LF16_BPS_200.csv")
tree_summary$bps_name <- bps_codes[match(tree_summary$bps_code, bps_codes$BPS_CODE), ]$BPS_NAME

overall_density_mgmt_lines = quantreg::rq(log10(plot_tpa) ~ log10(plot_qmd), tau = 0.95, 
                                          data = tree_summary[as.character(tree_summary$bps_code) %in% bps_enough, ])
overall_density_mgmt_lines

#-------------------
#make figure for density-management diagram with maxSDI lines (self-thinning lines)

breaks <- rep(c(1,2,5), 21)*(10^rep(-10:10, each=3))
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

#change the threshold for bps_enough before plotting to remove noise
ggplot(tree_summary[as.character(tree_summary$bps_code) %in% bps_enough, ]) +
  geom_point(aes(y = plot_tpa, x = plot_qmd, col = as.factor(bps_name)), alpha = 0.3) + 
  scale_x_log10(breaks = breaks, minor_breaks = minor_breaks) + 
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks) + 
  geom_quantile(aes(y = plot_tpa, x = plot_qmd, col = as.factor(bps_name)),
                stat = "quantile",
                method = "rq",
                quantiles = 0.99, size = 1.2) + 
  geom_vline(xintercept = 10) +
  geom_abline(aes(fill = "test"),
              slope = rep(overall_density_mgmt_lines$coefficients[2], 3),
              intercept = c(log10(10^overall_density_mgmt_lines$coefficients[1]),
                            log10(10^overall_density_mgmt_lines$coefficients[1] * 0.6),
                            log10(10^overall_density_mgmt_lines$coefficients[1] * 0.35))) +
  xlab("Quadratic mean diameter (in)") +
  ylab("Tree per acre") 



max_sdi <- data.frame(bps_code = as.double(unique(bps_enough)),
                      sdi_max = NA,
                      k_mean = NA)

for(i in c(1:nrow(max_sdi))){
  
  #calculate self-thinning line for each BPS
  model <- tryCatch(
    {
      quantreg::rq(log10(plot_tpa) ~ log10(plot_qmd), tau = 0.99, 
                   data = tree_summary[tree_summary$bps_code == max_sdi[i, "bps_code"], ])
    },
    error = function(cond) {
    }
  )
  # summary(model)
  print(coef(model))
  
  # print(max_sdi)
  #estimate SDI Max directly from the regression line for each BPS
  max_sdi$sdi_max[i] <- 10^((log10(10)*coef(model)[[2]] + coef(model)[[1]]))
  
  #Use Reineke's allometric coefficient (-1.605) and solve for k
  # K = tpa/(qmd^-1.605)
  max_sdi$k_mean[i] <- quantile(tree_summary[tree_summary$bps_code == max_sdi[i, "bps_code"], ]$plot_tpa *
                                  ((tree_summary[tree_summary$bps_code == max_sdi[i, "bps_code"], ]$plot_qmd)^1.605), 0.99, na.rm = TRUE)
  
  max_sdi$sdi_max_reineke[i] <- 10^(log10(10)*-1.605 + log10(max_sdi$k_mean[i]))
}

max_sdi$sdi_max
max_sdi$sdi_max_reineke
max_sdi[max_sdi$bps_code %in% c(11, 31), c("sdi_max", "sdi_max_reineke")] <- 0
plot(max_sdi$sdi_max ~ max_sdi$sdi_max_reineke,
     xlab = "SDI (Reineke method)",
     ylab = "SDI (quantile regression)")
abline(0,1)

#map of max SDI

bps_max_sdi <- sierra_bps %>% 
  terra::crop(terra::vect(tcsi_shape)) %>% 
  as.factor() %>%
  terra::classify(rcl = bps_codes[, c("VALUE", "BPS_CODE")])
# plot(bps_max_sdi)
# click(bps_max_sdi)
bps_max_sdi <- terra::classify(bps_max_sdi, rcl = max_sdi[, c("bps_code", "sdi_max_reineke")], others = NA)
plot(bps_max_sdi)

# plot(terra::vect(tcsi_shape), add = TRUE)

# terra::writeRaster(bps_max_sdi, "./Parameterization/management scenario data/max_sdi_bps_all_trees.tif", overwrite = TRUE)
# terra::writeRaster(bps_max_sdi, "./Parameterization/management scenario data/max_sdi_bps_dia5_reineke.tif", overwrite = TRUE)


