library(tidyverse)
library(sf)

logit <- function(x) log(x/(1-x))
invlogit <- function(x) exp(x)/(1+exp(x))


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
# bps <- terra::rast("D:/Data/landfire vegetation/BPS/landfire_bps_200_sierra/LF2016_BPS_200_CONUS/LC16_BPS_200.tif")
# plot(bps)
# 
# sierra_fia_plot <- sierra_fia_plot %>%
#   sf::st_transform(crs= st_crs(bps))
# 
# plot(sf::st_geometry(sierra_fia_plot), add = TRUE)
# 
# sierra_fia_plot$bps_code <- terra::extract(bps, vect(sierra_fia_plot), layer = "BPS_NAME")$BPS_CODE


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
                   mean_age = weighted.mean(TOTAGE, DRYBIO_TOTAL, na.rm = TRUE),
                   high_age = quantile(TOTAGE, 0.9, na.rm = TRUE),
                   .groups = "keep") %>%
  # filter(!is.na(high_age)) %>%
  droplevels() %>%
  left_join(., sierra_fia_plot, by = c("PLT_CN" = "CN")) %>%
  left_join(., sierra_fia_cond, by = c("PLT_CN" = "PLT_CN")) %>%
  # filter(cc > 0)  %>%
  group_by(forest_group) %>%
  filter(n() >= 20)



plot(tree_summary$plot_sdi ~ tree_summary$sum_sdi)
abline(0,1)
plot(tree_summary[tree_summary$mean_age > 50,]$plot_sdi ~ tree_summary[tree_summary$mean_age > 50,]$mean_age)
summary(lm(tree_summary[tree_summary$mean_age > 50,]$plot_sdi ~ tree_summary[tree_summary$mean_age > 50,]$mean_age))

plot(tree_summary[tree_summary$mean_age > 50,]$biomass ~ tree_summary[tree_summary$mean_age > 50,]$mean_age)
summary(lm(tree_summary[tree_summary$mean_age > 50,]$biomass ~ tree_summary[tree_summary$mean_age > 50,]$mean_age))

model <- lm(log(plot_sdi) ~ biomass*mean_age, data = tree_summary)
summary(model)
plot(effects::allEffects(model))


tree_summary$conifer <- as.factor(ifelse(as.numeric(as.character(tree_summary$forest_group)) < 500, 1, 2))

tree_summary$cc_bin <- cut(tree_summary$cc, breaks = c(0, 10, 25, 40, 60, 100), labels = FALSE)

tree_summary$cc_bin <- as.factor(tree_summary$cc_bin)

tree_summary$FORTYPCD <- as.factor(tree_summary$FORTYPCD)
tree_summary$forest_group <- as.factor(tree_summary$forest_group)
 
tree_summary$sqrt_biomass <- sqrt(tree_summary$biomass)

plot(tree_summary$cc ~ I(tree_summary$biomass^(0.5)))

test <- (lme4::lmer(logit(tree_summary$cc/100) ~ sqrt(biomass) * plot_qmd + (1|forest_group), data = tree_summary))
summary(test)

plot(effects::allEffects(test))

invlogit(predict(test, newdata = data.frame(biomass = 250,
                                   forest_group = as.character(280),
                                   plot_qmd = 10)))

invlogit(sqrt(mean(residuals(test)^2)))

plot((residuals(test)) ~ invlogit(predict(test)))
invlogit(sd(residuals(test)))
plot(invlogit(predict(test)) ~ I((test@frame$`sqrt(biomass)`)^2))



