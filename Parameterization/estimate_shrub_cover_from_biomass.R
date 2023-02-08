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
  # filter(OWNCD != 46) %>%
  mutate(IS_FOREST = ifelse(FORTYPCD %in%(c(1:998)), 1, 0)) %>%
  group_by(PLT_CN) %>%
  summarise(total_cond = sum(CONDPROP_UNADJ),
            natural = sum(STDORGCD, na.rm = TRUE),
            treatment = sum(TRTCD1, na.rm = TRUE),
            proportion_forest = sum(CONDPROP_UNADJ * IS_FOREST),
            cc = sum(CONDPROP_UNADJ * LIVE_CANOPY_CVR_PCT, na.rm = TRUE)) %>%
  filter(total_cond > 0.95 )

plot_forest_type <- all_fia_cond %>%
  mutate(FORTYPCD = ifelse(FLDSZCD == 1 | COND_STATUS_CD == 2, 1, FORTYPCD)) %>%
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

## MAke plot-level summary variables

tree_summary <- sierra_trees %>% 
  dplyr::group_by(PLT_CN) %>%
  # dplyr::filter(n() > 10) %>%
  # dplyr::filter(STATUSCD == 1) %>%
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
  left_join(., sierra_fia_cond, by = c("PLT_CN" = "PLT_CN"))



#read in understory data

under <- read.csv("D:/Data/fia/rFIA_downloads/CA_P2VEG_SUBP_STRUCTURE.csv") %>%
  filter(PLT_CN %in% tree_summary$PLT_CN,
         GROWTH_HABIT_CD == "SH",
         LAYER == 5) %>%
  group_by(PLT_CN, SUBP, CONDID) %>%
  summarise(cover = sum(COVER_PCT)) %>%
  summarise(understory_cover = mean(cover)) %>%
  left_join(tree_summary, by = "PLT_CN")

under$forest_group <- ifelse(is.na(under$forest_group), 1, under$forest_group)
under$forest_group <- as.factor(under$forest_group)
under$understory_cover <- under$understory_cover/100
under$understory_cover <- ifelse(under$understory_cover < 0.001, 0.001, under$understory_cover)
under$understory_cover <- ifelse(under$understory_cover > 0.999, 0.999, under$understory_cover)
under$biomass <- under$biomass * 100
under$biomass <- ifelse(is.na(under$biomass), 0, under$biomass)
under$biomass <- ifelse(under$biomass==0, 1, under$biomass)


plot(log(under$understory_cover) ~ log(under$biomass))
plot(log(under$understory_cover) ~ log(under$mean_age))
boxplot(log(under$understory_cover) ~ under$forest_group)

#mean age doesn't exist for many forest groups
model <- lm(log(understory_cover) ~ biomass + forest_group, data = under)
summary(model)

plot(effects::allEffects(model))


library(earth)

model  <- earth(logit(understory_cover) ~ log(biomass) + forest_group, data = under)
# plot(model)
plotmo(model)
summary(model)
# plot(effects::allEffects(model))


library(betareg)


model <- betareg(understory_cover ~ log(biomass) + mean_age + forest_group, data = under)
summary(model)

plot(effects::allEffects(model))


newdat <- data.frame(biomass = 1,
                     forest_group = factor(180),
                     mean_age = 0)

predict(model, newdata = newdat)

saveRDS(model, "understory_model_with_group.RDS")


model_no_age <- betareg(understory_cover ~ log(biomass) +  mean_age, data = under)
summary(model_no_age)
plot(effects::allEffects(model_no_age))
predict(model_no_age, newdata = newdat)

saveRDS(model_no_age, "understory_model_no_group.RDS")

# TO Do # TO Do newdata
#crosswalk from forest group to CWHR forest type






