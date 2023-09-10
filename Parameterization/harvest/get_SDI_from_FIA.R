## estimate SDImax from FIA
library("terra")
library("sf")
library("rFIA")
library("tidyverse")

#This is outdated!

#TODO check the stocking equations here: https://www.fia.fs.fed.us/library/sampling/docs/supplement4_121704.pdf
#For each ecoregion, calculate SDI following stocking equations


# For LANDIS outputs, either 1) link to FIA -> calculate SDI, or 2) use weibull parameters
# to model the diameter distribution
# 
# 
ecoregions <- terra::rast("./Models/Inputs/input_rasters_reproject/TCSI_ecoregions.tif")
plot(ecoregions)

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

all_fia_cond <- rbind(ca_fia_cond, nv_fia_cond)


# tcsi_fia <- sf::st_crop(all_fia, ecoregions)
# plot(ecoregions)
# plot(sf::st_geometry(tcsi_fia), add = TRUE)
# 
# eco_extract <- terra::extract(ecoregions, vect(tcsi_fia))
# tcsi_fia$ecoregion <- eco_extract$TCSI_ecoregions


sierra_shape <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  summarise(do.union = TRUE) %>% 
  st_make_valid() %>%
  smoothr::fill_holes(threshold = 0.5) %>%
  st_transform("EPSG:5070")

tcsi_shape <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  st_transform("EPSG:5070")



#metadata for BPS
bps_data <- read.csv('D:/Data/landfire vegetation/BPS/LF16_BPS_200.csv')

#-------------------------------------------------------------------------------
# Match FIA data to LANDFIRE 
#-------------------------------------------------------------------------------
bps <- terra::rast("D:/Data/landfire vegetation/BPS/landfire_bps_200_sierra/LF2016_BPS_200_CONUS/LC16_BPS_200.tif")
plot(bps)

all_fia_plot <- all_fia_plot %>%
  sf::st_transform(crs= st_crs(bps))

plot(sf::st_geometry(all_fia_plot), add = TRUE)

all_fia_plot$bps_code <- terra::extract(bps, vect(all_fia_plot), layer = "BPS_NAME")$BPS_CODE

all_fia_plot <- sf::st_crop(all_fia_plot, sierra_shape)


#-------------------------------------------------------------------------------
# Get tph and QMD for each plot

ca_trees <- read.csv("D:/data/fia/rFIA_downloads/CA_TREE.csv") %>%
  dplyr::filter(PLT_CN %in% all_fia_plot$CN)

nv_trees <- read.csv("D:/data/fia/rFIA_downloads/NV_TREE.csv") %>%
  dplyr::filter(PLT_CN %in% all_fia_plot$CN)

all_trees <- rbind(ca_trees, nv_trees)

tree_summary <- all_trees %>%
  dplyr::group_by(PLT_CN) %>%
  # dplyr::filter(DIA > 6) %>% #TODO check here!!
  dplyr::summarise(plot_bapa = sum(0.005454*(DIA^2)*TPA_UNADJ), 
                   plot_tpa = sum(TPA_UNADJ), 
                   plot_qmd = sqrt((plot_bapa/plot_tpa)/0.005454),
                   plot_sdi = plot_tpa * ((plot_qmd/10)^(-1.605)),
                   sum_sdi = sum(TPA_UNADJ * ((DIA/10)^(-1.605))),
                   .groups = "keep")%>%
  left_join(., all_fia_plot, by = c("PLT_CN" = "CN"))

tree_summary$bps_code <- tree_summary$bps_code %>%
  as.character()

#very different results from North et al.!
hist(tree_summary$plot_tpa)
hist(tree_summary$plot_qmd)
hist(tree_summary$sum_sdi)
plot(plot_tpa ~ sum_sdi, data = tree_summary)
plot(log(plot_qmd) ~ log(plot_tpa), data = tree_summary, log = c("xy"))

table(tree_summary$bps_code)
#-------------------------------------------------------------------------------
library("quantreg")

# max_sdi <- data.frame(ecoregion = 1:8,
#                       max_sdi = NA)
# 
# for(i in c(1:8)){
#   model <- rq(log(plot_qmd) ~ log(plot_tpa), tau = 0.95, data = tree_summary[tree_summary$ecoregion == i, ])
#   # summary(model)
#   # print(coef(model))
#   
#   N = exp((log(10) - coef(model)[[1]]) / coef(model)[[2]])
#   # print(max_sdi)
#   max_sdi$max_sdi[i] <- N
# }

tree_summary2 <- tree_summary %>%
  group_by(bps_code) %>% 
  filter(n() >= 100)

max_sdi <- data.frame(bps_code = as.double(unique(tree_summary2$bps_code)),
                      sdi_max = NA)

for(i in c(1:nrow(max_sdi))){
  model <- tryCatch(
    {
      rq(log(plot_tpa) ~ log(plot_qmd), tau = 0.9, 
        data = tree_summary2[tree_summary2$bps_code == max_sdi[i, "bps_code"], ])
      },
    error = function(cond) {
    }
  )
  # summary(model)
  print(coef(model))

  N = exp((log(10) - coef(model)[[1]]) / coef(model)[[2]])
  # print(max_sdi)
  max_sdi$sdi_max[i] <- N
}

max_sdi$sdi_max <- ifelse(max_sdi$sdi_max > 3000, 3000, max_sdi$sdi_max)


#-------------------------------------------------------------------------------
max_sdi_map <- bps$BPS_CODE %>%
  terra::crop(tcsi_shape)
max_sdi_map <- terra::classify(max_sdi_map, rcl = bps_data[, c("VALUE", "BPS_CODE")]) %>%
  terra::classify(rcl = max_sdi, others = NA)
plot(max_sdi_map)


#-------------------------------------------------------------------------------
sdi_map_init <- rast("D:/Data/FIA_LANDIS/FIA_LANDIS/tcsi_initial_sdi.tif")
plot(sdi_map_init, range = c(0, 4000))
sdi_map_yr80 <- rast("D:/Data/FIA_LANDIS/FIA_LANDIS/tcsi_80_hist_sdi.tif")
plot(sdi_map_yr80, range = c(0, 4000))

max_sdi_map <- max_sdi_map %>%
  terra::project(crs(sdi_map_init))

percent_sdi <- sdi_map_init / max_sdi_map

percent_sdi[percent_sdi > 2] <- 2

percent_sdi <- percent_sdi*100

plot(percent_sdi)

writeRaster(percent_sdi, "percent_sdi.tif")

