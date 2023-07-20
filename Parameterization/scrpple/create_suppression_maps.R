# create suppression map

library(raster)
library(sf)
library(tidyverse)

old_sup <- raster("C:/Users/Sam/Documents/Research/TCSI conservation finance/input_rasters_tcsi/full_suppression.tif")

template <- raster("./masks_boundaries/mask.tif")

# WUI
# high suppression
# http://silvis.forest.wisc.edu/data/wui-change/
ca_wui <- sf::st_read("D:/Data/WUI/ca_wui_cp12/ca_wui_cp12.shp") %>%
  sf::st_transform(crs = crs(template)) %>%
  sf::st_crop(extent(template))
  
nv_wui <- sf::st_read("D:/Data/WUI/nv_wui_cp12/nv_wui_cp12.shp") %>%
  sf::st_transform(crs = crs(template)) %>%
  sf::st_crop(extent(template))

ggplot(ca_wui) + 
  geom_sf(aes(fill = WUICLASS10))

ggplot(nv_wui) + 
  geom_sf(aes(fill = WUICLASS10))

nv_wui$WUICLASS10 <- as.factor(nv_wui$WUICLASS10)
ca_wui$WUICLASS10 <- as.factor(ca_wui$WUICLASS10)

nv_wui_raster <- rasterize(nv_wui, template, field = "WUICLASS10")
ca_wui_raster <- rasterize(ca_wui, template, field = "WUICLASS10")
tcsi_wui_raster <- merge(nv_wui_raster, ca_wui_raster)

reclassify_matrix <- matrix(data = c(1, 3,
                                     2, 3,
                                     3, 3,
                                     4, 3,
                                     5, 3,
                                     6, 3,
                                     7, 3,
                                     8, 3, 
                                     9, 1, 
                                     10, 1,
                                     11, 2,
                                     12, 2, 
                                     13, 0),
                            nrow = 13, ncol = 2, byrow = TRUE)

tcsi_wui_reclass <- raster::reclassify(tcsi_wui_raster, reclassify_matrix)

# roadless areas
# less suppression far from roads
# https://www.fs.usda.gov/main/roadless/2001roadlessrule/maps
# https://databasin.org/datasets/79e0f98053b644ec8b7c89fc597b2d59/

roadless <- sf::st_read("D:/Data/USFS Inventoried Roadless Areas for contiguous US/data/commonData/Data0/US_roadless.shp")%>%
  sf::st_transform(crs = crs(template)) %>%
  sf::st_crop(extent(template))

tcsi_roadless <- mask(tcsi_wui_reclass, roadless, updatevalue = 0, inverse = TRUE)

plot(sf::st_geometry(roadless), add = T)

# wilderness areas 
# less suppression in wilderness
# https://www.sciencebase.gov/catalog/item/4fc8f0e4e4b0bffa8ab259e7

wilderness <- sf::st_read("D:/Data/Wilderness/wilderness.shp") %>%
  sf::st_transform(crs = crs(template)) %>%
  sf::st_crop(extent(template))

tcsi_wild <- mask(tcsi_roadless, wilderness, updatevalue = 0, inverse = TRUE)

ggplot(wilderness) +
  geom_sf()

plot(test)
plot(sf::st_geometry(wilderness), add = T)

writeRaster(tcsi_wild, "./input_rasters_tcsi/suppression_lightning.tif", overwrite = TRUE,
            datatype = "INT2S")
writeRaster(tcsi_wui_reclass, "./input_rasters_tcsi/suppression_accidental.tif", overwrite = TRUE,
            datatype = "INT2S")

# private lands
# high suppression

# protected areas
# ? suppression
# https://www.sciencebase.gov/catalog/item/5f186a2082cef313ed843257