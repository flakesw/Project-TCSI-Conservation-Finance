# create masks for study area and for subset
library("tidyverse")
library("sf")
library("tmap")
library("raster")

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance/")


#import shapefile of TCSI region
tcsi <- sf::st_read("./masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
        sf::st_set_crs("EPSG:3857") %>% #originally in EPSG:3857 
        sf::st_transform("EPSG:9311") #apparently 2163 is deprecated and sf automatically updates it to 9311
st_bbox(tcsi)
crs(tcsi)
st_crs(tcsi)

#import the original mask
orig <- raster("./original TCSI inputs/TCSI_Scenario1/mask.tif") 
raster::crs(orig) <- "EPSG:9311"
bbox(orig)
raster::writeRaster(orig, "./masks_boundaries/mask.tif", overwrite = TRUE)

#get old bounding box and set new one for subset
old_extent <- as(extent(bbox(orig)), 'SpatialPolygons')
crs(old_extent) <- "EPSG:9311"
new_extent <- as(extent(-1810455, -1693455, -440000, -420000), 'SpatialPolygons')
crs(new_extent) <- "EPSG:9311"

#visualize the new extent
plot(orig)
plot(new_extent, add = TRUE)

tm_shape(orig) + tm_raster() +
  tm_shape(old_extent) + tm_borders(lwd = 3) + 
  tm_shape(tcsi) + tm_borders(lwd = 2) +
  tm_shape(new_extent) + tm_borders(lwd = 2) 
  
#subset the mask to the new extent
subset_mask <- crop(orig, new_extent)
raster::writeRaster(subset_mask, "./masks_boundaries/subset_mask.tif", overwrite = TRUE)

plot(subset_mask)

plot(st_as_sf(old_extent))
plot(subset_mask, add = TRUE)

tm_shape(tcsi) + tm_borders() +
  tm_shape(new_extent) + tm_borders()

#make a polygon of the new extent
tcsi_subset_boundary <- st_intersection(st_make_valid(tcsi), st_as_sf(new_extent))
plot(tcsi_subset_boundary)
sf::st_write(tcsi_subset_boundary, "./masks_boundaries/subset_polygon/subset_polygon.shp",
         append = FALSE)

