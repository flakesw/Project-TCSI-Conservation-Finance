#make slope steepness and azimuth from DEM
# dem from SRTM, downloaded from earthexplorer
library("tidyverse")
library("raster")

#import SRTM rasters
srtm_dir <- "D:/Data/SRTM 1 Arc-Second Global"
srtm_list <- list.files(srtm_dir) %>% 
  as.data.frame() %>%
  mutate(ext = tools::file_ext(.)) %>%
  filter(ext == "bil") %>%
  mutate(full_path = paste0(srtm_dir, "/", .))

raster_list <- lapply(srtm_list$full_path, raster::raster)

#mosaic rasters
srtm_stack <- do.call(mosaic, args = c(raster_list, fun = mean))

sierra <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  sf::st_transform(crs(srtm_stack))

dem <- raster::crop(srtm_stack, sierra) %>% raster::mask(sierra)

# plot(dem)

writeRaster(dem, "./Parameterization/calibration data/topography/sierra_dem.tif", overwrite = TRUE)

dem <- raster("./Parameterization/calibration data/topography/sierra_dem.tif")

slope <- raster::terrain(dem, opt = "slope", unit = "degrees") #%>%
 # raster::projectRaster(crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
writeRaster(slope, "./Parameterization/calibration data/topography/sierra_slope.tif", overwrite = TRUE,
            datatype = "INT2S")
rm(slope)

aspect <- raster::terrain(dem, opt = "aspect", unit = "degrees") #%>%
#  raster::projectRaster(crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
writeRaster(aspect, "./Parameterization/calibration data/topography/sierra_aspect.tif", overwrite = TRUE,
            datatype = "INT2S")
rm(aspect)
rm(dem)
