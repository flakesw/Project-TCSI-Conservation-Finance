library(sf)
library(terra)

output_folder <- "clipped_rasters"

sagehen_shape <- sf::st_read("C:/Users/swflake/Documents/TCSI-conservation-finance/Models/Inputs/masks_boundaries/sagehen/sagehen.shp")

raster_names <- list.files("projected_rasters")

rasters_to_clip <- terra::rast(list.files("projected_rasters", full.names = TRUE))

clipped_rasters <- terra::crop(rasters_to_clip, sagehen_shape)

terra::writeRaster(clipped_rasters, paste0(output_folder, "/", raster_names, "_clipped.tif"),
                   overwrite = TRUE)
