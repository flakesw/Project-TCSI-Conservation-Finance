#create test landscape with just one cell
#
library("raster")
ecoregions <- raster("./input_rasters_reproject/TCSI_ecoregions.tif")
values(ecoregions) <- 0
ecoregions[60, 251] <- 1 #choose which cell
writeRaster(ecoregions, "./input_rasters_reproject/ecoregions_test.tif", datatype = "INT2S", overwrite = TRUE)

