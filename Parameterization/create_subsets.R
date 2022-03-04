# subset the rasters for a reduced study area
library("raster")

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance")

#directory of original rasters
raster_list  <- list.files(path = "./input_rasters_reproject/", pattern='.tif$', all.files=TRUE, full.names=FALSE)

subset_mask <- raster("./masks_boundaries/subset_mask.tif")
crs(subset_mask) <- "EPSG:9311"
large_mask <- raster("./masks_boundaries/mask.tif")
crs(large_mask) <- "EPSG:9311"
mask_nad83 <- raster::projectRaster(large_mask, crs = "EPSG:6339")

for(i in 1:length(raster_list)){
  #import original raster
  raster1 <- raster(paste0("./input_rasters_reproject/", raster_list[i]))
  data_type <- dataType(raster1)
  crs(raster1) <- "EPSG:9311"
  
  #clip and mask raster to subset mask
  raster1_clip <- raster::crop(raster1, extent(subset_mask))
  raster1_clip <- mask(raster1_clip, subset_mask, maskvalue = 0, updatevalue = 0)

  #write raster
  raster::writeRaster(raster1_clip, 
                      paste0("./input_rasters_subset/",
                             substr(raster_list[i], 1, 
                                    nchar(raster_list[i])-4),
                             "_subset.tif"),
                      datatype = data_type,
                      overwrite = TRUE)
}
