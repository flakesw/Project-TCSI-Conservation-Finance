# subset the rasters for a reduced study area
library("raster")

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance")

#directory of original rasters
raster_list  <- list.files(path = "./Models/Inputs/input_rasters_reproject/", pattern='.tif$', all.files=TRUE, full.names=FALSE)

subset_mask <- raster("./Models/Inputs/masks_boundaries/subset_mask.tif")
raster::crs(subset_mask) <- "EPSG:9311"
large_mask <- raster("./Models/Inputs/masks_boundaries/mask.tif")
raster::crs(large_mask) <- "EPSG:9311"
# mask_nad83 <- raster::projectRaster(large_mask, crs = "EPSG:6339")

for(i in 1:length(raster_list)){
  
  #import original raster
  raster1 <- raster(paste0("./Models/Inputs/input_rasters_reproject/", raster_list[i]))
  
  if(raster::res(raster1)[1] != 180){
    next
  }
  data_type <- dataType(raster1)
  
  raster::crs(raster1) <- "EPSG:9311"
  
  #clip and mask raster to subset mask
  raster1_clip <- raster::crop(raster1, extent(subset_mask))
  raster1_clip <- raster::mask(raster1_clip, subset_mask, maskvalue = 0, updatevalue = 0)

  dataType(raster1_clip) <- data_type
  
  #write raster
  raster::writeRaster(raster1_clip, 
                      paste0("./Models/Inputs/input_rasters_subset/",
                             substr(raster_list[i], 1, 
                                    nchar(raster_list[i])-4),
                             ".tif"),
                      datatype = data_type,
                      overwrite = TRUE,
                      NAflag = 0)
  
  
}
