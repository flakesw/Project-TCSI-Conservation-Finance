# reproject all of the tiffs to get them in the same projection
# just doing this because I originally had files in all sorts of different
# projections, and 
library("raster")
library("tidyverse")

#final output CRS is EPSG:2163 (might change later!)
#I changed it. Apparently EPSG:2163 got deprecated. Everything is in EPSG:9311 now!

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance")

#directory of original rasters
raster_list  <- list.files(path = "./input_rasters_tcsi/", pattern='.tif$', all.files=TRUE, full.names=FALSE)
raster_list_cat <- list.files(path = "./input_rasters_tcsi/categorical/", pattern='.tif$', all.files=TRUE, full.names=FALSE)

large_mask <- raster("./masks_boundaries/mask.tif")
crs(large_mask) <- "EPSG:9311"
mask_nad83 <- raster::projectRaster(large_mask, crs = "EPSG:6339")

for(i in 1:length(raster_list)){
  #import original raster
  raster1 <- raster(paste0("./input_rasters_tcsi/", raster_list[i]))
  # print(NAvalue(raster1))
  data_type <- dataType(raster1)
  
  if(is.na(crs(raster1))){
    #some rasters have no projection -- resample them and force them to same
    #grid as the mask
    
    if(raster1@ncols == large_mask@ncols & raster1@nrows == large_mask@nrows){
      #replace values of template with values from input raster
      raster1 <- large_mask %>%
        `values<-`(values(raster1))
    } else{
      print("Rasters have different numbers of rows or cols")
      print(raster_list[i])
    }
    
  } else if(crs(raster1)@projargs != crs(large_mask)@projargs){
    #some rasters are in other projections (usually NAD83 UTM)
    raster1 <- raster::projectRaster(raster1, to = large_mask)
  }
  
  # print(NAvalue(raster1))
  
  #write raster
  raster::writeRaster(raster1, 
                      paste0("./input_rasters_reproject/",
                             substr(raster_list[i], 1, 
                                    nchar(raster_list[i])-4),
                             ".tif"),
                      datatype = data_type,
                      overwrite = TRUE)
}

for(i in 1:length(raster_list_cat)){
  #import original raster
  raster1 <- raster(paste0("./input_rasters_tcsi/categorical/", raster_list_cat[i]))
  # print(NAvalue(raster1))
  data_type <- dataType(raster1)
  if(is.na(crs(raster1))){
    #some rasters have no projection -- resample them and force them to same
    #grid as the mask
    
    if(raster1@ncols == large_mask@ncols & raster1@nrows == large_mask@nrows){
      #replace values of template with values from input raster
      raster1 <- large_mask %>%
        `values<-`(values(raster1))
    } else print("Rasters have different numbers of rows or cols")
    
  } else if(crs(raster1)@projargs != crs(large_mask)@projargs){
    #some rasters are in other projections (usually NAD83 UTM)
    raster1 <- raster::projectRaster(raster1, to = large_mask, method = "ngb")
    resample(raster1, large_mask, method = "ngb")
  }
  
  #TODO test this
  raster1[is.na(values(raster1))] <- 0
  
  # print(NAvalue(raster1))
  
  #write raster
  raster::writeRaster(raster1, 
                      paste0("./input_rasters_reproject/",
                             substr(raster_list_cat[i], 1, 
                                    nchar(raster_list_cat[i])-4),
                             ".tif"),
                      datatype = data_type,
                      overwrite = TRUE)
}


# test2 <- raster(paste0("./input_rasters_reproject/",
#                        substr(raster_list_cat[i], 1, 
#                               nchar(raster_list_cat[i])-4),
#                        ".tif"))
# values(test2)
