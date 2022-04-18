# reproject all of the tiffs to get them in the same projection
# just doing this because I originally had files in all sorts of different projections

library("tidyverse")
library("raster")
library("sf")

sf_use_s2(FALSE)

#Apparently EPSG:2163 got deprecated. Everything is in EPSG:9311 now!

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance")

#directory of original rasters
raster_list  <- list.files(path = "./Models/Inputs/input_rasters_tcsi/", pattern='.tif$', all.files=TRUE, full.names=FALSE)
raster_list_cat <- list.files(path = "./Models/Inputs/input_rasters_tcsi/categorical/", pattern='.tif$', all.files=TRUE, full.names=FALSE)

large_mask <- raster::raster("./Models/Inputs/masks_boundaries/mask.tif")

for(i in 1:length(raster_list)){
  print(raster_list[i])
  #import original raster
  raster1 <- raster::raster(paste0("./Models/Inputs/input_rasters_tcsi/", raster_list[i]))

  data_type <- raster::dataType(raster(paste0("./Models/Inputs/input_rasters_tcsi/", raster_list[i])))
    
  print(data_type)
  
  if(is.na(raster::crs(raster1))){
    
    if(nrow(raster1) == nrow(large_mask) & ncol(raster1) == ncol(large_mask)){
      
      #some rasters have no projection -- resample them and force them to same
      #grid as the mask
      #replace values of template with values from input raster
      message("Unknown CRS; transferring values to mask")
      raster2 <- large_mask 
      raster::values(raster2) <- raster::values(raster1)
      raster1 <- raster2
    }
    
  }else if((raster::crs(raster1, asText = TRUE) != raster::crs(large_mask, asText = TRUE))){
  
    if(raster::crs(raster1, asText = TRUE) != "" && grepl("laea", raster::crs(raster1,asText = TRUE))){
      message("Updating EPSG:2163 to 9311")
      raster::crs(raster1) <- "EPSG:9311" #fix the dumb EPSG 2153 to 9311 problem
    # }else if(raster::crs(raster1) != "" && grepl("+zone=10", raster::crs(raster1, proj = TRUE))){
    #   #for NAD83 rasters
    #   message("Raster in NAD83; reprojecting to 9311")
    #   # raster::crs(raster1) <-  "EPSG:6339"
    #   raster1 <- raster::project(raster1, large_mask, align = TRUE, gdal = FALSE)
    }
    }else if(nrow(raster1) == nrow(large_mask) & ncol(raster1) == ncol(large_mask)){
      
        #some rasters have no projection -- resample them and force them to same
        #grid as the mask
        #replace values of template with values from input raster
        message("Unknown CRS; transferring values to mask")
        raster2 <- large_mask 
        raster::values(raster2) <- raster::values(raster1)
        raster1 <- raster2
    }else{
        message("Rasters have different numbers of rows or cols")
        next
    }
  
  plot(large_mask)
  plot(raster1, add = TRUE)
  
  #set NAs to 0
  raster::values(raster1)[is.na(raster::values(raster1))] <- 0
  
  dataType(raster1) <- data_type
  
  #write raster
  raster::writeRaster(raster1, 
                      paste0("./Models/Inputs/input_rasters_reproject/",
                             substr(raster_list[i], 1, 
                                    nchar(raster_list[i])-4),
                             ".tif"),
                     filetype = "GTiff",
                     datatype = "FLT4S",
                     NAflag = 0,
                     overwrite = TRUE)
}



## same for categorical rasters
for(i in 1:length(raster_list_cat)){
  print(raster_list_cat[i])
  #import original raster
  raster1 <- raster::raster(paste0("./Models/Inputs/input_rasters_tcsi/categorical/", raster_list_cat[i]))
  
  data_type <- raster::dataType(raster(paste0("./Models/Inputs/input_rasters_tcsi/categorical/", raster_list_cat[i])))
  
  print(data_type)
  
  if(is.na(raster::crs(raster1))){
    
    if(nrow(raster1) == nrow(large_mask) & ncol(raster1) == ncol(large_mask)){
      
      #some rasters have no projection -- resample them and force them to same
      #grid as the mask
      #replace values of template with values from input raster
      message("Unknown CRS; transferring values to mask")
      raster2 <- large_mask 
      raster::values(raster2) <- raster::values(raster1)
      raster1 <- raster2
    }
    
  }else if((raster::crs(raster1, asText = TRUE) != raster::crs(large_mask, asText = TRUE))){
    
    if(raster::crs(raster1, asText = TRUE) != "" && grepl("laea", raster::crs(raster1,asText = TRUE))){
      message("Updating EPSG:2163 to 9311")
      raster::crs(raster1) <- "EPSG:9311" #fix the dumb EPSG 2153 to 9311 problem
      # }else if(raster::crs(raster1) != "" && grepl("+zone=10", raster::crs(raster1, proj = TRUE))){
      #   #for NAD83 rasters
      #   message("Raster in NAD83; reprojecting to 9311")
      #   # raster::crs(raster1) <-  "EPSG:6339"
      #   raster1 <- raster::project(raster1, large_mask, align = TRUE, gdal = FALSE)
    }
  }else if(nrow(raster1) == nrow(large_mask) & ncol(raster1) == ncol(large_mask)){
    
    #some rasters have no projection -- resample them and force them to same
    #grid as the mask
    #replace values of template with values from input raster
    message("Unknown CRS; transferring values to mask")
    raster2 <- large_mask 
    raster::values(raster2) <- raster::values(raster1)
    raster1 <- raster2
  }else{
    message("Rasters have different numbers of rows or cols")
    next
  }
  
  plot(large_mask)
  plot(raster1, add = TRUE)
  
  #set NAs to 0
  raster::values(raster1)[is.na(raster::values(raster1))] <- 0
  dataType(raster1) <- data_type
  
  #write raster
  raster::writeRaster(raster1, 
                      paste0("./Models/Inputs/input_rasters_reproject/",
                             substr(raster_list_cat[i], 1, 
                                    nchar(raster_list_cat[i])-4),
                             ".tif"),
                      filetype = "GTiff",
                      datatype = "INT4S",
                      NAflag = 0,
                      overwrite = TRUE)
}
