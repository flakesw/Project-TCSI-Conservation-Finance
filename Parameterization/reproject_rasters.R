# reproject all of the tiffs to get them in the same projection
# just doing this because I originally had files in all sorts of different projections
# library("raster")
library("tidyverse")
library("terra")
library("sf")

sf_use_s2(FALSE)

#Apparently EPSG:2163 got deprecated. Everything is in EPSG:9311 now!

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance")

#directory of original rasters
raster_list  <- list.files(path = "./Models/Inputs/input_rasters_tcsi/", pattern='.tif$', all.files=TRUE, full.names=FALSE)
raster_list_cat <- list.files(path = "./Models/Inputs/input_rasters_tcsi/categorical/", pattern='.tif$', all.files=TRUE, full.names=FALSE)

large_mask <- terra::rast("./Models/Inputs/masks_boundaries/mask.tif")

for(i in 1:length(raster_list)){
  
  #import original raster
  raster1 <- terra::rast(paste0("./Models/Inputs/input_rasters_tcsi/", raster_list[i]))
  NAflag(raster1) <- 0
  data_type <- ifelse(is.integer(terra::values(raster1)), "INT4S", "FLT4S")
  print(data_type)
  
  if(terra::crs(raster1) != terra::crs(large_mask)){
  
    if(terra::crs(raster1) != "" && grepl("laea", terra::crs(raster1, proj = TRUE))){
      message("Updating EPSG:2163 to 9311")
      terra::crs(raster1) <- "EPSG:9311" #fix the dumb EPSG 2153 to 9311 problem
    }else if(terra::crs(raster1) != "" && grepl("+zone=10", terra::crs(raster1, proj = TRUE))){
      #for NAD83 rasters
      message("Raster in NAD83; reprojecting to 9311")
      # terra::crs(raster1) <-  "EPSG:6339"
      raster1 <- terra::project(raster1, large_mask, align = TRUE, gdal = FALSE)
    }else if(nrow(raster1) == nrow(large_mask) & ncol(raster1) == ncol(large_mask)){
      
        #some rasters have no projection -- resample them and force them to same
        #grid as the mask
        #replace values of template with values from input raster
        message("Unknown CRS; transferring values to mask")
        raster2 <- large_mask 
        terra::values(raster2) <- terra::values(raster1)
        raster1 <- raster2
    }else{
        message("Rasters have different numbers of rows or cols")
        print(raster_list[i])
    }

  }
  
  plot(large_mask)
  plot(raster1, add = TRUE)
  
  #set NAs to 0
  terra::values(raster1)[is.na(terra::values(raster1))] <- 0
  
  #write raster
  terra::writeRaster(raster1, 
                      paste0("./Models/Inputs/input_rasters_reproject/",
                             substr(raster_list[i], 1, 
                                    nchar(raster_list[i])-4),
                             ".tif"),
                     filetype = "GTiff",
                     datatype = data_type,
                     NAflag = 0,
                     overwrite = TRUE)
}



## same for categorical rasters
for(i in 1:length(raster_list_cat)){
  
  #import original raster
  raster1 <- terra::rast(paste0("./Models/Inputs/input_rasters_tcsi/categorical/", raster_list_cat[i]))
  NAflag(raster1) <- 0
  data_type <- ifelse(is.integer(terra::values(raster1)), "INT4S", "FLT4S") #terra can't read data types yet
  print(data_type)
  terra::values(raster1) <- as.integer(terra::values(raster1))
  
  if(terra::crs(raster1) != terra::crs(large_mask)){
    
    if(terra::crs(raster1) != "" && grepl("laea", terra::crs(raster1, proj = TRUE))){
      message("Updating EPSG:2163 to 9311")
      terra::crs(raster1) <- "EPSG:9311" #fix the dumb EPSG 2153 to 9311 problem
    }else if(terra::crs(raster1) != "" && grepl("+zone=10", terra::crs(raster1, proj = TRUE))){
      #for NAD83 rasters
      message("Raster in NAD83; reprojecting to 9311")
      # terra::crs(raster1) <-  "EPSG:6339"
      raster1 <- terra::project(raster1, large_mask, align = TRUE, gdal = FALSE)
    }else if(nrow(raster1) == nrow(large_mask) & ncol(raster1) == ncol(large_mask)){
      
      #some rasters have no projection -- resample them and force them to same
      #grid as the mask
      #replace values of template with values from input raster
      message("Unknown CRS; transferring values to mask")
      raster2 <- large_mask 
      terra::values(raster2) <- terra::values(raster1)
      raster1 <- raster2
    }else{
      message("Rasters have different numbers of rows or cols")
      print(raster_list[i])
    }
    
  }
  
  plot(large_mask)
  plot(raster1, add = TRUE)
  
  #set NAs to 0
  terra::values(raster1)[is.na(terra::values(raster1))] <- 0
  
  #write raster
  terra::writeRaster(raster1, 
                     paste0("./Models/Inputs/input_rasters_reproject/",
                            substr(raster_list[i], 1, 
                                   nchar(raster_list[i])-4),
                            ".tif"),
                     filetype = "GTiff",
                     datatype = "INT2S",
                     NAflag = 0,
                     overwrite = TRUE)
}
