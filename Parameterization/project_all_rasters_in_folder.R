library("raster")
library("tidyverse")

setwd("./analysis")

project_to_template <- function(input_raster, template){
  #function to project landis input rasters with no CRS to an input file
  
  if(input_raster@ncols == template@ncols & input_raster@nrows == template@nrows){
    #replace values of template with values from input raster
    out_raster <- template %>%
      `values<-`(values(input_raster))
  } else print("Rasters have different numbers of rows or cols")
  
  # plot(out_raster)
  
  return(out_raster)
}

if(!dir.exists("./projected")){
  dir.create("./projected")
}

template <- raster("../masks_boundaries/subset_mask.tif")

raster_list <- list.files()
raster_list <- raster_list[extension(raster_list) %in% c(".img", ".tif")]

rasters_stripped <- sub('\\..*$', '', basename(raster_list))

for(i in 1:length(raster_list)){
  oldrast <- raster(raster_list[i])
  newrast <- project_to_template(oldrast, template)
  writeRaster(newrast, 
              paste0("./projected/",rasters_stripped[i], ".tif"),
              filetype = "GEOTiff",
              datatype = dataType(oldrast),
              overwrite = TRUE)
}
