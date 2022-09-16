# project landis outputs to real units
library(raster)
library(dplyr)

input_raster <- raster("C:/Users/swflake/Documents/LANDIS inputs/Model runs/Scenario1 - no disturbance - oneyear/NECN/LAI-1.img")
template_raster <- raster("C:/Users/swflake/Documents/LANDIS inputs/Inputs/masks_boundaries/mask.tif")
  
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

output <- project_to_template(input_raster, template_raster)

writeRaster(output, "LAI-initial.tif", datatype = "FLT4S", overwrite = TRUE)

