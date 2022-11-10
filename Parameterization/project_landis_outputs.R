# project landis outputs to real units
library(raster)
library(dplyr)

input_raster <- raster("./Models/Model run templates/Scenario1 - test - new management - grid/biomass/TotalBiomass-0.img")
template_raster <- raster("./Models/Inputs/masks_boundaries/mask_9311.tif")
  
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

new <- raster("./Models/Inputs/masks_boundaries/mask_nad83.tif")

out <- projectRaster(output, new)

writeRaster(out, "biomass_initial_nad83.tif", datatype = "FLT4S", overwrite = TRUE)

