#add CRS to LANDIS rasters
scrpple_folder <- "C:/Users/Sam/Documents/Research/TCSI conservation finance/TCSI_Scenario1/social-climate-fire/"
raster_list <- list.files(scrpple_folder)
dir.create(paste0(scrpple_folder, "projected"))

template <- raster("C:/Users/Sam/Documents/Research/TCSI conservation finance/TCSI_Scenario1/mask.tif") 
raster::crs(template) <- "EPSG:2163"

for(i in 1:40){ #for now, just the first 40 day-of-fire rasters
  input_raster <- raster(paste0(scrpple_folder, raster_list[i]))
  # plot(input_raster)
  
  if(input_raster@ncols == template@ncols & input_raster@nrows == template@nrows){
  #replace values of template with values from input raster
  out_raster <- template %>%
    `values<-`(values(input_raster)) 
  } else print("Rasters have different numbers of rows or cols")

  # plot(out_raster)
  
  writeRaster(out_raster, paste0(scrpple_folder, "projected/", raster_list[i]), overwrite = TRUE)
  }

# project_to_template <- function(input_raster, template){
# 
#   if(input_raster@ncols == template@ncols & input_raster@nrows == template@nrows){
#     #replace values of template with values from input raster
#     out_raster <- template %>%
#       `values<-`(values(input_raster)) 
#   } else print("Rasters have different numbers of rows or cols")
#   
#   # plot(out_raster)
#   
#   return(out_raster)
# }
