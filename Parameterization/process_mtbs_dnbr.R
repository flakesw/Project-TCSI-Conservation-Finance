#extract dnbr from individual mtbs fire records
#this isn't a finished script -- see calibrate_fire_severity.R for full approach

library("tidyverse")
library("raster")
library("sf")
setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance")

#folder with all the individual fire data (inside this folder are each folder for each year)
#everything is already unzipped, so there's a bunch of naked .tif files in here
mtbs_folder <- "D:/Data/mtbs_all_fires"

#these rasters are squares which extend past the fire boundary
mtbs_dnbr <- list.files(path = mtbs_folder, pattern = "*_dnbr.tif", 
                           full.names = TRUE, recursive = TRUE)

mtbs_sev <- list.files(path = mtbs_folder, pattern = "*_dnbr6.tif", 
                        full.names = TRUE, recursive = TRUE)

#fire boundaries to clip rasters to
mtbs_shape <- list.files(path = mtbs_folder, pattern = "*_burn_bndy.shp", 
                         full.names = TRUE, recursive = TRUE)

#rasters and shapefiles have different lengths -- either there are duplicates,
#extra shapefiles for some fires, or something else

raster_firenumbers <- mtbs_dnbr %>%
  base::basename() %>%
  base::strsplit("_") %>%
  purrr::map(1) %>%
  base::unlist()

shape_firenumbers <- mtbs_shape %>%
  base::basename() %>%
  base::strsplit("_") %>%
  purrr::map(1) %>%
  base::unlist()

#some fires have shapes but not rasters
table(shape_firenumbers %in% raster_firenumbers)
table(raster_firenumbers %in% shape_firenumbers)

mtbs_shape <- subset(mtbs_shape, shape_firenumbers %in% raster_firenumbers)

shape <- sf::st_read(mtbs_shape[1])
raster <- raster(mtbs_dnbr[1]) %>%
  crop(shape) %>%
  mask(shape)
plot(raster)

#severity goes 0 = masked out; 1 = low intensity or unburned; 2 = low; 3 = moderate severity; 4 = high; 5 = increased greenness 
raster2 <- raster(mtbs_sev[1]) %>%
  crop(shape) %>%
  mask(shape)
plot(raster2)

mean_dnbr <- mean(values(raster)[!is.na(values(raster)) & values(raster2) > 1 & values(raster2) != 5])


