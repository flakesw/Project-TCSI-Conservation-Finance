#wrangle all the mtbs data to get stuff to compare to

#TODO add: time since fire; fire departure

library("geoknife")
library("sf")
library("terra")
library("tidyverse")
# library("raster")
library("FedData") #for downloading SSURGO data
library("stars")# using stars is so much faster, since the whole files aren't loaded into RAM, just the subsets
# it's really an amazing upgrade from raster
# library("MODIStsp") # for downloading MODIS data
library("archive")
library("lubridate")
terraOptions(datatype="FLT8S")


setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance")

sierra_shape <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  sf::st_transform("EPSG:5070") %>%
  sf::st_union()
sierra_poly_wgs <- sierra_shape %>%
  sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

short_all <- read.csv("./Parameterization/calibration data/short_ignitions/short_drop_geometry.csv")
short <- readRDS("./Parameterization/calibration data/short_ignitions/short_sierra.RDS") %>%
  sf::st_transform("EPSG:5070")


mtbs_folder <- "D:/Data/mtbs_all_fires"

#these rasters are squares which extend past the fire boundary
# only using rdnbr rasters -- excludes some fires based on nbr
mtbs_dnbr <- list.files(path = mtbs_folder, pattern = "*_dnbr.tif", 
                        full.names = TRUE, recursive = TRUE)

mtbs_rdnbr <- list.files(path = mtbs_folder, pattern = "*_rdnbr.tif", 
                         full.names = TRUE, recursive = TRUE)

mtbs_sev <- list.files(path = mtbs_folder, pattern = "*_dnbr6.tif", 
                       full.names = TRUE, recursive = TRUE)

#fire boundaries to clip rasters to
mtbs_shape <- list.files(path = mtbs_folder, pattern = "*_burn_bndy.shp", 
                         full.names = TRUE, recursive = TRUE) 

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





#How many rows to collect before writing data to disk
data_length <- 1000000

#TODO add cell locations to data catcher!

create_data_catcher <- function(data_length){
  return(data.frame(fire_name = character(data_length),
                    year = character(data_length),
                    x = numeric(data_length),
                    y = numeric(data_length),
                    dnbr = numeric(data_length),
                    rdnbr = numeric(data_length),
                    mtbs_sev = numeric(data_length)))
}

data_catcher <- create_data_catcher(data_length)
#row to start on when adding data
row_tracker <- 1

start_time <- Sys.time()

#fires with fuels and daily progressions (year 2001) start at 314
for(i in 1:length(mtbs_shape)){
  
  error_flag <- FALSE
  try_again <- FALSE
  
  message(paste("Beginning processing fire", i))
  print(paste("Fire code", mtbs_shape[i]))
  
  boundary <- sf::st_read(mtbs_shape[i])%>%
    sf::st_make_valid() %>%
    sf::st_transform("EPSG:5070") %>%
    sf::st_cast(to = "MULTIPOLYGON") %>%
    dplyr::mutate(fireyear = substr(Ig_Date, 1, 4)) %>%
    dplyr::summarise(fireyear = fireyear[1],
                     Event_ID = Event_ID[1],
                     Ig_Date = Ig_Date[1])
  
  
  
  # if(boundary$fireyear[1] <= 2000 | is.na(boundary$fireyear[1])) next()
  
  #check that fire is inside boundary
  if(length(as.vector(sf::st_area(sf::st_intersection(boundary, sierra_shape)))) == 0){
    message("Fire outside of sierra border; moving to next fire")
    next()
  }else if(sum(sf::st_area(sf::st_intersection(boundary, sierra_shape))) < sum(sf::st_area(boundary))){
    message("Fire boundary partially outside Sierra border; setting boundary to intersection")
    boundary <- tryCatch(
      {
        sf::st_intersection(boundary, sierra_shape)
      },
      error = function(cond) {
        message("Error in geometry of boundary")
        error_flag <<- TRUE
      }
    )
  }
  
  
  
  dnbr_raster <-  tryCatch(
    {
      test <- terra::rast(mtbs_dnbr[i]) %>%
        terra::project(y = "EPSG:5070") %>%
        terra::crop(boundary) %>%
        terra::mask(boundary) 
      
    },
    error=function(cond) {
      
      message(paste("Error loading raster", mtbs_dnbr[i]))
      message("Here's the original error message:")
      message(cond)
      error_flag <<- TRUE
      
    }
  )
  
  if(error_flag){
    message("Moving to next fire record")
    next()
  } 
  
  
  if(ncell(dnbr_raster) > data_length){
    message("Fire too large")
    message(ncell(dnbr_raster))
    break()
    # next()
  }
  # 
  # terra::values(dnbr_raster) <- ifelse(terra::values(dnbr_raster) < -2000 | terra::values(dnbr_raster) > 2000, 
  #                                      NA, 
  #                                      terra::values(dnbr_raster))
  
  plot(dnbr_raster)
  
  rdnbr_raster <-  tryCatch(
    {
      test2 <- terra::rast(mtbs_rdnbr[i]) %>%
        terra::project(y = "EPSG:5070") %>%
        terra::crop(boundary) %>%
        terra::mask(boundary) 
      
    },
    error=function(cond) {
      
      message(paste("Error loading raster", mtbs_rdnbr[i]))
      message("Here's the original error message:")
      message(cond)
      error_flag <<- TRUE
      
    }
  )
  
  # terra::values(rdnbr_raster) <- ifelse(terra::values(rdnbr_raster) < -2000 | terra::values(rdnbr_raster) > 2000, 
  #                                       NA, 
  #                                       terra::values(rdnbr_raster))
  
  # subtract the dNBR offset -- based on what vegetation outside the burn perimeter
  # did in the mean time, as a control for phenology/weather/etc.
  
  #offset is already included in dNBR calculation -- not needed
  
  # if(length(boundary$dNBR_offst > 0)){
  #   terra::values(dnbr_raster) <- terra::values(dnbr_raster) - as.numeric(boundary$dNBR_offst)
  #   print(paste("dNBR offset is", boundary$dNBR_offst))
  # }else{
  #   print("No dNBR offset")
  # }
  
  #severity goes 0 = masked out; 1 = low intensity or unburned; 2 = low; 3 = moderate severity; 4 = high
  #; 5 = increased greenness, 6 = non-processing mask
  #what are decimal layers for?
  #make sure errors aren't introduced by the projection process!
  sev_raster <- tryCatch(
    { terra::rast(mtbs_sev[i]) %>%
        terra::project(y = "EPSG:5070") %>%
        terra::crop(boundary) %>%
        terra::mask(boundary)
      
    },
    error=function(cond) {
      
      message(paste("Error loading raster", mtbs_dnbr[i]))
      message("Here's the original error message:")
      message(cond)
      error_flag <<- TRUE
      
    }
  )
  
  
  if(error_flag){
    message("Moving to next fire record")
    next()
  } 
  
  plot(sev_raster)
  
  #if there's too much data loaded, write it to disk and make a new data catcher
  if(I(row_tracker + ncell(dnbr_raster)) > data_length){
    message("Writing data to disk")
    write.csv(data_catcher, paste0("./Parameterization/calibration data/fire severity/data_catcher_for_calibration ", 
                                   format(Sys.time(), format = "%y-%m-%d %H-%M-%S"), ".csv"))
    data_catcher <- create_data_catcher(data_length)
    #row to start on when adding data
    row_tracker <- 1
  }
  
  
  cells_burned <- !is.na(terra::values(dnbr_raster)) & 
    # terra::values(sev_raster) > 1 & 
    terra::values(sev_raster) != 5 & 
    terra::values(sev_raster) != 6
  
  #mean dNBR of cells that burned -- equivalent to SCRPPLE output
  mean_dnbr <- mean(terra::values(dnbr_raster)[cells_burned])
  
  fire_date <- boundary$Ig_Date
  
  label = raster_firenumbers[i]
  
  
  ncells <- sum(cells_burned)
  
  data <- data.frame(fire_name = rep(label, sum(cells_burned)),
                     year = rep(boundary$fireyear[1], sum(cells_burned)),
                     x = xFromCell(dnbr_raster, which(cells_burned)),
                     y = yFromCell(dnbr_raster, which(cells_burned)),
                     dnbr = terra::values(dnbr_raster)[cells_burned],
                     rdnbr = terra::values(rdnbr_raster)[cells_burned],
                     mtbs_sev = terra::values(sev_raster)[cells_burned]
                     
  )
  
  
  data_catcher[c(row_tracker:(row_tracker+nrow(data)-1)), ] <- data
  row_tracker <- row_tracker + nrow(data)
  
  message(paste("Finished processing fire", i))
  
}

#write what's leftover after the loop
write.csv(data_catcher, paste0("./Parameterization/calibration data/fire severity/data_catcher_for_calibration ", 
                               format(Sys.time(), format = "%y-%m-%d %H-%M-%S"), ".csv"))

end_time <- Sys.time()
end_time - start_time




#-----------------------------------

fire_severity_data <- list.files("./Parameterization/calibration data/fire severity/",
                                 pattern = "catcher",
                                 full.names = TRUE)[19:29]

col_types <- list(
  fire_name = col_character(),
  x = col_double(),
  y = col_double(),
  dnbr = col_double(),
  rdnbr = col_double(),
  mtbs_sev = col_integer()
)

short <- readRDS("./Parameterization/calibration data/short_ignitions/short_sierra.RDS")%>%
  filter(FIRE_SIZE >= 8)

short_annual <- short %>%
  group_by(FIRE_YEAR) %>%
  summarize(number = n())
plot(short_annual$number ~ short_annual$FIRE_YEAR)


#readr is incredible
data_all2 <- fire_severity_data %>%
  purrr::map_df(~read_csv(., col_types = col_types))

data_all2 <- filter(data_all2, mtbs_sev > 0, year > 1999)

boxplot(data_all2$dnbr~data_all2$mtbs_sev)
table(data_all2$mtbs_sev)/nrow(data_all2)
boxplot(data_all2[data_all2$mtbs_sev > 1, ]$dnbr~data_all2[data_all2$mtbs_sev > 1, ]$mtbs_sev)
table(data_all2[data_all2$mtbs_sev > 1, ]$mtbs_sev)/nrow(data_all2[data_all2$mtbs_sev > 1, ])
