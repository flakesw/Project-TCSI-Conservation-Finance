## Compare Merra2 FWI to LANDIS FWI from GridMET

#LANDIS climate
landis_clim <- read.csv("./Parameterization/calibration data/climate/Climate-future-input-log.csv")
n_sites_ecoregion <- table(rast("./Models/Inputs/input_rasters_tcsi/TCSI_ecoregions.tif")[])
n_sites_ecoregion <- data.frame(n = n_sites_ecoregion,
                                EcoregionIndex = as.numeric(names(n_sites_ecoregion)) - 1) %>%
  mutate(n = n.Freq)
landis_clim <- left_join(landis_clim, n_sites_ecoregion, by = "EcoregionIndex")
mean_fwi <- landis_clim %>%
  group_by(Year) %>%
  summarize(annual_mean_FWI = weighted.mean(FWI, n, na.rm = TRUE),
            summer_mean_FWI = weighted.mean(ifelse(Timestep >210 & Timestep < 270, FWI, NA),
                                            n, na.rm = TRUE))


tcsi_wgs <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  sf::st_transform("+proj=longlat +datum=WGS84 +no_defs") %>%
  st_make_valid()

dates_to_use <- seq(as.Date("2000-01-01"), as.Date("2020-12-31"), "days")

fwi_merra <- data.frame(Year = numeric(),
                        Timestep = numeric(),
                        mean_fwi = numeric())

for(i in 425:length(dates_to_use)){
  
  firedate <- dates_to_use[i]
  
  error_flag <- FALSE
  yearday <- format(as.Date(paste(firedate), format = "%Y-%m-%d"),
                    format = "%Y%j")
  fwi_date <- format(as.Date(paste(firedate), format = "%Y-%m-%d"),
                     format = "%Y%m%d")
  fwi_year <- as.numeric(substr(yearday, 1, 4))
  fwi_day <- as.numeric(substr(yearday, 5, 7))
  tail <- paste0(fwi_date, ".nc")
  file <- paste0("FWI.MERRA2.CORRECTED.Daily.Default.", tail)
  
  print(paste("Year = ", fwi_year))
  print(paste("Day = ", fwi_day))
  
  fwi_raster_name <- paste0("./Parameterization/calibration data/fwi/", fwi_year,"/", tail)
  
  # day_data <- spread_data %>% dplyr::filter(year_days == yearday)
  
  if(file.exists(fwi_raster_name)){
    message("FWI data already downloaded. Loading from disk.")
  } else{
    
    #create a directory if needed
    if(!(paste0("./Parameterization/calibration data/fwi/", fwi_year) %in% 
         list.dirs("./Parameterization/calibration data/fwi"))){
      dir.create(paste0("./Parameterization/calibration data/fwi/", fwi_year))
    }
    
    #download FWI raster, ~16 mb
    tryCatch(
      {
        download.file(url = paste0("https://portal.nccs.nasa.gov/datashare/GlobalFWI/v2.0/fwiCalcs.MERRA2/Default/MERRA2.CORRECTED/",fwi_year,"/",file), 
                      destfile = paste0("./Parameterization/calibration data/fwi/", fwi_year,"/", tail), method = "curl", quiet = FALSE,
                      cacheOK = TRUE)
      },
      error=function(cond) {
        
        message(paste("Error downloading climate data for ", fwi_date))
        message("Here's the original error message:")
        message(cond)
        error_flag <<- TRUE
        
      }
    )
  }
 
  fwi_raster <- terra::rast(fwi_raster_name)[["MERRA2.CORRECTED_FWI"]]
  terra::set.crs(fwi_raster, "+proj=longlat +datum=WGS84")
  fwi_raster <- fwi_raster %>%
    terra::crop(vect((tcsi_wgs))) #%>%
    # terra::project(crs(vect(boundary)))
  names(fwi_raster) <- yearday #I should use the "time" band but I couldn't figure it out
  # plot(fwi_raster)
  fwi_raster[] <- ifelse(is.na(fwi_raster[]), 0, fwi_raster[])
  
  fwi_merra[i, ] <- c(fwi_year, fwi_day, mean(fwi_raster[], na.rm = TRUE))
  
  # if(i == 1){ fwi_stack <- fwi_raster} else fwi_stack <- c(fwi_stack, fwi_raster)
}  


fwi_combined <- fwi_merra %>%
  left_join(landis_clim, by = c("Year", "Timestep"))
summary(lm(fwi_combined$FWI ~ fwi_combined$mean_fwi + 0))
            