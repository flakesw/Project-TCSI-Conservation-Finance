# Extract FWI from NASA global FWI dataset
# not used for analysis, but perhaps useful for someone in the future


all_dates <- seq(as.Date("1992/1/1"), as.Date("2018/12/31"), "days")

#TODO extract all average fire dates
all_fwi_data <- data.frame(date = all_dates,
                           min_fwi = numeric(length(all_dates)),
                           mean_fwi = numeric(length(all_dates)),
                           max_fwi = numeric(length(all_dates)))

short_ca$fwi <- NA

#for loop to download a raster of FWI, extract FWI values for the study area,
#and extract FWI for individual fires

pb <- progress_bar$new(total = length(all_dates))
start_time <- Sys.time()

for(i in 1:length(all_dates)){
  date <- all_dates[i]  
  
  year <- year(date)
  month <- format.Date(date, "%m")
  day <- format.Date(date, "%d")
  
  tail <- paste0(year, month, day, ".nc")
  file <- paste0("FWI.MERRA2.CORRECTED.Daily.Default.", tail)
  
  #create a directory if needed
  if(!(paste0("./calibration data/fwi/", year) %in% list.dirs("./calibration data/fwi"))){
    dir.create(paste0("./calibration data/fwi/", year))
  }
  
  #download FWI raster, ~16 mb
  download.file(url = paste0("https://portal.nccs.nasa.gov/datashare/GlobalFWI/v2.0/fwiCalcs.MERRA2/Default/MERRA2.CORRECTED/",year,"/",file), 
                destfile = paste0("./calibration data/fwi/", year,"/", tail), method = "curl", quiet = TRUE,
                cacheOK = TRUE)
  
  # raster package is the only one that really works for this, because of errors in the file.
  # it seems to work fine. Not sure if there's a way to specify a different header for netcdf?
  # TODO figure out how to suppress messages on loading
  fwi <- raster(paste0("./calibration data/fwi/", year,"/", tail), 
                varname = "MERRA2.CORRECTED_FWI",
                ncdf = TRUE, quiet = TRUE) %>%
    raster::crop(ca_region_wgs84)
  
  all_fwi_data$min_fwi[i] <- min(values(fwi), na.rm = TRUE)
  all_fwi_data$mean_fwi[i] <- mean(values(fwi), na.rm = TRUE)
  all_fwi_data$max_fwi[i] <- max(values(fwi), na.rm = TRUE)
  
  if(date %in% short_ca$DISCOVERY_DATE){
    #extract points for fires which occurred on the date
    points <- filter(short_ca, date_clean == date) %>%
      st_transform(crs(fwi))
    points$fwi <- raster::extract(fwi, points)
    
    #assign fwi to fires in short dataset
    short_ca[which(short_ca$FOD_ID %in% points$FOD_ID), ]$fwi <- points$fwi
  }
  
  
  #delete the raster -- optional
  # unlink(paste0("./calibration data/fwi/", year,"/", tail))
  
  pb$tick()
  
}

end_time <- Sys.time()
elapsed_time <- end_time - start_time

write.csv(all_fwi_data, file = "./calibration data/extracted_fwi_ca_region.csv")

# TODO: write short_ca with fwi assigned
