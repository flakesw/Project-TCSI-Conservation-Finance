#suppression effectiveness

#this is a little clunky -- it uses fire perimeters from GeoMAC that have some
# data we need; point data from the Short database; and processed fireline
# data from Gannon et al. 2020. Probably we could cut out the GeoMAC
# part and just use Short's data.


#Gannon, B. M., Thompson, M. P., Deming, K. Z., Bayham, J., Wei, Y., & O’Connor, C. D. (2020). 
#A Geospatial Framework to Assess Fireline Effectiveness for Large Wildfires in the Western USA. 
#Fire, 3(3), 43. https://doi.org/10.3390/fire3030043

# Short, K. C. (n.d.). Spatial wildfire occurrence data for the United States, 1992-2018 
# [FPA_FOD_20210617] (5th Edition) [Data set]. https://doi.org/10.2737/RDS-2013-0009.5



library("sf")
library("tidyverse")
library("lubridate")
library("raster")

#using data from Gannon et al 2020

fireline <- read.csv("./Parameterization/calibration data/suppression/fire_perimeters.csv")

#rename some fires to match GeoMAC
fireline$Fire[7] <- "Miles"
fireline$Fire[10] <- "Ranch"
fireline$Fire[28] <- "Arrastra Creek"
fireline$Fire[31] <- "MILLI 0843 CS"

fireline$Fire <- toupper(fireline$Fire)

#fire perimeters
perims <- sf::st_read("./Parameterization/calibration data/geomac_all_years/perims_2000_2022.shp")
perims$incidentna <- toupper(perims$incidentna)
perims <- perims[perims$incidentna %in% fireline$Fire, ]
perims <- rmapshaper::ms_simplify(perims) %>%
  right_join(fireline, by = c("incidentna" = "Fire"))
perims <- perims %>%
  # arrange(pooownerun) %>%
  filter(duplicated(incidentna) == FALSE)
  
#Short dataset
short <- sf::st_read("./Parameterization/calibration data/short_ignitions/Data/FPA_FOD_20210617.gdb", layer = "Fires")
st_geometry_type(short)
st_crs(short)
# short <- st_transform(short, 2163)
names(short)
short2 <- subset(short, FIRE_NAME %in% perims$incidentna) %>%
  right_join(as.data.frame(perims), by = c("FIRE_CODE" = "firecode", "FIRE_NAME" = "incidentna"))%>%
  arrange(desc(FIRE_YEAR)) %>%
  filter(duplicated(FIRE_NAME) == FALSE) 
rm(short)
gc()

short2 <- short2 %>%
  mutate(DISCOVERY_DATE = as.Date(DISCOVERY_DATE, origin = "1970-01-01"),
         CONT_DATE = as.Date(CONT_DATE, origin = "1970-01-01"),
         datecurren = as.Date(datecurren, origin = "1970-01-01")) %>%
  mutate(CONT_DATE = as.Date(ifelse(is.na(CONT_DATE), datecurren, CONT_DATE), origin = "1970-01-01"))
short2[1, "CONT_DATE"] <- sf::st_drop_geometry(short2[1, "datecurren"])

#FWI data

years <- short2$FIRE_YEAR
first_day <- short2$DISCOVERY_DATE
last_day <- short2$CONT_DATE


date_range <- lubridate::ymd()
for(i in 1:length(first_day)){
  if(!is.na(first_day[i]) & !is.na(last_day[i])){
    date_range_new <- seq(first_day[i], last_day[i], by = "days")
    date_range <- c(date_range, date_range_new)
  }
}

all_dates <- date_range[!duplicated(date_range)]

#TODO extract all average fire dates
all_fwi_data <- data.frame(date = all_dates,
                           min_fwi = numeric(length(all_dates)),
                           mean_fwi = numeric(length(all_dates)),
                           max_fwi = numeric(length(all_dates)))

fire_fwi <- data.frame(FIRE_NAME = character(0),
                       DATE = as.Date(integer(0), origin = "1970-01-01"),
                       FWI = numeric(0))

short2$fwi <- NA

short_box <- sf::st_bbox(short2)

#for loop to download a raster of FWI, extract FWI values for the study area,
#and extract FWI for individual fires

# pb <- progress_bar$new(total = length(all_dates))
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
    # raster::projectRaster(crs = crs(short2)) %>%
    raster::crop(short_box)
  
  all_fwi_data$min_fwi[i] <- min(values(fwi), na.rm = TRUE)
  all_fwi_data$mean_fwi[i] <- mean(values(fwi), na.rm = TRUE)
  all_fwi_data$max_fwi[i] <- max(values(fwi), na.rm = TRUE)
  
  for(j in 1:nrow(short2)){
    if(!is.na(short2[j, ]$DISCOVERY_DATE) & !is.na(short2[j, ]$CONT_DATE)){
      date_range <- as.Date(seq(short2[j, ]$DISCOVERY_DATE, short2[j, ]$CONT_DATE, by = "days"))
      if(date %in% date_range){
        fwi_val <- raster::extract(fwi, short2[j, ])
        print(fwi_val)
        fire_fwi <- rbind(fire_fwi, data.frame(FIRE_NAME = short2$FIRE_NAME[j], 
                                                  DATE = date, 
                                                  FWI = fwi_val))
      }
    }
  }
  
  # if(date %in% short2$DISCOVERY_DATE){
  #   #extract points for fires which occurred on the date
  #   points <- filter(short2, short2$DISCOVERY_DATE == date) %>%
  #     st_transform(crs(fwi))
  #   points$fwi <- raster::extract(fwi, points)
  #   
  #   #assign fwi to fires in short dataset
  #   short_ca[which(short_ca$FOD_ID %in% points$FOD_ID), ]$fwi <- points$fwi
  # }
  
  
  #delete the raster -- optional
  unlink(paste0("./calibration data/fwi/", year,"/", tail))
  
  # pb$tick()
  
}

time_elapsed <- Sys.time() - start_time



fire_fwi2 <- fire_fwi

short3 <- short2 %>%
  left_join(fire_fwi2, by = c("FIRE_NAME" = "FIRE_NAME")) %>%
  sf::st_drop_geometry() %>%
  filter(!is.na(FWI)) %>%
  group_by(FIRE_NAME) %>%
  summarise(max_fwi = max(FWI),
            mean_fwi = mean(FWI),
            min_fwi = min(FWI),
            median_fwi = median(FWI),
            days_28 = sum(FWI > 28),
            days_60 = sum(FWI>60),
            cause = first(NWCG_CAUSE_CLASSIFICATION),
            across(Perim:HTr.Ratio, mean))

short3$Prop_perim_held <- short3$Held/short3$Perim
  
plot(short3$Tr.Ratio ~ short3$max_fwi)
plot(short3$Er.Ratio ~ short3$max_fwi)
plot(short3$HEr.Ratio ~ short3$max_fwi)
plot(short3$HTr.Ratio ~ short3$max_fwi)

plot(short3$Prop_perim_held ~ short3$max_fwi)
  summary(lm(short3$Prop_perim_held ~ short3$max_fwi))
  abline(coef(lm(short3$Prop_perim_held ~ short3$max_fwi)))
  summary(lm(short3$Prop_perim_held ~ poly(short3$max_fwi, 2)))
plot(short3$Prop_perim_held ~ short3$mean_fwi)
  summary(lm(short3$Prop_perim_held ~ short3$mean_fwi))
  abline(coef(lm(short3$Prop_perim_held ~ short3$mean_fwi)))
  summary(lm(short3$Prop_perim_held ~ poly(short3$mean_fwi, 2)))
plot(short3$Prop_perim_held ~ short3$min_fwi)
  summary(lm(short3$Prop_perim_held ~ short3$min_fwi))
plot(short3$Prop_perim_held ~ short3$days_28)
  summary(lm(short3$Prop_perim_held ~ short3$days_28))
  abline(coef(lm(short3$Prop_perim_held ~ short3$days_28)))
plot(short3$Prop_perim_held ~ short3$days_60)
  summary(lm(short3$Prop_perim_held ~ short3$days_60))
  
  
#Fig. 5 from Gannon et al. 2020, coded by FWI  
ggplot() + 
  geom_point(data = short3, mapping = aes(x = Tr.Ratio, y = HTr.Ratio, col = max_fwi, size = max_fwi))
#Fig. 5 from Gannon et al. 2020, coded by FWI and ignition cause
ggplot() + 
  geom_point(data = short3, mapping = aes(x = Tr.Ratio, y = HTr.Ratio, col = cause, size = max_fwi))

ggplot() + 
  geom_point(data = short3, mapping = aes(x = mean_fwi, y = Prop_perim_held, col = cause))

ggplot() + 
  geom_point(data = short3, mapping = aes(x = mean_fwi, y = Prop_perim_held, col = cause)) + 
  geom_smooth(data = short3, mapping = aes(x = mean_fwi, y = Prop_perim_held, col = cause), method='lm')
summary(lm(Prop_perim_held ~ mean_fwi*cause, data = short3[short3$cause %in% c("Natural", "Human"), ]))
  