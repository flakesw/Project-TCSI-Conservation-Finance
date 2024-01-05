#ladder fuels and ndvi relationship
library(stars)
ladder <- read_stars("D:/Data/Landfire fuels/sierra/landfire_ladder_2019.tif")
plot(ladder)

modis_list <- list.files("D:/Data/modis/downloaded_appEARS", full.names = TRUE)
modis_rasters <- data.frame(file = modis_list[grep("NDVI", modis_list)])

modis_rasters$date <- str_match(modis_rasters$file, "(?:_doy)(\\d+)")[, 2] %>%
  as.Date("%Y%j")

first_date <- as.Date("2019-07-01")

modis_rasters$distance_ig <- (first_date - modis_rasters$date) 
modis_file <- modis_rasters[which.min(modis_rasters[modis_rasters$distance_ig>0, "distance_ig"]), "file"]

modis_before <- stars::read_stars(modis_file) * 0.0001

modis_before <- stars::st_warp(modis_before, dest = ladder, method = "near")

modis_before <- terra::rast(modis_before)
ladder <- terra::rast(ladder)

data <- data.frame(ndvi = modis_before[],
                   ladder = ladder[])


