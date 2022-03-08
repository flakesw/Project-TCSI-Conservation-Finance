#add 2020-2021 fires to dataset

library(XML)
url <- "https://ftp.wildfire.gov/public/incident_specific_data/calif_n/!2021_FEDERAL_Incidents/"
doc <- htmlParse(readLines(url), asText=TRUE)
links <- xpathSApply(doc, "//a/@href")
free(doc)
links


wanted <- links[grepl("Amelia_1\\.2.*", links)]
GetMe <- paste(url, wanted, sep = "")
lapply(seq_along(GetMe), 
       function(x) download.file(GetMe[x], wanted[x], mode = "wb"))


library(curl)
url = "ftp://ftp.wildfire.gov/public/incident_specific_data/calif_n/!2021_FEDERAL_Incidents/"
h = new_handle(dirlistonly=TRUE)
con = curl(url, "r", h)
tbl = read.table(con, stringsAsFactors=TRUE, fill=TRUE)
close(con)

tbl


library(sf)
library(tidyverse)

sierra <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  sf::st_transform("EPSG:5070")

# layers <- sf::st_layers("D:/Data/nifc_op_data/Public_EventDataArchive_2020.gdb")
op_data_2020 <- sf::st_read("D:/Data/nifc_op_data/Public_EventDataArchive_2020.gdb", layer = "EventPolygon") %>%
  dplyr::filter(FeatureCategory == "Wildfire Daily Fire Perimeter") %>%
  sf::st_transform(sierra) %>%
  sf::st_intersection(sierra)
  
op_data_2021 <- sf::st_read("D:/Data/nifc_op_data/Public_EventDataArchive_2021.gdb", layer = "Event_Polygon_2021") %>%
  dplyr::filter(FeatureCategory == "Wildfire Daily Fire Perimeter") %>%
  sf::st_transform(sierra) %>%
  sf::st_intersection(sierra)

daily_perims_2000_2018 <- sf::st_read("./Parameterization/calibration data/geomac fire boundaries/combined_boundaried_2000-2019.shp") %>%
  sf::st_transform(crs = sf::st_crs(sierra)) %>%
  sf::st_intersection(sierra) %>%
  dplyr::mutate(gisacres = as.numeric(gisacres), perimeterd = as.Date(perimeterd))

table(op_data_2020$IRWINID)  

plot(st_geometry(op_data_2020))

