setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance/")

sierra_poly <- sf::st_read("../masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp")
tcsi_poly <- sf::st_read("../masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp")

final_perims <- st_read("./calibration data/geomac fire boundaries/Historic_GeoMAC_Perimeters_Combined_2000-2018-shp/US_HIST_FIRE_PERIMTRS_2000_2018_DD83.shp") %>%
  st_intersect(tcsi_poly)

plot(st_geometry(final_perims))

#TODO limit to sierra region
#TODO make polygons enclosing fires to compare with other datasets, see https://gis.stackexchange.com/questions/345823/identify-spatially-contiguous-clusters-in-raster-data-using-kmeans


