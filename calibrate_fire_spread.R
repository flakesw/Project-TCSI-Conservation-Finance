setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance/")
geomac <- st_read("./calibration data/geomac fire boundaries/Historic_GeoMAC_Perimeters_Combined_2000-2018-shp/US_HIST_FIRE_PERIMTRS_2000_2018_DD83.shp")

plot(st_geometry(geomac))

#TODO limit to sierra region
#TODO make polygons enclosing fires to compare with other datasets, see https://gis.stackexchange.com/questions/345823/identify-spatially-contiguous-clusters-in-raster-data-using-kmeans


