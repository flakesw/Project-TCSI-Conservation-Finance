#trim initial communities
library(raster)
ic_raster <- raster("C:/Users/Sam/Documents/Maps/extracted/initial_communities_DRG.tif")
ic_list <- read.csv("C:/Users/Sam/Documents/Research/TCSI conservation finance/climate_IC/TCSI_IC6.csv")

ic_reduced <- subset(ic_list, MapCode %in% values(ic_raster))

# values(ic_raster) %in% ic_reduced$MapCode

write.csv(ic_reduced, "C:/Users/Sam/Documents/Maps/extracted/initial_communities_reduced.csv")
