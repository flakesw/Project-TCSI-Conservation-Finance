library(raster)
library(rgdal)
library(sf)
library(sp)
library(tidyverse)
setwd("C:/Users/Sam/Documents/Research/California forest economics")

# setwd("./TCSI_Scenario1")

# rasters use EPSG 2163 -- US National Atlas Equal Area
# NAD83 is 4269

lightning <- raster("./TCSI_Scenario1/lightning1.tif")
plot(lightning) # this looks wrong
# TODO fix lightning map
lightning$lightning1 

accidental <- raster("accidental1.tif")
plot(accidental)
#this seems reasonable?
accidental$accidental1
#lightning goes from 0-190
#accidental goes from 0-21
#why the difference?


ic_map <- raster("ic_mapcode.tif")
plot(ic_map)
image(ic_map)
cellStats(ic_map, range)
#each pixel has its own map code

ic_map2 <- raster("./TCSI_Scenario1/ic_mapcode5.tif")
plot(ic_map2)

ic_map$ic_mapcode
ic_map2$ic_mapcode5
identical(ic_map, ic_map2) #FALSE
identical(getValues(ic_map), getValues(ic_map2)) #FALSE
ic_vals <- getValues(ic_map)
ic_vals2 <- getValues(ic_map2)
ic_vals[1:100] - ic_vals2[1:100] # all = 0
which(ic_vals != ic_vals2) #just cell 2868 is not the same?

ic_vals[2868] #193130
ic_vals2[2868] #193098
# only one map code is different?
# does it matter which one we use?

ic_list <- read.csv(file = "TCSI_IC6.csv", header = T)
ic_list[ic_vals[2868], ] #Just PseuMenz, age 80, biomass 462
ic_list[ic_vals2[2868], ] #Just PseuMenz, age 110, biomass 573. Pretty much identical

length(unique(ic_list$MapCode)) #274068 map codes represented
n_rows_per_map_code1 <- aggregate(ic_list$SpeciesName, list(ic_list$MapCode), FUN = length)
# the raster has values up to 1291994 and 520000 cells -- why more values than cells?
#Does the ic list need to have 13329983 rows? Sanity check on this

test_cell1 <- ic_list[ic_list$MapCode == 717142, ]
test_cell2 <- ic_list[ic_list$MapCode == 717192, ] #neighboring pixel

aggregate(test_cell1$CohortBiomass, by = list(test_cell1$SpeciesName), FUN = sum)
aggregate(test_cell2$CohortBiomass, by = list(test_cell2$SpeciesName), FUN = sum)
#cells are pretty similar except for much higher AbieConc in cell 2

#obs: urban areas are not well masked

ic_list2 <- read.csv(".././climate_IC/180m_IC5.csv")
n_rows_per_map_code2 <- aggregate(ic_list2$SpeciesName, list(ic_list2$MapCode), FUN = length)
length(unique(ic_list2$MapCode)) #same as ic_list
test_cell_list2_1 <- ic_list2[ic_list2$MapCode == 717142, ]
aggregate(test_cell_list2_1$CohortBiomass, by = list(test_cell_list2_1$SpeciesName), FUN = sum)

head(n_rows_per_map_code1)
head(n_rows_per_map_code2)

# list2 (180m_IC5) has more species. Generally the most important species are 
# largely the same biomass


#-------------------------------------------------------------------------------
# climate files
clim_past <- read.csv(file = "TCSI_idaho_v5.csv", header = T)
clim2 <- read.csv(file = "TCSI_idaho_2.csv")
identical(clim_past, clim2) # TRUE

#these two files are the same

#-------------------------------------------------------------------------------
#fire results

day_of_fire <- raster("./social-climate-fire/day-of-fire-2.img")
plot(day_of_fire)

event_id <- raster("./social-climate-fire/event-ID-6.img")
plot(event_id)

spread_prob <- raster("./social-climate-fire/fire-spread-probability-24.img")
plot(spread_prob)

fire_intensity <- raster("./social-climate-fire/fire-intensity-25.img")
plot(fire_intensity)

#-------------------------------------------------------------------------------
# NECN 

ag_npp <- raster("./NECN/AG_NPP-20.img")
plot(ag_npp)


#scratch work
deadwood <- raster("./input_rasters_tcsi/DeadWoodBiomass-10.img")
crs(deadwood)
raster::writeRaster(deadwood, "DeadWoodBiomass-10.tif")
