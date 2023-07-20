#calibrate LAI
library("raster")
library("tidyverse")

setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance/Scenario1 - one cell/")

#one-cell landscape
#import LAI rasters

lai_rast_list <- paste0("NECN/LAI-", seq(5, 80, by = 5), ".img")

lai_stack <- stack(lai_rast_list)

lai_vals <- sapply(unstack(lai_stack), function(r){max(values(r))})

#full rasters
setwd("C:/Users/Sam/Documents/Research/TCSI conservation finance/subset_scenario3 - new suppression/")

lai_rast_list <- paste0("NECN/LAI-", seq(5, 40, by = 5), ".img")

lai_stack <- stack(lai_rast_list)

lai_vals <- sapply(unstack(lai_stack), function(r){mean(values(r))})
