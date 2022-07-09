# create some outputs for DHSVM
library(tidyverse)
library(terra)

mask <- terra::rast("./Models/Inputs/masks_boundaries/mask.tif")

project_to_template <- function(input_raster, template){
  #function to project landis input rasters with no CRS to an input file
  
    out_raster <- template
    values(out_raster) <- values(input_raster)
  
  # plot(out_raster)
  
  return(out_raster)
}

test_raster <- terra::rast("E:/TCSI LANDIS/Scenario1 - historical - Run 1/NECN/LAI-5.img")
test <- project_to_template(test_raster, mask)


#we need:
# maps of CC
# maps of LAI
# maps of vegetation type

#DHSVM format:

# ############################ Vegetation  1  ########################
# Vegetation Description   1 = Broadleaf Evergreen Forest 
# Impervious Fraction      1 = 0.0
# Detention Fraction       1 = 0
# Detention Decay          1 = 0
# Overstory Present        1 = TRUE      
# Understory Present       1 = FALSE             
# Fractional Coverage      1 = 1.0         
# Hemi Fract Coverage      1 = 
#   Clumping Factor          1 =
#   Leaf Angle A             1 =
#   Leaf Angle B             1 =
#   Scattering Parameter     1 =
#   Trunk Space              1 = 0.5         
# Aerodynamic Attenuation  1 = 2.5     
# Radiation Attenuation    1 = 0.2            # for total radiation including both beam and diffuse   
# Diffuse Radiation Attenuation  1 = 0.215   
# Max Snow Int Capacity    1 = 0.03 
# Snow Interception Eff    1 = 0.6              
# Mass Release Drip Ratio  1 = 0.4             
# Height                   1 = 25
# Canopy Gap Diameter      1 = 
#   Overstory Monthly LAI    1 = 5.0 5.0 5.0 5.0 8.0 10.0 10.0 8.0 5.0 5.0 5.0 5.0 
# Understory Monthly LAI   1 = 
#   Maximum Resistance       1 = 4000  
# Minimum Resistance       1 = 460    
# Moisture Threshold       1 = 0.33      
# Vapor Pressure Deficit   1 = 4000      
# Rpc                      1 = .108 
# Overstory Monthly Alb    1 = 0.14 0.14 0.14 0.13 0.13 0.12 0.11 0.11 0.12 0.13 0.14 0.14
# Understory Monthly Alb   1 = 
#   Number of Root Zones     1 = 3 		   
# Root Zone Depths         1 = 0.10 0.15 0.2    
# Overstory Root Fraction  1 = 0.20 0.40 0.40      
# Understory Root Fraction 1 =          
#   Monthly Light Extinction 1 = 0.065 0.065 0.065 0.065 0.065 0.065 0.065 0.065 0.065 0.065 0.065 0.065
# Canopy View Adj Factor   1 = 1.0

