# create stack of annual mtbs fire severity mosaic

library("raster")
library("sf")


#get all the mtbs data to use for fire severity => combustion buoyancy

# first, unzip all of the annual MTBS mosaics -- there is a .bat file in the directory, but if
# you need it, just make a batch file in the main directory that contains all years' zip files,
# and run this script that will recursively unzip all zip files in the directory. 
# see https://serverfault.com/questions/8092/how-do-i-extract-all-archives-in-the-subdirectories-of-this-folder
#
# FOR /D /r %%F in ("*") DO (
# pushd %CD%
#   cd %%F
# FOR %%X in (*.rar *.zip) DO (
#   "C:\Program Files\7-zip\7z.exe" x "%%X"
# )
# popd
# )

template <- raster("./masks_boundaries/mask.tif")
values(template) <- 1
tcsi_poly <- sf::st_read("./masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  sf::st_zm() %>%
  sf::st_transform(crs(template))

#where are the files? Path to all .tifs
mtbs_files <- paste0("./calibration data/mtbs/severity_mosaic/composite_data/MTBS_BSmosaics/", 2000:2019, "/mtbs_CONUS_", 2000:2019, ".tif")

#import all mtbs years
# they can't be raster::stacked because the extents are inexplicably different for each year
mtbs <- lapply(mtbs_files, raster)

#reproject vector to match raster
tcsi_poly_reproject <- sf::st_transform(tcsi_poly, crs = crs(mtbs[[1]]))

# crop rasters to study area and coarsen resolution to match project files
# this is slow, maybe could be optimized
mtbs <- mtbs %>%
  purrr::map( ~ crop(.x, tcsi_poly_reproject)) %>%
  purrr::map( ~ raster::aggregate(.x, fact = 6, fun = mean)) %>%
  purrr::map( ~ projectRaster(.x, to = template)) %>%
  stack()

writeRaster(mtbs, "./calibration data/mtbs/severity_mosaic/combined_mosaic.tif", overwrite = TRUE)
