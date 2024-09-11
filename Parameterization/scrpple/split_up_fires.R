#split up shapefile for fire
filename <- "D:/Data/mtbs_all_fires/2021/ca3987612137920210714_20200708_20220714"

boundary <- sf::st_read("D:/Data/mtbs_all_fires/2021/ca3987612137920210714_20200708_20220714_burn_bndy.shp")
grid<-st_make_grid(boundary, cellsize = 30000, square = TRUE)
plot(st_geometry(boundary))
plot(grid, add = TRUE)

boundary_split <- st_intersection(boundary, grid)
plot(st_geometry(boundary_split))


raster <- terra::rast("D:/Data/mtbs_all_fires/2021/ca3987612137920210714_20200708_20220714_dnbr.tif") %>%
  terra::project(y = "EPSG:5070") %>%
  terra::crop(boundary) %>%
  terra::mask(boundary)
sev_raster <- terra::rast("D:/Data/mtbs_all_fires/2021/ca3987612137920210714_20200708_20220714_dnbr6.tif") %>%
  terra::project(y = "EPSG:5070") %>%
  terra::crop(boundary) %>%
  terra::mask(boundary)
rdnbr_raster <- terra::rast("D:/Data/mtbs_all_fires/2021/ca3987612137920210714_20200708_20220714_rdnbr.tif") %>%
  terra::project(y = "EPSG:5070") %>%
  terra::crop(boundary) %>%
  terra::mask(boundary)


rasters_split <- list()

for(i in 1:nrow(boundary_split)){
  sf::st_write((boundary_split[i, ]), paste0(filename, "_part_", i, "_burn_bndy.shp"), append = FALSE)
  terra::writeRaster(terra::crop(raster, vect(boundary_split[i, ])), paste0(filename, "_part_", i, "_dnbr.tif"), overwrite = TRUE)
  terra::writeRaster(terra::crop(sev_raster, vect(boundary_split[i, ])), paste0(filename, "_part_", i, "_dnbr6.tif"), overwrite = TRUE)
  terra::writeRaster(terra::crop(rdnbr_raster, vect(boundary_split[i, ])), paste0(filename, "_part_", i, "_rdnbr.tif"), overwrite = TRUE)
}


