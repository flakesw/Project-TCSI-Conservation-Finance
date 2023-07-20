#Calibrate tree mortality given fire severity
library("tidyverse")
library("lme4")
library("effects")
library("sf")
library("terra")
library("fuzzyjoin")

#-------------------------------------------------------------------------------
#set up MTBS data
#-------------------------------------------------------------------------------

#folder with all the individual fire data (inside this folder are each folder for each year)
#everything is already unzipped, so there's a bunch of naked .tif files in here
mtbs_folder <- "D:/Data/mtbs_all_fires"

#these rasters are squares which extend past the fire boundary
# only using rdnbr rasters -- excludes some fires based on nbr
mtbs_dnbr <- list.files(path = mtbs_folder, pattern = "*_dnbr.tif", 
                        full.names = TRUE, recursive = TRUE)

mtbs_sev <- list.files(path = mtbs_folder, pattern = "*_dnbr6.tif", 
                       full.names = TRUE, recursive = TRUE)

#fire boundaries to clip rasters to
mtbs_shape <- list.files(path = mtbs_folder, pattern = "*_burn_bndy.shp", 
                         full.names = TRUE, recursive = TRUE)

#rasters and shapefiles have different lengths -- either there are duplicates,
#extra shapefiles for some fires, or something else like that


raster_firenumbers <- mtbs_dnbr %>%
  base::basename() %>%
  base::strsplit("_") %>%
  purrr::map(1) %>%
  base::unlist()

shape_firenumbers <- mtbs_shape %>%
  base::basename() %>%
  base::strsplit("_") %>%
  purrr::map(1) %>%
  base::unlist()

#some fires have shapes but not rasters
table(shape_firenumbers %in% raster_firenumbers)
table(raster_firenumbers %in% shape_firenumbers)

mtbs_shape <- mtbs_shape[which(shape_firenumbers %in% raster_firenumbers)]

#------------------------------------------------------------------------------
#import tree mortality data
ftm_fire <- read.csv("./Parameterization/calibration data/fire_mortality/FTM_fires.csv") %>%
  mutate(Fire = toupper(Fire)) %>%
  # filter(!(Fire_type %in% "RX")) %>%
  filter(State == "California")

ftm_fire_loc <- sf::st_as_sf(ftm_fire,
                             coords = c("Longitude", "Latitude"),
                             crs = "EPSG:4326")

ftm_tree <- read.csv("./Parameterization/calibration data/fire_mortality/FTM_trees.csv") %>%
  mutate(Genus_species = gsub(" or lasiocarpa", "", Genus_species))%>%
  mutate(Genus_species = gsub(" or ponderosa", "", Genus_species))
bark_data <- read.csv("./Parameterization/calibration data/fire_mortality/Species_BarkThickness.csv") %>%
  mutate(Genus_Species = gsub(" ", "_", Genus_Species))

#crosswalk MTBS to incident name
mtbs_points <- sf::st_read("D:/Data/mtbs_mosaic/mtbs_fod_pts_data/mtbs_FODpoints_DD.shp") %>%
  sf::st_transform(st_crs(ftm_fire_loc)) %>%
  sf::st_crop(sf::st_bbox(ftm_fire_loc)) %>%
  dplyr::mutate(year = substr(Ig_Date, 1,4))
# plot(sf::st_geometry(mtbs_points))
# plot(sf::st_geometry(ftm_fire_loc), add = TRUE, col = "blue")

ftm_fire_loc$nearest_mtbs <- mtbs_points[sf::st_nearest_feature(ftm_fire_loc, mtbs_points), "Incid_Name"]
ftm_fire_loc <- ftm_fire_loc %>%
  mutate(Fire = gsub("SEKI ", "", Fire)) %>%
  mutate(Fire = gsub(" PNF", "", Fire)) %>%
  mutate(Fire = gsub("WHIS", "", Fire))

# perform fuzzy matching left join to get MTBS name if available,
# then get mean DNBR from MTBS raster layers
joined <- stringdist_join(ftm_fire_loc, mtbs_points, 
                by= c("Fire" = "Incid_Name"), #match based on team
                mode='left', #use left join
                method = "jw", #use jw distance metric
                max_dist=99, 
                distance_col='dist') %>%
  group_by(Fire) %>%
  dplyr::filter(dist == 0 & yr_fire == year) %>%
  ungroup() %>%
  mutate(raster_index = map(Event_ID, .f = function(x) grep(x, toupper(mtbs_dnbr)))) %>%
  filter(raster_index != 0) %>%
  mutate(mean_dnbr = map_dbl(Event_ID, .f = function(x){
                            raster <- terra::rast(mtbs_dnbr[grep(x, toupper(mtbs_dnbr))])
                            sev <- terra::rast(mtbs_sev[grep(x, toupper(mtbs_sev))])
                            
                            #get rid of unburned cells -- those aren't included in fires in SCRPPLE
                            values(raster)[!(values(sev) %in% c(2,3,4))] <- NA
                            
                            boundary <- sf::st_read(mtbs_shape[grep(x, toupper(mtbs_shape))])
                            offset <- boundary$dNBR_offst
                              
                            raster %>%
                            `-`(offset) %>%
                            terra::mask(terra::vect(boundary)) %>%
                            values() %>%
                            mean(na.rm = TRUE) %>% 
                            return()}))


#------------------------------------------------------------------------------
#join trees and dnbr
library("lme4")

trees <- left_join(ftm_tree, joined, by = "YrFireName") %>%
         left_join(bark_data, by = c("Genus_species" = "Genus_Species"))

trees$bark_thickness = trees$DBH_cm * trees$BT_coef

boxplot(trees$mean_dnbr ~ trees$yr2status)
t.test(trees$mean_dnbr ~ trees$yr2status)
boxplot(trees$bark_thickness ~ trees$yr2status)
t.test(trees$bark_thickness ~ trees$yr2status)

dnbr_mod <- glmer(yr2status ~ mean_dnbr + bark_thickness + (1|YrFireName) + (1|Species), 
                data = trees, family = binomial(link = "logit"))
summary(dnbr_mod) #these are the parameters for the CohortMortality equation in SCRPPLE
# plot(effects::allEffects(dnbr_mod))


#------------------------------------------------------------------------------
# Get the AgeDBH relationship
fia_trees <- read.csv("D:/data/fia/rFIA_downloads/CA_TREE.csv") %>%
  mutate(dia_cm = DIA*2.54)
table(fia_trees$SPCD)

sp_ref <- read.csv("D:/Data/fia/FIADB_REFERENCE/REF_SPECIES.csv") %>%
  mutate(SpeciesLandis = paste0(substr(GENUS, 1, 4), stringr::str_to_title(substr(SPECIES, 1, 4)))) %>%
  filter(SPCD != 143)  #subspecies we don't want

species <- read.csv("./Models/Inputs/scrpple/SCRPPLE_spp_sierra.csv")$SpeciesCode
species <- species[species %in% sp_ref$SpeciesLandis]
spcd_to_use <- sp_ref[sp_ref$SpeciesLandis %in% species, "SPCD"]


dia_age <- function(maxDBH, TOTAGE, alpha){
  return((maxDBH * TOTAGE) / (TOTAGE + alpha))
}

possibly_nls <- possibly(nls, otherwise = NA)

bark_growth_params <- fia_trees %>%
  filter(SPCD %in% spcd_to_use) %>%
  group_by(SPCD) %>%
  mutate(maxDBH = quantile(dia_cm, 0.95, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::nest_by(SPCD) %>%
  mutate(model = list(possibly_nls(dia_cm ~ dia_age(maxDBH, TOTAGE, alpha), 
                          data = data,
                          start = list(alpha = max(200, na.rm = TRUE))))) %>%
  filter(!is.na(model[1])) %>%
  mutate(alpha = broom::tidy(model) %>% pluck(., 2, 1)) %>%
  mutate(maxDBH = data$maxDBH[[1]]) %>%
  left_join(dplyr::select(sp_ref, SPCD, SPECIES_SYMBOL, SpeciesLandis), by = "SPCD") %>%
  left_join(bark_data, by = c("SPECIES_SYMBOL" = "PLANTS_Species_Code")) %>%
  mutate(maxBark = maxDBH * BT_coef)


for(i in 1:nrow(bark_growth_params)){
  if(is.na(bark_growth_params$maxBark[i])) next()
    ages <- seq(0,220)
    dnbr <- 400
    
    d <- bark_growth_params$maxBark[i] * ages/(ages + bark_growth_params$alpha[i])
    # plot(d ~ ages)
    
    p <- predict(dnbr_mod, 
               newdata = data.frame(mean_dnbr = dnbr, 
                                    bark_thickness = d),
               re.form = ~0,
               allow.new.levels = TRUE)
    prob_mort <- boot::inv.logit(p)
    plot(prob_mort ~ ages,
       xlab = "Age",
       ylab = "P(mort)",
       main = paste(bark_growth_params$SpeciesLandis[i], ", DNBR = ", dnbr))
    graphics::text(x = 0, y = min(prob_mort), pos = 4, labels = paste0("Prob of mortality at age 100 = ", round(prob_mort[101], 2)))
    
}


