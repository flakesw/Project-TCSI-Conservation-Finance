#use regressions to get SDI from LANDIS biomass cohorts
library("tidyverse")
library("terra")

get_percent_sdi_landscape <- function(path, year){
      
    comm_input <- read.csv(paste0(path, "/community-input-file-", year, ".csv"))

    comm_input = left_join(comm_input, sp_ref %>% dplyr::select(SPCD, SpeciesLandis),
                           by = c("SpeciesName" = "SpeciesLandis")) %>%
      filter(CohortAge > 5) #this does not remove any cohorts because of the LANDIS timestep, 
                            #but I'm leaving this here so you can change the age cutoff. This
                            #age was chosen to match a cutoff of 5 inches, from FIA data. You
                            #could do something more sophisticated
    
    comm_input <- comm_input %>%
      dplyr::rename(cohort_biomass = CohortBiomass,
                    AGE_BIN = CohortAge)
    
    #this takes a very long time, since there are about 10 million rows
    comm_input$SDI_cohort = exp(predict(sdi_cohort_model, 
                                        newdata = comm_input, 
                                        allow.new.levels = TRUE))
    
    plot_comm <- comm_input %>%
      group_by(MapCode) %>%
      summarise(SDI_pred = sum(SDI_cohort, na.rm = TRUE))
    plot_comm$SDI_plot = exp(predict(sdi_plot_correction, newdata = plot_comm)) * 0.52 #correction to match treemap
    
    
    
    #link to map
    comm_map <- terra::rast(paste0(path, "/output-community-", year, ".img"))
    
    comm_map <- terra::classify(comm_map, 
                                rcl = dplyr::select(plot_comm, MapCode, SDI_plot),
                                others = 0)
    # plot(comm_map)
    
    # writeRaster(comm_map, "initial_sdi.tif")
    
    comm_map2 <- terra::rast("./Models/Inputs/masks_boundaries/mask_9311.tif")
    values(comm_map2) <- values(comm_map)
    bps_max_sdi2 <- bps_max_sdi %>% 
      terra::project(comm_map2) %>%
      terra::crop(comm_map2) %>%
      terra::mask(comm_map2, maskvalues = c(NA, 0), updatevalue = NA)
    
    # plot(bps_max_sdi2)
    
    percent_max_sdi <- comm_map2 / bps_max_sdi2 * 100 %>%
      terra::clamp(., lower = 0, upper = 100, values = TRUE) #why isn't clamp working?
    values(percent_max_sdi)[values(percent_max_sdi)>100] <- 100
    
    return(percent_max_sdi)
   
}

#-------------------------------------------
# import data

sdi_cohort_model <- readRDS("./Parameterization/management scenario data/sdi_cohort_model.RDS")
# sdi_cohort_model <- readRDS("sdi_cohort_model.RDS")
sdi_plot_correction <- readRDS("./Parameterization/management scenario data/sdi_plot_correction_model.RDS")

sp_ref <- read.csv("./Parameterization/management scenario data/REF_SPECIES.csv") %>%
  mutate(SpeciesLandis = paste0(substr(GENUS, 1, 4), stringr::str_to_title(substr(SPECIES, 1, 4)))) %>%
  filter(SPCD != 143) #subspecies we don't want

bps_max_sdi <- terra::rast("./Parameterization/management scenario data/max_sdi_bps_dia5.tif")
bps_max_sdi[bps_max_sdi == 0] <- NA


#---------------------------------------------
# Bring in LANDIS layers


#what folder do all the runs to be analyze live in?
scenario_folder <- "E:/TCSI LANDIS/LANDIS runs"
# scenario_folder <- "./Models/Model runs"

scenarios <- list.dirs(scenario_folder, recursive = FALSE) %>%
  `[`(grep("Scenario", .))
# scenarios <- scenarios[-c(1)]
scenarios <- c(scenarios, "C:/Users/swflake/Documents/TCSI-conservation-finance/Models/Model runs/Scenario6 - miroc - test necnv7")
scenarios <- scenarios[c(85, 73)]


#some helper functions
read_plus <- function(flnm) {
  read_csv(flnm, show_col_types = FALSE) %>% 
    mutate(filename = as.character(flnm),
           run_name = basename(substr(flnm, 0, regexpr("/[^/]*$", flnm)))) 
  
}

get_mgmt <- function(scenario){
  list.files(scenario, pattern = "Scenario") %>%
    pluck(1) %>%
    as.character() %>%
    strsplit(x = ., split = "[.]") %>%
    pluck(1, 1) %>%
    strsplit(x = ., split = "[_]") %>%
    pluck(1, 1)
}

get_climate <- function(scenario){
  list.files(scenario, pattern = "NECN_Succession") %>%
    pluck(1) %>%
    as.character() %>%
    strsplit(x = ., split = "[.]") %>%
    pluck(1, 1)
}



scenario_type <- data.frame(run_name = character(length(scenarios)), 
                            mgmt = character(length(scenarios)),
                            climate = character(length(scenarios)))

scenario_type <- scenario_type %>%
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(4, 1)))) %>%
  mutate(mgmt = unlist(map(scenarios, get_mgmt))) %>%
  mutate(climate = ifelse(grepl(pattern = "miroc", run_name), "MIROC", 
                          ifelse(grepl(pattern = "cnrm", run_name), "CNRM", "Historical"))) 


#--------------------------------


years <- seq(0, 80, by = 10)

comm_inputs <- expand.grid(scenarios, years)

path <- comm_inputs[1, 1]
year <- comm_inputs[2, 1]



percent_max_sdi_rasters <- vector(mode = "list", length = nrow(comm_inputs))

for(i in 1:nrow(comm_inputs)){
  #TODO remake this a purrr::map workflow
  
  percent_max_sdi_rasters[[i]] <- get_percent_sdi_landscape(path = comm_inputs[i, 1], year = comm_inputs[i, 2])

  }

plot(percent_max_sdi_rasters[[18]],
     mar = c(2.1, 2.1, 2.1, 8.1),
     plg = list(title = "%MaxSDI",
                at = c(0, 35, 60, 100),
                c(1.5,1.5)))

hist(percent_max_sdi_rasters[[18]])
mean(percent_max_sdi_rasters[[18]][], na.rm = TRUE)


