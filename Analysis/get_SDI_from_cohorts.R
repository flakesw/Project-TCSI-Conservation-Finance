#use regressions to get SDI from LANDIS biomass cohorts

sdi_cohort_model_no_sp <- readRDS("./Parameterization/management scenario data/sdi_cohort_model_no_sp.RDS")
sdi_cohort_model <- readRDS("./Parameterization/management scenario data/sdi_cohort_model.RDS")

sp_ref <- read.csv("./Parameterization/management scenario data/REF_SPECIES.csv") %>%
  mutate(SpeciesLandis = paste0(substr(GENUS, 1, 4), stringr::str_to_title(substr(SPECIES, 1, 4)))) %>%
  filter(SPCD != 143) #subspecies we don't want

bps_max_sdi <- terra::rast("./Parameterization/management scenario data/max_sdi_bps_dia5_reineke.tif")
bps_max_sdi[bps_max_sdi == 0] <- NA

#---------------------------------------------
# Bring in LANDIS layers
# comm_input <- read.csv("./Parameterization/management scenario data/test_comm_data/scen7/community-input-file-80.csv")
# comm_input <- read.csv("E:/TCSI LANDIS/LANDIS runs/Scenario2 - historical - Run 1/community-input-file-0.csv")
# comm_input <- read.csv("E:/TCSI LANDIS/LANDIS runs/Scenario5 - historical - Run 1/community-input-file-80.csv")
comm_input <- read.csv("./Parameterization/management scenario data/community-input-file-0.csv")

comm_input = left_join(comm_input, sp_ref %>% dplyr::select(SPCD, SpeciesLandis),
                       by = c("SpeciesName" = "SpeciesLandis")) %>%
  filter(CohortAge > 10, #this is all of the cohorts because of the LANDIS timestep, but I'm leaving this here out of principle
         CohortBiomass > 50) #get rid of the tiny cohorts
comm_input <- comm_input %>%
  dplyr::rename(cohort_biomass = CohortBiomass,
                AGE_BIN = CohortAge)

#this takes a very long time, since there are about 10 million rows
comm_input$SDI_cohort = exp(predict(sdi_cohort_model, 
                                    newdata = comm_input, 
                                    allow.new.levels = TRUE))
comm_input$SDI_cohort2 = exp(predict(sdi_cohort_model_no_sp,
                                    newdata = comm_input,
                                    allow.new.levels = TRUE))
comm_input$SDI_cohort <- ifelse(comm_input$SDI_cohort > 20, 20, comm_input$SDI_cohort)

plot_comm <- comm_input %>%
  group_by(MapCode) %>%
  summarise(SDI_pred = sum(SDI_cohort, na.rm = TRUE))
# plot_comm$SDI_plot = exp(predict(sdi_plot_correction, newdata = plot_comm)) * 0.6 #correction to match treemap
plot_comm$SDI_plot <- plot_comm$SDI_pred
hist(plot_comm$SDI_plot)

#link to map
# comm_map <- terra::rast("./Parameterization/management scenario data/test_comm_data/scen7/output-community-80.img")
comm_map <- terra::rast("./Parameterization/management scenario data/output-community-0.img")
# comm_map <- terra::rast("E:/TCSI LANDIS/LANDIS runs/Scenario2 - historical - Run 1/output-community-0.img")
# comm_map <- terra::rast("E:/TCSI LANDIS/LANDIS runs/Scenario5 - historical - Run 1/output-community-80.img")

# table(values(comm_map) %in% plot_comm$MapCode)
# table(values(comm_map)[!(values(comm_map) %in% comm_input$MapCode)])

comm_map <- terra::classify(comm_map, 
                            rcl = dplyr::select(plot_comm, MapCode, SDI_plot),
                            others = 0)
plot(comm_map)

# writeRaster(comm_map, "./Parameterization/management scenario data/initial_LANDIS_sdi_10yr_correction.tif")

#TODO save initial SDI map and max SDI map
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
plot(percent_max_sdi,
     mar = c(2.1, 2.1, 2.1, 8.1),
     plg = list(title = "%MaxSDI",
                at = c(0, 35, 60, 100),
                c(1.5,1.5)))

hist(percent_max_sdi,
     main = "%MaxSDI")
mean(values(percent_max_sdi), na.rm = TRUE)

library("vioplot")
vioplot(values(percent_max_sdi)[!is.na(values(percent_max_sdi))])
mean(values(percent_max_sdi), na.rm = TRUE)
# terra::writeRaster(percent_max_sdi, "percent_max_sdi_initial.tif")