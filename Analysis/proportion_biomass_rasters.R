#compare biomass to biomass max
library("terra")
library("tidyverse")

biomass_max <- terra::rast("./misc/biomass_max.tif")
biomass_initial <- terra::rast("./misc/biomass_initial.tif")

plot(biomass_max)

final_biomass_layers <- list.files("E:/TCSI LANDIS/LANDIS runs/", full.names = TRUE)
final_biomass_layers <- paste0(final_biomass_layers, "/biomass/TotalBiomass-80.img")


#-------------------------------------------------------------------------------
# Import NECN summary tables and plot biomass
#-------------------------------------------------------------------------------

# Wrangle the NECN biomass tables

# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.


#what folder do all the runs to be analyzed live in?
scenario_folder <- "E:/TCSI LANDIS/LANDIS runs/"
nodist_folder <- "E:/TCSI LANDIS/No disturbance/"
# scenario_folder <- "C:/Users/swflake/Documents/LANDIS inputs/Model templates"
# scenario_folder <- "./Models/Model run templates/"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) %>%
  `[`(grep("Scenario", .))
#TODO adjust if needed
scenarios <- scenarios[grep("Scenario8|Scenario9|Scenario10|Scenario7", scenarios)]
nodist_scenarios <- list.dirs(nodist_folder, recursive = FALSE) %>%
  `[`(grep("No disturbance", .))


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
    pluck(1, 1)%>%
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
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(5, 1)))) %>%
  mutate(mgmt = unlist(map(scenarios, get_mgmt))) %>%
  mutate(climate = ifelse(grepl(pattern = "miroc", run_name), "MIROC", 
                          ifelse(grepl(pattern = "cnrm", run_name), "CNRM", "Historical"))) 

scenario_type_nodist <- data.frame(run_name = character(length(nodist_scenarios)), 
                                   mgmt = character(length(nodist_scenarios)),
                                   climate = character(length(nodist_scenarios)))

scenario_type_nodist <- scenario_type_nodist %>%
  mutate(run_name = unlist(map(strsplit(nodist_scenarios, split = "/"), pluck(5, 1)))) %>%
  mutate(mgmt = "No disturbance") %>%
  mutate(climate = "Historical")

necn_summaries <- paste0(scenarios, "/NECN-succession-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name"))

necn_summaries_nodist <- paste0(nodist_scenarios, "/NECN-succession-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type_nodist, c("run_name" = "run_name"))

necn_summaries2 <- necn_summaries %>%
  group_by(run_name, Time) %>%
  summarise(TotalAGB = weighted.mean(AGB, NumSites),
            mgmt = mgmt[1],
            climate = climate[1])

necn_summaries2_nodist <- necn_summaries_nodist %>%
  group_by(run_name, Time) %>%
  summarise(TotalAGB = weighted.mean(AGB, NumSites),
            mgmt = mgmt[1],
            climate = climate[1])

necn_summaries <- rbind(necn_summaries, necn_summaries_nodist)
necn_summaries2 <- rbind(necn_summaries2, necn_summaries2_nodist)
#-------------------------------------------------------------------------------
# Figures
#-------------------------------------------------------------------------------

#Harvest over time
#see process_harvest_tables.R

# ggplot(data = necn_summaries2[necn_summaries2$climate %in% c("Historical"), ], mapping = aes(x = Time+2020, y = TotalAGB)) +
#   geom_point(color="steelblue") +
#   labs(title = "Aboveground biomass",
#        subtitle = "by management scenario and climate scenario",
#        y = "Average AGB (g m-2)", x = "Simulation Year") +
#   geom_smooth( color = "black") +
#   facet_wrap(~ mgmt + climate)
# 
# 
# ggplot(data = necn_summaries2, mapping = aes(x = Time+2020, y = TotalAGB)) +
#   geom_point(color="steelblue") +
#   labs(title = "Aboveground biomass",
#        subtitle = "by management scenario and climate scenario",
#        y = "Average AGB (g m-2)", x = "Simulation Year") +
#   geom_smooth( color = "black")
# 
# ggplot(data = harvest_summaries, mapping = aes(x = Time, y = AcresHarvested)) +
#   geom_point(color="steelblue") +
#   labs(title = "Area harvested",
#        subtitle = "by management scenario and climate scenario",
#        y = "Area harvested (acres)", x = "Timestep") +
#   geom_smooth( color = "black") +
#   facet_wrap(~ mgmt + climate)



#-------------------------------------------------------------------------------
# Import biomass layers
#-------------------------------------------------------------------------------

biomass_max <- terra::rast("./misc/biomass_max.tif")
biomass_initial <- terra::rast("./misc/biomass_initial.tif")

plot(biomass_max)

final_biomass_layers <- list.files("E:/TCSI LANDIS/LANDIS runs/", full.names = TRUE) %>%
  `[`(grep("Scenario", .)) %>%
  `[`(grep("historical", .))
final_biomass_layers <- list.files("E:/TCSI LANDIS/LANDIS runs/", full.names = TRUE) %>%
  # `[`(grep("Scenario8|Scenario9|Scenario10|Scenario7", .)) #CHANGE HERE to choose different scenarios
  `[`(grep("Scenario1|Scenario2|Scenario3|Scenario4|Scenario5|Scenario6", .))
# final_biomass_layers <- final_biomass_layers[-length(final_biomass_layers)] #remove the scenario 7 run with no biomass output
final_biomass_layers <- paste0(final_biomass_layers, "/biomass/TotalBiomass-80.img")
final_biomass <- rast(final_biomass_layers)

biomass_max2 <- final_biomass[[1]]
values(biomass_max2) <- values(biomass_max)

final_biomass_prop <- final_biomass/biomass_max2

mean_prop_scen1 <- mean(final_biomass_prop[[1:5]])
plot(mean_prop_scen1)
hist(mean_prop_scen1)
values(mean_prop_scen1) <- cut(values(mean_prop_scen1), c(0, .35, .6, 1, 10))
plot(mean_prop_scen1)
sum(values(mean_prop_scen1)<0.35 & !is.na(values(mean_prop_scen1)))/sum(!is.na(values(mean_prop_scen1)))
sum(values(mean_prop_scen1)<0.6 & !is.na(values(mean_prop_scen1)))/sum(!is.na(values(mean_prop_scen1)))


mean_prop_scen2 <- mean(final_biomass_prop[[6:10]])
plot(mean_prop_scen2)
hist(mean_prop_scen2)
values(mean_prop_scen2) <- cut(values(mean_prop_scen2), c(0, .35, .6, 1, 10))
plot(mean_prop_scen2)
sum(values(mean_prop_scen2)<0.35 & !is.na(values(mean_prop_scen2)))/sum(!is.na(values(mean_prop_scen2)))
sum(values(mean_prop_scen2)<0.6 & !is.na(values(mean_prop_scen2)))/sum(!is.na(values(mean_prop_scen2)))

mean_prop_scen3 <- mean(final_biomass_prop[[11:17]])
plot(mean_prop_scen3)
hist(mean_prop_scen3)
values(mean_prop_scen3) <- cut(values(mean_prop_scen3), c(0, .35, .6, 1, 10))
plot(mean_prop_scen3)
sum(values(mean_prop_scen3)<0.35 & !is.na(values(mean_prop_scen3)))/sum(!is.na(values(mean_prop_scen3)))
sum(values(mean_prop_scen3)<0.6 & !is.na(values(mean_prop_scen3)))/sum(!is.na(values(mean_prop_scen3)))

mean_prop_scen4 <- mean(final_biomass_prop[[18:22]])
plot(mean_prop_scen4)
hist(mean_prop_scen4)
values(mean_prop_scen4) <- cut(values(mean_prop_scen4), c(0, .35, .6, 1, 10))
plot(mean_prop_scen4)
sum(values(mean_prop_scen4)<0.35 & !is.na(values(mean_prop_scen4)))/sum(!is.na(values(mean_prop_scen4)))
sum(values(mean_prop_scen4)<0.6 & !is.na(values(mean_prop_scen4)))/sum(!is.na(values(mean_prop_scen4)))

mean_prop_scen5 <- mean(final_biomass_prop[[23:27]])
plot(mean_prop_scen5)
hist(mean_prop_scen5)
values(mean_prop_scen5) <- cut(values(mean_prop_scen5), c(0, .35, .6, 1, 10))
plot(mean_prop_scen5)
sum(values(mean_prop_scen5)<0.35 & !is.na(values(mean_prop_scen5)))/sum(!is.na(values(mean_prop_scen5)))
sum(values(mean_prop_scen5)<0.6 & !is.na(values(mean_prop_scen5)))/sum(!is.na(values(mean_prop_scen5)))

mean_prop_scen6 <- mean(final_biomass_prop[[28:32]])
plot(mean_prop_scen6)
hist(mean_prop_scen6)
values(mean_prop_scen6) <- cut(values(mean_prop_scen6), c(0, .35, .6, 1, 10))
plot(mean_prop_scen6)
sum(values(mean_prop_scen6)<0.35 & !is.na(values(mean_prop_scen6)))/sum(!is.na(values(mean_prop_scen6)))
sum(values(mean_prop_scen6)<0.6 & !is.na(values(mean_prop_scen6)))/sum(!is.na(values(mean_prop_scen6)))

mean_prop_scen7 <- rast("E:/TCSI LANDIS/LANDIS runs/Scenario7 - test - 20 years/biomass/TotalBiomass-70.img")/biomass_max2
plot(mean_prop_scen7)
hist(mean_prop_scen7)
values(mean_prop_scen7) <- cut(values(mean_prop_scen7), c(0, .35, .6, 1, 10))
plot(mean_prop_scen7)
sum(values(mean_prop_scen7)<0.35 & !is.na(values(mean_prop_scen7)))/sum(!is.na(values(mean_prop_scen7)))
sum(values(mean_prop_scen7)<0.6 & !is.na(values(mean_prop_scen7)))/sum(!is.na(values(mean_prop_scen7)))

mean_prop_scen7_new <- rast("E:/TCSI LANDIS/LANDIS runs/Scenario7 - test - 20 
                            years - Copy/biomass/TotalBiomass-70.img")/biomass_max2
plot(mean_prop_scen7_new)
hist(mean_prop_scen7_new)
values(mean_prop_scen7_new) <- cut(values(mean_prop_scen7_new), c(0, .35, .6, 1, 10))
plot(mean_prop_scen7_new)
sum(values(mean_prop_scen7_new)<0.35 & !is.na(values(mean_prop_scen7_new)))/sum(!is.na(values(mean_prop_scen7_new)))
sum(values(mean_prop_scen7_new)<0.6 & !is.na(values(mean_prop_scen7_new)))/sum(!is.na(values(mean_prop_scen7_new)))

mean_prop_scen8 <- rast("E:/TCSI LANDIS/LANDIS runs/Scenario8 - test - 100 years/biomass/TotalBiomass-60.img")/biomass_max2
plot(mean_prop_scen8)
hist(mean_prop_scen8)
sum(values(mean_prop_scen8)<0.35 & !is.na(values(mean_prop_scen8)))/sum(!is.na(values(mean_prop_scen8)))
sum(values(mean_prop_scen8)<0.6 & !is.na(values(mean_prop_scen8)))/sum(!is.na(values(mean_prop_scen8)))
values(mean_prop_scen8) <- cut(values(mean_prop_scen8), c(0, .35, .6, 1, 10))
plot(mean_prop_scen8)


mean_prop_scen8_new <- rast("E:/TCSI LANDIS/LANDIS runs/Scenario8 - test - 100 years - Copy/biomass/TotalBiomass-60.img")/biomass_max2
plot(mean_prop_scen8_new)
hist(mean_prop_scen8_new)
sum(values(mean_prop_scen8_new)<0.35 & !is.na(values(mean_prop_scen8_new)))/sum(!is.na(values(mean_prop_scen8_new)))
sum(values(mean_prop_scen8_new)<0.6 & !is.na(values(mean_prop_scen8_new)))/sum(!is.na(values(mean_prop_scen8_new)))
values(mean_prop_scen8_new) <- cut(values(mean_prop_scen8_new), c(0, .35, .6, 1, 10))
plot(mean_prop_scen8_new)

#TODO new scenario
mean_prop_scen8_heavy <- rast("C:/Users/swflake/Documents/LANDIS inputs/Model runs/Scenario8 - test - 100 years - heavy/biomass/TotalBiomass-80.img")/biomass_max2
plot(mean_prop_scen8_heavy)
hist(mean_prop_scen8_heavy)
sum(values(mean_prop_scen8_heavy)<0.35 & !is.na(values(mean_prop_scen8_heavy)))/sum(!is.na(values(mean_prop_scen8_heavy)))
sum(values(mean_prop_scen8_heavy)<0.6 & !is.na(values(mean_prop_scen8_heavy)))/sum(!is.na(values(mean_prop_scen8_heavy)))
values(mean_prop_scen8_heavy) <- cut(values(mean_prop_scen8_heavy), c(0, .35, .6, 1, 10))
plot(mean_prop_scen8_heavy)


mean_prop_scen9 <- rast("E:/TCSI LANDIS/LANDIS runs/Scenario9 - test - 100 years - sdi60/biomass/TotalBiomass-70.img")/biomass_max2
plot(mean_prop_scen9)
hist(mean_prop_scen9)
values(mean_prop_scen9) <- cut(values(mean_prop_scen9), c(0, .35, .6, 1, 10))
plot(mean_prop_scen9)
sum(values(mean_prop_scen9)<0.35 & !is.na(values(mean_prop_scen9)))/sum(!is.na(values(mean_prop_scen9)))
sum(values(mean_prop_scen9)<0.6 & !is.na(values(mean_prop_scen9)))/sum(!is.na(values(mean_prop_scen9)))

mean_prop_scen9_new <- rast("E:/TCSI LANDIS/LANDIS runs/Scenario9 - test - 100 years - sdi60 - Copy/biomass/TotalBiomass-70.img")/biomass_max2
plot(mean_prop_scen9_new)
hist(mean_prop_scen9_new)
values(mean_prop_scen9_new) <- cut(values(mean_prop_scen9_new), c(0, .35, .6, 1, 10))
plot(mean_prop_scen9_new)
sum(values(mean_prop_scen9_new)<0.35 & !is.na(values(mean_prop_scen9_new)))/sum(!is.na(values(mean_prop_scen9_new)))
sum(values(mean_prop_scen9_new)<0.6 & !is.na(values(mean_prop_scen9_new)))/sum(!is.na(values(mean_prop_scen9_new)))


mean_prop_scen10 <- rast("E:/TCSI LANDIS/LANDIS runs/Scenario10 - test - pillars/biomass/TotalBiomass-70.img")/biomass_max2
plot(mean_prop_scen10)
hist(mean_prop_scen10)
values(mean_prop_scen10) <- cut(values(mean_prop_scen10), c(0, .35, .6, 1, 10))
plot(mean_prop_scen10)
sum(values(mean_prop_scen10)<0.35 & !is.na(values(mean_prop_scen10)))/sum(!is.na(values(mean_prop_scen10)))
sum(values(mean_prop_scen10)<0.6 & !is.na(values(mean_prop_scen10)))/sum(!is.na(values(mean_prop_scen10)))


mean_prop_scen10_new <- rast("E:/TCSI LANDIS/LANDIS runs/Scenario10 - test - pillars - Copy/biomass/TotalBiomass-70.img")/biomass_max2
plot(mean_prop_scen10_new)
hist(mean_prop_scen10_new)
values(mean_prop_scen10_new) <- cut(values(mean_prop_scen10_new), c(0, .35, .6, 1, 10))
plot(mean_prop_scen10_new)
sum(values(mean_prop_scen10_new)<0.35 & !is.na(values(mean_prop_scen10_new)))/sum(!is.na(values(mean_prop_scen10_new)))
sum(values(mean_prop_scen10_new)<0.6 & !is.na(values(mean_prop_scen10_new)))/sum(!is.na(values(mean_prop_scen10_new)))

