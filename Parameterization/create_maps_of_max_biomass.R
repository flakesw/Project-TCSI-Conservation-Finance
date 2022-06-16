#get maximum biomass maps from LANDIS runs
library("stars")

biomass_list <- list.files("C:/Users/Sam/Documents/GlobusEndpoint/Biomass", full.names = TRUE)

biomass_stack  <- stars::read_stars(biomass_list)

biomass_max <- stars::st_apply(biomass_stack, 1:2, max)
plot(biomass_max)

biomass_mean <- stars::st_apply(biomass_stack, 1:2, mean)
plot(biomass_mean)

# biomass_sd <- stars::st_apply(biomass_stack, 1:2,  function(x) sd(as.vector(x)))
# plot(biomass_sd)

biomass_35 <- biomass_max * 0.35
plot(biomass_35)

initial_biomass <- stars::read_stars("C:/Users/Sam/Documents/GlobusEndpoint/TotalBiomass-0.img")
plot(initial_biomass)

hist(initial_biomass)

hist(biomass_35)

change_in_biomass <- biomass_max - initial_biomass
plot(change_in_biomass)

final_mean_vs_biomass_35 <- biomass_mean - biomass_35
plot(final_mean_vs_biomass_35)

initial_vs_biomass_35 <- initial_biomass - biomass_35
plot(final_mean_vs_biomass_35)
