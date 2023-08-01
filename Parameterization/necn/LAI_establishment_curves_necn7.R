#LAI establishment curves

#This script has several steps:

# 1) estimate leaf area for plots
# 2) calibrate estimates to LANDIS LAI values
# 3) get number of regen per plot
# 4) fit distributions to regen~LAI values

#TODO select plots more carefully; crop to study area
#Filter to only good forested plots

#libraries
library("tidyverse")
library("rFIA")
library("fitdistrplus")
#options
options(scipen = 999)


states <- c("CA")
tables <- c("TREE","SEEDLING","PLOT")
directory <- "D:/Data/fia/rFIA_downloads"


#species reference data
sp_ref <- read.csv("D:/Data/fia/FIADB_REFERENCE/REF_SPECIES.csv")

#import fia data
#using rFIA package automatically knits the tables together; you could also
# use readr::read_csv() or import several csvs then rbind() 
fia <- readFIA(dir = directory,
               tables = tables,
               states = states)

trees <- fia$TREE
seedlings <- fia$SEEDLING
plot <- fia$PLOT

# Filter what plots to use -----------------------------------------------------
# Not all plots are in forest, some have been recently treated, etc, and we need
# to filter those out





# Assigns parameters to each species group code --------------------------------
#TODO get data from McPherson et al. 2018

# Data are ultimately from Nowak, D. 1996. Notes: Estimating Leaf Area and Leaf Biomass of Open-Grown Deciduous Urban Trees. Forest Science 42:504–507.
# Updated data are in McPherson, E. G., Q. Xiao, N. S. van Doorn, N. Johnson, S. Albers, and P. J. Peper. 2018. Shade factors for 149 taxa of in-leaf urban trees in the USA. Urban Forestry & Urban Greening 31:204–211.
# but McPherson has a complex model structure that will be a pain to translate

# There is a simplified equation where shade factor depends on diameter and species,
# found in the i-Tree manual: https://www.fs.usda.gov/nrs/pubs/gtr/gtr-nrs200-2021_appendixes/gtr_nrs200-2021_appendix3.pdf

group_ref_shade <- read.csv("./Parameterization/calibration data/REF_SPECIES_GROUP_with_shade.csv")

#this file has coefficients for this equation:
#y = 0.0617 * ln(DBH) + 0.615 + species-specific shading coefficient; DBH in cm

#similar to this> https://esajournals-onlinelibrary-wiley-com.prox.lib.ncsu.edu/doi/full/10.1002/eap.2646

trees <- trees %>%
  dplyr::mutate(DIA_cm = DIA * 2.54,
                HT_m = HT / 3.3808,
                PLOT.YEAR = paste(PLT_CN, INVYR, sep=".")) %>%
  dplyr::filter(STATUSCD == 1) %>%
  dplyr::left_join((group_ref_shade %>% dplyr::select(SPGRPCD, ShadeCoef_iTree))) %>%
  #shade factor equation from iTree
  dplyr::mutate(ShadeFactor = 0.0617 * log(DIA_cm) + 0.615 + ShadeCoef_iTree,
                CrownHeight = HT_m * CR/100,
                CrownWidth =  0.8304 + 27.82 * (DIA_cm/100) - 10.68 * ((DIA_cm/100)^2),
                CrownWidth = ifelse(DIA_cm > 100, 20, CrownWidth),
                HWR = CrownHeight / CrownWidth) %>% 
  # equation for crown width from Coombes, A., J. Martin, and D. Slater. 2019. Defining the allometry of stem and crown diameter of urban trees. Urban Forestry & Urban Greening 44:126421.
  # capped at 20 m because the quadratic shape doesn't play well after that
  dplyr::mutate(LEAF.AREA = exp( -4.3309 + 0.2942 * CrownHeight + 0.7312 * CrownWidth + 
                                   5.7217 * ShadeFactor + 
                                   -0.0148 * pi * CrownWidth*(CrownHeight + CrownWidth)/2)) %>%
  #gotta do something special if the trees are too big or too small, following the iTree equations
  #sorry this code is pretty clumsy
  dplyr::mutate(LEAF.AREA = ifelse(HWR > 2.0, 
                                   exp( -4.3309 + 0.2942 * CrownHeight + 0.7312 * (CrownHeight/2) + 
                                         5.7217 * ShadeFactor + 
                                         -0.0148 * pi * (CrownHeight/2)*(CrownHeight + (CrownHeight/2))/2) *
                                     HWR/2.0,
                                   LEAF.AREA)) %>%
  dplyr::mutate(LEAF.AREA = ifelse(HWR < 0.5,
                                   exp( -4.3309 + 0.2942 * CrownHeight + 0.7312 * (CrownHeight*2) + 
                                          5.7217 * ShadeFactor + 
                                          -0.0148 * pi * (CrownHeight*2)*(CrownHeight + (CrownHeight*2))/2) *
                                     0.5/HWR,
                                   LEAF.AREA))

plot_leaf_area <- trees %>%
  group_by(PLOT.YEAR) %>%
  #m2 per tree * trees per acre * acre per m2 = meters squared leaf area per meter squared ground
  summarise(LAI = sum(LEAF.AREA*TPA_UNADJ/4046, na.rm = TRUE)) %>% 
  filter(!is.na(LAI) & !is.infinite(LAI))

#This is probably a crazy overestimate, because the equations are for open-grown trees
#This scales the mean LAI to fit a number from remote sensing, a landis run, etc.
mean_lai_desired <- 3
plot_leaf_area <- plot_leaf_area %>%
  mutate(LAI = LAI * mean_lai_desired/mean(LAI, na.rm = TRUE))
hist(plot_leaf_area$LAI)

## get seedlings

seedlings$PLOT.YEAR <- paste(seedlings$PLT_CN, seedlings$INVYR, sep=".")



#species we want to model
spp_to_use <- c("ABMA",
                "PIPO",
                "PSME",
                "POTR5",
                "PISA2",
                "ALRU2")

#get SPCD for each species. This will be different depending on how the species are named,
#but we want a crosswalk from the names used in LANDIS to FIA SPCD somehow
#In this case, I used the USDA PLANTS symbol, which is found in the FIA species reference table
# to crosswalk to FIA species code
spcd_to_use <- sp_ref[sp_ref$SPECIES_SYMBOL %in% spp_to_use, ] %>%
  dplyr::arrange(SPECIES_SYMBOL) %>%
  as.data.frame() %>%
  `[[`("SPCD")

#TODO refactor with group_by %>% summarise
for (i in 1:length(spcd_to_use)){
  SPCD <- spcd_to_use[i]
  Table <- seedlings[seedlings$SPCD == SPCD,]
  
  if(nrow(Table) == 0) next #this breaks the rest of the code, because further down expects a column for every species
  
  Sums <- aggregate(Table$TREECOUNT, by=list(PLOT.YEAR = Table$PLOT.YEAR), FUN=sum)
  Sums$x <- ifelse(is.na(Sums$x), 0, Sums$x)
  
  colnames(Sums) <- c("PLOT.YEAR", spp_to_use[i])
  plot_leaf_area <- left_join(plot_leaf_area, Sums, by = "PLOT.YEAR")
}

plot_leaf_area <- plot_leaf_area %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

hist(plot_leaf_area$LAI)
plot(plot_leaf_area$PIPO ~ plot_leaf_area$LAI)
plot(plot_leaf_area$PSME ~ plot_leaf_area$LAI)

write.csv(plot_leaf_area, file = paste("plot_leaf_area_test.csv", sep=""))

#Fit Weibull curves to the LAI data
weibull_models <- plot_leaf_area %>%
  tidyr::pivot_longer(cols = all_of(spp_to_use),
                      names_to = "Species",
                      values_to = "Count") %>%
  group_by(Species) %>%
  filter(Count > 0) %>%
  mutate(PLOT.LAI = ifelse(LAI < 0.01, 0.01, LAI)) %>%
  group_by(Species) %>%
  summarise(weibull = list(fitdist(PLOT.LAI, "weibull")))


#plot the histogram and weibull fit for each species
pdf(file = "Weibull LAI distribution by species.pdf")
map2(.x = weibull_models$weibull, 
     .y = weibull_models$Species, 
     .f = ~ denscomp(.x, 
                     main = .y,
                     xlab = "LAI"))
dev.off()

weibull_params <- weibull_models %>%
  mutate(shape = map_dbl(.x = .$weibull, .f = ~ pluck(.x, "estimate", "shape")),
         scale = map_dbl(.x = .$weibull, .f = ~ pluck(.x, "estimate", "scale"))) %>%
  dplyr::select(-c(weibull))

write.csv(weibull_params, "weibull_params.csv")

