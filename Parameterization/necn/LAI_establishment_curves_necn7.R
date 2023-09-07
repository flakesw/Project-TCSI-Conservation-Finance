#LAI establishment curves

#This script has several steps:

# 1) estimate leaf area for plots
# 2) calibrate estimates to LANDIS LAI values
# 3) get number of regen per plot
# 4) fit distributions to regen~LAI values

# TODO filter potential plots to only those within seeding radius of adult trees.
# This would be pretty complicated, but it would match the LANDIS logic
# Alternatively, make sure we're only using data from the correct forest type,
# elevation, or some climatic variable, to make sure that we're not reducing 
# regeneration capacity by including lots of plots outside the range of the species

#libraries
library("tidyverse")
library("rFIA")
# library("fitdistrplus")
library("nls2")
library("minpack.lm")
#options
options(scipen = 999)


states <- c("CA", "NV", "OR", "AZ")
tables <- c("TREE","SEEDLING","PLOT", "COND")
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
plot <- fia$PLOT
cond <- fia$COND
seedlings <- fia$SEEDLING

rm(fia)
gc()

# Filter what plots to use -----------------------------------------------------
# Not all plots are in forest, some have been recently treated, etc, and we need
# to filter those out

cond_to_use <- cond %>%
  filter(!(DSTRBCD1 %in% c(30,31,32,46,53,54,80)),
         !(DSTRBCD2 %in% c(30,31,32,46,53,54,80)),
         !(DSTRBCD3 %in% c(30,31,32,46,53,54,80)),
         TRTCD1 == 0 | is.na(TRTCD1),
         TRTCD2 == 0 | is.na(TRTCD2),
         TRTCD3 == 0 | is.na(TRTCD3)) %>%
  mutate(IS_FOREST = ifelse(FORTYPCD %in%(c(1:998)), 1, 0)) %>%
  group_by(PLT_CN) %>%
  summarise(total_cond = sum(CONDPROP_UNADJ),
            natural = sum(STDORGCD, na.rm = TRUE),
            treatment = sum(TRTCD1, na.rm = TRUE),
            proportion_forest = sum(CONDPROP_UNADJ * IS_FOREST)) %>%
  filter(total_cond > 0.95,
         proportion_forest > 0.95)

plots_to_use <- plot %>%
  filter(PLOT_STATUS_CD == 1) %>%
  left_join(cond_to_use, by = c("CN" = "PLT_CN")) %>%
  dplyr::select(CN, proportion_forest)


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

#wrangle trees -- subset to plots chosen above, and calculate information needed
#to estimate leaf area, and thus leaf area index per plot
trees <- trees %>%
  right_join(., plots_to_use, by = c("PLT_CN" = "CN")) %>%
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

#calculate leaf area index per plot
plot_leaf_area <- trees %>%
  group_by(PLOT.YEAR) %>%
  #m2 per tree * trees per acre * acre per m2 = meters squared leaf area per meter squared ground
  summarise(LAI = sum(LEAF.AREA*TPA_UNADJ/4046, na.rm = TRUE)) %>% 
  filter(!is.na(LAI) & !is.infinite(LAI)) %>%
  filter(LAI < 20)

#This is probably a crazy overestimate, because the equations are for open-grown trees
#This scales the mean LAI to fit a number from remote sensing, a landis run, etc.
mean(plot_leaf_area$LAI)
hist(plot_leaf_area$LAI)
mean_lai_desired <- 3
plot_leaf_area <- plot_leaf_area %>%
  mutate(LAI = LAI * mean_lai_desired/mean(LAI, na.rm = TRUE))
hist(plot_leaf_area$LAI)



## get seedlings
seedlings <- seedlings %>%
  mutate(PLOT.YEAR = paste(PLT_CN, INVYR, sep="."),
         TPA_UNADJ = ifelse(is.na(TPA_UNADJ), 0, TPA_UNADJ)) %>%
  filter(PLOT.YEAR %in% plot_leaf_area$PLOT.YEAR) %>%
  # filter(TOTAGE <= 5) %>% #only useful in RMRS zone, and only collected fora  subset of trees
  group_by(PLOT.YEAR) %>%
  mutate(SEEDLING_COUNT = sum(TPA_UNADJ)) %>%
  slice_head(n = 1)

# table(seedlings$SPCD)


#species we want to model
sp_data <- read.csv("./Models/Inputs/necn/NECN_Spp_Table.csv")
spp_to_use_all <- sp_data$SpeciesCode

#get SPCD for each species. This will be different depending on how the species are named,
#but we want a crosswalk from the names used in LANDIS to FIA SPCD somehow
#In this case, I used the USDA PLANTS symbol, which is found in the FIA species reference table
# to crosswalk to FIA species code

sp_ref$SpeciesCode <- paste0(substr(sp_ref$GENUS, 1, 4), substr(sp_ref$SPECIES, 1, 4) %>% stringr::str_to_title()) 

spcd_to_use <- sp_ref[sp_ref$SpeciesCode %in% spp_to_use_all, ] %>%
  dplyr::arrange(SPECIES_SYMBOL) %>%
  as.data.frame() %>%
  `[[`("SPCD")

#manually add SPCDs to allow some species to use data from several similar SPCDs
#Some species are not common enough in the FIA dataset, so we'll combined them together
# to increase the sample size (e.g., Pinus ponderosa and washoensis)
spp_crosswalk <- tibble(SpeciesCode = spp_to_use_all,
                        SPCD = list(15,
                                    20,
                                    c(312, 492, 231),  #bigleaf maple
                                    c(333, 807), #buckeye, shade intolerant
                                    c(351, 352), #alder, shade intolerant
                                    361, #madrone, moderate shade intolerant hardwood
                                    81, #incense-cedar, intermediate conifer
                                    c(492, 231),    #dogwood, shade tolerant understory
                                    64,    #western juniper, intolerant conifer
                                    631,    #tanoak, shade tolerant hardwood
                                    c(101, 103, 108),    #whitebark pine, shade intolerant conifer
                                    c(101, 103, 108),    #knobcone pine, shade intolerant
                                    c(101, 103, 108),    #limber pine, shade intolerant
                                    c(122, 137, 116),    #jeffrey pine, shade intolerant
                                    117,    #sugar pine, intermediate
                                    133,    #monophylla, shade intolerant
                                    c(117, 119),    #white pine, intermediate
                                    c(122, 137, 116),    #ponderosa, intolerant
                                    c(127, 122, 137, 116),    #sabiana, intolerant
                                    c(122, 137, 116),    #washoe, intolerant
                                    746,    #aspen, intolerant
                                    202,    #douglas fir, tolerant
                                    c(805, 839),    #chrysolepis, intermediate
                                    c(807, 815, 818),    #douglas, intolerant,
                                    c(807, 815, 818),    #garry, intolerant
                                    c(807, 815, 818),    #kellogg, intolerant
                                    c(805, 839),    #wislizeni, indermediate
                                    c(492, 231),    #yew, shade tolerant understory
                                    c(492, 231),    #california nutmeg, very shade tolerant understory
                                    c(202, 81, 264),    #western hemlock, shade tolerant
                                    981, #california bay, intermediate
                                    c(492, 231), #shrubs
                                    c(492, 231), #shrubs
                                    c(492, 231)))   #shrubs
                                    
                                  
# spp_crosswalk  <- sp_ref[sp_ref$SpeciesCode %in% spp_to_use, ] %>%
#                     dplyr::arrange(SPECIES_SYMBOL) %>%
#                     as.data.frame() %>%
#                     dplyr::select(SpeciesCode, SPCD)
spp_to_use <- spp_crosswalk$SpeciesCode


#Find how many seedlings are in each plot
#TODO refactor with group_by %>% summarise
for (i in 1:nrow(spp_crosswalk)){
  SPCD <- spp_crosswalk[[i, "SPCD"]][[1]]
  Table <- seedlings[seedlings$SPCD %in% SPCD,]
  
  if(nrow(Table) == 0) next #this breaks the rest of the code, because further down expects a column for every species
  
  Sums <- aggregate(Table$TREECOUNT, by=list(PLOT.YEAR = Table$PLOT.YEAR), FUN=sum)
  Sums$x <- ifelse(is.na(Sums$x), 0, Sums$x)
  
  colnames(Sums) <- c("PLOT.YEAR", spp_to_use[i])
  plot_leaf_area <- left_join(plot_leaf_area, Sums, by = "PLOT.YEAR")
}

plot_leaf_area <- plot_leaf_area %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

n_seedlings <- plot_leaf_area %>%
               tidyr::pivot_longer(cols = all_of(spp_to_use),
                        names_to = "Species",
                        values_to = "Count") %>%
  group_by(Species) %>%
  summarize(total = sum(Count))



write.csv(plot_leaf_area, file = paste("plot_leaf_area_test.csv", sep=""))

#-------------------------------------
# this is more appropriate as a method -- we want to fit p(seedling | LAI),
# so we need to standardize by the number of plots in each LAI bin

#make histogram for proportion
nBins <- 20
minLAI <- 0.1
plot_seedling_histogram <- plot_leaf_area %>%
  tidyr::pivot_longer(cols = all_of(spp_to_use),
                      names_to = "Species",
                      values_to = "Count") %>%
  mutate(LAI = ifelse(LAI < minLAI, minLAI, LAI)) %>%
  mutate(lai_bin = base::cut(LAI, breaks = nBins),
         present = ifelse(Count>0, 1, 0)) %>%
  group_by(Species, lai_bin) %>%
  summarise(n_present = sum(present),
            n_plots_bin = n(),
            prop_present = n_present / n_plots_bin) %>%
  ungroup() %>%
  group_by(Species) %>%
  mutate(prop_present = prop_present / sum(prop_present)) %>% #proportion of plots in the bin with seedlings
  mutate(lai = strsplit(as.character(lai_bin), split = ",") %>% #calculate the midpoint of the bin
           map(., .f = ~gsub("\\(|\\]", "", .)) %>%
           map(pluck(1)) %>% #actually, we should use the left side of the bin, not zero -- otherwise
                              # we all the distributions having no regen at LAI = 0
           # map(., .f = ~mean(as.numeric(.))) %>%
           unlist() %>%
           as.numeric())

#way different shapes from the pure count approach!
ggplot(plot_seedling_histogram, aes(x = lai, y = prop_present, color = Species)) +
  geom_line() +
  xlab(label = "Leaf Area Index") +
  ylab(label = "Proportion of plots with seedlings")


#function to fit the weibull distribution curve to binned data using NLS
fit_weibull <- function(dat) {
  #you might have to play with these starting values
  pars <- expand.grid(a=seq(0.1,10, len=50), #shape
                      b=seq(0.1, 20, len=10), #scale
                      c = 0 #threshold parameter; this sets the "floor" for the curve,
                      # the y-intercept for shape parameter > 1
                      # or asymptote for low values of shape parameters
                      # d=seq(0.1, 4, len=10) #removed d parameter, which scales everything vertically
                      #it does allow better fit but harder to converge
  )

  
  # a floor could be set by replacing c with a constant, rather than 
  # estimating it, though this could cause issues with convergence.
  # first round to get approximate starting values
  res <- nls2(prop_present ~ ((a/b) * ((lai/b)^(a-1)) * exp(- (lai/b)^a)) + c, data=dat,
              start=pars, algorithm='brute-force')

  #get better estimates using minpack
  res1 <- nlsLM(prop_present ~ ((a/b) * ((lai/b)^(a-1)) * exp(- (lai/b)^a)) + c, 
          data=dat,
          start=as.list(coef(res)))
  
  #sometimes c will be negative, which would allow seedlings to sometimes be negative. 
  #If that happens, refit the model with c = 0
  # if(coef(res1)[3] < 0) {
  #   res1 <- nlsLM(prop_present ~ ((a/b) * ((lai/b)^(a-1)) * exp(- (lai/b)^a)), 
  #                 data=dat,
  #                 start=as.list(coef(res)[c(1,2)]))
  # }
  
  
  return(res1)
}

#fit the models using function above, and pull out the shape and scale  
#parameters with broom::tidy()

weibull_models <- plot_seedling_histogram %>%
  # filter(Species == "AescCali") %>%
  mutate(PLOT.LAI = ifelse(lai < 0.01, 0.01, lai)) %>%
  ungroup() %>%
  dplyr::nest_by(Species) %>%
  mutate(model = list(fit_weibull(data))) %>%
  mutate(shape = broom::tidy(model) %>% pluck(., 2, 1),
         scale = broom::tidy(model) %>% pluck(., 2, 2),
         location = broom::tidy(model) %>% pluck(., 2, 3, .default = 0)) #sometimes c doesn't exist; set it to 0 in that case

#make a figure for each species with empirical data and distribution
#this is pretty gross looking but not that complicated really.
#We loop through each item of the list (.l), which loops us through each species,
#where we make a ggplot figure for each species
newdat <- list(lai = seq(0, 15, length.out = 100))
pmap(.l = list(dat = weibull_models$data, 
               sp = weibull_models$Species,
               mod = weibull_models$model),
     .f = function(dat, sp, mod){ 
       ggplot(data = dat, aes(x = lai, y = prop_present)) + 
         geom_point() + 
         ggtitle(label = sp) +
         geom_line(data = data.frame(pred = predict(mod, newdata = newdat),
                                     lai = newdat$lai),
                   aes(y = pred, x = lai))
     })


write.csv(dplyr::select(weibull_models, Species, shape, scale, location), "weibull_establishment_params.csv")