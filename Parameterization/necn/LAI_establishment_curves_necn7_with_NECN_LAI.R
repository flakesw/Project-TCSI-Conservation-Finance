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


#------------------------------------------------
# Crosswalk LANDIS and FIA species codes
#------------------------------------------------

func_table <- read.csv("./Models/Inputs/necn/NECN_Functional_Table.csv")
sp_table <- read.csv("./Models/Inputs/necn/NECN_Spp_Table.csv") %>%
  left_join(func_table, by = "FunctionalGroupIndex") 


#we need to assign each species group a value for KLAI and MaximumLAI
spgrp_lai <- data.frame(SPGRPCD = unique(trees$SPGRPCD),
                        FunctionalGroupIndex = numeric(length(unique(trees$SPGRPCD)))) %>%
  arrange(SPGRPCD) %>%
  mutate(FunctionalGroupIndex = ifelse(SPGRPCD %in% c(1:24), 1, FunctionalGroupIndex))%>%
  mutate(FunctionalGroupIndex = ifelse(SPGRPCD %in% c(25:47), 2, FunctionalGroupIndex))%>%
  mutate(FunctionalGroupIndex = ifelse(SPGRPCD %in% c(17,18), 4, FunctionalGroupIndex))%>%
  mutate(FunctionalGroupIndex = ifelse(SPGRPCD %in% c(48), 3, FunctionalGroupIndex)) %>%
  left_join(func_table %>% dplyr::select(FunctionalGroupIndex, KLAI, MaximumLAI))

spp_to_use_all <- sp_table$SpeciesCode

#get SPCD for each species. This will be different depending on how the species are named,
#but we want a crosswalk from the names used in LANDIS to FIA SPCD somehow
#In this case, I used the USDA PLANTS symbol, which is found in the FIA species reference table
# to crosswalk to FIA species code

sp_ref$SpeciesCode <- paste0(substr(sp_ref$GENUS, 1, 4), substr(sp_ref$SPECIES, 1, 4) %>% stringr::str_to_title()) 


spp_crosswalk <- sp_ref[sp_ref$SpeciesCode %in% spp_to_use_all, ] %>%
  dplyr::arrange(SPECIES_SYMBOL) %>%
  dplyr::select(SpeciesCode, SPCD)


# for seedlings, some species don't have enough to get good estimates. This table
# combines several SPCD for each species (e.g., ponderosa and washoe pine) to 
# improve parameter estimates for rare species
spp_crosswalk_combine <- tibble(SpeciesCode = spp_to_use_all,
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

spp_to_use <- spp_crosswalk$SpeciesCode
spcd_to_use <- spp_crosswalk$SPCD
all_spcd <- unique(trees$SPCD)

#-------------------------------------------------
# Calculate per-tree LAI contribution
#-------------------------------------------------

#we need to make sure the trees have ages so we can bin them
age_model <- readRDS("linear_models_age_from_diam.RDS") #old model from a different project
age_model2 <- lm(log(TOTAGE) ~ poly(log(DIA), 3), data = trees[!is.na(trees$DIA) & !is.na(trees$TOTAGE), ])

trees <- trees %>%
  mutate(PLOT.YEAR = paste(PLT_CN, INVYR, sep=".")) %>%
  right_join(., plots_to_use, by = c("PLT_CN" = "CN")) %>%
    dplyr::mutate(DIA_cm = DIA * 2.54,
                  HT_m = HT / 3.3808,
                  PLOT.YEAR = paste(PLT_CN, INVYR, sep=".")) %>%
    dplyr::filter(STATUSCD == 1) %>%
    dplyr::left_join(spgrp_lai)

for(i in 1:length(all_spcd)){

  spcd = all_spcd[i]
  sp = spp_crosswalk[match(spcd, spp_crosswalk$spcd), "SpeciesCode"]

  if(sp %in% age_model$SpeciesName){
    
    trees[trees$SPCD == spcd, "TOTAGE2"] <- exp(predict(age_model$model[age_model$SpeciesName == sp][[1]],
                                                       newdata = trees[trees$SPCD == spcd, ]))
    
  } else{
    trees[trees$SPCD == spcd, "TOTAGE2"] <- exp(predict(age_model2,
                                                       newdata = trees[trees$SPCD == spcd, ]))
    
  }
}

trees$age <- ifelse(is.na(trees$TOTAGE), trees$TOTAGE2, trees$TOTAGE)
breaks <- seq(0, max(trees$age, na.rm = TRUE) + (10 - max(trees$age, na.rm = TRUE) %% 10), by = 5)
trees$age_bin <- base::cut(trees$age, breaks = breaks, labels = breaks[-1], right = TRUE)

trees_bin <- trees %>%
  group_by(PLOT.YEAR, SPCD, age_bin) %>%
  summarise(cohort_biomass = sum(CARBON_AG/0.47, na.rm = TRUE) * 0.11, #sum biomass and convert to g m-2
            KLAI = KLAI[1],
            MaximumLAI = MaximumLAI[1]) %>%
  mutate(LAI_tree = MaximumLAI * cohort_biomass/(KLAI + cohort_biomass))
  
  

#calculate leaf area index per plot
plot_leaf_area <- trees_bin %>%
  group_by(PLOT.YEAR) %>%
  #m2 per tree * trees per acre * acre per m2 = meters squared leaf area per meter squared ground
  summarise(LAI = sum(LAI_tree)) %>%
  filter(!is.na(LAI) & !is.infinite(LAI)) %>%
  filter(LAI < 20)

hist(plot_leaf_area$LAI)

# #This is probably a crazy overestimate, because the equations are for open-grown trees
# #This scales the mean LAI to fit a number from remote sensing, a landis run, etc.
# mean(plot_leaf_area$LAI)
# hist(plot_leaf_area$LAI)
# mean_lai_desired <- 3
# plot_leaf_area <- plot_leaf_area %>%
#   mutate(LAI = LAI * mean_lai_desired/mean(LAI, na.rm = TRUE))
# hist(plot_leaf_area$LAI)



## get seedlings
seedlings <- seedlings %>%
  mutate(PLOT.YEAR = paste(PLT_CN, INVYR, sep="."),
         TPA_UNADJ = ifelse(is.na(TPA_UNADJ), 0, TPA_UNADJ)) %>%
  filter(PLOT.YEAR %in% plot_leaf_area$PLOT.YEAR) %>%
  # filter(TOTAGE <= 5) %>% #only useful in RMRS zone, and only collected fora  subset of trees
  group_by(PLOT.YEAR) %>%
  mutate(SEEDLING_COUNT = sum(TPA_UNADJ)) %>%
  slice_head(n = 1)


#Find how many seedlings are in each plot
#TODO refactor with group_by %>% summarise
for (i in 1:nrow(spp_crosswalk)){
  SPCD <- spp_crosswalk_combine[[i, "SPCD"]][[1]]
  Table <- seedlings[seedlings$SPCD %in% SPCD,]
  
  if(nrow(Table) == 0) next #this breaks the rest of the code, because further down expects a column for every species
  
  Sums <- aggregate(Table$TREECOUNT, by=list(PLOT.YEAR = Table$PLOT.YEAR), FUN=sum)
  Sums$x <- ifelse(is.na(Sums$x), 0, Sums$x)
  
  colnames(Sums) <- c("PLOT.YEAR", spp_to_use[i])
  plot_leaf_area <- left_join(plot_leaf_area, Sums, by = "PLOT.YEAR")
}

plot_leaf_area <- plot_leaf_area %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

spp_to_use2 <- spp_to_use[which(spp_to_use %in% names(plot_leaf_area))]

n_seedlings <- plot_leaf_area %>%
               tidyr::pivot_longer(cols = all_of(spp_to_use2),
                        names_to = "Species",
                        values_to = "Count") %>%
  group_by(Species) %>%
  summarize(total = sum(Count))



write.csv(plot_leaf_area, file = paste("plot_leaf_area_test.csv", sep=""))

#-------------------------------------
# this is more appropriate as a method -- we want to fit p(seedling | LAI),
# so we need to standardize by the number of plots in each LAI bin

#make histogram for proportion
nBins <- 30
minLAI <- 0.05
plot_seedling_histogram <- plot_leaf_area %>%
  tidyr::pivot_longer(cols = all_of(spp_to_use2),
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
           # map(pluck(1)) %>% #actually, we should use the left side of the bin, not zero -- otherwise
           #                    # we all the distributions having no regen at LAI = 0
           map(., .f = ~mean(as.numeric(.))) %>%
           unlist() %>%
           as.numeric())

ggplot(plot_seedling_histogram, aes(x = lai, y = prop_present, color = Species)) +
  geom_line() +
  xlab(label = "Leaf Area Index") +
  ylab(label = "Proportion of plots with seedlings")


#set bounds to parameter estimates to send to the function that fits weibull parameters.
#bound the location parameter at 0 so that we don't get negative P_est.
#You could set other constraints if you're not getting realistic shapes
species_bounds <- tibble(Species = spp_to_use)
species_bounds$lower = list(c(-Inf, -Inf, 0))
species_bounds$upper = list(c(Inf, Inf, Inf))

#for some species, set the minimum shape parameter to 1 to force a negative-exponential shape
species_bounds[species_bounds$Species %in% c("AescCali", "PinuJeff", "PinuPond", 
                                               "PinuSabi", "QuerChry", "QuerDoug", 
                                               "QuerGarr"), ]$upper <- list(c(1, Inf, Inf))

plot_seedling_histogram <- left_join(plot_seedling_histogram, species_bounds)

#function to fit the weibull distribution curve to binned data using NLS
fit_weibull <- function(dat, lower = c(-Inf, -Inf, 0), upper = c(Inf, Inf, Inf)) {
  print(upper)
  
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
              start=pars, algorithm='brute-force',
              upper = upper,
              lower = lower,
              weights = dat$n_plots_bin)

  #get better estimates using minpack
  res1 <- nlsLM(prop_present ~ ((a/b) * ((lai/b)^(a-1)) * exp(- (lai/b)^a)) + c, 
          data=dat,
          start=as.list(coef(res)),
          upper = upper,
          lower = lower,
          weights = dat$n_plots_bin)
  
  # #sometimes c will be negative, which would allow seedlings to sometimes be negative. 
  # #If that happens, refit the model with c = 0
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
  mutate(model = list(fit_weibull(data, data$lower[[1]], data$upper[[1]]))) %>%
  mutate(shape = broom::tidy(model) %>% pluck(., 2, 1),
         scale = broom::tidy(model) %>% pluck(., 2, 2),
         location = broom::tidy(model) %>% pluck(., 2, 3, .default = 0)) #sometimes c doesn't exist; set it to 0 in that case

#make a figure for each species with empirical data and distribution
#this is pretty gross looking but not that complicated really.
#We loop through each item of the list (.l), which loops us through each species,
#where we make a ggplot figure for each species
newdat <- list(lai = seq(0, 20, length.out = 100))
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
