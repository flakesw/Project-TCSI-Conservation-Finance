#get age cutoffs for species

diam_age_sp <- readRDS("C:/Users/Sam/Documents/Research/LANDIS-tools/Tools/DHSVM/linear_models_age_from_diam.RDS")

species <- diam_age_sp$SpeciesName
diam <- numeric()

for(i in 1:nrow(diam_age_sp)){
  diam[i] <- predict(diam_age_sp[2][[1]][[i]], newdata = data.frame(DIA = 5))
}

