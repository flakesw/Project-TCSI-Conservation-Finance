library(tidyverse)
library(sf)


# crosswalk biomass/age to SDI
ca_fia_plot <- read.csv("D:/data/fia/rFIA_downloads/CA_PLOT.csv") %>%
  dplyr::filter(!is.na(LAT) & !is.na(LON)) %>%
  sf::st_as_sf(coords = c("LON", "LAT"), remove = TRUE, crs = "EPSG:4269") %>%
  sf::st_transform(crs = "EPSG:5070")

nv_fia_plot <- read.csv("D:/data/fia/rFIA_downloads/NV_PLOT.csv")%>%
  dplyr::filter(!is.na(LAT) & !is.na(LON)) %>%
  sf::st_as_sf(coords = c("LON", "LAT"), remove = TRUE, crs = "EPSG:4269") %>%
  sf::st_transform(crs = "EPSG:5070")

all_fia_plot <- rbind(ca_fia_plot, nv_fia_plot) %>%
  sf::st_transform(crs = "EPSG:5070")

ca_fia_cond <- read.csv("D:/Data/fia/rFIA_downloads/CA_COND.csv")
nv_fia_cond <- read.csv("D:/data/fia/rFIA_downloads/NV_COND.csv")

forest_ref <- read.csv("D:/Data/fia/FIADB_REFERENCE/REF_FOREST_TYPE.csv")

all_fia_cond <- rbind(ca_fia_cond, nv_fia_cond)



sierra_shape <- sf::st_read("./Models/Inputs/masks_boundaries/WIP_Capacity_V1Draft/WIP_Capacity_V1Draft.shp") %>%
  summarise(do.union = TRUE) %>% 
  st_make_valid() %>%
  smoothr::fill_holes(threshold = 0.5) %>%
  st_transform("EPSG:5070")

tcsi_shape <- sf::st_read("./Models/Inputs/masks_boundaries/tcsi_area_shapefile/TCSI_v2.shp") %>%
  st_transform("EPSG:5070")

sierra_fia_plot <- sf::st_join(all_fia_plot, sierra_shape, join = st_within) %>%
  filter(do.union == TRUE)

plot(sf::st_geometry(sierra_shape))
plot(sf::st_geometry(sierra_fia_plot), add = TRUE)

sierra_fia_cond <- all_fia_cond %>%
  filter(PLT_CN %in% sierra_fia_plot$CN) %>%
  filter(OWNCD != 46) %>%
  mutate(IS_FOREST = ifelse(FORTYPCD %in%(c(1:998)), 1, 0)) %>%
  group_by(PLT_CN) %>%
  summarise(total_cond = sum(CONDPROP_UNADJ),
            natural = sum(STDORGCD, na.rm = TRUE),
            treatment = sum(TRTCD1, na.rm = TRUE),
            proportion_forest = sum(CONDPROP_UNADJ * IS_FOREST),
            cc = sum(CONDPROP_UNADJ * LIVE_CANOPY_CVR_PCT, na.rm = TRUE)) %>%
  filter(total_cond > 0.95 )

plot_forest_type <- all_fia_cond %>%
  group_by(PLT_CN, FORTYPCD) %>%
  summarise(total_fortypcd = sum(CONDPROP_UNADJ)) %>%
  slice_max(total_fortypcd)

sierra_fia_cond <- left_join(sierra_fia_cond, plot_forest_type, by = "PLT_CN")
sierra_fia_cond$forest_group <- forest_ref[match(sierra_fia_cond$FORTYPCD, forest_ref$VALUE), "TYPGRPCD"]

 #-------------------------------------------------------------------------------
# Match FIA data to LANDFIRE 
#-------------------------------------------------------------------------------
# bps <- terra::rast("D:/Data/landfire vegetation/BPS/landfire_bps_200_sierra/LF2016_BPS_200_CONUS/LC16_BPS_200.tif")
# plot(bps)
# 
# sierra_fia_plot <- sierra_fia_plot %>%
#   sf::st_transform(crs= st_crs(bps))
# 
# plot(sf::st_geometry(sierra_fia_plot), add = TRUE)
# 
# sierra_fia_plot$bps_code <- terra::extract(bps, vect(sierra_fia_plot), layer = "BPS_NAME")$BPS_CODE


#----
#Trees
ca_trees <- read.csv("D:/data/fia/rFIA_downloads/CA_TREE.csv") %>%
  dplyr::filter(PLT_CN %in% sierra_fia_plot$CN)

nv_trees <- read.csv("D:/data/fia/rFIA_downloads/NV_TREE.csv") %>%
  dplyr::filter(PLT_CN %in% sierra_fia_plot$CN)

sierra_trees <- rbind(ca_trees, nv_trees)

sierra_trees <- filter(sierra_trees, PLT_CN %in% sierra_fia_plot$CN)

breaks <- seq(0, max(sierra_trees$TOTAGE, na.rm = TRUE) + (10 - max(sierra_trees$TOTAGE, na.rm = TRUE) %% 10), by = 5)

sierra_trees <- sierra_trees%>%
  filter(STATUSCD == 1) %>%
  mutate(DRYBIO_TOTAL = CARBON_AG * 2,
         AGE_BIN = as.numeric(base::cut(TOTAGE, breaks)),
         SPCD = as.character(SPCD))

plot(log(TPA_UNADJ) ~ log(AGE_BIN), data = sierra_trees) #sort of linear, but really variable
plot(log(TPA_UNADJ) ~ log(DRYBIO_TOTAL), data = sierra_trees) #nice linear relationship!

test_mod <- lm(log(TPA_UNADJ) ~ log(AGE_BIN)*log(DRYBIO_TOTAL), data = sierra_trees)
summary(test_mod)

## MAke plot-level summary variables

tree_summary <- sierra_trees %>% 
  dplyr::group_by(PLT_CN) %>%
  dplyr::filter(DIA >5) %>%
  # dplyr::filter(n() > 10) %>%
  dplyr::filter(STATUSCD == 1) %>%
  dplyr::summarise(total_trees = n(),
                   trees_with_age = sum(!is.na(TOTAGE)),
                   plot_bapa = sum(0.005454*(DIA^2)*TPA_UNADJ), 
                   plot_tpa = sum(TPA_UNADJ), 
                   plot_qmd = sqrt((plot_bapa/plot_tpa)/0.005454),
                   plot_sdi = plot_tpa * ((plot_qmd/10)^(-1.605)),
                   sum_sdi = sum(TPA_UNADJ * ((DIA/10)^(-1.605))),
                   biomass = sum(DRYBIO_TOTAL * TPA_UNADJ) / 892, #convert to megagrams per ha
                   mean_age = mean(TOTAGE, na.rm = TRUE),
                   high_age = quantile(TOTAGE, 0.9, na.rm = TRUE),
                   .groups = "keep") %>%
  # filter(!is.na(high_age)) %>%
  droplevels() %>%
  left_join(., sierra_fia_plot, by = c("PLT_CN" = "CN")) %>%
  left_join(., sierra_fia_cond, by = c("PLT_CN" = "PLT_CN")) %>%
  filter(cc > 0)  %>%
  group_by(forest_group) %>%
  filter(n() >= 20)

tree_summary$conifer <- as.factor(ifelse(as.numeric(as.character(tree_summary$forest_group)) < 500, 1, 2))

tree_summary$cc_bin <- cut(tree_summary$cc, breaks = c(0, 10, 25, 40, 60, 100), labels = FALSE)

tree_summary$cc_bin <- as.factor(tree_summary$cc_bin)

tree_summary$FORTYPCD <- as.factor(tree_summary$FORTYPCD)
tree_summary$forest_group <- as.factor(tree_summary$forest_group)
 
tree_summary$sqrt_biomass <- sqrt(tree_summary$biomass)

plot(tree_summary$cc ~ I(tree_summary$biomass^(0.5)))

test <- (lm(tree_summary$cc ~ tree_summary$biomass * tree_summary$FORTYPCD))
summary(test)
test <- MASS::rlm(cc ~ sqrt(biomass) + 0, data = tree_summary)
summary(test)

tree_summary$cc_preds <- predict(test, newdata = tree_summary)
tree_summary$cc_preds[tree_summary$cc_preds < 0] <- 0
tree_summary$cc_preds_bin <- cut(tree_summary$cc_preds, breaks = c(0, 10, 25, 40, 60, 100), labels = FALSE)

# plot(tree_summary$cc_preds_bin ~ I(tree_summary$cc_bin + rnorm(nrow(tree_summary), 0, 0.1)))

tab <- table(tree_summary$cc_preds_bin, tree_summary$cc_bin)
ca <- sum(diag(tab)) / sum(tab)

plot(tree_summary$cc ~ tree_summary$biomass)
curve(predict(test, newdata = data.frame(biomass=x)), add=TRUE, lwd = 2, col = "blue")


m <- polr(cc_bin ~ sqrt(biomass) * forest_group, data = tree_summary, Hess=TRUE)

table(predict(m, newdata = tree_summary), tree_summary$cc_bin)
ca <- sum(diag(table(predict(m, newdata = tree_summary), tree_summary$cc_bin))) /
  sum(table(predict(m, newdata = tree_summary), tree_summary$cc_bin))


plot(tree_summary$cc ~ tree_summary$biomass)
for(group in unique(tree_summary$forest_group)){
  newdata <- data.frame(biomass = seq(0, 7e5, length.out = 1000),
                        forest_group = rep(group, 1000))
  lines(predict(test, newdata = newdata) ~ newdata$biomass)
}

plot(tree_summary$cc ~ tree_summary$biomass)
plot(residuals(test) ~ fitted(test))
abline(h = 0)
sqrt(mean(residuals(test)^2))

test <- nls(cc ~ SSlogis(sqrt(biomass), Asym, xmid, scal), data=tree_summary)
summary(fit)
sqrt(mean(residuals(fit)^2))

plot(tree_summary$cc ~ tree_summary$plot_bapa)
curve(predict(fit, newdata = data.frame(plot_bapa=x)), add=TRUE)
plot(tree_summary$cc ~ tree_summary$plot_bapa)
curve(predict(fit, newdata = data.frame(plot_bapa=x)), add=TRUE)

# tree_summary <- tree_summary %>%# tree_summary <- tree_summary %>%# tree_summary <- tree_summary %>%
#   filter(!is.na(mean_age))

plot(log10(tree_summary$sum_sdi) ~ log10(tree_summary$biomass),
     xlab = "Plot biomass",
     ylab = "Plot SDI")
summary(lm(log10(tree_summary$sum_sdi) ~ log10(tree_summary$biomass)))
abline(coef(lm(log10(tree_summary$sum_sdi) ~ log10(tree_summary$biomass))))

plot(log(tree_summary$sum_sdi) ~ tree_summary$mean_age)
plot(log(tree_summary$sum_sdi) ~ tree_summary$high_age)
plot(biomass ~ log(plot_tpa), data = tree_summary)
plot(log(plot_qmd) ~ log(plot_tpa), data = tree_summary)
mod <- (lm(log(sum_sdi) ~ log(biomass) + as.factor(bps_code) + mean_age, data = tree_summary[!is.na(tree_summary$bps_code) & !is.na(tree_summary$mean_age), ]))
summary(mod)
plot((predict(mod)) ~ log(tree_summary[!is.na(tree_summary$bps_code)& !is.na(tree_summary$mean_age), ]$sum_sdi),
     xlab = "observed SDI",
     ylab = "Predicted SDI")
abline(0,1)
plot(exp(predict(mod)) ~ (tree_summary[!is.na(tree_summary$bps_code)& !is.na(tree_summary$mean_age), ]$sum_sdi))


fortypcd_enough <- names(which(table(tree_summary$FORTYPCD) > 50))
bps_enough <- names(which(table(tree_summary$bps_code) > 200))

ggplot(tree_summary[as.character(tree_summary$FORTYPCD) %in% fortypcd_enough, ]) + 
  geom_point(aes(y = plot_tpa, x = plot_qmd, col = as.factor(FORTYPCD))) + 
  scale_x_log10() + 
  scale_y_log10() + 
  geom_quantile(aes(y = plot_tpa, x = plot_qmd, col = as.factor(FORTYPCD)),
              stat = "quantile", 
              quantiles = 0.9)

bps_codes <- read.csv("D:/Data/landfire vegetation/BPS/LF16_BPS_200.csv")
tree_summary$bps_name <- bps_codes[match(tree_summary$bps_code, bps_codes$BPS_CODE), ]$BPS_NAME

ggplot(tree_summary[as.character(tree_summary$bps_code) %in% bps_enough, ]) + 
  geom_point(aes(y = plot_tpa, x = plot_qmd, col = as.factor(bps_name))) + 
  scale_x_log10() + 
  scale_y_log10() + 
  geom_quantile(aes(y = plot_tpa, x = plot_qmd, col = as.factor(bps_name)),
                stat = "quantile",
                method = "rq",
                quantiles = 0.95) + 
  geom_vline(xintercept = 10) +
  geom_abline(intercept= log10(10^4.2),
              slope=-1.54, colour="blue", size = 1.5)+
  geom_abline(intercept= log10(10^4.2 * 0.65),
            slope=-1.54, colour="blue", size = 1.5)+
  geom_abline(intercept= log10(10^4.2 * 0.35),
              slope=-1.54, colour="blue", size = 1.5)

max_sdi <- data.frame(bps_code = as.double(unique(bps_enough)),
                      sdi_max = NA,
                      k_mean = NA)

for(i in c(1:nrow(max_sdi))){
  model <- tryCatch(
    {
      quantreg::rq(log10(plot_tpa) ~ log10(plot_qmd), tau = 0.95, 
         data = tree_summary[tree_summary$bps_code == max_sdi[i, "bps_code"], ])
    },
    error = function(cond) {
    }
  )
  # summary(model)
  print(coef(model))
  
  # print(max_sdi)
  max_sdi$sdi_max[i] <- 10^((log10(10)*coef(model)[[2]] + coef(model)[[1]]))
  max_sdi$k_mean[i] <- quantile(tree_summary[tree_summary$bps_code == max_sdi[i, "bps_code"], ]$plot_tpa /
    ((tree_summary[tree_summary$bps_code == max_sdi[i, "bps_code"], ]$plot_qmd)^-1.605), 0.95, na.rm = TRUE)
  
  max_sdi$sdi_max_reineke[i] <- 10^(log10(10)*-1.605 + log10(max_sdi$k_mean[i]))
}

max_sdi$sdi_max
max_sdi$sdi_max_reineke

plot(log(tree_summary$plot_qmd) ~ log(tree_summary$plot_tpa))


#map of max SDI

bps_tcsi <- bps %>% crop(vect(tcsi_shape)) %>% as.factor() %>%
  classify(rcl = bps_data[, c("VALUE", "BPS_CODE")])
plot(bps_tcsi)

bps_tcsi <- classify(bps_tcsi$BPS_CODE, rcl = max_sdi[, c("bps_code", "sdi_max")], othersNA = TRUE)
plot(bps_tcsi)

#cohort-level data analysis

age_cohort_summary <- sierra_trees %>%
  dplyr::filter(DIA > 5) %>%
  dplyr::group_by(PLT_CN, SPCD, AGE_BIN) %>%
  dplyr::summarise(cohort_biomass = sum(DRYBIO_TOTAL * TPA_UNADJ),
                   cohort_tpa = sum(TPA_UNADJ),
                   cohort_ba = sum(0.005454*(DIA^2)*TPA_UNADJ),
                   # cohort_ba2 = pi*sum((DIA/2/12)^2 * TPA_UNADJ), #equivalent
                   cohort_d = sqrt((cohort_ba/cohort_tpa)/0.005454),
                   cohort_sdi = TPA_UNADJ*(DIA/10)^1.6)  %>%
  ungroup() %>%
  dplyr::filter(cohort_biomass > 3000) %>%
  group_by(SPCD) %>%
  filter(n() > 1000) %>%
  dplyr::mutate(SPCD = as.factor(SPCD),
                AGE_BIN = as.numeric(AGE_BIN))

age_cohort_summary <- tree_summary %>%
  dplyr::select(PLT_CN, biomass) %>%
  dplyr::right_join(age_cohort_summary, by = "PLT_CN") %>%
  drop_na 

age_cohort_summary <- age_cohort_summary %>%
  left_join(sierra_fia_plot, by = c("PLT_CN" = "CN")) %>%
  left_join(., sierra_fia_cond, by = c("PLT_CN" = "PLT_CN")) %>%
  dplyr::filter(!is.na(FORTYPCD))

#fit cohort SDI model
plot(log(cohort_sdi) ~ log(cohort_biomass), data = age_cohort_summary)
plot(log(cohort_sdi) ~ log(AGE_BIN), data = age_cohort_summary)
plot(log(cohort_sdi) ~ log(cohort_d), data = age_cohort_summary)
sdi_cohort_model <- (lm(log(cohort_sdi) ~ poly(log(cohort_biomass), 3) + poly(log(AGE_BIN), 3) + SPCD, data = age_cohort_summary))
summary(sdi_cohort_model)


plot(log(cohort_d) ~ log(cohort_tpa), data = age_cohort_summary)

ggplot(age_cohort_summary) + 
  geom_point(aes(y = cohort_tpa, x = cohort_d, col = SPCD)) + 
  scale_y_log10() +
  scale_x_log10()

library("earth")
sdi_piecewise <- earth(log(cohort_sdi) ~ log(cohort_biomass) + log(AGE_BIN), 
                       data = age_cohort_summary,
                       degree = 3)
summary(sdi_piecewise)
# plot(sdi_piecewise)

sdi_robust <- (MASS::rlm(log(cohort_sdi) ~ poly(log(cohort_biomass), 3) + poly(log(AGE_BIN), 3) + SPCD, data = age_cohort_summary))
summary(sdi_robust)


summary(sdi_cohort_model)
plot(exp(fitted(sdi_cohort_model)) ~ age_cohort_summary$cohort_sdi,
     xlab = "Cohort SDI",
     ylab = "Predicted cohort SDI")#this model kinda sucks
abline(0,1) 

plot(exp(fitted(sdi_piecewise)) ~ age_cohort_summary$cohort_sdi,
     xlab = "Cohort SDI",
     ylab = "Predicted cohort SDI")
abline(0,1) 

plot(exp(fitted(sdi_robust)) ~ age_cohort_summary$cohort_sdi,
     xlab = "Cohort SDI",
     ylab = "Predicted cohort SDI")
abline(0,1) 


plot(log(cohort_ba) ~ log(cohort_biomass), data = age_cohort_summary)


plot(cohort_ba ~ cohort_d, data = age_cohort_summary) #this relationship will give us TPA

# age_cohort_summary$ba_predicted <- exp(predict(ba_model))
# plot(age_cohort_summary$ba_predicted ~ age_cohort_summary$cohort_ba)
# abline(0,1)

age_cohort_summary$tpa_calculated <- 1/(0.005454 * (age_cohort_summary$cohort_d^2) / age_cohort_summary$cohort_ba)

# age_cohort_summary$tpa_predicted <- exp(predict(tpa_model))
# plot(age_cohort_summary$tpa_predicted ~ age_cohort_summary$cohort_tpa)
# abline(0,1)
# plot(residuals(tpa_model) ~ fitted(tpa_model))

#fit model for BA
ba_model <- lm(log(cohort_ba) ~ log(cohort_biomass)*AGE_BIN + SPCD, data = age_cohort_summary)
summary(ba_model)

#fit model for TPA for cohort
tpa_model <- lm(log(cohort_tpa) ~ log(cohort_biomass)*AGE_BIN + SPCD, data = age_cohort_summary)
summary(tpa_model)
plot(residuals(tpa_model) ~ fitted(tpa_model))
plot(fitted(tpa_model) ~ log(age_cohort_summary$cohort_tpa))
abline(0,1)

plot(log(cohort_tpa) ~ log(cohort_d), data = age_cohort_summary)
plot(log(cohort_tpa) ~ log(cohort_biomass), data = age_cohort_summary)
plot(log(cohort_tpa) ~ log(AGE_BIN), data = age_cohort_summary)

d_model <- lm(log(cohort_d) ~ log(cohort_biomass)*log(AGE_BIN) + SPCD, data = age_cohort_summary)
summary(d_model)
plot(log(cohort_d) ~ log(cohort_biomass), data = age_cohort_summary)
plot(log(cohort_d) ~ log(AGE_BIN), data = age_cohort_summary)

# plot(age_cohort_summary$tpa_calculated ~ age_cohort_summary$cohort_tpa)#equivalent



# Fit model for diameter
# 
# sierra_trees_age <- sierra_trees %>%
#   filter(!is.na(AGE_BIN)) %>%
#   group_by(SPCD) %>%
#   filter(n() > 10) %>%
#   ungroup()


# plot(log(DIA) ~ log(as.numeric(AGE_BIN)), data = sierra_trees_age)
# d_model <- lm(log(DIA) ~ poly(log(AGE_BIN), 2)*SPCD, data = sierra_trees_age)
# summary(d_model)
# plot(effects::Effect(focal.predictors = "AGE_BIN", mod = d_model))

#--------------------------------------------------
#create predictions
newdata <- age_cohort_summary[age_cohort_summary$SPCD %in% sierra_trees_age$SPCD, ]

newdata$cohort_d_orig <- newdata$cohort_d

newdata$cohort_d <- exp(predict(d_model, newdata = newdata))
newdata$d_predicted <- newdata$cohort_d
newdata$ba_predicted <- exp(predict(ba_model, newdata = newdata))

newdata$tpa_predicted <- exp(predict(tpa_model, newdata = newdata)) #this is with a predicted D

newdata$tpa_calculated <-  ((0.005454 * (newdata$d_predicted)^2) / newdata$ba_predicted)^-1

newdata$sdi_cohort_predicted <- exp(predict(sdi_cohort_model, newdata = newdata))
newdata$sdi_cohort_predicted2 <- exp(predict(sdi_piecewise, newdata = newdata))

plot(newdata$d_predicted ~ newdata$cohort_d_orig)
abline(0,1)
plot(newdata$tpa_predicted ~ newdata$cohort_tpa)
abline(0,1)
error_tpa <- sqrt(sum((newdata$tpa_predicted -newdata$cohort_tpa)^2))
plot(newdata$tpa_calculated ~ newdata$cohort_tpa, log = "xy")
abline(0,1)
error_tpa2 <- sqrt(sum((newdata$tpa_calculated -newdata$cohort_tpa)^2))

new_summary <- newdata %>%
  filter(d_predicted > 5) %>%
  group_by(PLT_CN) %>%
  summarise(sdi_predicted = sum(tpa_predicted * ((d_predicted/10)^(-1.605))),
            sdi_calculated = sum(tpa_calculated * ((d_predicted/10)^(-1.605))),
            sdi_summed = sum(sdi_cohort_predicted),
            sdi_summed_piecewise = sum(sdi_cohort_predicted2)) %>%
  left_join(tree_summary, by = "PLT_CN")

plot(new_summary$sdi_summed ~ new_summary$sum_sdi, log = "xy")
abline(0,1)
summary(lm(new_summary$sdi_summed ~ new_summary$sum_sdi))

plot(new_summary$sdi_summed_piecewise ~ new_summary$sum_sdi, log = "xy")
abline(0,1)
summary(lm(new_summary$sdi_summed_piecewise ~ new_summary$sum_sdi))


plot(new_summary$sdi_predicted ~ new_summary$sum_sdi, log = "xy",
     xlab = "Observed SDI (summation)",
     ylab = "Predicted SDI")
abline(0,1)
summary(lm(new_summary$sdi_predicted ~ new_summary$sum_sdi))


plot(new_summary$sdi_calculated ~ new_summary$sum_sdi, log = "xy")
abline(0,1)
summary(lm(log(new_summary$sdi_calculated) ~ log(new_summary$sum_sdi)))



#explore plot-level data

plot(plot_sdi ~ sum_sdi, data = tree_summary)
#not a very good relationship between biomass and SDI
plot(log(sum_sdi) ~ log(biomass), data = tree_summary)

plot(sum_sdi ~ mean_age, data = tree_summary, log = c("xy"))
plot(log(plot_tpa) ~ mean_age, data = tree_summary)
summary(lm(log(plot_tpa) ~ mean_age, data = tree_summary))

plot(log(plot_tpa) ~ log(biomass), data = tree_summary)
summary(lm(log(plot_tpa) ~ log(biomass), data = tree_summary))

summary(lm(log(plot_tpa) ~ mean_age + log(biomass), data = tree_summary))

plot(plot_tpa ~ plot_bapa, data = tree_summary)

plot(plot_bapa ~ biomass, data = tree_summary)
summary(lm(plot_bapa ~ biomass, data = tree_summary))

test_mod <- lm(log(plot_tpa) ~ log(biomass)*log(mean_age), data = tree_summary)
summary(test_mod)



## chunk up by age

test <- all_trees[all_trees$PLT_CN == unique(all_trees$PLT_CN)[200], ]
plotrix::weighted.hist(test$DIA, test$TPA_UNADJ)
hist(test$DIA)
plot(test$DIA ~ test$TOTAGE)
plot(test$DRYBIO_TOTAL ~ test$TOTAGE)

plot(I(test$DRYBIO_TOTAL*test$TPA_UNADJ) ~ test$TOTAGE, log = c("xy"))

breaks <- seq(0, max(all_trees$TOTAGE) + (10 - max(all_trees$TOTAGE) %% 10), by = 5)

#really nice power relationship between age bin and biomass
test$agebin <- base::cut(test$TOTAGE, breaks)
test2 <- test %>%
  group_by(agebin) %>%
  summarise(biomass_bin = sum(DRYBIO_TOTAL*TPA_UNADJ),
            tpa = sum(TPA_UNADJ))

plot(log(test2$biomass_bin) ~ test2$agebin)
plot(tpa ~ agebin, data = test2)
plot(tpa ~ biomass_bin, data = test2)

plot(log(test2$biomass_bin) ~ test2$agebin)
plot(tpa ~ as.numeric(agebin), data = test2)
plot(tpa ~ biomass_bin, data = test2)



#----------------

library("ForestFit")







