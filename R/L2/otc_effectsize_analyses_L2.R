# TITLE:          Effect size analyses
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske
# DATA INPUT:     L2 manipulated data from the L2 folder
# DATA OUTPUT:    Effect size analyses
# DATE:           Jan 2023


# clear environment
rm(list = ls())

# Load packages
library(tidyverse)
library(metafor)
library(plotrix)
library(multcomp)

# set working directory
MA_dir<-Sys.getenv("MADIR")

# read in data
esmd_clean <- read.csv(file.path(MA_dir,"L2/otc_effect_sizes_L2.csv")) # main data
esmd_clean_allyears <- read.csv(file.path(MA_dir,"L2/otc_data_cleaned_allyears_L2.csv")) # data for all years



#### preliminary cleaning ####
# removing variables that have <10 effect sizes
esmd_clean %>%
  group_by(Var_type_broad) %>%
  summarize(count = n())
esmd_clean <- esmd_clean %>%
  filter(!(Var_type_broad == "Biomass_total" |
             Var_type_broad == "Flower_weight" |
             Var_type_broad == "Phen_leaf_lifespan" |
             Var_type_broad == "Phen_preflwr_length" |
             Var_type == "LMA"))
esmd_clean_allyears <- esmd_clean_allyears %>%
  filter(!(Var_type_broad == "Biomass_total" |
             Var_type_broad == "Flower_weight" |
             Var_type_broad == "Phen_leaf_lifespan" |
             Var_type_broad == "Phen_preflwr_length" |
             Var_type == "LMA"))

# fixing species names for measurements that don't have a species
esmd_clean$Genus_Species[esmd_clean$Genus_Species == "_"] <- ""
esmd_clean_allyears$Genus_Species[esmd_clean_allyears$Genus_Species == "_"] <- ""

# which species do not have data from BIEN?
test <- esmd_clean %>%
  group_by(Genus_Species, Func_group) %>%
  summarise(has_lat = ifelse(any(!is.na(Max_lat_of_species)), "True", "False")) %>%
  filter(has_lat == "False") %>%
  filter(!(Genus_Species == ""))

## fixing species for random effect
# if species is blank, then input func type, if not keep species listed
# main data
esmd_clean2 <- esmd_clean
esmd_clean2$Genus_Species <- ifelse(esmd_clean2$Genus_Species == "",
                                    esmd_clean2$Func_group,
                                    esmd_clean2$Genus_Species)
esmd_clean2$Pub_number <- as.factor(esmd_clean2$Pub_number) # making pub number a factor
# data for all years
esmd_clean_allyears2 <- esmd_clean_allyears
esmd_clean_allyears2$Genus_Species <- ifelse(esmd_clean_allyears2$Genus_Species == "",
                                             esmd_clean_allyears2$Func_group,
                                             esmd_clean_allyears$Genus_Species)
esmd_clean_allyears2$Pub_number <- as.factor(esmd_clean_allyears2$Pub_number) # making pub number a factor



#### main model ####
## all variables & year limited
res.rma.all <- rma.mv(yi, vi, mods=~Var_type_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_clean)
res.rma.all

## all variables & year limited - species fixed
res.rma.all2 <- rma.mv(yi, vi, mods=~Var_type_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_clean2)
res.rma.all2 ## using this model, results are the same as the first model but its more accurate since all measurements are included

## all variables & years - species fixed
res.rma.all.allyears2 <- rma.mv(yi, vi, mods=~Var_type_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_clean_allyears2)
res.rma.all.allyears2



#### trait-specific models ####
## aboveground biomass
esmd_ab_biomass <- esmd_clean2 %>%
  filter(Var_type_broad == "Biomass_above")
# removing latitudes < 0 for range edge analyses
esmd_ab_biomass_latdiff <- esmd_ab_biomass %>%
  filter(!(Latitude <= 0))
# removing func groups that are blank - only from functional group test
esmd_ab_biomass2 <- esmd_ab_biomass %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_ab_biomass3 <- esmd_ab_biomass %>%
  filter(!(Native_Status == ""))
# models for aboveground biomass
res.rma.ab.biomass <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_biomass)
res.rma.ab.biomass
model_preds_ab_biomass <- predict(res.rma.ab.biomass)
res.rma.ab.biomass2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_biomass2)
res.rma.ab.biomass2
summary(glht(res.rma.ab.biomass2, linfct=cbind(contrMat(rep(1,8), type="Tukey"))), test=adjusted("holm"))
res.rma.ab.biomass3 <- rma.mv(yi, vi, mods=~Abs_Latitude,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_biomass)
res.rma.ab.biomass3
model_preds_ab_biomass3 <- predict(res.rma.ab.biomass3)
res.rma.ab.biomass4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_biomass_latdiff)
res.rma.ab.biomass4
model_preds_ab_biomass4 <- predict(res.rma.ab.biomass4)
res.rma.ab.biomass5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_biomass)
res.rma.ab.biomass5
model_preds_ab_biomass5 <- predict(res.rma.ab.biomass5)
res.rma.ab.biomass6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_ab_biomass)
res.rma.ab.biomass6
summary(glht(res.rma.ab.biomass6, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.ab.biomass7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_biomass3)
res.rma.ab.biomass7
summary(glht(res.rma.ab.biomass7, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.ab.biomass8 <- rma.mv(yi, vi, mods=~Elevation_m,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_biomass)
res.rma.ab.biomass8
res.rma.ab.biomass9 <- rma.mv(yi, vi, mods=~Mean_annual_temp,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_biomass)
res.rma.ab.biomass9
model_preds_ab_biomass9 <- predict(res.rma.ab.biomass9)
res.rma.ab.biomass10 <- rma.mv(yi, vi, mods=~Mean_annual_precip,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_biomass)
res.rma.ab.biomass10
model_preds_ab_biomass10 <- predict(res.rma.ab.biomass10)


## belowground biomass
esmd_bl_biomass <- esmd_clean2 %>%
  filter(Var_type_broad == "Biomass_below")
# removing latitudes < 0 for range edge analyses
esmd_bl_biomass_latdiff <- esmd_bl_biomass %>%
  filter(!(Latitude <= 0))
# removing func groups that are blank - only from functional group test
esmd_bl_biomass2 <- esmd_bl_biomass %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_bl_biomass3 <- esmd_bl_biomass %>%
  filter(!(Native_Status == ""))
# models for belowground biomass
res.rma.bl.biomass <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_bl_biomass)
res.rma.bl.biomass
model_preds_bl_biomass <- predict(res.rma.bl.biomass)
res.rma.bl.biomass2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_bl_biomass2)
res.rma.bl.biomass2
summary(glht(res.rma.bl.biomass2, linfct=cbind(contrMat(rep(1,4), type="Tukey"))), test=adjusted("holm"))
res.rma.bl.biomass3 <- rma.mv(yi, vi, mods=~Abs_Latitude,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_bl_biomass)
res.rma.bl.biomass3
model_preds_bl_biomass3 <- predict(res.rma.bl.biomass3)
res.rma.bl.biomass4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_bl_biomass_latdiff)
res.rma.bl.biomass4
model_preds_bl_biomass4 <- predict(res.rma.bl.biomass4)
res.rma.bl.biomass5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_bl_biomass)
res.rma.bl.biomass5
model_preds_bl_biomass5 <- predict(res.rma.bl.biomass5)
res.rma.bl.biomass6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_bl_biomass)
res.rma.bl.biomass6
summary(glht(res.rma.bl.biomass6, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.bl.biomass7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_bl_biomass3)
res.rma.bl.biomass7
summary(glht(res.rma.bl.biomass7, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.bl.biomass8 <- rma.mv(yi, vi, mods=~Elevation_m,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_bl_biomass)
res.rma.bl.biomass8
res.rma.bl.biomass9 <- rma.mv(yi, vi, mods=~Mean_annual_temp,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_bl_biomass)
res.rma.bl.biomass9
model_preds_bl_biomass9 <- predict(res.rma.bl.biomass9)
res.rma.bl.biomass10 <- rma.mv(yi, vi, mods=~Mean_annual_precip,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_bl_biomass)
res.rma.bl.biomass10
model_preds_bl_biomass10 <- predict(res.rma.bl.biomass10)


## flower number
esmd_flwr_num <- esmd_clean2 %>%
  filter(Var_type_broad == "Flower_num")
# removing latitudes < 0 for range edge analyses
esmd_flwr_num_latdiff <- esmd_flwr_num %>%
  filter(!(Latitude <= 0))
# removing func groups that are blank - only from functional group test
esmd_flwr_num2 <- esmd_flwr_num %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_flwr_num3 <- esmd_flwr_num %>%
  filter(!(Native_Status == ""))
# models for flower number
res.rma.flwr.num <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_flwr_num)
res.rma.flwr.num
model_preds_flwr_num <- predict(res.rma.flwr.num)
res.rma.flwr.num2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_flwr_num2)
res.rma.flwr.num2
summary(glht(res.rma.flwr.num2, linfct=cbind(contrMat(rep(1,3), type="Tukey"))), test=adjusted("holm"))
res.rma.flwr.num3 <- rma.mv(yi, vi, mods=~Abs_Latitude,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_flwr_num)
res.rma.flwr.num3
model_preds_flwr_num3 <- predict(res.rma.flwr.num3)
res.rma.flwr.num4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_flwr_num_latdiff)
res.rma.flwr.num4
model_preds_flwr_num4 <- predict(res.rma.flwr.num4)
res.rma.flwr.num5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_flwr_num)
res.rma.flwr.num5
model_preds_flwr_num5 <- predict(res.rma.flwr.num5)
res.rma.flwr.num6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_flwr_num)
res.rma.flwr.num6
summary(glht(res.rma.flwr.num6, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.flwr.num7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_flwr_num3)
res.rma.flwr.num7
summary(glht(res.rma.flwr.num7, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.flwr.num8 <- rma.mv(yi, vi, mods=~Elevation_m, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_flwr_num)
res.rma.flwr.num8
res.rma.flwr.num9 <- rma.mv(yi, vi, mods=~Mean_annual_temp, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_flwr_num)
res.rma.flwr.num9
model_preds_flwr_num9 <- predict(res.rma.flwr.num9)
res.rma.flwr.num10 <- rma.mv(yi, vi, mods=~Mean_annual_precip, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_flwr_num)
res.rma.flwr.num10
model_preds_flwr_num10 <- predict(res.rma.flwr.num10)


## fruit number
esmd_fruit_num <- esmd_clean2 %>%
  filter(Var_type_broad == "Fruit_num")
# removing latitudes < 0 for range edge analyses
esmd_fruit_num_latdiff <- esmd_fruit_num %>%
  filter(!(Latitude <= 0))
# removing func groups that are blank
esmd_fruit_num2 <- esmd_fruit_num %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_fruit_num3 <- esmd_fruit_num %>%
  filter(!(Native_Status == ""))
# models for fruit number
res.rma.fruit.num <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fruit_num)
res.rma.fruit.num
model_preds_fruit_num <- predict(res.rma.fruit.num)
res.rma.fruit.num2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fruit_num2)
res.rma.fruit.num2
summary(glht(res.rma.fruit.num2, linfct=cbind(contrMat(rep(1,4), type="Tukey"))), test=adjusted("holm"))
res.rma.fruit.num3 <- rma.mv(yi, vi, mods=~Abs_Latitude,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fruit_num)
res.rma.fruit.num3
model_preds_fruit_num3 <- predict(res.rma.fruit.num3)
res.rma.fruit.num4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fruit_num_latdiff)
res.rma.fruit.num4
model_preds_fruit_num4 <- predict(res.rma.fruit.num4)
res.rma.fruit.num5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fruit_num)
res.rma.fruit.num5
model_preds_fruit_num5 <- predict(res.rma.fruit.num5)
res.rma.fruit.num6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_fruit_num)
res.rma.fruit.num6
summary(glht(res.rma.fruit.num6, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.fruit.num7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fruit_num3)
res.rma.fruit.num7
summary(glht(res.rma.fruit.num7, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.fruit.num8 <- rma.mv(yi, vi, mods=~Elevation_m,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_fruit_num)
res.rma.fruit.num8
res.rma.fruit.num9 <- rma.mv(yi, vi, mods=~Mean_annual_temp,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_fruit_num)
res.rma.fruit.num9
model_preds_fruit_num9 <- predict(res.rma.fruit.num9)
res.rma.fruit.num10 <- rma.mv(yi, vi, mods=~Mean_annual_precip,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_fruit_num)
res.rma.fruit.num10
model_preds_fruit_num10 <- predict(res.rma.fruit.num10)


## fruit weight
esmd_fruit_weight <- esmd_clean2 %>%
  filter(Var_type_broad == "Fruit_weight")
# removing latitudes < 0 for range edge analyses
esmd_fruit_weight_latdiff <- esmd_fruit_weight %>%
  filter(!(Latitude <= 0))
# removing func groups that are blank
esmd_fruit_weight2 <- esmd_fruit_weight %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_fruit_weight3 <- esmd_fruit_weight %>%
  filter(!(Native_Status == ""))
# models for fruit weight
res.rma.fruit.weight <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fruit_weight)
res.rma.fruit.weight
model_preds_fruit_weight <- predict(res.rma.fruit.weight)
res.rma.fruit.weight2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fruit_weight2)
res.rma.fruit.weight2
summary(glht(res.rma.fruit.weight2, linfct=cbind(contrMat(rep(1,3), type="Tukey"))), test=adjusted("holm"))
res.rma.fruit.weight3 <- rma.mv(yi, vi, mods=~Abs_Latitude,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fruit_weight)
res.rma.fruit.weight3
model_preds_fruit_weight3 <- predict(res.rma.fruit.weight3)
res.rma.fruit.weight4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fruit_weight_latdiff)
res.rma.fruit.weight4
model_preds_fruit_weight4 <- predict(res.rma.fruit.weight4)
res.rma.fruit.weight5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fruit_weight)
res.rma.fruit.weight5
model_preds_fruit_weight5 <- predict(res.rma.fruit.weight5)
res.rma.fruit.weight6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_fruit_weight)
res.rma.fruit.weight6
summary(glht(res.rma.fruit.weight6, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.fruit.weight7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fruit_weight3)
res.rma.fruit.weight7
summary(glht(res.rma.fruit.weight7, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.fruit.weight8 <- rma.mv(yi, vi, mods=~Elevation_m,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_fruit_weight)
res.rma.fruit.weight8
res.rma.fruit.weight9 <- rma.mv(yi, vi, mods=~Mean_annual_temp,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_fruit_weight)
res.rma.fruit.weight9
model_preds_fruit_weight9 <- predict(res.rma.fruit.weight9)
res.rma.fruit.weight10 <- rma.mv(yi, vi, mods=~Mean_annual_precip,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_fruit_weight)
res.rma.fruit.weight10
model_preds_fruit_weight10 <- predict(res.rma.fruit.weight10)


## growth
esmd_growth <- esmd_clean2 %>%
  filter(Var_type_broad == "Growth")
# removing latitudes < 0 for range edge analyses
esmd_growth_latdiff <- esmd_growth %>%
  filter(!(Latitude <= 0))
# removing func groups that are blank
esmd_growth2 <- esmd_growth %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_growth3 <- esmd_growth %>%
  filter(!(Native_Status == ""))
# models for growth
res.rma.growth <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_growth)
res.rma.growth
model_preds_growth <- predict(res.rma.growth)
res.rma.growth2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_growth2)
res.rma.growth2
summary(glht(res.rma.growth2, linfct=cbind(contrMat(rep(1,6), type="Tukey"))), test=adjusted("holm"))
res.rma.growth3 <- rma.mv(yi, vi, mods=~Abs_Latitude,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_growth)
res.rma.growth3
model_preds_growth3 <- predict(res.rma.growth3)
res.rma.growth4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_growth_latdiff)
res.rma.growth4
model_preds_growth4 <- predict(res.rma.growth4)
res.rma.growth5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_growth)
res.rma.growth5
model_preds_growth5 <- predict(res.rma.growth5)
res.rma.growth6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_growth)
res.rma.growth6
summary(glht(res.rma.growth6, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.growth7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_growth3)
res.rma.growth7
summary(glht(res.rma.growth7, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.growth8 <- rma.mv(yi, vi, mods=~Elevation_m,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_growth)
res.rma.growth8 
res.rma.growth9 <- rma.mv(yi, vi, mods=~Mean_annual_temp,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_growth)
res.rma.growth9 
model_preds_growth9 <- predict(res.rma.growth9)
res.rma.growth10 <- rma.mv(yi, vi, mods=~Mean_annual_precip,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_growth)
res.rma.growth10 
model_preds_growth10 <- predict(res.rma.growth10)


## leaf growth
esmd_leaf_growth <- esmd_clean2 %>%
  filter(Var_type_broad == "Leaf_growth")
# removing latitudes < 0 for range edge analyses
esmd_leaf_growth_latdiff <- esmd_leaf_growth %>%
  filter(!(Latitude <= 0))
# removing func groups that are blank
esmd_leaf_growth2 <- esmd_leaf_growth %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_leaf_growth3 <- esmd_leaf_growth %>%
  filter(!(Native_Status == ""))
# models for leaf growth
res.rma.leaf.growth <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_leaf_growth)
res.rma.leaf.growth
model_preds_leaf_growth <- predict(res.rma.leaf.growth)
res.rma.leaf.growth2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_leaf_growth2)
res.rma.leaf.growth2
summary(glht(res.rma.leaf.growth2, linfct=cbind(contrMat(rep(1,5), type="Tukey"))), test=adjusted("holm"))
res.rma.leaf.growth3 <- rma.mv(yi, vi, mods=~Abs_Latitude,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_leaf_growth)
res.rma.leaf.growth3
model_preds_leaf_growth3 <- predict(res.rma.leaf.growth3)
res.rma.leaf.growth4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_leaf_growth_latdiff)
res.rma.leaf.growth4
model_preds_leaf_growth4 <- predict(res.rma.leaf.growth4)
res.rma.leaf.growth5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_leaf_growth)
res.rma.leaf.growth5
model_preds_leaf_growth5 <- predict(res.rma.leaf.growth5)
res.rma.leaf.growth6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_leaf_growth)
res.rma.leaf.growth6
summary(glht(res.rma.leaf.growth6, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.leaf.growth7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_leaf_growth3)
res.rma.leaf.growth7
summary(glht(res.rma.leaf.growth7, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.leaf.growth8 <- rma.mv(yi, vi, mods=~Elevation_m,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_leaf_growth)
res.rma.leaf.growth8 
res.rma.leaf.growth9 <- rma.mv(yi, vi, mods=~Mean_annual_temp,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_leaf_growth)
res.rma.leaf.growth9 
model_preds_leaf_growth9 <- predict(res.rma.leaf.growth9)
res.rma.leaf.growth10 <- rma.mv(yi, vi, mods=~Mean_annual_precip,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_leaf_growth)
res.rma.leaf.growth10 
model_preds_leaf_growth10 <- predict(res.rma.leaf.growth10)


## percent cover
esmd_cover <- esmd_clean2 %>%
  filter(Var_type_broad == "Percent_cover")
# removing latitudes < 0 for range edge analyses
esmd_cover_latdiff <- esmd_cover %>%
  filter(!(Latitude <= 0))
# removing func groups that are blank - only from functional group test
esmd_cover2 <- esmd_cover %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_cover3 <- esmd_cover %>%
  filter(!(Native_Status == ""))
# models for perc cover
res.rma.cover <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_cover)
res.rma.cover
model_preds_cover <- predict(res.rma.cover)
res.rma.cover2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_cover2)
res.rma.cover2
summary(glht(res.rma.cover2, linfct=cbind(contrMat(rep(1,7), type="Tukey"))), test=adjusted("holm"))
res.rma.cover3 <- rma.mv(yi, vi, mods=~Abs_Latitude,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_cover)
res.rma.cover3
model_preds_cover3 <- predict(res.rma.cover3)
res.rma.cover4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_cover_latdiff)
res.rma.cover4
model_preds_cover4 <- predict(res.rma.cover4)
res.rma.cover5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_cover)
res.rma.cover5
model_preds_cover5 <- predict(res.rma.cover5)
res.rma.cover6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_cover)
res.rma.cover6
summary(glht(res.rma.cover6, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.cover7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_cover3)
res.rma.cover7
summary(glht(res.rma.cover7, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.cover8 <- rma.mv(yi, vi, mods=~Elevation_m,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_cover)
res.rma.cover8 
res.rma.cover9 <- rma.mv(yi, vi, mods=~Mean_annual_temp,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_cover)
res.rma.cover9 
model_preds_cover9 <- predict(res.rma.cover9)
res.rma.cover10 <- rma.mv(yi, vi, mods=~Mean_annual_precip,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_cover)
res.rma.cover10 
model_preds_cover10 <- predict(res.rma.cover10)


## aboveground nitrogen
esmd_ab_n <- esmd_clean2 %>%
  filter(Var_type_broad == "Nitrogen_above")
# removing latitudes < 0 for range edge analyses
esmd_ab_n_latdiff <- esmd_ab_n %>%
  filter(!(Latitude <= 0))
# removing func groups that are blank - only from functional group test
esmd_ab_n2 <- esmd_ab_n %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_ab_n3 <- esmd_ab_n %>%
  filter(!(Native_Status == ""))
# models for aboveground N
res.rma.ab.n <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_n)
res.rma.ab.n
model_preds_ab_n <- predict(res.rma.ab.n)
res.rma.ab.n2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_n2)
res.rma.ab.n2
summary(glht(res.rma.ab.n2, linfct=cbind(contrMat(rep(1,7), type="Tukey"))), test=adjusted("holm"))
res.rma.ab.n3 <- rma.mv(yi, vi, mods=~Abs_Latitude,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_n)
res.rma.ab.n3
model_preds_ab_n3 <- predict(res.rma.ab.n3)
res.rma.ab.n4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_n_latdiff)
res.rma.ab.n4
model_preds_ab_n4 <- predict(res.rma.ab.n4)
res.rma.ab.n5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_n)
res.rma.ab.n5
model_preds_ab_n5 <- predict(res.rma.ab.n5)
res.rma.ab.n6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_ab_n)
res.rma.ab.n6
summary(glht(res.rma.ab.n6, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.ab.n7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_n3)
res.rma.ab.n7
summary(glht(res.rma.ab.n7, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.ab.n8 <- rma.mv(yi, vi, mods=~Elevation_m,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_ab_n)
res.rma.ab.n8 
res.rma.ab.n9 <- rma.mv(yi, vi, mods=~Mean_annual_temp,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_ab_n)
res.rma.ab.n9 
model_preds_ab_n9 <- predict(res.rma.ab.n9)
res.rma.ab.n10 <- rma.mv(yi, vi, mods=~Mean_annual_precip,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_ab_n)
res.rma.ab.n10 
model_preds_ab_n10 <- predict(res.rma.ab.n10)


## belowground nitrogen
esmd_bl_n <- esmd_clean2 %>%
  filter(Var_type_broad == "Nitrogen_below")
# removing latitudes < 0 for range edge analyses
esmd_bl_n_latdiff <- esmd_bl_n %>%
  filter(!(Latitude <= 0))
# removing func groups that are blank - only from functional group test
esmd_bl_n2 <- esmd_bl_n %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_bl_n3 <- esmd_bl_n %>%
  filter(!(Native_Status == ""))
# models for belowground N
res.rma.bl.n <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_bl_n)
res.rma.bl.n
model_preds_bl_n <- predict(res.rma.bl.n)
res.rma.bl.n2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_bl_n2)
res.rma.bl.n2
summary(glht(res.rma.bl.n2, linfct=cbind(contrMat(rep(1,4), type="Tukey"))), test=adjusted("holm"))
res.rma.bl.n3 <- rma.mv(yi, vi, mods=~Abs_Latitude,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_bl_n)
res.rma.bl.n3
model_preds_bl_n3 <- predict(res.rma.bl.n3)
res.rma.bl.n4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_bl_n_latdiff)
res.rma.bl.n4
model_preds_bl_n4 <- predict(res.rma.bl.n4)
res.rma.bl.n5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_bl_n)
res.rma.bl.n5
model_preds_bl_n5 <- predict(res.rma.bl.n5)
res.rma.bl.n6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_bl_n)
res.rma.bl.n6
summary(glht(res.rma.bl.n6, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.bl.n7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_bl_n3)
res.rma.bl.n7
summary(glht(res.rma.bl.n7, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.bl.n8 <- rma.mv(yi, vi, mods=~Elevation_m,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_bl_n)
res.rma.bl.n8 
res.rma.bl.n9 <- rma.mv(yi, vi, mods=~Mean_annual_temp,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_bl_n)
res.rma.bl.n9 
model_preds_bl_n9 <- predict(res.rma.bl.n9)
res.rma.bl.n10 <- rma.mv(yi, vi, mods=~Mean_annual_precip,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_bl_n)
res.rma.bl.n10 
model_preds_bl_n10 <- predict(res.rma.bl.n10)


## spring phenophases
esmd_spring <- esmd_clean2 %>%
  filter(Var_type_broad == "Phen_early")
# removing latitudes < 0 for range edge analyses
esmd_spring_latdiff <- esmd_spring %>%
  filter(!(Latitude <= 0))
# removing func groups that are blank - only from functional group test
esmd_spring2 <- esmd_spring %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_spring3 <- esmd_spring %>%
  filter(!(Native_Status == ""))
# models for spring phenphases
res.rma.spring <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_spring)
res.rma.spring
model_preds_spring <- predict(res.rma.spring)
res.rma.spring2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_spring2)
res.rma.spring2
summary(glht(res.rma.spring2, linfct=cbind(contrMat(rep(1,5), type="Tukey"))), test=adjusted("holm"))
res.rma.spring3 <- rma.mv(yi, vi, mods=~Abs_Latitude,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_spring)
res.rma.spring3
model_preds_spring3 <- predict(res.rma.spring3)
res.rma.spring4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_spring_latdiff)
res.rma.spring4
model_preds_spring4 <- predict(res.rma.spring4)
res.rma.spring5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_spring)
res.rma.spring5
model_preds_spring5 <- predict(res.rma.spring5)
res.rma.spring6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_spring)
res.rma.spring6
summary(glht(res.rma.spring6, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.spring7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_spring3)
res.rma.spring7
summary(glht(res.rma.spring7, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.spring8 <- rma.mv(yi, vi, mods=~Elevation_m,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_spring)
res.rma.spring8 
res.rma.spring9 <- rma.mv(yi, vi, mods=~Mean_annual_temp,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_spring)
res.rma.spring9 
model_preds_spring9 <- predict(res.rma.spring9)
res.rma.spring10 <- rma.mv(yi, vi, mods=~Mean_annual_precip,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_spring)
res.rma.spring10 
model_preds_spring10 <- predict(res.rma.spring10)


## fall phenophases
esmd_fall <- esmd_clean2 %>%
  filter(Var_type_broad == "Phen_late")
# removing latitudes < 0 for range edge analyses
esmd_fall_latdiff <- esmd_fall %>%
  filter(!(Latitude <= 0))
# removing func groups that are blank - only from functional group test
esmd_fall2 <- esmd_fall %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_fall3 <- esmd_fall %>%
  filter(!(Native_Status == ""))
# models for fall phenophases
res.rma.fall <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fall)
res.rma.fall
model_preds_fall <- predict(res.rma.fall)
res.rma.fall2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fall2)
res.rma.fall2
summary(glht(res.rma.fall2, linfct=cbind(contrMat(rep(1,4), type="Tukey"))), test=adjusted("holm"))
res.rma.fall3 <- rma.mv(yi, vi, mods=~Abs_Latitude,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fall)
res.rma.fall3
model_preds_fall3 <- predict(res.rma.fall3)
res.rma.fall4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fall_latdiff,control=list(rel.tol=1e-8))
res.rma.fall4
model_preds_fall4 <- predict(res.rma.fall4)
res.rma.fall5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fall)
res.rma.fall5
model_preds_fall5 <- predict(res.rma.fall5)
res.rma.fall6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_fall)
res.rma.fall6
summary(glht(res.rma.fall6, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.fall7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fall3)
res.rma.fall7
summary(glht(res.rma.fall7, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.fall8 <- rma.mv(yi, vi, mods=~Elevation_m,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_fall)
res.rma.fall8 
res.rma.fall9 <- rma.mv(yi, vi, mods=~Mean_annual_temp,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_fall)
res.rma.fall9 
model_preds_fall9 <- predict(res.rma.fall9)
res.rma.fall10 <- rma.mv(yi, vi, mods=~Mean_annual_precip,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_fall)
res.rma.fall10 
model_preds_fall10 <- predict(res.rma.fall10)


## flwr lifespan
esmd_flwr_lifespan <- esmd_clean2 %>%
  filter(Var_type_broad == "Phen_flwr_lifespan")
# removing latitudes < 0 for range edge analyses
esmd_flwr_lifespan_latdiff <- esmd_flwr_lifespan %>%
  filter(!(Latitude <= 0))
# removing func groups that are blank - only from functional group test
esmd_flwr_lifespan2 <- esmd_flwr_lifespan %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_flwr_lifespan3 <- esmd_flwr_lifespan %>%
  filter(!(Native_Status == ""))
# models for flwr_lifespan phenophases
res.rma.flwr.lifespan <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_flwr_lifespan)
res.rma.flwr.lifespan
model_preds_flwr_lifespan <- predict(res.rma.flwr.lifespan)
res.rma.flwr.lifespan2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_flwr_lifespan2)
res.rma.flwr.lifespan2
summary(glht(res.rma.flwr.lifespan2, linfct=cbind(contrMat(rep(1,3), type="Tukey"))), test=adjusted("holm"))
res.rma.flwr.lifespan3 <- rma.mv(yi, vi, mods=~Abs_Latitude,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_flwr_lifespan)
res.rma.flwr.lifespan3
model_preds_flwr_lifespan3 <- predict(res.rma.flwr.lifespan3)
res.rma.flwr.lifespan4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_flwr_lifespan_latdiff)
res.rma.flwr.lifespan4
model_preds_flwr_lifespan4 <- predict(res.rma.flwr.lifespan4)
res.rma.flwr.lifespan5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Site/Genus_Species),data=esmd_flwr_lifespan)
res.rma.flwr.lifespan5
model_preds_flwr_lifespan5 <- predict(res.rma.flwr.lifespan5)
res.rma.flwr.lifespan6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_flwr_lifespan)
res.rma.flwr.lifespan6
summary(glht(res.rma.flwr.lifespan6, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.flwr.lifespan7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_flwr_lifespan3)
res.rma.flwr.lifespan7
summary(glht(res.rma.flwr.lifespan7, linfct=cbind(contrMat(rep(1,2), type="Tukey"))), test=adjusted("holm"))
res.rma.flwr.lifespan8 <- rma.mv(yi, vi, mods=~Elevation_m,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_flwr_lifespan)
res.rma.flwr.lifespan8
res.rma.flwr.lifespan9 <- rma.mv(yi, vi, mods=~Mean_annual_temp,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_flwr_lifespan)
res.rma.flwr.lifespan9
model_preds_flwr_lifespan9 <- predict(res.rma.flwr.lifespan9)
res.rma.flwr.lifespan10 <- rma.mv(yi, vi, mods=~Mean_annual_precip,random=list(~1|Pub_number/Site/Genus_Species), data=esmd_flwr_lifespan)
res.rma.flwr.lifespan10
model_preds_flwr_lifespan10 <- predict(res.rma.flwr.lifespan10)


### testing to see if finer var types differ ###
esmd_spring_fine <- esmd_clean2 %>%
  filter(Var_type_broad == "Phen_early")
esmd_fall_fine <- esmd_clean2 %>%
  filter(Var_type_broad == "Phen_late")
esmd_growth_fine <- esmd_clean2 %>%
  filter(Var_type_broad == "Growth")
esmd_leafgrowth_fine <- esmd_clean2 %>%
  filter(Var_type_broad == "Leaf_growth")
res.rma.spring.fine <- rma.mv(yi, vi, mods=~Var_type-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_spring_fine)
res.rma.spring.fine
res.rma.fall.fine <- rma.mv(yi, vi, mods=~Var_type-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_fall_fine)
res.rma.fall.fine
res.rma.growth.fine <- rma.mv(yi, vi, mods=~Var_type-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_growth_fine)
res.rma.growth.fine
res.rma.leafgrowth.fine <- rma.mv(yi, vi, mods=~Var_type-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_leafgrowth_fine)
res.rma.leafgrowth.fine


### testing to see if finer functional groups differ ###
esmd_trees <- esmd_clean2 %>%
  filter(Func_group == "Ever_Tree" | Func_group == "Decid_Tree")
esmd_shrubs <- esmd_clean2 %>%
  filter(Func_group == "Ever_Shrub" | Func_group == "Decid_Shrub")
esmd_forbs <- esmd_clean2 %>%
  filter(Func_group == "Legume_Forb" | Func_group == "Forb")
res.rma.trees <- rma.mv(yi, vi, mods=~Func_group-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_trees)
res.rma.trees
res.rma.shrubs <- rma.mv(yi, vi, mods=~Func_group-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_shrubs)
res.rma.shrubs
res.rma.forbs <- rma.mv(yi, vi, mods=~Func_group-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_forbs)
res.rma.forbs


### testing to see if tissue types differ ###
res.rma.tissue.a.bio <- rma.mv(yi, vi, mods=~Tissue_Type_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_biomass)
res.rma.tissue.a.bio
count_tissue_a <- esmd_ab_biomass %>%
  group_by(Tissue_Type_broad) %>%
  count()
res.rma.a.n <- rma.mv(yi, vi, mods=~Tissue_Type_broad-1, random=list(~1|Pub_number/Site/Genus_Species),data=esmd_ab_n)
res.rma.a.n
count_tissue_n <- esmd_ab_n %>%
  group_by(Tissue_Type_broad) %>%
  count()


### testing correlation btwn latitude and elevation ###
# all data
esmd_lat_corr <- esmd_clean2 %>%
  filter(!(Abs_Latitude < 20)) # only 2 studies < 20
cor.test(esmd_lat_corr$Elevation_m,esmd_lat_corr$Abs_Latitude,method = "pearson")
lm1 <- lm(esmd_lat_corr$Elevation_m~esmd_lat_corr$Abs_Latitude)
png("lat_elev_cor.png", units="in", width=7, height=6, res=300)
plot(esmd_lat_corr$Elevation_m~esmd_lat_corr$Abs_Latitude,
     ylab = "Elevation (m)",
     xlab = "Absolute Latitude (°)")
abline(lm1)
dev.off()
summary(lm1)
# flower number
cor.test(esmd_flwr_num$Elevation_m,esmd_flwr_num$Abs_Latitude,method = "pearson")
lm1 <- lm(esmd_flwr_num$Elevation_m~esmd_flwr_num$Abs_Latitude)
plot(esmd_flwr_num$Elevation_m~esmd_flwr_num$Abs_Latitude)
abline(lm1)
summary(lm1)
# fruit number
cor.test(esmd_fruit_num$Elevation_m,esmd_fruit_num$Abs_Latitude,method = "pearson")
lm1 <- lm(esmd_fruit_num$Elevation_m~esmd_fruit_num$Abs_Latitude)
plot(esmd_fruit_num$Elevation_m~esmd_fruit_num$Abs_Latitude)
abline(lm1)
summary(lm1)
# fruit weight
cor.test(esmd_fruit_weight$Elevation_m,esmd_fruit_weight$Abs_Latitude,method = "pearson")
lm1 <- lm(esmd_fruit_weight$Elevation_m~esmd_fruit_weight$Abs_Latitude)
plot(esmd_fruit_weight$Elevation_m~esmd_fruit_weight$Abs_Latitude)
abline(lm1)
summary(lm1)
# belowground N
cor.test(esmd_bl_n$Elevation_m,esmd_bl_n$Abs_Latitude,method = "pearson")
lm1 <- lm(esmd_bl_n$Elevation_m~esmd_bl_n$Abs_Latitude)
plot(esmd_bl_n$Elevation_m~esmd_bl_n$Abs_Latitude)
abline(lm1)
summary(lm1)
# spring
cor.test(esmd_spring$Elevation_m,esmd_spring$Abs_Latitude,method = "pearson")
lm1 <- lm(esmd_spring$Elevation_m~esmd_spring$Abs_Latitude)
plot(esmd_spring$Elevation_m~esmd_spring$Abs_Latitude)
abline(lm1)
summary(lm1)
# fall
cor.test(esmd_fall$Elevation_m,esmd_fall$Abs_Latitude,method = "pearson")
lm1 <- lm(esmd_fall$Elevation_m~esmd_fall$Abs_Latitude)
plot(esmd_fall$Elevation_m~esmd_fall$Abs_Latitude)
abline(lm1)
summary(lm1)


### testing correlation btwn latitude and amount warmed ###
# all data
esmd_lat_warm_corr <- esmd_clean2 %>%
  filter(!(Abs_Latitude < 20)) # only 2 studies < 20
cor.test(esmd_lat_warm_corr$Amount_warmed_C,esmd_lat_warm_corr$Abs_Latitude,method = "pearson")
lm1 <- lm(esmd_lat_warm_corr$Amount_warmed_C~esmd_lat_warm_corr$Abs_Latitude)
plot(esmd_lat_warm_corr$Amount_warmed_C~esmd_lat_warm_corr$Abs_Latitude,
     ylab = "Amount warmed",
     xlab = "Absolute Latitude (°)")
abline(lm1)
summary(lm1)


### testing correlation btwn latitude and experiment length ###
# all data
esmd_lat_length_corr <- esmd_clean2 %>%
  filter(!(Abs_Latitude < 20)) # only 2 studies < 20
cor.test(esmd_lat_length_corr$Years_warmed,esmd_lat_length_corr$Abs_Latitude,method = "pearson")
lm1 <- lm(esmd_lat_length_corr$Years_warmed~esmd_lat_length_corr$Abs_Latitude)
plot(esmd_lat_length_corr$Years_warmed~esmd_lat_length_corr$Abs_Latitude,
     ylab = "Years warmed",
     xlab = "Absolute Latitude (°)")
abline(lm1)
summary(lm1)



### Uploading R data for model estimates for figures
path_out = paste0(MA_dir,'L2/')
saveRDS(model_preds_ab_biomass, paste(path_out,'amount_warmed_model_ab_biomass.rds'))
saveRDS(model_preds_ab_n, paste(path_out,'amount_warmed_model_ab_n.rds'))
saveRDS(model_preds_bl_biomass, paste(path_out,'amount_warmed_model_bl_biomass.rds'))
saveRDS(model_preds_cover, paste(path_out,'amount_warmed_model_cover.rds'))
saveRDS(model_preds_flwr_lifespan, paste(path_out,'amount_warmed_model_flwr_lifespan.rds'))
saveRDS(model_preds_fall, paste(path_out,'amount_warmed_model_fall.rds'))
saveRDS(model_preds_growth, paste(path_out,'amount_warmed_model_growth.rds'))
saveRDS(model_preds_leaf_growth, paste(path_out,'amount_warmed_model_leaf_growth.rds'))
saveRDS(model_preds_bl_n, paste(path_out,'amount_warmed_model_bl_n.rds'))
saveRDS(model_preds_spring, paste(path_out,'amount_warmed_model_spring.rds'))
saveRDS(model_preds_flwr_num, paste(path_out,'amount_warmed_model_flwr_num.rds'))
saveRDS(model_preds_fruit_num, paste(path_out,'amount_warmed_model_fruit_num.rds'))
saveRDS(model_preds_fruit_weight, paste(path_out,'amount_warmed_model_fruit_weight.rds'))

saveRDS(model_preds_ab_biomass3, paste(path_out,'lat_model_ab_biomass.rds'))
saveRDS(model_preds_ab_n3, paste(path_out,'lat_model_ab_n.rds'))
saveRDS(model_preds_bl_biomass3, paste(path_out,'lat_model_bl_biomass.rds'))
saveRDS(model_preds_cover3, paste(path_out,'lat_model_cover.rds'))
saveRDS(model_preds_flwr_lifespan3, paste(path_out,'lat_model_flwr_lifespan.rds'))
saveRDS(model_preds_fall3, paste(path_out,'lat_model_fall.rds'))
saveRDS(model_preds_growth3, paste(path_out,'lat_model_growth.rds'))
saveRDS(model_preds_leaf_growth3, paste(path_out,'lat_model_leaf_growth.rds'))
saveRDS(model_preds_bl_n3, paste(path_out,'lat_model_bl_n.rds'))
saveRDS(model_preds_spring3, paste(path_out,'lat_model_spring.rds'))
saveRDS(model_preds_flwr_num3, paste(path_out,'lat_model_flwr_num.rds'))
saveRDS(model_preds_fruit_num3, paste(path_out,'lat_model_fruit_num.rds'))
saveRDS(model_preds_fruit_weight3, paste(path_out,'lat_model_fruit_weight.rds'))

saveRDS(model_preds_ab_biomass4, paste(path_out,'lat_diff_model_ab_biomass.rds'))
saveRDS(model_preds_ab_n4, paste(path_out,'lat_diff_model_ab_n.rds'))
saveRDS(model_preds_bl_biomass4, paste(path_out,'lat_diff_model_bl_biomass.rds'))
saveRDS(model_preds_cover4, paste(path_out,'lat_diff_model_cover.rds'))
saveRDS(model_preds_flwr_lifespan4, paste(path_out,'lat_diff_model_flwr_lifespan.rds'))
saveRDS(model_preds_fall4, paste(path_out,'lat_diff_model_fall.rds'))
saveRDS(model_preds_growth4, paste(path_out,'lat_diff_model_growth.rds'))
saveRDS(model_preds_leaf_growth4, paste(path_out,'lat_diff_model_leaf_growth.rds'))
saveRDS(model_preds_bl_n4, paste(path_out,'lat_diff_model_bl_n.rds'))
saveRDS(model_preds_spring4, paste(path_out,'lat_diff_model_spring.rds'))
saveRDS(model_preds_flwr_num4, paste(path_out,'lat_diff_model_flwr_num.rds'))
saveRDS(model_preds_fruit_num4, paste(path_out,'lat_diff_model_fruit_num.rds'))
saveRDS(model_preds_fruit_weight4, paste(path_out,'lat_diff_model_fruit_weight.rds'))

saveRDS(model_preds_ab_biomass5, paste(path_out,'years_w_model_ab_biomass.rds'))
saveRDS(model_preds_ab_n5, paste(path_out,'years_w_model_ab_n.rds'))
saveRDS(model_preds_bl_biomass5, paste(path_out,'years_w_model_bl_biomass.rds'))
saveRDS(model_preds_cover5, paste(path_out,'years_w_model_cover.rds'))
saveRDS(model_preds_flwr_lifespan5, paste(path_out,'years_w_model_flwr_lifespan.rds'))
saveRDS(model_preds_fall5, paste(path_out,'years_w_model_fall.rds'))
saveRDS(model_preds_growth5, paste(path_out,'years_w_model_growth.rds'))
saveRDS(model_preds_leaf_growth5, paste(path_out,'years_w_model_leaf_growth.rds'))
saveRDS(model_preds_bl_n5, paste(path_out,'years_w_model_bl_n.rds'))
saveRDS(model_preds_spring5, paste(path_out,'years_w_model_spring.rds'))
saveRDS(model_preds_flwr_num5, paste(path_out,'years_w_model_flwr_num.rds'))
saveRDS(model_preds_fruit_num5, paste(path_out,'years_w_model_fruit_num.rds'))
saveRDS(model_preds_fruit_weight5, paste(path_out,'years_w_model_fruit_weight.rds'))

saveRDS(model_preds_ab_biomass9, paste(path_out,'mat_model_ab_biomass.rds'))
saveRDS(model_preds_ab_n9, paste(path_out,'mat_model_ab_n.rds'))
saveRDS(model_preds_bl_biomass9, paste(path_out,'mat_model_bl_biomass.rds'))
saveRDS(model_preds_cover9, paste(path_out,'mat_model_cover.rds'))
saveRDS(model_preds_flwr_lifespan9, paste(path_out,'mat_model_flwr_lifespan.rds'))
saveRDS(model_preds_fall9, paste(path_out,'mat_model_fall.rds'))
saveRDS(model_preds_growth9, paste(path_out,'mat_model_growth.rds'))
saveRDS(model_preds_leaf_growth9, paste(path_out,'mat_model_leaf_growth.rds'))
saveRDS(model_preds_bl_n9, paste(path_out,'mat_model_bl_n.rds'))
saveRDS(model_preds_spring9, paste(path_out,'mat_model_spring.rds'))
saveRDS(model_preds_flwr_num9, paste(path_out,'mat_model_flwr_num.rds'))
saveRDS(model_preds_fruit_num9, paste(path_out,'mat_model_fruit_num.rds'))
saveRDS(model_preds_fruit_weight9, paste(path_out,'mat_model_fruit_weight.rds'))

saveRDS(model_preds_ab_biomass10, paste(path_out,'map_model_ab_biomass.rds'))
saveRDS(model_preds_ab_n10, paste(path_out,'map_model_ab_n.rds'))
saveRDS(model_preds_bl_biomass10, paste(path_out,'map_model_bl_biomass.rds'))
saveRDS(model_preds_cover10, paste(path_out,'map_model_cover.rds'))
saveRDS(model_preds_flwr_lifespan10, paste(path_out,'map_model_flwr_lifespan.rds'))
saveRDS(model_preds_fall10, paste(path_out,'map_model_fall.rds'))
saveRDS(model_preds_growth10, paste(path_out,'map_model_growth.rds'))
saveRDS(model_preds_leaf_growth10, paste(path_out,'map_model_leaf_growth.rds'))
saveRDS(model_preds_bl_n10, paste(path_out,'map_model_bl_n.rds'))
saveRDS(model_preds_spring10, paste(path_out,'map_model_spring.rds'))
saveRDS(model_preds_flwr_num10, paste(path_out,'map_model_flwr_num.rds'))
saveRDS(model_preds_fruit_num10, paste(path_out,'map_model_fruit_num.rds'))
saveRDS(model_preds_fruit_weight10, paste(path_out,'map_model_fruit_weight.rds'))







### test analysis using example data
# tpos and tneg - number of pos and neg tuberculosis cases for vaccinated individuals
# cpos and cneg - number of pos and neg tuberculosis cases for unvaccinated individuals
dat <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos,
              di = cneg, data = dat.bcg, append = TRUE)
res <- rma(yi, vi, data = dat)
res
# the estimated average log relative risk is equal to ˆµ = −0.7145 (95% CI:
# −1.0669 to −0.3622). For easier interpretation, it may be useful to transform these values
# back to the relative risk scale through exponentiation (i.e., exp(ˆµ) = 0.49 with 95% CI: 0.34
# to 0.70). The results therefore suggest that the risk of a tuberculosis infection in vaccinated
# individuals is on average half as large as the infection risk without the vaccination. The null
# hypothesis H0 : µ = 0 can be clearly rejected (z = −3.97, p < 0.0001).
res <- rma(yi, vi, mods = ~ ablat + year, data = dat)
res
# The estimated amount of residual heterogeneity is equal to tau^2 = 0.1108, suggesting that
# (0.3132 − 0.1108)/.3132 = 65% of the total amount of heterogeneity can be accounted for by
# including the two moderators in the model. However, while we can reject H0 : β1 = β2 = 0
# based on the omnibus test (QM = 12.20, df = 2, p < 0.01), only absolute latitude appears
# to have a significant influence on the effectiveness of the vaccine (i.e., for H0 : β1 = 0, we find
# z = −2.74 with p < 0.01, while for H0 : β2 = 0, we find z = 0.13 with p = 0.90). The test for
# residual heterogeneity is significant (QE = 28.33, df = 10, p < 0.01), possibly indicating that
# other moderators not considered in the model are influencing the vaccine effectiveness.