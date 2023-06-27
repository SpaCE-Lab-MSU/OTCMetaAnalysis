# TITLE:          Effect size analyses
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Pat Bills
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
esmd_clean <- read.csv(file.path(MA_dir,"L2/otc_effect_sizes_L2.csv"))


### thoughts
# how to do lat difference analyses?
    # remove data > 60? could test if these are outliers
    # also, shouldn't limit the y-axis in the figure unless I remove that data in analyses too
# for random effects, only include vars that are sig?
    # e.g., year round warming only as a random effect if it is significant for that variable


## workflow
# first, run the overall model to see how each variable type responds to warming
    # include publication as a random effect
# for grouped variables (i.e., spring and fall phenology), run models testing the ungrouped variables responses
    # include publication as a random effect
# then, for each group variables, run separate models testing the effects of each predictor (i.e., lat, years warmed, etc.)
    # include publication as a random effect
    # if amount warmed is significant for that variable, include it as a random effect


#### main stats ####
# removing variables that have <10 effect sizes
esmd_clean %>%
  group_by(Var_type_broad) %>%
  summarize(count = n())
esmd_clean <- esmd_clean %>%
  filter(!(Var_type_broad == "Biomass_total" |
             Var_type_broad == "Flower_weight" |
             Var_type_broad == "Phen_leaf_lifespan" |
             Var_type_broad == "Phen_preflwr_length"))
# fixing species names for measurements that don't have a species
esmd_clean$Genus_Species[esmd_clean$Genus_Species == "_"] <- ""


## all variables
res.rma.all <- rma.mv(yi, vi, mods=~Var_type_broad-1, random=list(~1|Pub_number/Genus_Species),data=esmd_clean)
res.rma.all
# all comparisons
# Values of P=1.0 indicate that there is no evidence of a difference between variables
# Holm correction used here because it was used in the Kuebbing phenology meta-analysis
summary(glht(res.rma.all, linfct=cbind(contrMat(rep(1,13), type="Tukey"))), test=adjusted("holm"))


## aboveground biomass
esmd_ab_biomass <- esmd_clean %>%
  filter(Var_type_broad == "Biomass_above")
esmd_ab_biomass_latdiff <- esmd_ab_biomass %>%
  filter(!(Lat_difference > 60))
esmd_ab_biomass_lat <- esmd_ab_biomass %>%
  filter(!(Latitude < 0))
# removing func groups that are blank - only from functional group test
esmd_ab_biomass2 <- esmd_ab_biomass %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_ab_biomass3 <- esmd_ab_biomass %>%
  filter(!(Native_Status == ""))
# models for aboveground biomass
# testing if amount warmed has an effect
res.rma.ab.biomass <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Genus_Species),data=esmd_ab_biomass)
res.rma.ab.biomass # no effect - not including in other models
res.rma.ab.biomass2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Genus_Species),data=esmd_ab_biomass2)
res.rma.ab.biomass2 # graminoid: 0.036, total: 0.0006
res.rma.ab.biomass3 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number/Genus_Species),data=esmd_ab_biomass_lat)
res.rma.ab.biomass3 # no effect
res.rma.ab.biomass4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Genus_Species),data=esmd_ab_biomass_latdiff)
res.rma.ab.biomass4 # no effect
res.rma.ab.biomass5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Genus_Species),data=esmd_ab_biomass)
res.rma.ab.biomass5 # no effect
res.rma.ab.biomass6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_ab_biomass)
res.rma.ab.biomass6 # yes: 0.0013
res.rma.ab.biomass7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Genus_Species),data=esmd_ab_biomass3)
res.rma.ab.biomass7 # native: 0.0827


## belowground biomass
esmd_bl_biomass <- esmd_clean %>%
  filter(Var_type_broad == "Biomass_below")
esmd_bl_biomass_latdiff <- esmd_bl_biomass %>%
  filter(!(Lat_difference > 60))
esmd_bl_biomass_lat <- esmd_bl_biomass %>%
  filter(!(Latitude < 0))
# removing func groups that are blank - only from functional group test
esmd_bl_biomass2 <- esmd_bl_biomass %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_bl_biomass3 <- esmd_bl_biomass %>%
  filter(!(Native_Status == ""))
# models for belowground biomass
# testing if amount warmed has an effect
res.rma.bl.biomass <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Genus_Species),data=esmd_bl_biomass)
res.rma.bl.biomass # no effect - not including in other models
res.rma.bl.biomass2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Genus_Species),data=esmd_bl_biomass2)
res.rma.bl.biomass2 # no effect
res.rma.bl.biomass3 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number/Genus_Species),data=esmd_bl_biomass_lat)
res.rma.bl.biomass3 # no effect
res.rma.bl.biomass4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Genus_Species),data=esmd_bl_biomass_latdiff)
res.rma.bl.biomass4 # no effect
res.rma.bl.biomass5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Genus_Species),data=esmd_bl_biomass)
res.rma.bl.biomass5 # no effect
res.rma.bl.biomass6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_bl_biomass)
res.rma.bl.biomass6 # yes: 0.01
res.rma.bl.biomass7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Genus_Species),data=esmd_bl_biomass3)
res.rma.bl.biomass7 # non-native: 0.0016


## flower number
esmd_flwr_num <- esmd_clean %>%
  filter(Var_type_broad == "Flower_num")
esmd_flwr_num_latdiff <- esmd_flwr_num %>%
  filter(!(Lat_difference > 60))
esmd_flwr_num_lat <- esmd_flwr_num %>%
  filter(!(Latitude < 0))
# removing func groups that are blank - only from functional group test
esmd_flwr_num2 <- esmd_flwr_num %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_flwr_num3 <- esmd_flwr_num %>%
  filter(!(Native_Status == ""))
# models for flower number
# testing if amount warmed has an effect
res.rma.flwr.num <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Genus_Species),data=esmd_flwr_num)
res.rma.flwr.num # no effect - not including in other models
res.rma.flwr.num2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Genus_Species),data=esmd_flwr_num2)
res.rma.flwr.num2 # no effect
res.rma.flwr.num3 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number/Genus_Species),data=esmd_flwr_num_lat)
res.rma.flwr.num3 # latitude: 0.0931
res.rma.flwr.num4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Genus_Species),data=esmd_flwr_num_latdiff)
res.rma.flwr.num4 # no effect
res.rma.flwr.num5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Genus_Species),data=esmd_flwr_num)
res.rma.flwr.num5 # no effect
res.rma.flwr.num6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_flwr_num)
res.rma.flwr.num6 # no effect
res.rma.flwr.num7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Genus_Species),data=esmd_flwr_num3)
res.rma.flwr.num7 # no effect


## fruit number
esmd_fruit_num <- esmd_clean %>%
  filter(Var_type_broad == "Fruit_num")
esmd_fruit_num_latdiff <- esmd_fruit_num %>%
  filter(!(Lat_difference > 60))
esmd_fruit_num_lat <- esmd_fruit_num %>%
  filter(!(Latitude < 0))
# removing func groups that are blank
esmd_fruit_num2 <- esmd_fruit_num %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_fruit_num3 <- esmd_fruit_num %>%
  filter(!(Native_Status == ""))
# models for fruit number
# testing if amount warmed has an effect
res.rma.fruit.num <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number)/Genus_Species,data=esmd_fruit_num)
res.rma.fruit.num # no effect - not including in other models
res.rma.fruit.num2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Genus_Species),data=esmd_fruit_num2)
res.rma.fruit.num2 # graminoid: 0.01
res.rma.fruit.num3 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number/Genus_Species),data=esmd_fruit_num_lat)
res.rma.fruit.num3 # no effect
res.rma.fruit.num4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Genus_Species),data=esmd_fruit_num_latdiff)
res.rma.fruit.num4 # no effect
res.rma.fruit.num5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Genus_Species),data=esmd_fruit_num)
res.rma.fruit.num5 # years warmed: 0.0125
res.rma.fruit.num6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_fruit_num)
res.rma.fruit.num6 # no effect
res.rma.fruit.num7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Genus_Species),data=esmd_fruit_num3)
res.rma.fruit.num7 # native: 0.0545, non-native: 0.0010


## fruit weight
esmd_fruit_weight <- esmd_clean %>%
  filter(Var_type_broad == "Fruit_weight")
esmd_fruit_weight_latdiff <- esmd_fruit_weight %>%
  filter(!(Lat_difference > 60))
esmd_fruit_weight_lat <- esmd_fruit_weight %>%
  filter(!(Latitude < 0))
# removing func groups that are blank
esmd_fruit_weight2 <- esmd_fruit_weight %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_fruit_weight3 <- esmd_fruit_weight %>%
  filter(!(Native_Status == ""))
# models for fruit weight
# testing if amount warmed has an effect
res.rma.fruit.weight <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Genus_Species),data=esmd_fruit_weight)
res.rma.fruit.weight # no effect - not including in other models
res.rma.fruit.weight2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Genus_Species),data=esmd_fruit_weight2)
res.rma.fruit.weight2 # forb: 0.0546, graminoid: 0.0035
res.rma.fruit.weight3 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number/Genus_Species),data=esmd_fruit_weight_lat)
res.rma.fruit.weight3 # latitude: 0.0212
res.rma.fruit.weight4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Genus_Species),data=esmd_fruit_weight_latdiff)
res.rma.fruit.weight4 # no effect
res.rma.fruit.weight5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Genus_Species),data=esmd_fruit_weight)
res.rma.fruit.weight5 # no effect
res.rma.fruit.weight6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_fruit_weight)
res.rma.fruit.weight6 # yes: 0.0182
res.rma.fruit.weight7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Genus_Species),data=esmd_fruit_weight3)
res.rma.fruit.weight7 # native: <0.0001, non-native: 0.0592


## growth
esmd_growth <- esmd_clean %>%
  filter(Var_type_broad == "Growth")
esmd_growth_latdiff <- esmd_growth %>%
  filter(!(Lat_difference > 60))
esmd_growth_lat <- esmd_growth %>%
  filter(!(Latitude < 0))
# removing func groups that are blank
esmd_growth2 <- esmd_growth %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_growth3 <- esmd_growth %>%
  filter(!(Native_Status == ""))
# models for growth
# testing if amount warmed has an effect
res.rma.growth <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Genus_Species),data=esmd_growth)
res.rma.growth # no effect - not including in other models
res.rma.growth2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Genus_Species),data=esmd_growth2)
res.rma.growth2 # forb: <0.0001, graminoid: 0.002, Shrub: 0.0017, Total: 0.0129
res.rma.growth3 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number/Genus_Species),data=esmd_growth_lat)
res.rma.growth3 # no effect
res.rma.growth4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Genus_Species),data=esmd_growth_latdiff)
res.rma.growth4 # no effect
res.rma.growth5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Genus_Species),data=esmd_growth)
res.rma.growth5 # no effect
res.rma.growth6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_growth)
res.rma.growth6 # yes: 0.0008, no: <0.0001
res.rma.growth7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Genus_Species),data=esmd_growth3)
res.rma.growth7 # native: <0.0001, non-native: 0.0098


## leaf growth
esmd_leaf_growth <- esmd_clean %>%
  filter(Var_type_broad == "Leaf_Growth")
esmd_leaf_growth_latdiff <- esmd_leaf_growth %>%
  filter(!(Lat_difference > 60))
esmd_leaf_growth_lat <- esmd_leaf_growth %>%
  filter(!(Latitude < 0))
# removing func groups that are blank
esmd_leaf_growth2 <- esmd_leaf_growth %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_leaf_growth3 <- esmd_leaf_growth %>%
  filter(!(Native_Status == ""))
# models for leaf growth
# testing if amount warmed has an effect
res.rma.leaf.growth <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Genus_Species),data=esmd_leaf_growth)
res.rma.leaf.growth # yes effect - including as random effect in other models (havent done this yet)
res.rma.leaf.growth2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Genus_Species),data=esmd_leaf_growth2)
res.rma.leaf.growth2 # graminoid: 0.0065, shrub: 0.0028
res.rma.leaf.growth3 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number/Genus_Species),data=esmd_leaf_growth_lat)
res.rma.leaf.growth3 # no effect
res.rma.leaf.growth4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Genus_Species),data=esmd_leaf_growth_latdiff)
res.rma.leaf.growth4 # no effect
res.rma.leaf.growth5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Genus_Species),data=esmd_leaf_growth)
res.rma.leaf.growth5 # no effect
res.rma.leaf.growth6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_leaf_growth)
res.rma.leaf.growth6 # yes: 0.0017
res.rma.leaf.growth7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Genus_Species),data=esmd_leaf_growth3)
res.rma.leaf.growth7 # native: 0.007, non-native: 0.0156


## percent cover
esmd_cover <- esmd_clean %>%
  filter(Var_type_broad == "Percent_cover")
esmd_cover_latdiff <- esmd_cover %>%
  filter(!(Lat_difference > 60))
esmd_cover_lat <- esmd_cover %>%
  filter(!(Latitude < 0))
# removing func groups that are blank - only from functional group test
esmd_cover2 <- esmd_cover %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_cover3 <- esmd_cover %>%
  filter(!(Native_Status == ""))
# models for perc cover
# testing if amount warmed has an effect
res.rma.cover <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Genus_Species),data=esmd_cover)
res.rma.cover # no effect - not including in other models
res.rma.cover2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Genus_Species),data=esmd_cover2)
res.rma.cover2 # bryophyte: <0.0001, lichen: 0.0006, shrub: 0.0001
res.rma.cover3 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number/Genus_Species),data=esmd_cover_lat)
res.rma.cover3 # no effect
res.rma.cover4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Genus_Species),data=esmd_cover_latdiff)
res.rma.cover4 # no effect
res.rma.cover5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Genus_Species),data=esmd_cover)
res.rma.cover5 # no effect
res.rma.cover6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_cover)
res.rma.cover6 # no effect
res.rma.cover7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Genus_Species),data=esmd_cover3)
res.rma.cover7 # no effect


## aboveground nitrogen
esmd_ab_n <- esmd_clean %>%
  filter(Var_type_broad == "Nitrogen_above")
esmd_ab_n_latdiff <- esmd_ab_n %>%
  filter(!(Lat_difference > 60))
esmd_ab_n_lat <- esmd_ab_n %>%
  filter(!(Latitude < 0))
# removing func groups that are blank - only from functional group test
esmd_ab_n2 <- esmd_ab_n %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_ab_n3 <- esmd_ab_n %>%
  filter(!(Native_Status == ""))
# models for aboveground N
# testing if amount warmed has an effect
res.rma.ab.n <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Genus_Species),data=esmd_ab_n)
res.rma.ab.n # no effect - not including in other models
res.rma.ab.n2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Genus_Species),data=esmd_ab_n2)
res.rma.ab.n2 # graminoid: 0.0036, Shrub: 0.0021, tree: 0.0845
res.rma.ab.n3 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number/Genus_Species),data=esmd_ab_n_lat)
res.rma.ab.n3 # no effect
res.rma.ab.n4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Genus_Species),data=esmd_ab_n_latdiff)
res.rma.ab.n4 # no effect
res.rma.ab.n5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Genus_Species),data=esmd_ab_n)
res.rma.ab.n5 # years warmed: 0.0692
res.rma.ab.n6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_ab_n)
res.rma.ab.n6 # yes: <0.0001
res.rma.ab.n7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Genus_Species),data=esmd_ab_n3)
res.rma.ab.n7 # native: <0.0001


## belowground nitrogen
esmd_bl_n <- esmd_clean %>%
  filter(Var_type_broad == "Nitrogen_below")
esmd_bl_n_latdiff <- esmd_bl_n %>%
  filter(!(Lat_difference > 60))
esmd_bl_n_lat <- esmd_bl_n %>%
  filter(!(Latitude < 0))
# removing func groups that are blank - only from functional group test
esmd_bl_n2 <- esmd_bl_n %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_bl_n3 <- esmd_bl_n %>%
  filter(!(Native_Status == ""))
# models for belowground N
# testing if amount warmed has an effect
res.rma.bl.n <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Genus_Species),data=esmd_bl_n)
res.rma.bl.n # yes effect - including as random effect in other models (not including for now)
res.rma.bl.n2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Genus_Species),data=esmd_bl_n2)
res.rma.bl.n2 # no effect
res.rma.bl.n3 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number/Genus_Species),data=esmd_bl_n_lat)
res.rma.bl.n3 # latitude: 0.0009
res.rma.bl.n4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Genus_Species),data=esmd_bl_n_latdiff)
res.rma.bl.n4 # no effect
res.rma.bl.n5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Genus_Species),data=esmd_bl_n)
res.rma.bl.n5 # no effect
res.rma.bl.n6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_bl_n)
res.rma.bl.n6 # no effect
res.rma.bl.n7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Genus_Species),data=esmd_bl_n3)
res.rma.bl.n7


## spring phenophases
esmd_spring <- esmd_clean %>%
  filter(Var_type_broad == "Phen_early")
esmd_spring_latdiff <- esmd_spring %>%
  filter(!(Lat_difference > 60))
esmd_spring_lat <- esmd_spring %>%
  filter(!(Latitude < 0))
# removing func groups that are blank - only from functional group test
esmd_spring2 <- esmd_spring %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_spring3 <- esmd_spring %>%
  filter(!(Native_Status == ""))
# models for spring phenphases
# testing if amount warmed has an effect
res.rma.spring <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Genus_Species),data=esmd_spring)
res.rma.spring # no effect - not including in other models
res.rma.spring2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Genus_Species),data=esmd_spring2)
res.rma.spring2 # forb: 0.0275, graminoid: 0.0148, shrub: 0.0032
res.rma.spring3 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number/Genus_Species),data=esmd_spring_lat)
res.rma.spring3 # no effect
res.rma.spring4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Genus_Species),data=esmd_spring_latdiff)
res.rma.spring4 # no effect
res.rma.spring5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Genus_Species),data=esmd_spring)
res.rma.spring5 # years warmed: 0.0023
res.rma.spring6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_spring)
res.rma.spring6 # yes: 0.0212, no: 0.0401
res.rma.spring7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Genus_Species),data=esmd_spring3)
res.rma.spring7 # native: 0.0013, non-native:0.0515


## fall phenophases
esmd_fall <- esmd_clean %>%
  filter(Var_type_broad == "Phen_late")
esmd_fall_latdiff <- esmd_fall %>%
  filter(!(Lat_difference > 60))
esmd_fall_lat <- esmd_fall %>%
  filter(!(Latitude < 0))
# removing func groups that are blank - only from functional group test
esmd_fall2 <- esmd_fall %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_fall3 <- esmd_fall %>%
  filter(!(Native_Status == ""))
# models for fall phenophases
# testing if amount warmed has an effect
res.rma.fall <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Genus_Species),data=esmd_fall)
res.rma.fall # no effect - not including in other models
res.rma.fall2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Genus_Species),data=esmd_fall2)
res.rma.fall2 # no effect
res.rma.fall3 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number/Genus_Species),data=esmd_fall_lat)
res.rma.fall3 # no effect
res.rma.fall4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Genus_Species),data=esmd_fall_latdiff)
res.rma.fall4 # no effect
res.rma.fall5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Genus_Species),data=esmd_fall)
res.rma.fall5 # no effect
res.rma.fall6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_fall)
res.rma.fall6 # no: 0.0319
res.rma.fall7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Genus_Species),data=esmd_fall3)
res.rma.fall7 # no effect


## flwr lifespan
esmd_flwr_lifespan <- esmd_clean %>%
  filter(Var_type_broad == "Phen_flwr_lifespan")
esmd_flwr_lifespan_latdiff <- esmd_flwr_lifespan %>%
  filter(!(Lat_difference > 60))
esmd_flwr_lifespan_lat <- esmd_flwr_lifespan %>%
  filter(!(Latitude < 0))
# removing func groups that are blank - only from functional group test
esmd_flwr_lifespan2 <- esmd_flwr_lifespan %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_flwr_lifespan3 <- esmd_flwr_lifespan %>%
  filter(!(Native_Status == ""))
# models for flwr_lifespan phenophases
# testing if amount warmed has an effect
res.rma.flwr.lifespan <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number/Genus_Species),data=esmd_flwr_lifespan)
res.rma.flwr.lifespan # no effect - not including in other models
res.rma.flwr.lifespan2 <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number/Genus_Species),data=esmd_flwr_lifespan2)
res.rma.flwr.lifespan2 # graminoid: 0.0691
res.rma.flwr.lifespan3 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number/Genus_Species),data=esmd_flwr_lifespan_lat)
res.rma.flwr.lifespan3 # no effect
res.rma.flwr.lifespan4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number/Genus_Species),data=esmd_flwr_lifespan_latdiff)
res.rma.flwr.lifespan4 # no effect
res.rma.flwr.lifespan5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number/Genus_Species),data=esmd_flwr_lifespan)
res.rma.flwr.lifespan5 # years warmed: 0.0022
res.rma.flwr.lifespan6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_flwr_lifespan)
res.rma.flwr.lifespan6
res.rma.flwr.lifespan7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number/Genus_Species),data=esmd_flwr_lifespan3)
res.rma.flwr.lifespan7




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