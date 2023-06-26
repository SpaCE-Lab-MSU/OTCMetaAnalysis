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


#### main stats ####
## removing variables that have <10 effect sizes
esmd_clean %>%
  group_by(Var_type_broad) %>%
  summarize(count = n())
esmd_clean <- esmd_clean %>%
  filter(!(Var_type_broad == "Biomass_total" |
             Var_type_broad == "Flower_weight" |
             Var_type_broad == "Phen_leaf_lifespan" |
             Var_type_broad == "Phen_preflwr_length"))


## aboveground biomass
esmd_ab_biomass <- esmd_clean %>%
  filter(Var_type_broad == "Biomass_above")
esmd_ab_biomass_latdiff <- esmd_ab_biomass %>%
  filter(!(Lat_difference > 60))
# removing func groups that are blank - only from functional group test
esmd_ab_biomass2 <- esmd_ab_biomass %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_ab_biomass3 <- esmd_ab_biomass %>%
  filter(!(Native_Status == ""))
# models for aboveground biomass
res.rma.ab.biomass <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_ab_biomass2)
res.rma.ab.biomass
res.rma.ab.biomass2 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_ab_biomass)
res.rma.ab.biomass2
res.rma.ab.biomass3 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_ab_biomass_latdiff)
res.rma.ab.biomass3
res.rma.ab.biomass4 <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_ab_biomass)
res.rma.ab.biomass4
res.rma.ab.biomass5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_ab_biomass)
res.rma.ab.biomass5
res.rma.ab.biomass6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number,~1|Func_group_broad), data=esmd_ab_biomass)
res.rma.ab.biomass6
res.rma.ab.biomass7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_ab_biomass3)
res.rma.ab.biomass7
# all comparisons - needs some work
summary(glht(res.rma.ab.biomass, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## belowground biomass
esmd_bl_biomass <- esmd_clean %>%
  filter(Var_type_broad == "Biomass_below")
esmd_bl_biomass_latdiff <- esmd_bl_biomass %>%
  filter(!(Lat_difference > 60))
# removing func groups that are blank - only from functional group test
esmd_bl_biomass2 <- esmd_bl_biomass %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_bl_biomass3 <- esmd_bl_biomass %>%
  filter(!(Native_Status == ""))
# models for belowground biomass
res.rma.bl.biomass <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_bl_biomass2)
res.rma.bl.biomass
res.rma.bl.biomass2 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_bl_biomass)
res.rma.bl.biomass2
res.rma.bl.biomass3 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_bl_biomass_latdiff)
res.rma.bl.biomass3
res.rma.bl.biomass4 <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_bl_biomass)
res.rma.bl.biomass4
res.rma.bl.biomass5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_bl_biomass)
res.rma.bl.biomass5
res.rma.bl.biomass6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number,~1|Func_group_broad), data=esmd_bl_biomass)
res.rma.bl.biomass6
res.rma.bl.biomass7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_bl_biomass3)
res.rma.bl.biomass7
# all comparisons - needs some work
summary(glht(res.rma.bl.biomass, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## flower number
esmd_flwr_num <- esmd_clean %>%
  filter(Var_type_broad == "Flower_num")
esmd_flwr_num_latdiff <- esmd_flwr_num %>%
  filter(!(Lat_difference > 60))
# removing func groups that are blank - only from functional group test
esmd_flwr_num2 <- esmd_flwr_num %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_flwr_num3 <- esmd_flwr_num %>%
  filter(!(Native_Status == ""))
# models for flower number
res.rma.flwr.num <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_flwr_num2)
res.rma.flwr.num
res.rma.flwr.num2 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_flwr_num)
res.rma.flwr.num2
res.rma.flwr.num3 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_flwr_num_latdiff)
res.rma.flwr.num3
res.rma.flwr.num4 <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_flwr_num)
res.rma.flwr.num4
res.rma.flwr.num5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_flwr_num)
res.rma.flwr.num5
res.rma.flwr.num6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number,~1|Func_group_broad), data=esmd_flwr_num)
res.rma.flwr.num6
res.rma.flwr.num7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_flwr_num3)
res.rma.flwr.num7
# all comparisons - needs some work
summary(glht(res.rma.flwr.num, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## fruit number
esmd_fruit_num <- esmd_clean %>%
  filter(Var_type_broad == "Fruit_num")
esmd_fruit_num_latdiff <- esmd_fruit_num %>%
  filter(!(Lat_difference > 60))
# removing func groups that are blank
esmd_fruit_num2 <- esmd_fruit_num %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_fruit_num3 <- esmd_fruit_num %>%
  filter(!(Native_Status == ""))
# models for fruit number
res.rma.fruit.num <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fruit_num2)
res.rma.fruit.num
res.rma.fruit.num2 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fruit_num)
res.rma.fruit.num2
res.rma.fruit.num3 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fruit_num_latdiff)
res.rma.fruit.num3
res.rma.fruit.num4 <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fruit_num)
res.rma.fruit.num4
res.rma.fruit.num5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fruit_num)
res.rma.fruit.num5
res.rma.fruit.num6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number,~1|Func_group_broad), data=esmd_fruit_num)
res.rma.fruit.num6
res.rma.fruit.num7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fruit_num3)
res.rma.fruit.num7
# all comparisons - needs some work
summary(glht(res.rma.fruit.num, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## fruit weight
esmd_fruit_weight <- esmd_clean %>%
  filter(Var_type_broad == "Fruit_weight")
esmd_fruit_weight_latdiff <- esmd_fruit_weight %>%
  filter(!(Lat_difference > 60))
# removing func groups that are blank
esmd_fruit_weight2 <- esmd_fruit_weight %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_fruit_weight3 <- esmd_fruit_weight %>%
  filter(!(Native_Status == ""))
# models for fruit weight
res.rma.fruit.weight <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fruit_weight2)
res.rma.fruit.weight
res.rma.fruit.weight2 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fruit_weight)
res.rma.fruit.weight2
res.rma.fruit.weight3 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fruit_weight_latdiff)
res.rma.fruit.weight3
res.rma.fruit.weight4 <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fruit_weight)
res.rma.fruit.weight4
res.rma.fruit.weight5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fruit_weight)
res.rma.fruit.weight5
res.rma.fruit.weight6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number,~1|Func_group_broad), data=esmd_fruit_weight)
res.rma.fruit.weight6
res.rma.fruit.weight7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fruit_weight3)
res.rma.fruit.weight7
# all comparisons - needs some work
summary(glht(res.rma.fruit.weight, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## growth
esmd_growth <- esmd_clean %>%
  filter(Var_type_broad == "Growth")
esmd_growth_latdiff <- esmd_growth %>%
  filter(!(Lat_difference > 60))
# removing func groups that are blank
esmd_growth2 <- esmd_growth %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_growth3 <- esmd_growth %>%
  filter(!(Native_Status == ""))
# models for growth
res.rma.growth <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_growth2)
res.rma.growth
res.rma.growth2 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_growth)
res.rma.growth2
res.rma.growth3 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_growth_latdiff)
res.rma.growth3
res.rma.growth4 <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_growth)
res.rma.growth4
res.rma.growth5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_growth)
res.rma.growth5
res.rma.growth6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number,~1|Func_group_broad), data=esmd_growth)
res.rma.growth6
res.rma.growth7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_growth3)
res.rma.growth7
# all comparisons - needs some work
summary(glht(res.rma.growth, linfct=cbind(contrMat(rep(1,6), type="Tukey"))), test=adjusted("none"))


## leaf growth
esmd_leaf_growth <- esmd_clean %>%
  filter(Var_type_broad == "Leaf_Growth")
esmd_leaf_growth_latdiff <- esmd_leaf_growth %>%
  filter(!(Lat_difference > 60))
# removing func groups that are blank
esmd_leaf_growth2 <- esmd_leaf_growth %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_leaf_growth3 <- esmd_leaf_growth %>%
  filter(!(Native_Status == ""))
# models for leaf growth
res.rma.leaf.growth <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_leaf_growth2)
res.rma.leaf.growth
res.rma.leaf.growth2 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_leaf_growth)
res.rma.leaf.growth2
res.rma.leaf.growth3 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_leaf_growth_latdiff)
res.rma.leaf.growth3
res.rma.leaf.growth4 <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_leaf_growth)
res.rma.leaf.growth4
res.rma.leaf.growth5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_leaf_growth)
res.rma.leaf.growth5
res.rma.leaf.growth6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number,~1|Func_group_broad), data=esmd_leaf_growth)
res.rma.leaf.growth6
res.rma.leaf.growth7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_leaf_growth3)
res.rma.leaf.growth7
# all comparisons - needs some work
summary(glht(res.rma.leaf.growth, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## percent cover
esmd_cover <- esmd_clean %>%
  filter(Var_type_broad == "Percent_cover")
esmd_cover_latdiff <- esmd_cover %>%
  filter(!(Lat_difference > 60))
# removing func groups that are blank - only from functional group test
esmd_cover2 <- esmd_cover %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_cover3 <- esmd_cover %>%
  filter(!(Native_Status == ""))
# models for perc cover
res.rma.cover <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_cover2)
res.rma.cover
res.rma.cover2 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_cover)
res.rma.cover2
res.rma.cover3 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_cover_latdiff)
res.rma.cover3
res.rma.cover4 <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_cover)
res.rma.cover4
res.rma.cover5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_cover)
res.rma.cover5
res.rma.cover6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number,~1|Func_group_broad), data=esmd_cover)
res.rma.cover6
res.rma.cover7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_cover3)
res.rma.cover7
# all comparisons - needs some work
summary(glht(res.rma.cover, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## aboveground nitrogen
esmd_ab_n <- esmd_clean %>%
  filter(Var_type_broad == "Nitrogen_above")
esmd_ab_n_latdiff <- esmd_ab_n %>%
  filter(!(Lat_difference > 60))
# removing func groups that are blank - only from functional group test
esmd_ab_n2 <- esmd_ab_n %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_ab_n3 <- esmd_ab_n %>%
  filter(!(Native_Status == ""))
# models for aboveground N
res.rma.ab.n <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_ab_n2)
res.rma.ab.n
res.rma.ab.n2 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_ab_n)
res.rma.ab.n2
res.rma.ab.n3 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_ab_n_latdiff)
res.rma.ab.n3
res.rma.ab.n4 <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_ab_n)
res.rma.ab.n4
res.rma.ab.n5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_ab_n)
res.rma.ab.n5
res.rma.ab.n6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number,~1|Func_group_broad), data=esmd_ab_n)
res.rma.ab.n6
res.rma.ab.n7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_ab_n3)
res.rma.ab.n7
# all comparisons - needs some work
summary(glht(res.rma.ab.n, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## belowground nitrogen
esmd_bl_n <- esmd_clean %>%
  filter(Var_type_broad == "Nitrogen_below")
esmd_bl_n_latdiff <- esmd_bl_n %>%
  filter(!(Lat_difference > 60))
# removing func groups that are blank - only from functional group test
esmd_bl_n2 <- esmd_bl_n %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_bl_n3 <- esmd_bl_n %>%
  filter(!(Native_Status == ""))
# models for belowground N
res.rma.bl.n <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_bl_n2)
res.rma.bl.n
res.rma.bl.n2 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_bl_n)
res.rma.bl.n2
res.rma.bl.n3 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_bl_n_latdiff)
res.rma.bl.n3
res.rma.bl.n4 <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_bl_n)
res.rma.bl.n4
res.rma.bl.n5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_bl_n)
res.rma.bl.n5
res.rma.bl.n6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number,~1|Func_group_broad), data=esmd_bl_n)
res.rma.bl.n6
res.rma.bl.n7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_bl_n3)
res.rma.bl.n7
# all comparisons - needs some work
summary(glht(res.rma.bl.n, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## spring phenophases
esmd_spring <- esmd_clean %>%
  filter(Var_type_broad == "Phen_early")
esmd_spring_latdiff <- esmd_spring %>%
  filter(!(Lat_difference > 60))
# removing func groups that are blank - only from functional group test
esmd_spring2 <- esmd_spring %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_spring3 <- esmd_spring %>%
  filter(!(Native_Status == ""))
# models for spring phenphases
res.rma.spring <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_spring2)
res.rma.spring
res.rma.spring2 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_spring)
res.rma.spring2
res.rma.spring3 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_spring_latdiff)
res.rma.spring3
res.rma.spring4 <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_spring)
res.rma.spring4
res.rma.spring5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_spring)
res.rma.spring5
res.rma.spring6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number,~1|Func_group_broad), data=esmd_spring)
res.rma.spring6
res.rma.spring7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_spring3)
res.rma.spring7
# all comparisons - needs some work
summary(glht(res.rma.spring, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## fall phenophases
esmd_fall <- esmd_clean %>%
  filter(Var_type_broad == "Phen_late")
esmd_fall_latdiff <- esmd_fall %>%
  filter(!(Lat_difference > 60))
# removing func groups that are blank - only from functional group test
esmd_fall2 <- esmd_fall %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_fall3 <- esmd_fall %>%
  filter(!(Native_Status == ""))
# models for fall phenophases
res.rma.fall <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fall2)
res.rma.fall
res.rma.fall2 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fall)
res.rma.fall2
res.rma.fall3 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fall_latdiff)
res.rma.fall3
res.rma.fall4 <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fall)
res.rma.fall4
res.rma.fall5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fall)
res.rma.fall5
res.rma.fall6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number,~1|Func_group_broad), data=esmd_fall)
res.rma.fall6
res.rma.fall7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_fall3)
res.rma.fall7
# all comparisons - needs some work
summary(glht(res.rma.fall, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## flwr lifespan
esmd_flwr_lifespan <- esmd_clean %>%
  filter(Var_type_broad == "Phen_flwr_lifespan")
esmd_flwr_lifespan_latdiff <- esmd_flwr_lifespan %>%
  filter(!(Lat_difference > 60))
# removing func groups that are blank - only from functional group test
esmd_flwr_lifespan2 <- esmd_flwr_lifespan %>%
  filter(!(Func_group_broad == ""))
# removing native status that is blank, only for native status test
esmd_flwr_lifespan3 <- esmd_flwr_lifespan %>%
  filter(!(Native_Status == ""))
# models for flwr_lifespan phenophases
res.rma.flwr.lifespan <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_flwr_lifespan2)
res.rma.flwr.lifespan
res.rma.flwr.lifespan2 <- rma.mv(yi, vi, mods=~Latitude,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_flwr_lifespan)
res.rma.flwr.lifespan2
res.rma.flwr.lifespan3 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_flwr_lifespan_latdiff)
res.rma.flwr.lifespan3
res.rma.flwr.lifespan4 <- rma.mv(yi, vi, mods=~Amount_warmed_C,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_flwr_lifespan)
res.rma.flwr.lifespan4
res.rma.flwr.lifespan5 <- rma.mv(yi, vi, mods=~Years_warmed,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_flwr_lifespan)
res.rma.flwr.lifespan5
res.rma.flwr.lifespan6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number), data=esmd_flwr_lifespan)
res.rma.flwr.lifespan6
res.rma.flwr.lifespan7 <- rma.mv(yi, vi, mods=~Native_Status-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_flwr_lifespan3)
res.rma.flwr.lifespan7
# all comparisons - needs some work
summary(glht(res.rma.flwr.lifespan, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))




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