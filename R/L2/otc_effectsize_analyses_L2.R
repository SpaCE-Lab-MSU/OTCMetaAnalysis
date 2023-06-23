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




#### main stats (single variable): update var_type based on what variable to look at ####
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
# removing func groups that are blank - only from functional group test
esmd_ab_biomass2 <- esmd_ab_biomass %>%
  filter(!(Func_group_broad == ""))
# models for aboveground biomass
res.rma.ab.biomass <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_ab_biomass2)
res.rma.ab.biomass
res.rma.ab.biomass2 <- rma(yi, vi, mods=~Latitude,data=esmd_ab_biomass)
res.rma.ab.biomass2
res.rma.ab.biomass3 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_ab_biomass)
res.rma.ab.biomass3
res.rma.ab.biomass4 <- rma(yi, vi, mods=~Amount_warmed_C,data=esmd_ab_biomass)
res.rma.ab.biomass4
res.rma.ab.biomass5 <- rma(yi, vi, mods=~Years_warmed,data=esmd_ab_biomass)
res.rma.ab.biomass5
res.rma.ab.biomass6 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number), data=esmd_ab_biomass)
res.rma.ab.biomass6
# all comparisons - needs some work
summary(glht(res.rma.ab.biomass, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## belowground biomass
esmd_bl_biomass <- esmd_clean %>%
  filter(Var_type_broad == "Biomass_below")
# removing func groups that are blank - only from functional group test
esmd_bl_biomass2 <- esmd_bl_biomass %>%
  filter(!(Func_group_broad == ""))
# models for belowground biomass
res.rma.bl.biomass <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm), data=esmd_bl_biomass2)
res.rma.bl.biomass
res.rma.bl.biomass2 <- rma.mv(yi, vi, c,~1|Latitude),data=esmd_bl_biomass)
res.rma.bl.biomass2
res.rma.bl.biomass3 <- rma(yi, vi, mods=~Latitude,data=esmd_bl_biomass)
res.rma.bl.biomass3
res.rma.bl.biomass4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number, ~1|Year_round_warm),data=esmd_bl_biomass)
res.rma.bl.biomass4
res.rma.bl.biomass5 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_bl_biomass)
res.rma.bl.biomass5
# all comparisons - needs some work
summary(glht(res.rma.bl.biomass, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## flower number
esmd_flwr_num <- esmd_clean %>%
  filter(Var_type_broad == "Flower_num")
# removing func groups that are blank - only from functional group test
esmd_flwr_num2 <- esmd_flwr_num %>%
  filter(!(Func_group_broad == ""))
# models for aboveground biomass
res.rma.flwr.num <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm), data=esmd_flwr_num2)
res.rma.flwr.num
res.rma.flwr.num2 <- rma.mv(yi, vi, c,~1|Latitude),data=esmd_flwr_num)
res.rma.flwr.num2
res.rma.flwr.num3 <- rma(yi, vi, mods=~Latitude,data=esmd_flwr_num)
res.rma.flwr.num3
res.rma.flwr.num4 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_flwr_num)
res.rma.flwr.num4
# all comparisons - needs some work
summary(glht(res.rma.flwr.num, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## fruit number
esmd_fruit_num <- esmd_clean %>%
  filter(Var_type_broad == "Fruit_num")
# removing func groups that are blank
esmd_fruit_num2 <- esmd_fruit_num %>%
  filter(!(Func_group_broad == ""))
# models for bloveground biomass
res.rma.fruit.num <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm), data=esmd_fruit_num2)
res.rma.fruit.num
res.rma.fruit.num2 <- rma.mv(yi, vi, c,~1|Latitude),data=esmd_fruit_num)
res.rma.fruit.num2
res.rma.fruit.num3 <- rma(yi, vi, mods=~Latitude,data=esmd_fruit_num)
res.rma.fruit.num3
res.rma.fruit.num4 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_fruit_num)
res.rma.fruit.num4
# all comparisons - needs some work
summary(glht(res.rma.fruit.num, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## fruit weight
esmd_fruit_weight <- esmd_clean %>%
  filter(Var_type_broad == "Fruit_weight")
# removing func groups that are blank
esmd_fruit_weight2 <- esmd_fruit_weight %>%
  filter(!(Func_group_broad == ""))
# models for bloveground biomass
res.rma.fruit.weight <- rma.mv(yi, vi, mods=~Func_group_broad-1, random=list(~1|Pub_number, ~1|Year_round_warm), data=esmd_fruit_weight2)
res.rma.fruit.weight
res.rma.fruit.weight2 <- rma.mv(yi, vi, c,~1|Latitude),data=esmd_fruit_weight)
res.rma.fruit.weight2
res.rma.fruit.weight3 <- rma(yi, vi, mods=~Latitude,data=esmd_fruit_weight)
res.rma.fruit.weight3
res.rma.fruit.weight4 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_fruit_weight)
res.rma.fruit.weight4
# all comparisons - needs some work
summary(glht(res.rma.fruit.weight, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## growth
esmd_growth <- esmd_clean %>%
  filter(Var_type_broad == "Growth")
# removing func groups that are blank
esmd_growth2 <- esmd_growth %>%
  filter(!(Func_group_broad == ""))
# models for growth
res.rma.growth <- rma(yi, vi, mods=~Func_group_broad-1, data=esmd_growth2) # comparing func groups
res.rma.growth
res.rma.growth2 <- rma.mv(yi, vi, random=list(~1|Pub_number/Genus_Species,~1|Latitude), data=esmd_growth) # overall model w/ random effects
res.rma.growth2
res.rma.growth3 <- rma(yi, vi, mods=~Latitude,data=esmd_growth)
res.rma.growth3
res.rma.growth4 <- rma(yi, vi, mods=~Lat_difference,data=esmd_growth)
res.rma.growth4
res.rma.growth5 <- rma(yi, vi, mods=~Amount_warmed_C,data=esmd_growth)
res.rma.growth5
res.rma.growth6 <- rma(yi, vi, mods=~Years_warmed,data=esmd_growth)
res.rma.growth6
res.rma.growth7 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number), data=esmd_growth)
res.rma.growth7
# all comparisons - needs some work
summary(glht(res.rma.growth, linfct=cbind(contrMat(rep(1,6), type="Tukey"))), test=adjusted("none"))


## leaf growth
esmd_leaf_growth <- esmd_clean %>%
  filter(Var_type_broad == "Leaf_Growth")
# removing func groups that are blank
esmd_leaf_growth2 <- esmd_leaf_growth %>%
  filter(!(Func_group_broad == ""))
# removing NA rows from amount warmed
esmd_leaf_growth <- esmd_leaf_growth %>%
  filter(!is.na(Amount_warmed_C))
# models for aboveground biomass
res.rma.leaf.growth <- rma(yi, vi, mods=~Func_group_broad-1, data=esmd_leaf_growth2)
res.rma.leaf.growth
res.rma.leaf.growth2 <- rma.mv(yi, vi, c,~1|Latitude),data=esmd_leaf_growth)
res.rma.leaf.growth2
res.rma.leaf.growth3 <- rma(yi, vi, mods=~Latitude,data=esmd_leaf_growth)
res.rma.leaf.growth3
res.rma.leaf.growth4 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number,~1|Amount_warmed_C,~1|Years_warmed), data=esmd_leaf_growth)
res.rma.leaf.growth4
# all comparisons - needs some work
summary(glht(res.rma.leaf.growth, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## percent cover
esmd_cover <- esmd_clean %>%
  filter(Var_type_broad == "Percent_cover")
# removing func groups that are blank - only from functional group test
esmd_cover2 <- esmd_cover %>%
  filter(!(Func_group_broad == ""))
# models for height
res.rma.cover <- rma(yi, vi, mods=~Func_group_broad-1, data=esmd_cover2)
res.rma.cover
res.rma.cover2 <- rma.mv(yi, vi, random=list(~1|Pub_number/Genus_Species,~1|Latitude),data=esmd_cover)
res.rma.cover2
res.rma.cover3 <- rma(yi, vi, mods=~Latitude,data=esmd_cover)
res.rma.cover3
res.rma.cover4 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_cover)
res.rma.cover4
# all comparisons - needs some work
summary(glht(res.rma.cover, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## aboveground nitrogen
esmd_ab_n <- esmd_clean %>%
  filter(Var_type_broad == "Nitrogen_above")
# removing func groups that are blank - only from functional group test
esmd_ab_n2 <- esmd_ab_n %>%
  filter(!(Func_group_broad == ""))
# models for height
res.rma.ab.n <- rma(yi, vi, mods=~Func_group_broad-1, data=esmd_ab_n2)
res.rma.ab.n
res.rma.ab.n2 <- rma.mv(yi, vi, random=list(~1|Pub_number/Genus_Species,~1|Latitude),data=esmd_ab_n)
res.rma.ab.n2
res.rma.ab.n3 <- rma(yi, vi, mods=~Latitude,data=esmd_ab_n)
res.rma.ab.n3
res.rma.ab.n4 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_ab_n)
res.rma.ab.n4
# all comparisons - needs some work
summary(glht(res.rma.ab.n, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## belowground nitrogen
esmd_bl_n <- esmd_clean %>%
  filter(Var_type_broad == "Nitrogen_below")
# removing func groups that are blank - only from functional group test
esmd_bl_n2 <- esmd_bl_n %>%
  filter(!(Func_group_broad == ""))
# models for height
res.rma.bl.n <- rma(yi, vi, mods=~Func_group_broad-1, data=esmd_bl_n2)
res.rma.bl.n
res.rma.bl.n2 <- rma.mv(yi, vi, random=list(~1|Pub_number/Genus_Species,~1|Latitude),data=esmd_bl_n)
res.rma.bl.n2
res.rma.bl.n3 <- rma(yi, vi, mods=~Latitude,data=esmd_bl_n)
res.rma.bl.n3
res.rma.bl.n4 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_bl_n)
res.rma.bl.n4
# all comparisons - needs some work
summary(glht(res.rma.bl.n, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## spring phenophases
esmd_spring <- esmd_clean %>%
  filter(Var_type_broad == "Phen_early")
# removing func groups that are blank - only from functional group test
esmd_spring2 <- esmd_spring %>%
  filter(!(Func_group_broad == ""))
# models for height
res.rma.spring <- rma(yi, vi, mods=~Func_group_broad-1, data=esmd_spring2)
res.rma.spring
res.rma.spring2 <- rma.mv(yi, vi, random=list(~1|Pub_number/Genus_Species,~1|Latitude),data=esmd_spring)
res.rma.spring2
res.rma.spring3 <- rma(yi, vi, mods=~Latitude,data=esmd_spring)
res.rma.spring3
res.rma.spring4 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number/Genus_Species), data=esmd_spring)
res.rma.spring4
# all comparisons - needs some work
summary(glht(res.rma.spring, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))


## fall phenophases
esmd_fall <- esmd_clean %>%
  filter(Var_type_broad == "Phen_late")
# removing func groups that are blank - only from functional group test
esmd_fall2 <- esmd_fall %>%
  filter(!(Func_group_broad == ""))
# models for height
res.rma.fall <- rma(yi, vi, mods=~Func_group_broad-1, data=esmd_fall2)
res.rma.fall
res.rma.fall2 <- rma.mv(yi, vi, random=list(~1|Pub_number/Genus_Species,~1|Latitude),data=esmd_fall)
res.rma.fall2
res.rma.fall3 <- rma(yi, vi, mods=~Latitude,data=esmd_fall)
res.rma.fall3
res.rma.fall4 <- rma.mv(yi, vi, mods=~Lat_difference,random=list(~1|Pub_number,~1|Year_round_warm),data=esmd_fall)
res.rma.fall4
res.rma.fall5 <- rma.mv(yi, vi, mods=~Year_round_warm-1,random=list(~1|Pub_number), data=esmd_fall)
res.rma.fall5
# all comparisons - needs some work
summary(glht(res.rma.fall, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))






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