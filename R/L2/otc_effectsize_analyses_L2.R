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
otc_data <- read.csv(file.path(MA_dir,"L2/otc_data_cleaned_L2.csv"))


# calculating effect sizes
# https://rfunctions.blogspot.com/2016/10/meta-analysis-in-r.html
esmd <- escalc(measure="SMD", m1i=Warmed_Mean, m2i=Ambient_Mean, # SMD = Hedge's g
               sd1i=Warmed_SD, sd2i=Ambient_SD,
               n1i=Warmed_N, n2i=Ambient_N,
               data=otc_data)

# remove rows with incomplete data
esmd_clean <- esmd %>%
  filter(!is.na(vi))


# is there a significant relationship between the amount warmed by the experiment and our effect sizes?
# if yes, this indicates that we need to account for this variation in our model (random effect?)
res.rma.C <- rma(yi, vi, mods = ~ Amount_warmed_C, data=esmd_clean, method="REML")
res.rma.C
# no, so we don't need to account for the amount warmed

# is there a significant relationship between the latitude of the experiment and our effect sizes?
# if yes, this indicates that we need to account for this variation in our model (random effect?)
res.rma.lat <- rma(yi, vi, mods = ~ Latitude, data=esmd_clean, method="REML")
res.rma.lat
# yes, there is an effect of latitude

# is there a significant relationship between year-round warming and our effect sizes?
# if yes, this indicates that we need to account for this variation in our model (random effect?)
res.rma.year <- rma(yi, vi, mods = ~ Year_round_warm, data=esmd_clean, method="REML")
res.rma.year
# no, so we don't need to account for year-round warming

# is there a significant relationship between months warmed and our effect sizes?
# if yes, this indicates that we need to account for this variation in our model (random effect?)
res.rma.months <- rma(yi, vi, mods = ~ Years_warmed, data=esmd_clean, method="REML")
res.rma.months
# no, so we don't need to account for months warmed



### main stats (all variables):
# removing func groups that are blank
esmd_clean2<- esmd_clean %>%
  filter(!(Var_type == ""))
# equal effects model for height
res.rma.all <- rma(yi, vi, mods=~Var_type, data=esmd_clean2)
res.rma.all
# all comparisons
# Values of P=1.0 indicate that there is no evidence of a difference between variables
# Holm correction used here because it was used in the Kuebbing phenology meta-analysis
summary(glht(res.rma.all, linfct=cbind(contrMat(rep(1,31), type="Tukey"))), test=adjusted("holm"))



### main stats (single variable): update var_type based on what variable to look at
unique(esmd_clean$Var_type)
esmd_clean %>% 
  count(Var_type)

## height
esmd_height <- esmd_clean %>%
  filter(Var_type == "Height")
# removing func groups that are blank
esmd_height <- esmd_height %>%
  filter(!(Func_group_broad == ""))
# models for height
res.rma.height <- rma(yi, vi, mods=~Func_group_broad-1, data=esmd_height)
res.rma.height
res.rma.height2 <- rma(yi, vi, data=esmd_height)
res.rma.height2
res.rma.height3 <- rma(yi, vi, mods=~Latitude,data=esmd_height)
res.rma.height3
# all comparisons - needs some work
summary(glht(res.rma.height, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))

## biomass
esmd_ab_biomass <- esmd_clean %>%
  filter(Var_type == "Biomass_above")
# removing func groups that are blank
esmd_ab_biomass <- esmd_ab_biomass %>%
  filter(!(Func_group_broad == ""))
# models for height
res.rma.ab.biomass <- rma(yi, vi, mods=~Func_group_broad-1, data=esmd_ab_biomass)
res.rma.ab.biomass
res.rma.ab.biomass2 <- rma(yi, vi, data=esmd_ab_biomass)
res.rma.ab.biomass2
res.rma.ab.biomass3 <- rma(yi, vi, mods=~Latitude,data=esmd_ab_biomass)
res.rma.ab.biomass3
# all comparisons - needs some work
summary(glht(res.rma.ab.biomass, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))

## percent cover
esmd_cover <- esmd_clean %>%
  filter(Var_type == "Percent_cover")
# removing func groups that are blank
esmd_cover <- esmd_cover %>%
  filter(!(Func_group_broad == ""))
# models for height
res.rma.cover <- rma(yi, vi, mods=~Func_group_broad-1, data=esmd_cover)
res.rma.cover
res.rma.cover2 <- rma(yi, vi, data=esmd_cover)
res.rma.cover2
res.rma.cover3 <- rma(yi, vi, mods=~Latitude,data=esmd_cover)
res.rma.cover3
# all comparisons - needs some work
summary(glht(res.rma.cover, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))

## nitrogen
esmd_ab_n <- esmd_clean %>%
  filter(Var_type == "Nitrogen_above")
# removing func groups that are blank
esmd_ab_n <- esmd_ab_n %>%
  filter(!(Func_group_broad == ""))
# models for height
res.rma.ab.n <- rma(yi, vi, mods=~Func_group_broad-1, data=esmd_ab_n)
res.rma.ab.n
res.rma.ab.n2 <- rma(yi, vi, data=esmd_ab_n)
res.rma.ab.n2
res.rma.ab.n3 <- rma(yi, vi, mods=~Latitude,data=esmd_ab_n)
res.rma.ab.n3
# all comparisons - needs some work
summary(glht(res.rma.ab.n, linfct=cbind(contrMat(rep(1,9), type="Tukey"))), test=adjusted("none"))



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