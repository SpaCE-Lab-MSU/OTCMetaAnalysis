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


### main stats: update var_type based on what variable to look at
unique(esmd_clean$Var_type)
esmd_clean %>% 
  count(Var_type)

## height
esmd_height <- esmd_clean %>%
  filter(Var_type == "Height")
# equal effects model for height
res.rma.height <- rma(yi, vi, data=esmd_height, method="EE")
res.rma.height

