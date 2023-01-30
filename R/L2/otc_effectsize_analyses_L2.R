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

# https://ecologyforacrowdedplanet.wordpress.com/2013/05/10/using-metafor-and-ggplot-togetherpart-1/
# example stats: update var_type based on what variable to look at
unique(esmd_clean$Var_type)
esmd_clean %>% 
  count(Var_type)
esmd_var_type <- esmd_clean %>%
  filter(Var_type == "Height" | Var_type == "Biomass_above" | Var_type == "Flower_num" |
           Var_type == "Percent_cover" | Var_type == "Nitrogen_above" | Var_type == "Shoot_length")

# running stats
SMD.ma<-rma.uni(yi,vi,method="REML",data=esmd_var_type)
summary(SMD.ma)

