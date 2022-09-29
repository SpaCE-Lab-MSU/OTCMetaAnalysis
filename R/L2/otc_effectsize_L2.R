# TITLE:          Effect sizes
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Pat Bills
# DATA INPUT:     L1 data sheets from the L1 folder
# DATA OUTPUT:    Effect sizes
# DATE:           Sep 2022

# Load packages
library(tidyverse)
library(metafor)

# set working directory
MA_dir<-Sys.getenv("MADIR")

# read in data
otc_data <- read.csv(file.path(MA_dir,"L1/otc_data_sample_sizes.csv"))

# example code for calculating effect sizes (eventually)
# https://rfunctions.blogspot.com/2016/10/meta-analysis-in-r.html
esmd <- escalc(measure="ROM", m1i=Warmed_Mean, m2i=Ambient_Mean,
               sd1i=Warmed_Var, sd2i=Ambient_Var,
               n1i=Warmed_N, n2i=Ambient_N,
               data=otc_data)

# https://ecologyforacrowdedplanet.wordpress.com/2013/05/10/using-metafor-and-ggplot-togetherpart-1/
# example stats: using biomass for now
esmd_bio_above <- esmd %>%
  filter(Var_type == "Biomass_above")
# running stats
SMD.ma<-rma.uni(yi,vi,method="REML",data=esmd_bio_above)
summary(SMD.ma)
# basic plot
forest.rma(SMD.ma)

