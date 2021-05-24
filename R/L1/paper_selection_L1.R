# TITLE:          OTC Meta-Analysis paper selection
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Pat Bills
# DATA INPUT:     A csv containing info on what traits are measured in each paper
# DATA OUTPUT:    A csv with non-relevant papers removed
# DATE:           May 2021

# clear environment
rm(list=ls())

# load packages
library(tidyverse)

# set working directory
MA_dir<-Sys.getenv("MADIR")

# read in data
leaf <- read.csv(file.path(MA_dir,"L0/leaf_traits.csv"))
plant <- read.csv(file.path(MA_dir,"L0/plant_traits.csv"))

# remove "total" row at the bottom of the sheet
leaf <- leaf[-c(43, 44, 45), ]
plant <- plant[-c(178), ]

# subset dataframes to keep rows that record a plant response variable that is present in >= papers (prev. calculated)
leaf_subset <- leaf %>%
  filter(leaf_length_width == 1 | SLA == 1)

plant_subset <- plant %>%
  filter(N_conc == 1 | perc_cover == 1 | biomass == 1 | height == 1 | shoot_length == 1 |
           phenology == 1 | flower_fruit_num == 1 | flower_fruit_biomass == 1)

# save these csv's with the final paper numbers to L1 folder
write.csv(leaf_subset, file.path(MA_dir,"L1/leaf_trait_papers_L1.csv"), row.names = F)
write.csv(plant_subset, file.path(MA_dir,"L1/plant_trait_papers_L1.csv"), row.names = F)
