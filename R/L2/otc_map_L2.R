# TITLE:          Map of OTC studies
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Pat Bills
# DATA INPUT:     An L1 dataframe with latitude and longitude of studies
# DATA OUTPUT:    Map containing points for each study location
# DATE:           Aug 2022

# clear environment
rm(list = ls())

#https://datavizpyr.com/how-to-make-world-map-with-ggplot2-in-r/
# Load packages
library(tidyverse)
library(maps)

# set working directory
MA_dir<-Sys.getenv("MADIR")

# Load data
effect <- read.csv(file.path(MA_dir,"L2/otc_effect_sizes_L2.csv"))
world <- map_data("world")

# make map
png("otc_plot_L2.png", units="in", width=10, height=6, res=300)
ggplot() +
  geom_map(
    data = world, map = world,
    aes(map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = effect,
    aes(Longitude, Latitude),
    alpha = 0.7,
    color = "red"
  ) +
  theme_classic()
dev.off()
