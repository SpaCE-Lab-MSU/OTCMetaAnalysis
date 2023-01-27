# TITLE:          Map of OTC studies
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Pat Bills
# DATA INPUT:     An L1 dataframe with latitude and longitude of studies
# DATA OUTPUT:    Map containing points for each study location
# DATE:           Aug 2022

#https://datavizpyr.com/how-to-make-world-map-with-ggplot2-in-r/
# Load packages
library(tidyverse)
library(maps)

# set working directory
MA_dir<-Sys.getenv("MADIR")

# Load data
coord <- read.csv(file.path(MA_dir,"L1/otc_data_coordinates_L1.csv"))
world <- map_data("world")

# removing coordinates for pubs that were removed
coord <- coord %>%
  filter(!(Pub_number == 4 | Pub_number == 5 | Pub_number == 19 | Pub_number == 23 |
           Pub_number == 28 | Pub_number == 31 | Pub_number == 35 | Pub_number == 36 |
           Pub_number == 37 | Pub_number == 39 | Pub_number == 49 | Pub_number == 52 |
           Pub_number == 5 | Pub_number == 58 | Pub_number == 67 | Pub_number == 68 |
           Pub_number == 72 | Pub_number == 76 | Pub_number == 79 | Pub_number == 84 |
           Pub_number == 89 | Pub_number == 125 | Pub_number == 142 | Pub_number == 143 |
           Pub_number == 146 | Pub_number == 154 | Pub_number == 155 | Pub_number == 156 |
           Pub_number == 162 | Pub_number == 177 | Pub_number == 179 |
           Pub_number == 181 | Pub_number == 183 | Pub_number == 185 | Pub_number == 189 |
           Pub_number == 197 | Pub_number == 198 | Pub_number == 202 | Pub_number == 206 |
           Pub_number == 208 | Pub_number == 212 | Pub_number == 214 | Pub_number == 215 |
           Pub_number == 217 | Pub_number == 220 | Pub_number == 226 | Pub_number == 228 |
           Pub_number == 232 | Pub_number == 234))

# make map
png("otc_plot_L2.png", units="in", width=10, height=6, res=300)
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = coord,
    aes(Long, Lat),
    alpha = 0.7,
    color = "red"
  ) +
  theme_classic()
dev.off()
