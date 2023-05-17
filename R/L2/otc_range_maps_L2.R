# TITLE:          Range maps for species
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Pat Bills
# DATA INPUT:     Range maps and species information from MA data
# DATA OUTPUT:    
# DATE:           April 2023


# thoughts
# IUCN spatial data: would be great, but has very few species
# BIEN: only has range maps for Americas, but occurrence data could work
# GBIF: make my own estimated northern range limit based on occurrence data


# clear environment
rm(list = ls())

# Load packages
library(tidyverse)
library(BIEN)
library(terra)
library(maps)

# set working directory
MA_dir<-Sys.getenv("MADIR")

# effect size data
effect <- read.csv(file.path(MA_dir,"L2/otc_effect_sizes_L2.csv"))

# most common species?
names(which.max(table(effect$Genus_Species)))
count <- effect %>% 
  count(Genus_Species)



### range maps ###
# Carex bigelowii (most common species)
Carex_bigelowii <- effect %>%
  filter(Genus_Species == "Carex_bigelowii")
Carex_bigelowii_range <- BIEN_ranges_load_species(species = "Carex bigelowii")
map('world',
    fill = TRUE ,
    col = "grey",
    bg = "light blue")
plot(Carex_bigelowii_range[1],
     col = "green",
     add = TRUE)
points(cbind(Carex_bigelowii$Longitude,Carex_bigelowii$Latitude),
       col = "blue",
       pch = 20,
       cex = 1)

# Betula nana (second most common species)
Betula_nana <- effect %>%
  filter(Genus_Species == "Betula_nana")
Betula_nana_range <- BIEN_ranges_load_species(species = "Betula nana")
map('world',
    fill = TRUE ,
    col = "grey",
    bg = "light blue")
plot(Betula_nana_range[1],
     col = "green",
     add = TRUE)
points(cbind(Betula_nana$Longitude,Betula_nana$Latitude),
       col = "blue",
       pch = 20,
       cex = 1)

# Lonicera hispida (less common species)
Lonicera_hispida <- effect %>%
  filter(Genus_Species == "Lonicera_hispida")
Lonicera_hispida_range <- BIEN_ranges_load_species(species = "Lonicera hispida")
map('world',
    fill = TRUE ,
    col = "grey",
    bg = "light blue")
plot(Lonicera_hispida_range[1],
     col = "green",
     add = TRUE)
points(cbind(Lonicera_hispida$Longitude,Lonicera_hispida$Latitude),
       col = "blue",
       pch = 20,
       cex = 1)



### occurrence data ###
Carex_bigelowii_full <- BIEN_occurrence_species(species = "Carex bigelowii",
                                                    cultivated = TRUE,
                                                    all.taxonomy = TRUE,
                                                    native.status = TRUE,
                                                    observation.type = TRUE,
                                                    political.boundaries = TRUE)
map('world', fill = TRUE, col= "grey", bg = "light blue") 
points(cbind(Carex_bigelowii_full$longitude,
             Carex_bigelowii_full$latitude),
       col = "red",
       pch = 20,
       cex = 1) 
points(cbind(Carex_bigelowii$Longitude,
             Carex_bigelowii$Latitude),
       col = "blue",
       pch = 20,
       cex = 1) 
