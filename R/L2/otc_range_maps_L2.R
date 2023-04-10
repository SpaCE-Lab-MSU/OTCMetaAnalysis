# TITLE:          Range maps for species
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Pat Bills
# DATA INPUT:     Range maps and species information from MA data
# DATA OUTPUT:    
# DATE:           April 2023


# thoughts
# IUCN spatial data: would be great, but has very few species
# BIEN: can use range maps? how to calculate distance to northern edge?
# GBIF: make my own estimated northern range limit based on occurrence data


# clear environment
rm(list = ls())

# Load packages
library(tidyverse)
library(BIEN)
library(terra)
library(maps)
library(red)

# BIEN
# occurrence data
Xanthium_strumarium_full <- BIEN_occurrence_species(species = "Xanthium strumarium",
                                                   cultivated = TRUE,
                                                   all.taxonomy = TRUE,
                                                   native.status = TRUE,
                                                   observation.type = TRUE,
                                                   political.boundaries = TRUE)
map('world', fill = TRUE, col= "grey", bg = "light blue") 
points(cbind(Xanthium_strumarium_full$longitude,
             Xanthium_strumarium_full$latitude),
       col = "red",
       pch = 20,
       cex = 1) 
points(cbind(Xanthium_strumarium$longitude,
             Xanthium_strumarium$latitude),
       col = "blue",
       pch = 20,
       cex = 1) 

# range map
Xanthium_strumarium_range <- BIEN_ranges_load_species(species = "Xanthium strumarium")
map('world',
    fill = TRUE ,
    col = "grey",
    bg = "light blue")
plot(Xanthium_strumarium_range[1],
     col = "green",
     add = TRUE)
points(cbind(Xanthium_strumarium$longitude,Xanthium_strumarium$latitude),
       col = "blue",
       pch = 20,
       cex = 1)


# trying with my species
Acer_rubrum_range <- BIEN_ranges_load_species(species = "Carex bigelowii")
map('world',
    fill = TRUE ,
    col = "grey",
    bg = "light blue")
plot(Acer_rubrum_range[1],
     col = "green",
     add = TRUE)
