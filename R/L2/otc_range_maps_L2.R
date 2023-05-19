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
library(rgbif)
library(maptools)
library(latticeExtra)

# set working directory
MA_dir<-Sys.getenv("MADIR")

# effect size data
effect <- read.csv(file.path(MA_dir,"L2/otc_effect_sizes_L2.csv"))

# most common species?
names(which.max(table(effect$Genus_Species)))
count <- effect %>% 
  count(Genus_Species)



### range maps - BIEN ###
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




### occurrence data - BIEN ###
# Carex bigelowii
Carex_bigelowii <- effect %>%
  filter(Genus_Species == "Carex_bigelowii")
Carex_bigelowii_full <- BIEN_occurrence_species(species = "Carex bigelowii",
                                                    cultivated = TRUE,
                                                    all.taxonomy = TRUE,
                                                    native.status = TRUE,
                                                    observation.type = TRUE,
                                                    political.boundaries = TRUE)
# Betula nana
Betula_nana <- effect %>%
  filter(Genus_Species == "Betula_nana")
Betula_nana_full <- BIEN_occurrence_species(species = "Betula nana",
                                                cultivated = TRUE,
                                                all.taxonomy = TRUE,
                                                native.status = TRUE,
                                                observation.type = TRUE,
                                                political.boundaries = TRUE)
# Deschampsia antarctica
Deschampsia_antarctica <- effect %>%
  filter(Genus_Species == "Deschampsia_antarctica")
Deschampsia_antarctica_full <- BIEN_occurrence_species(species = "Deschampsia antarctica",
                                            cultivated = TRUE,
                                            all.taxonomy = TRUE,
                                            native.status = TRUE,
                                            observation.type = TRUE,
                                            political.boundaries = TRUE)

# Lonicera hispida
Lonicera_hispida <- effect %>%
  filter(Genus_Species == "Lonicera_hispida")
Lonicera_hispida_full <- BIEN_occurrence_species(species = "Lonicera hispida",
                                                       cultivated = TRUE,
                                                       all.taxonomy = TRUE,
                                                       native.status = TRUE,
                                                       observation.type = TRUE,
                                                       political.boundaries = TRUE)
Hylocomium_splendens <- effect %>%
  filter(Genus_Species == "Hylocomium_splendens")
Hylocomium_splendens_full <- BIEN_occurrence_species(species = "Hylocomium splendens",
                                                 cultivated = TRUE,
                                                 all.taxonomy = TRUE,
                                                 native.status = TRUE,
                                                 observation.type = TRUE,
                                                 political.boundaries = TRUE)



# setting up map parameters
mp <- map('world', fill = TRUE, plot=F) 
SP <- map2SpatialPolygons(mp, IDs = mp$names, 
                          proj4string = CRS("+proj=longlat +datum=WGS84"))
DATA <- data.frame(seq_len(length(SP)), row.names = names(SP))
SPDF <- SpatialPolygonsDataFrame(SP, data = DATA)

# mapping
# Carex bigelowii
spplot(SPDF, col.regions = "transparent", colorkey = FALSE,
       par.settings = list(axis.line = list(col = "transparent"))) +
  layer(panel.points(longitude, latitude, col="blue", pch=19), data=Carex_bigelowii_full) +
  layer(panel.points(Longitude, Latitude, col="red", pch=19), data=Carex_bigelowii)
# Betula nana
spplot(SPDF, col.regions = "transparent", colorkey = FALSE,
       par.settings = list(axis.line = list(col = "transparent"))) +
  layer(panel.points(longitude, latitude, col="blue", pch=19), data=Betula_nana_full) +
  layer(panel.points(Longitude, Latitude, col="red", pch=19), data=Betula_nana)
# Deschampsia antarctica
spplot(SPDF, col.regions = "transparent", colorkey = FALSE,
       par.settings = list(axis.line = list(col = "transparent"))) +
  layer(panel.points(longitude, latitude, col="blue", pch=19), data=Deschampsia_antarctica_full) +
  layer(panel.points(Longitude, Latitude, col="red", pch=19), data=Deschampsia_antarctica)
# Lonicera hispida
spplot(SPDF, col.regions = "transparent", colorkey = FALSE,
       par.settings = list(axis.line = list(col = "transparent"))) +
  layer(panel.points(longitude, latitude, col="blue", pch=19), data=Lonicera_hispida_full) +
  layer(panel.points(Longitude, Latitude, col="red", pch=19), data=Lonicera_hispida)
# Hylocomium splendens
spplot(SPDF, col.regions = "transparent", colorkey = FALSE,
       par.settings = list(axis.line = list(col = "transparent"))) +
  layer(panel.points(longitude, latitude, col="blue", pch=19), data=Hylocomium_splendens_full) +
  layer(panel.points(Longitude, Latitude, col="red", pch=19), data=Hylocomium_splendens)

# old map
#map('world', fill = TRUE, col= "grey", bg = "light blue") 
#points(cbind(Carex_bigelowii_full$longitude,
#             Carex_bigelowii_full$latitude),
#       col = "red",
#       pch = 20,
#       cex = 1) 
#points(cbind(Carex_bigelowii$Longitude,
#             Carex_bigelowii$Latitude),
#       col = "blue",
#       pch = 20,
#       cex = 1) 




### occurrence data - GBIF ###
taxonKey <- name_backbone("Carex bigelowii")$usageKey
occ_download(pred("taxonKey", 2722383))
x <- occ_download_get('0248073-230224095556074') %>%
  occ_download_import()

plot(map_fetch(taxonKey = 2722383, bin = "hex", srs="EPSG:3857",
               hexPerTile = 30, style = "purpleYellow-noborder.poly"))


