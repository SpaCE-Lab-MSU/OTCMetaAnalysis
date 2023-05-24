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

# list all species + their counts
# this is just to see which species are the most common
names(which.max(table(effect$Genus_Species)))
count <- effect %>% 
  count(Genus_Species)

# list of all species
# first removing species that aren't named or only contain genus
effect2 <- effect %>%
  filter(!(Genus_Species == "_" | Genus_Species == "Sphagnum_" | Genus_Species == "Draba_" |
             Genus_Species == "Carex_" | Genus_Species == "Oxytropis_" | Genus_Species == "Luzula_" |
             Genus_Species == "Stereocaulon_" | Genus_Species == "Cladonia_" | Genus_Species == "Nephroma_" |
             Genus_Species == "Peltigera_"))
# replace underscore with space
effect2$Genus_Species<-gsub("_", " ", effect2$Genus_Species, fixed=TRUE)
# store each species in a list
species <- as.list(unique(effect2$Genus_Species))

# subset species list into chunks to run through the BIEN occurrence function
species2 <- species[1:5]
species3 <- species[6:10]
species4 <- species[11:15]
species5 <- species[16:20]
species6 <- species[21:25]
species7 <- species[26:30]
species8 <- species[31:35]
species9 <- species[36:40]
species10 <- species[41:45]
species11 <- species[46:50]
species12 <- species[51:55]
species13 <- species[56:60]
species14 <- species[61:65]
species15 <- species[66:70]
species16 <- species[71:75]
species17 <- species[76:80]
species18 <- species[81:85]
species19 <- species[86:90]


### occurrence data - BIEN ###
# getting BIEN occurrence data for all species
# making a function to loop through all species in my data frame and get their occurrences from BIEN
occurrences <- function(spp) {
  
  list_of_occ <- list()
  
  for(i in 1:length(spp)){
    df <- BIEN_occurrence_species(species = spp[[i]],
                                    cultivated = TRUE,
                                    all.taxonomy = TRUE,
                                    native.status = TRUE,
                                    observation.type = TRUE,
                                    political.boundaries = TRUE)
    list_of_occ[[i]] <- df
  }
  return(list_of_occ)
}
# store the results of the function
# change "species#" and "df_list#" to match the subset of species I want to work with
df_list18 <- occurrences(spp = species19)



# checking some specific species
Picea_glauca <- BIEN_occurrence_species(species = "Picea glauca",
                                                cultivated = TRUE,
                                                all.taxonomy = TRUE,
                                                native.status = TRUE,
                                                observation.type = TRUE,
                                                political.boundaries = TRUE)
Picea_laxa <- BIEN_occurrence_species(species = "Picea laxa",
                                        cultivated = TRUE,
                                        all.taxonomy = TRUE,
                                        native.status = TRUE,
                                        observation.type = TRUE,
                                        political.boundaries = TRUE)
Kobresia_tibetica <- BIEN_occurrence_species(species = "Kobresia tibetica",
                                            cultivated = TRUE,
                                            all.taxonomy = TRUE,
                                            native.status = TRUE,
                                            observation.type = TRUE,
                                            political.boundaries = TRUE)
Carex_bigelowii <- BIEN_occurrence_species(species = "Carex bigelowii",
                                                    cultivated = TRUE,
                                                    all.taxonomy = TRUE,
                                                    native.status = TRUE,
                                                    observation.type = TRUE,
                                                    political.boundaries = TRUE)
Deschampsia_antarctica <- BIEN_occurrence_species(species = "Deschampsia antarctica",
                                            cultivated = TRUE,
                                            all.taxonomy = TRUE,
                                            native.status = TRUE,
                                            observation.type = TRUE,
                                            political.boundaries = TRUE)
Lonicera_hispida <- BIEN_occurrence_species(species = "Lonicera hispida",
                                                       cultivated = TRUE,
                                                       all.taxonomy = TRUE,
                                                       native.status = TRUE,
                                                       observation.type = TRUE,
                                                       political.boundaries = TRUE)
Hylocomium_splendens <- BIEN_occurrence_species(species = "Hylocomium splendens",
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

# mapping some species
# Carex bigelowii
Carex_bigelowii <- effect %>%
  filter(Genus_Species == "Carex_bigelowii")
spplot(SPDF, col.regions = "transparent", colorkey = FALSE,
       par.settings = list(axis.line = list(col = "transparent"))) +
  layer(panel.points(longitude, latitude, col="blue", pch=19), data=Carex_bigelowii_full) +
  layer(panel.points(Longitude, Latitude, col="red", pch=19), data=Carex_bigelowii)
# Betula nana
Betula_nana <- effect %>%
  filter(Genus_Species == "Betula_nana")
spplot(SPDF, col.regions = "transparent", colorkey = FALSE,
       par.settings = list(axis.line = list(col = "transparent"))) +
  layer(panel.points(longitude, latitude, col="blue", pch=19), data=Betula_nana_full) +
  layer(panel.points(Longitude, Latitude, col="red", pch=19), data=Betula_nana)
# Deschampsia antarctica
Deschampsia_antarctica <- effect %>%
  filter(Genus_Species == "Deschampsia_antarctica")
spplot(SPDF, col.regions = "transparent", colorkey = FALSE,
       par.settings = list(axis.line = list(col = "transparent"))) +
  layer(panel.points(longitude, latitude, col="blue", pch=19), data=Deschampsia_antarctica_full) +
  layer(panel.points(Longitude, Latitude, col="red", pch=19), data=Deschampsia_antarctica)
# Lonicera hispida
Lonicera_hispida <- effect %>%
  filter(Genus_Species == "Lonicera_hispida")
spplot(SPDF, col.regions = "transparent", colorkey = FALSE,
       par.settings = list(axis.line = list(col = "transparent"))) +
  layer(panel.points(longitude, latitude, col="blue", pch=19), data=Lonicera_hispida_full) +
  layer(panel.points(Longitude, Latitude, col="red", pch=19), data=Lonicera_hispida)
# Hylocomium splendens
Hylocomium_splendens <- effect %>%
  filter(Genus_Species == "Hylocomium_splendens")
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


