# TITLE:          Range maps for species
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske
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
species20 <- species[91:95]
species21 <- species[96:100]
species22 <- species[101:105]
species23 <- species[106:110]
species24 <- species[111:115]
species25 <- species[116:120]
species26 <- species[121:125]
species27 <- species[126:130]
species28 <- species[131:135]
species29 <- species[136:140]
species30 <- species[141:145]
species31 <- species[146:150]
species32 <- species[151:155]
species33 <- species[156:160]
species34 <- species[161:165]
species35 <- species[166:170]
species36 <- species[171:175]
species37 <- species[176:180]
species38 <- species[181:187]


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
df_list37 <- occurrences(spp = species38)



# checking some specific species
Achillea_millefolium <- BIEN_occurrence_species(species = "Achillea millefolium",
                                         cultivated = TRUE,
                                         all.taxonomy = TRUE,
                                         native.status = TRUE,
                                         observation.type = TRUE,
                                         political.boundaries = TRUE)
Arrhenatherum_elatius <- BIEN_occurrence_species(species = "Arrhenatherum elatius",
                                                cultivated = TRUE,
                                                all.taxonomy = TRUE,
                                                native.status = TRUE,
                                                observation.type = TRUE,
                                                political.boundaries = TRUE)
Barbarea_vulgaris <- BIEN_occurrence_species(species = "Barbarea vulgaris",
                                                 cultivated = TRUE,
                                                 all.taxonomy = TRUE,
                                                 native.status = TRUE,
                                                 observation.type = TRUE,
                                                 political.boundaries = TRUE)
Cardamina_hirsuta <- BIEN_occurrence_species(species = "Cardamine hirsuta",
                                                 cultivated = TRUE,
                                                 all.taxonomy = TRUE,
                                                 native.status = TRUE,
                                                 observation.type = TRUE,
                                                 political.boundaries = TRUE)
Carex_pensylvanica <- BIEN_occurrence_species(species = "Carex pensylvanica",
                                                 cultivated = TRUE,
                                                 all.taxonomy = TRUE,
                                                 native.status = TRUE,
                                                 observation.type = TRUE,
                                                 political.boundaries = TRUE)
Centaurea_stoebe <- BIEN_occurrence_species(species = "Centaurea stoebe",
                                              cultivated = TRUE,
                                              all.taxonomy = TRUE,
                                              native.status = TRUE,
                                              observation.type = TRUE,
                                              political.boundaries = TRUE)
Daucus_carota <- BIEN_occurrence_species(species = "Daucus carota",
                                            cultivated = TRUE,
                                            all.taxonomy = TRUE,
                                            native.status = TRUE,
                                            observation.type = TRUE,
                                            political.boundaries = TRUE)
Dactylis_glomerata <- BIEN_occurrence_species(species = "Dactylis glomerata",
                                         cultivated = TRUE,
                                         all.taxonomy = TRUE,
                                         native.status = TRUE,
                                         observation.type = TRUE,
                                         political.boundaries = TRUE)
Danthonia_spicata <- BIEN_occurrence_species(species = "Danthonia spicata",
                                         cultivated = TRUE,
                                         all.taxonomy = TRUE,
                                         native.status = TRUE,
                                         observation.type = TRUE,
                                         political.boundaries = TRUE)
Elymus_repens <- BIEN_occurrence_species(species = "Elymus repens",
                                         cultivated = TRUE,
                                         all.taxonomy = TRUE,
                                         native.status = TRUE,
                                         observation.type = TRUE,
                                         political.boundaries = TRUE)
Euthamia_graminifolia <- BIEN_occurrence_species(species = "Euthamia graminifolia",
                                         cultivated = TRUE,
                                         all.taxonomy = TRUE,
                                         native.status = TRUE,
                                         observation.type = TRUE,
                                         political.boundaries = TRUE)
Fragaria_vesca <- BIEN_occurrence_species(species = "Fragaria vesca",
                                                 cultivated = TRUE,
                                                 all.taxonomy = TRUE,
                                                 native.status = TRUE,
                                                 observation.type = TRUE,
                                                 political.boundaries = TRUE)
Hypericum_perforatum <- BIEN_occurrence_species(species = "Hypericum perforatum",
                                                 cultivated = TRUE,
                                                 all.taxonomy = TRUE,
                                                 native.status = TRUE,
                                                 observation.type = TRUE,
                                                 political.boundaries = TRUE)
Phleum_pratense <- BIEN_occurrence_species(species = "Phleum pratense",
                                                 cultivated = TRUE,
                                                 all.taxonomy = TRUE,
                                                 native.status = TRUE,
                                                 observation.type = TRUE,
                                                 political.boundaries = TRUE)
Poa_compressa <- BIEN_occurrence_species(species = "Poa compressa",
                                           cultivated = TRUE,
                                           all.taxonomy = TRUE,
                                           native.status = TRUE,
                                           observation.type = TRUE,
                                           political.boundaries = TRUE)
Potentilla_recta <- BIEN_occurrence_species(species = "Potentilla recta",
                                                 cultivated = TRUE,
                                                 all.taxonomy = TRUE,
                                                 native.status = TRUE,
                                                 observation.type = TRUE,
                                                 political.boundaries = TRUE)
Pteridium_aquilinum <- BIEN_occurrence_species(species = "Pteridium aquilinum",
                                                 cultivated = TRUE,
                                                 all.taxonomy = TRUE,
                                                 native.status = TRUE,
                                                 observation.type = TRUE,
                                                 political.boundaries = TRUE)
Rumex_acetosella <- BIEN_occurrence_species(species = "Rumex acetosella",
                                                 cultivated = TRUE,
                                                 all.taxonomy = TRUE,
                                                 native.status = TRUE,
                                                 observation.type = TRUE,
                                                 political.boundaries = TRUE)
Solidago_canadensis <- BIEN_occurrence_species(species = "Solidago canadensis",
                                                 cultivated = TRUE,
                                                 all.taxonomy = TRUE,
                                                 native.status = TRUE,
                                                 observation.type = TRUE,
                                                 political.boundaries = TRUE)
Taraxacum_officinale <- BIEN_occurrence_species(species = "Taraxacum officinale",
                                                 cultivated = TRUE,
                                                 all.taxonomy = TRUE,
                                                 native.status = TRUE,
                                                 observation.type = TRUE,
                                                 political.boundaries = TRUE)
Veronica_arvensis <- BIEN_occurrence_species(species = "Veronica arvensis",
                                                 cultivated = TRUE,
                                                 all.taxonomy = TRUE,
                                                 native.status = TRUE,
                                                 observation.type = TRUE,
                                                 political.boundaries = TRUE)


Aster_alpinus <- BIEN_occurrence_species(species = "Aster alpinus",
                                               cultivated = TRUE,
                                               all.taxonomy = TRUE,
                                               native.status = TRUE,
                                               observation.type = TRUE,
                                               political.boundaries = TRUE)
Aster_bellidiastrum <- BIEN_occurrence_species(species = "Bellidiaster montanus",
                                         cultivated = TRUE,
                                         all.taxonomy = TRUE,
                                         native.status = TRUE,
                                         observation.type = TRUE,
                                         political.boundaries = TRUE)
Asterolasia_trymalioides <- BIEN_occurrence_species(species = "Asterolasia trymalioides",
                                               cultivated = TRUE,
                                               all.taxonomy = TRUE,
                                               native.status = TRUE,
                                               observation.type = TRUE,
                                               political.boundaries = TRUE)
Artemisia_pubescens <- BIEN_occurrence_species(species = "Artemisia glacialis",
                                             cultivated = TRUE,
                                             all.taxonomy = TRUE,
                                             native.status = TRUE,
                                             observation.type = TRUE,
                                             political.boundaries = TRUE)
Artemisia_genipi <- BIEN_occurrence_species(species = "Artemisia genipi",
                                               cultivated = TRUE,
                                               all.taxonomy = TRUE,
                                               native.status = TRUE,
                                               observation.type = TRUE,
                                               political.boundaries = TRUE)
Anthoxanthum_nitens <- BIEN_occurrence_species(species = "Anthoxanthum nitens",
                                               cultivated = TRUE,
                                               all.taxonomy = TRUE,
                                               native.status = TRUE,
                                               observation.type = TRUE,
                                               political.boundaries = TRUE)
Aulacomnium_turgidum <- BIEN_occurrence_species(species = "Aulacomnium turgidum",
                                               cultivated = TRUE,
                                               all.taxonomy = TRUE,
                                               native.status = TRUE,
                                               observation.type = TRUE,
                                               political.boundaries = TRUE)
Carex_parvula <- BIEN_occurrence_species(species = "Hemicarex pygmaea",
                                                cultivated = TRUE,
                                                all.taxonomy = TRUE,
                                                native.status = TRUE,
                                                observation.type = TRUE,
                                                political.boundaries = TRUE)
Carex_tibetikobresia <- BIEN_occurrence_species(species = "Kobresia tibetica",
                                         cultivated = TRUE,
                                         all.taxonomy = TRUE,
                                         native.status = TRUE,
                                         observation.type = TRUE,
                                         political.boundaries = TRUE)
Carex_moorcroftii <- BIEN_occurrence_species(species = "Carex moorcroftii",
                                                cultivated = TRUE,
                                                all.taxonomy = TRUE,
                                                native.status = TRUE,
                                                observation.type = TRUE,
                                                political.boundaries = TRUE)
Carex_alatauensis <- BIEN_occurrence_species(species = "Kobresia persica",
                                             cultivated = TRUE,
                                             all.taxonomy = TRUE,
                                             native.status = TRUE,
                                             observation.type = TRUE,
                                             political.boundaries = TRUE)
Craspedia_aurantia <- BIEN_occurrence_species(species = "Craspedia jamesii",
                                             cultivated = TRUE,
                                             all.taxonomy = TRUE,
                                             native.status = TRUE,
                                             observation.type = TRUE,
                                             political.boundaries = TRUE)
Craspedia_gracilis <- BIEN_occurrence_species(species = "Craspedia coolaminica",
                                              cultivated = TRUE,
                                              all.taxonomy = TRUE,
                                              native.status = TRUE,
                                              observation.type = TRUE,
                                              political.boundaries = TRUE)
Dryas_ajanensis <- BIEN_occurrence_species(species = "Dryas octopetala subsp. ajanensis",
                                              cultivated = TRUE,
                                              all.taxonomy = TRUE,
                                              native.status = TRUE,
                                              observation.type = TRUE,
                                              political.boundaries = TRUE)
Eremogone_capillaris <- BIEN_occurrence_species(species = "Eremogone capillaris",
                                             cultivated = TRUE,
                                             all.taxonomy = TRUE,
                                             native.status = TRUE,
                                             observation.type = TRUE,
                                             political.boundaries = TRUE)
Elymus_nutans <- BIEN_occurrence_species(species = "Elymus triglumis",
                                                cultivated = TRUE,
                                                all.taxonomy = TRUE,
                                                native.status = TRUE,
                                                observation.type = TRUE,
                                                political.boundaries = TRUE)
Helianthemum_squamatum <- BIEN_occurrence_species(species = "Cistus squamatus",
                                         cultivated = TRUE,
                                         all.taxonomy = TRUE,
                                         native.status = TRUE,
                                         observation.type = TRUE,
                                         political.boundaries = TRUE)
Harpochloa_falx <- BIEN_occurrence_species(species = "Melica falx",
                                                  cultivated = TRUE,
                                                  all.taxonomy = TRUE,
                                                  native.status = TRUE,
                                                  observation.type = TRUE,
                                                  political.boundaries = TRUE)
Koeleria_capensis <- BIEN_occurrence_species(species = "Lasiochloa villosa",
                                                  cultivated = TRUE,
                                                  all.taxonomy = TRUE,
                                                  native.status = TRUE,
                                                  observation.type = TRUE,
                                                  political.boundaries = TRUE)
Luzula_modesta <- BIEN_occurrence_species(species = "Luzula modesta",
                                                cultivated = TRUE,
                                                all.taxonomy = TRUE,
                                                native.status = TRUE,
                                                observation.type = TRUE,
                                                political.boundaries = TRUE)
Lotus_dorycnium <- BIEN_occurrence_species(species = "Dorycnium pentaphyllum",
                                          cultivated = TRUE,
                                          all.taxonomy = TRUE,
                                          native.status = TRUE,
                                          observation.type = TRUE,
                                          political.boundaries = TRUE)
Potentilla_sericea <- BIEN_occurrence_species(species = "Potentilla sericea",
                                               cultivated = TRUE,
                                               all.taxonomy = TRUE,
                                               native.status = TRUE,
                                               observation.type = TRUE,
                                               political.boundaries = TRUE)
Potentilla_saundersiana <- BIEN_occurrence_species(species = "Potentilla saundersiana",
                                              cultivated = TRUE,
                                              all.taxonomy = TRUE,
                                              native.status = TRUE,
                                              observation.type = TRUE,
                                              political.boundaries = TRUE)
Pimelea_alpina <- BIEN_occurrence_species(species = "Banksia alpina",
                                              cultivated = TRUE,
                                              all.taxonomy = TRUE,
                                              native.status = TRUE,
                                              observation.type = TRUE,
                                              political.boundaries = TRUE)
Pleurozium_schreberi <- BIEN_occurrence_species(species = "Pleruozium schreberi",
                                          cultivated = TRUE,
                                          all.taxonomy = TRUE,
                                          native.status = TRUE,
                                          observation.type = TRUE,
                                          political.boundaries = TRUE)
Poa_alpina <- BIEN_occurrence_species(species = "Poa alpina",
                                              cultivated = TRUE,
                                              all.taxonomy = TRUE,
                                              native.status = TRUE,
                                              observation.type = TRUE,
                                              political.boundaries = TRUE)
Ranunculus_victoriensis <- BIEN_occurrence_species(species = "Ranunculus victoriensis",
                                      cultivated = TRUE,
                                      all.taxonomy = TRUE,
                                      native.status = TRUE,
                                      observation.type = TRUE,
                                      political.boundaries = TRUE)
Sesleria_albicans <- BIEN_occurrence_species(species = "Sesleria albicans",
                                               cultivated = TRUE,
                                               all.taxonomy = TRUE,
                                               native.status = TRUE,
                                               observation.type = TRUE,
                                               political.boundaries = TRUE)
Senecio_pinnatifolius <- BIEN_occurrence_species(species = "Senecio pinnatifolius",
                                             cultivated = TRUE,
                                             all.taxonomy = TRUE,
                                             native.status = TRUE,
                                             observation.type = TRUE,
                                             political.boundaries = TRUE)
Sieversia_pentapetala <- BIEN_occurrence_species(species = "Geum pentapetalum",
                                                 cultivated = TRUE,
                                                 all.taxonomy = TRUE,
                                                 native.status = TRUE,
                                                 observation.type = TRUE,
                                                 political.boundaries = TRUE)
Sibiraea_angustata <- BIEN_occurrence_species(species = "Sibiraea angustata",
                                                 cultivated = TRUE,
                                                 all.taxonomy = TRUE,
                                                 native.status = TRUE,
                                                 observation.type = TRUE,
                                                 political.boundaries = TRUE)
Sulla_coronaria <- BIEN_occurrence_species(species = "Hedysarum coronarium",
                                                 cultivated = TRUE,
                                                 all.taxonomy = TRUE,
                                                 native.status = TRUE,
                                                 observation.type = TRUE,
                                                 political.boundaries = TRUE)
Spiraea_mongolica <- BIEN_occurrence_species(species = "Spiraea gemmata",
                                             cultivated = TRUE,
                                             all.taxonomy = TRUE,
                                             native.status = TRUE,
                                             observation.type = TRUE,
                                             political.boundaries = TRUE)
Sporobolus_pumilus <- BIEN_occurrence_species(species = "Spartina patens",
                                             cultivated = TRUE,
                                             all.taxonomy = TRUE,
                                             native.status = TRUE,
                                             observation.type = TRUE,
                                             political.boundaries = TRUE)
Stellaria_laeta <- BIEN_occurrence_species(species = "Stellaria laeta",
                                              cultivated = TRUE,
                                              all.taxonomy = TRUE,
                                              native.status = TRUE,
                                              observation.type = TRUE,
                                              political.boundaries = TRUE)
Trifolium_pallescens <- BIEN_occurrence_species(species = "Trifolium pallescens",
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


