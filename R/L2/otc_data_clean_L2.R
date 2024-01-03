# TITLE:          L1 data clean-up
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske
# DATA INPUT:     L1 sample size data sheet from google drive
# DATA OUTPUT:    cleaned L2 version of the sample size dataframe
# DATE:           Jan 2023


# Clear all existing data
rm(list=ls())

# Load packages
library(tidyverse)
library(metafor)
library(elevatr)
library(raster)
library(sp)
library(geodata)
library(terra)

# set working directory
MA_dir<-Sys.getenv("MADIR")
list.files(MA_dir)

# read in data
sample <- read.csv(file.path(MA_dir,"L1/otc_data_sample_sizes (1).csv"))


### remove data with "remove" in a row
# these rows are being removed because they either don't have sample sizes, or don't have SE/SD
sample_rem <- sample %>%
  filter(!(To_Remove == "remove"))


### fixing error types
# removing data that doesn't specify variation type (SD, SE, etc.)
unique(sample_rem$Value_error)
sample_rem <- sample_rem %>%
  filter(!Value_error == "")

# changing 2SE to 1SE
sample_2SE <- sample_rem %>%
  filter(Value_error == "mean_2SE") %>%
  mutate(Warmed_Var = Warmed_Var/2) %>%
  mutate(Ambient_Var = Ambient_Var/2)
sample_2SE$Value_error[sample_2SE$Value_error == "mean_2SE"] <- "mean_SE"

# removing the 2SE pub from original data, then merging the fixed version back in
sample_rem <- sample_rem %>%
  filter(!(Pub_number == 170))
sample_rem <- rbind(sample_rem, sample_2SE)

# converting SE and 95CI to SD
# http://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm
# SD = SE * sqrt(N)
unique(sample_rem$Value_error)
sample_rem$Warmed_SD1<-ifelse(sample_rem$Value_error=="mean_SE",
                                 sample_rem$Warmed_Var*sqrt(sample_rem$Warmed_N),
                                 sample_rem$Warmed_Var)
sample_rem$Ambient_SD1<-ifelse(sample_rem$Value_error=="mean_SE",
                                  sample_rem$Ambient_Var*sqrt(sample_rem$Ambient_N),
                                  sample_rem$Ambient_Var)
# if a sample is 95%CI, setting the "SD1" column as NA
sample_rem$Warmed_SD1[sample_rem$Value_error == 'mean_95'] = NA
sample_rem$Ambient_SD1[sample_rem$Value_error == 'mean_95'] = NA

# SD = sqrt(N) * 95CI / 3.92
# making new columns for 95% -> SD
sample_rem$Warmed_SD2<-ifelse(sample_rem$Value_error=="mean_95",
                             sqrt(sample_rem$Warmed_N)*(sample_rem$Warmed_Var/3.92),
                             sample_rem$Warmed_Var)
sample_rem$Ambient_SD2<-ifelse(sample_rem$Value_error=="mean_95",
                               sqrt(sample_rem$Ambient_N)*(sample_rem$Ambient_Var/3.92),
                              sample_rem$Ambient_Var)
# if a sample is SD or SE, setting the "SD2" column as NA
sample_rem$Warmed_SD2[sample_rem$Value_error == 'mean_SD'] = NA
sample_rem$Ambient_SD2[sample_rem$Value_error == 'mean_SD'] = NA
sample_rem$Warmed_SD2[sample_rem$Value_error == 'mean_SE'] = NA
sample_rem$Ambient_SD2[sample_rem$Value_error == 'mean_SE'] = NA

# merging the two SD columns into one for each treatment
sample_coal <- sample_rem %>%
  mutate(Warmed_SD = coalesce(Warmed_SD1,Warmed_SD2)) %>%
  mutate(Ambient_SD = coalesce(Ambient_SD1,Ambient_SD2))

# making every "value_error" be SD
sample_coal$Value_error <- "mean_SD"

# removing old SD columns and uneeded columns
sample_coal$Warmed_SD1 <- NULL
sample_coal$Warmed_SD2 <- NULL
sample_coal$Ambient_SD1 <- NULL
sample_coal$Ambient_SD2 <- NULL
sample_coal$To_Remove <- NULL
sample_coal$Comments <- NULL
sample_coal$Notes <- NULL
sample_coal$Warmed_Var <- NULL
sample_coal$Ambient_Var <- NULL


### fixing spelling + groupings
# checking spelling on native/non-native
unique(sample_coal$Native_Status)
sample_coal$Native_Status[sample_coal$Native_Status == 'Non_native'] = 'Non_Native'

# checking unique species functional groups
unique(sample_coal$Func_group)

# making a new column for functional groups - merging some and removing others
sample_coal$Func_group_broad <- sample_coal$Func_group
sample_coal$Func_group_broad[sample_coal$Func_group == 'Moss'] = 'Bryophyte'
sample_coal$Func_group_broad[sample_coal$Func_group == 'Legume_Forb'] = 'Forb'
sample_coal$Func_group_broad[sample_coal$Func_group == 'Graminoid_Forb'] = ''
sample_coal$Func_group_broad[sample_coal$Func_group == 'Spikemoss'] = ''
sample_coal$Func_group_broad[sample_coal$Func_group == 'Vascular'] = ''
sample_coal$Func_group_broad[sample_coal$Func_group == 'Moss_Lichen'] = ''
sample_coal$Func_group_broad[sample_coal$Func_group == 'Decid_Shrub'] = 'Shrub'
sample_coal$Func_group_broad[sample_coal$Func_group == 'Ever_Shrub'] = 'Shrub'
sample_coal$Func_group_broad[sample_coal$Func_group == 'Ever_Tree'] = 'Tree'
sample_coal$Func_group_broad[sample_coal$Func_group == 'Decid_Tree'] = 'Tree'
unique(sample_coal$Func_group_broad)

# checking that variable types are all spelled the same
unique(sample_coal$Var_type)
sample_coal$Var_type[sample_coal$Var_type == 'Perc_cover'] = 'Percent_cover'

# renaming Phen_start_male and Phen_start_female to Phen_flwr since their definitions are the same
sample_coal$Var_type[sample_coal$Var_type == 'Phen_start_male'] = 'Phen_flwr'
sample_coal$Var_type[sample_coal$Var_type == 'Phen_start_female'] = 'Phen_flwr'
unique(sample_coal$Var_type)

# making column for broader variable types
# mainly delineating early vs. late season phenological events here - following Stuble et al. paper 
# from the Kuebbing lab
# also merging together similar var types (based on convo in committee meeting)
sample_coal$Var_type_broad <- sample_coal$Var_type
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_leaf_appear'] = 'Phen_early'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_leaf_expand'] = 'Phen_early'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_senes'] = 'Phen_late'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_seed_set'] = 'Phen_late'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_bud_break'] = 'Phen_early'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_flwr'] = 'Phen_early'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_stem_elong'] = 'Phen_early'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_abscission'] = 'Phen_late'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_emergence'] = 'Phen_early'

sample_coal$Var_type_broad[sample_coal$Var_type == 'Height'] = 'Growth'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Shoot_length'] = 'Growth'

sample_coal$Var_type_broad[sample_coal$Var_type == 'SLA'] = 'Leaf_growth'
sample_coal$Var_type_broad[sample_coal$Var_type == 'LMA'] = 'Leaf_growth'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Leaf_area'] = 'Leaf_growth'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Leaf_length'] = 'Leaf_growth'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Leaf_width'] = 'Leaf_growth'
unique(sample_coal$Var_type_broad)

# check that all biomass data has tissue type entered & that its the same
check_biomass <- sample_coal %>%
  filter(Var_type == "Biomass_above" | Var_type == "Biomass_below" | Var_type == "Biomass_total")
unique(check_biomass$Tissue_Type)

# check that all nitrogen data has tissue type entered & that its the same
check_n <- sample_coal %>%
  filter(Var_type == "Nitrogen_above" | Var_type == "Nitrogen_below")
unique(check_n$Tissue_Type)

# fixing tissue type names in the full data
sample_coal$Tissue_Type[sample_coal$Tissue_Type == 'Leaves'] = 'Leaf'
sample_coal$Tissue_Type[sample_coal$Tissue_Type == 'Shoots'] = 'Shoot'
sample_coal$Tissue_Type[sample_coal$Tissue_Type == 'Coarse_Root'] = 'Coarse_root'
sample_coal$Tissue_Type[sample_coal$Tissue_Type == 'Fine_Root'] = 'Fine_root'
sample_coal$Tissue_Type[sample_coal$Tissue_Type == 'Roots'] = 'Root'
unique(sample_coal$Tissue_Type)

# making broader tissue type category
sample_coal$Tissue_Type_broad <- sample_coal$Tissue_Type
sample_coal$Tissue_Type_broad[sample_coal$Tissue_Type == 'Old_Roots'] = 'Root'
sample_coal$Tissue_Type_broad[sample_coal$Tissue_Type == 'New_Roots'] = 'Root'
sample_coal$Tissue_Type_broad[sample_coal$Tissue_Type == 'Rhizomes'] = 'Root'
sample_coal$Tissue_Type_broad[sample_coal$Tissue_Type == 'Reproductive_F'] = 'Reproductive'
sample_coal$Tissue_Type_broad[sample_coal$Tissue_Type == 'Reproductive_M'] = 'Reproductive'
sample_coal$Tissue_Type_broad[sample_coal$Tissue_Type == 'Vegetative_F'] = 'Vegetative'
sample_coal$Tissue_Type_broad[sample_coal$Tissue_Type == 'Vegetative_M'] = 'Vegetative'
sample_coal$Tissue_Type_broad[sample_coal$Tissue_Type == 'Coarse_root'] = 'Root'
sample_coal$Tissue_Type_broad[sample_coal$Tissue_Type == 'Fine_root'] = 'Root'
sample_coal$Tissue_Type_broad[sample_coal$Tissue_Type == 'Shoot_M'] = 'Shoot'
sample_coal$Tissue_Type_broad[sample_coal$Tissue_Type == 'Shoot_F'] = 'Shoot'
sample_coal$Tissue_Type_broad[sample_coal$Tissue_Type == 'Old_Total'] = 'Total'
sample_coal$Tissue_Type_broad[sample_coal$Tissue_Type == 'New_Total'] = 'Total'
unique(sample_coal$Tissue_Type_broad)

# checking genus names
unique(sort(sample_coal$Genus))
sample_coal$Genus[sample_coal$Genus == 'Agrostis '] = 'Agrostis'
sample_coal$Genus[sample_coal$Genus == 'Androsace '] = 'Androsace'
sample_coal$Genus[sample_coal$Genus == 'Festuca '] = 'Festuca'
sample_coal$Genus[sample_coal$Genus == 'Helianthemum '] = 'Helianthemum'
sample_coal$Genus[sample_coal$Genus == 'Pleruocarpous'] = 'Pleurocarpous'
sample_coal$Genus[sample_coal$Genus == 'Ranunculus '] = 'Ranunculus'

# adding family name
sample_coal$Family <- NA
sample_coal$Family[sample_coal$Genus == 'Abies'] = 'Pinaceae'
sample_coal$Family[sample_coal$Genus == 'Acer'] = 'Sapindaceae'
sample_coal$Family[sample_coal$Genus == 'Achillea'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Agropyron'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Agrostis'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Anaphalis'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Andreae'] = 'Andreaeaceae'
sample_coal$Family[sample_coal$Genus == 'Andromeda'] = 'Ericaceae'
sample_coal$Family[sample_coal$Genus == 'Androsace'] = 'Primulaceae'
sample_coal$Family[sample_coal$Genus == 'Anemonoides'] = 'Ranunculaceae'
sample_coal$Family[sample_coal$Genus == 'Antennaria'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Anthoxanthum'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Anthyllis'] = 'Fabaceae'
sample_coal$Family[sample_coal$Genus == 'Arctagrostis'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Arctous'] = 'Ericaceae'
sample_coal$Family[sample_coal$Genus == 'Arenaria'] = 'Caryophyllaceae'
sample_coal$Family[sample_coal$Genus == 'Arrhenatherum'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Artemisia'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Asclepias'] = 'Apocynaceae'
sample_coal$Family[sample_coal$Genus == 'Asphodelus'] = 'Asphodelaceae'
sample_coal$Family[sample_coal$Genus == 'Aster'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Asterolasia'] = 'Rutaceae'
sample_coal$Family[sample_coal$Genus == 'Aulacomnium'] = 'Aulacomniaceae'
sample_coal$Family[sample_coal$Genus == 'Avenella'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Avicennia'] = 'Acanthaceae'
sample_coal$Family[sample_coal$Genus == 'Barbarea'] = 'Brassicaceae'
sample_coal$Family[sample_coal$Genus == 'Bartramia'] = 'Bartramiaceae'
sample_coal$Family[sample_coal$Genus == 'Betula'] = 'Betulaceae'
sample_coal$Family[sample_coal$Genus == 'Bistorta'] = 'Polygonaceae'
sample_coal$Family[sample_coal$Genus == 'Bituminaria'] = 'Fabaceae'
sample_coal$Family[sample_coal$Genus == 'Brachyscome'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Cardamine'] = 'Brassicaceae'
sample_coal$Family[sample_coal$Genus == 'Carduus'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Carex'] = 'Cyperaceae'
sample_coal$Family[sample_coal$Genus == 'Cassiope'] = 'Ericaceae'
sample_coal$Family[sample_coal$Genus == 'Celmisia'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Centaurea'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Cerastium'] = 'Caryophyllaceae'
sample_coal$Family[sample_coal$Genus == 'Cetraria'] = 'Parmeliaceae'
sample_coal$Family[sample_coal$Genus == 'Chorisodontium'] = 'Dicranaceae'
sample_coal$Family[sample_coal$Genus == 'Cladonia'] = 'Cladoniaceae'
sample_coal$Family[sample_coal$Genus == 'Colobanthus'] = 'Caryophyllaceae'
sample_coal$Family[sample_coal$Genus == 'Comarum'] = 'Rosaceae'
sample_coal$Family[sample_coal$Genus == 'Coris'] = 'Primulaceae'
sample_coal$Family[sample_coal$Genus == 'Craspedia'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Cynodon'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Dactylis'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Danthonia'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Daucus'] = 'Apiaceae'
sample_coal$Family[sample_coal$Genus == 'Daphne'] = 'Thymelaeaceae'
sample_coal$Family[sample_coal$Genus == 'Dasiphora'] = 'Rosaceae'
sample_coal$Family[sample_coal$Genus == 'Deschampsia'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Diapensia'] = 'Diapensiaceae'
sample_coal$Family[sample_coal$Genus == 'Dicranum'] = 'Dicranaceae'
sample_coal$Family[sample_coal$Genus == 'Distichlis'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Draba'] = 'Brassicaceae'
sample_coal$Family[sample_coal$Genus == 'Dryas'] = 'Rosaceae'
sample_coal$Family[sample_coal$Genus == 'Dupontia'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Echium'] = 'Boraginaceae'
sample_coal$Family[sample_coal$Genus == 'Elymus'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Empetrum'] = 'Ericaceae'
sample_coal$Family[sample_coal$Genus == 'Eremogone'] = 'Caryophyllaceae'
sample_coal$Family[sample_coal$Genus == 'Erigeron'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Eriophorum'] = 'Cyperaceae'
sample_coal$Family[sample_coal$Genus == 'Euthamia'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Festuca'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Flavocetraria'] = 'Parmeliaceae'
sample_coal$Family[sample_coal$Genus == 'Fragaria'] = 'Rosaceae'
sample_coal$Family[sample_coal$Genus == 'Furcraea'] = 'Asparagaceae'
sample_coal$Family[sample_coal$Genus == 'Geum'] = 'Rosaceae'
sample_coal$Family[sample_coal$Genus == 'Gentiana'] = 'Gentianaceae'
sample_coal$Family[sample_coal$Genus == 'Gypsophila'] = 'Caryophyllaceae'
sample_coal$Family[sample_coal$Genus == 'Harpochloa'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Helianthemum'] = 'Cistaceae'
sample_coal$Family[sample_coal$Genus == 'Helictochloa'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Hennediella'] = 'Pottiaceae'
sample_coal$Family[sample_coal$Genus == 'Heteropogon'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Hieracium'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Hordeum'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Hylocomium'] = 'Hylocomiaceae'
sample_coal$Family[sample_coal$Genus == 'Hypericum'] = 'Hypericaceae'
sample_coal$Family[sample_coal$Genus == 'Kalmia'] = 'Ericaceae'
sample_coal$Family[sample_coal$Genus == 'Koeleria'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Lagotis'] = 'Plantaginaceae'
sample_coal$Family[sample_coal$Genus == 'Larix'] = 'Pinaceae'
sample_coal$Family[sample_coal$Genus == 'Leptorhynchos'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Lonicera'] = 'Caprifoliaceae'
sample_coal$Family[sample_coal$Genus == 'Lotus'] = 'Nelumbonaceae'
sample_coal$Family[sample_coal$Genus == 'Lupinus'] = 'Fabaceae'
sample_coal$Family[sample_coal$Genus == 'Luzula'] = 'Juncaceae'
sample_coal$Family[sample_coal$Genus == 'Lycopodium'] = 'Lycopodiaceae'
sample_coal$Family[sample_coal$Genus == 'Medicago'] = 'Fabaceae'
sample_coal$Family[sample_coal$Genus == 'Nephroma'] = 'Nephromataceae'
sample_coal$Family[sample_coal$Genus == 'Ochrolechia'] = 'Ochrolechiaceae'
sample_coal$Family[sample_coal$Genus == 'Oxyria'] = 'Polygonaceae'
sample_coal$Family[sample_coal$Genus == 'Oxytropis'] = 'Fabaceae'
sample_coal$Family[sample_coal$Genus == 'Papaver'] = 'Papaveraceae'
sample_coal$Family[sample_coal$Genus == 'Parnassia'] = 'Celastraceae'
sample_coal$Family[sample_coal$Genus == 'Peltigera'] = 'Peltigeraceae'
sample_coal$Family[sample_coal$Genus == 'Phleum'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Picea'] = 'Pinaceae'
sample_coal$Family[sample_coal$Genus == 'Pimelea'] = 'Thymelaeaceae'
sample_coal$Family[sample_coal$Genus == 'Plantago'] = 'Plantaginaceae'
sample_coal$Family[sample_coal$Genus == 'Pleurozium'] = 'Hylocomiaceae'
sample_coal$Family[sample_coal$Genus == 'Poa'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Polytrichastrum'] = 'Polytrichaceae'
sample_coal$Family[sample_coal$Genus == 'Polytrichum'] = 'Polytrichaceae'
sample_coal$Family[sample_coal$Genus == 'Potentilla'] = 'Rosaceae'
sample_coal$Family[sample_coal$Genus == 'Primula'] = 'Primulaceae'
sample_coal$Family[sample_coal$Genus == 'Pseudoroegneria'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Pteridium'] = 'Dennstaedtiaceae'
sample_coal$Family[sample_coal$Genus == 'Ranunculus'] = 'Ranunculaceae'
sample_coal$Family[sample_coal$Genus == 'Rhododendron'] = 'Ericaceae'
sample_coal$Family[sample_coal$Genus == 'Rubus'] = 'Rosaceae'
sample_coal$Family[sample_coal$Genus == 'Rumex'] = 'Polygonaceae'
sample_coal$Family[sample_coal$Genus == 'Rytidosperma'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Salix'] = 'Salicaceae'
sample_coal$Family[sample_coal$Genus == 'Sanguisorba'] = 'Rosaceae'
sample_coal$Family[sample_coal$Genus == 'Sanionia'] = 'Amblystegiaceae'
sample_coal$Family[sample_coal$Genus == 'Santolina'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Saussurea'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Saxifraga'] = 'Saxifragaceae'
sample_coal$Family[sample_coal$Genus == 'Schoenoplectus'] = 'Cyperaceae'
sample_coal$Family[sample_coal$Genus == 'Scorzoneroides'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Selaginella'] = 'Selaginellaceae'
sample_coal$Family[sample_coal$Genus == 'Senecio'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Sesleria'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Sieversia'] = 'Rosaceae'
sample_coal$Family[sample_coal$Genus == 'Silene'] = 'Caryophyllaceae'
sample_coal$Family[sample_coal$Genus == 'Solidago'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Sparganium'] = 'Typhaceae'
sample_coal$Family[sample_coal$Genus == 'Sphaerophorus'] = 'Sphaerophoraceae'
sample_coal$Family[sample_coal$Genus == 'Sphagnum'] = 'Sphagnaceae'
sample_coal$Family[sample_coal$Genus == 'Spiraea'] = 'Rosaceae'
sample_coal$Family[sample_coal$Genus == 'Sporobolus'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Stellaria'] = 'Caryophyllaceae'
sample_coal$Family[sample_coal$Genus == 'Stellera'] = 'Thymelaeaceae'
sample_coal$Family[sample_coal$Genus == 'Stereocaulon'] = 'Stereocaulaceae'
sample_coal$Family[sample_coal$Genus == 'Stiburus'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Stipa'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Sulla'] = 'Fabaceae'
sample_coal$Family[sample_coal$Genus == 'Taraxacum'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Thalictrum'] = 'Ranunculaceae'
sample_coal$Family[sample_coal$Genus == 'Teucrium'] = 'Lamiaceae'
sample_coal$Family[sample_coal$Genus == 'Thamnolia'] = 'Icmadophilaceae'
sample_coal$Family[sample_coal$Genus == 'Themeda'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Tofieldia'] = 'Tofieldiaceae'
sample_coal$Family[sample_coal$Genus == 'Trichostema'] = 'Lamiaceae'
sample_coal$Family[sample_coal$Genus == 'Trifolium'] = 'Fabaceae'
sample_coal$Family[sample_coal$Genus == 'Triglochin'] = 'Juncaginaceae'
sample_coal$Family[sample_coal$Genus == 'Tristachya'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Usnea'] = 'Parmeliaceae'
sample_coal$Family[sample_coal$Genus == 'Vaccinium'] = 'Ericaceae'
sample_coal$Family[sample_coal$Genus == 'Veronica'] = 'Plantaginaceae'
sample_coal$Family[sample_coal$Genus == 'Zizania'] = 'Poaceae'
# most common families?
sample_coal %>% 
  count(Family)

# make combo genus species column
sample_coal$Genus_Species <- paste0(sample_coal$Genus,"_",sample_coal$Species)


### make column for latitude difference between max latitude of species + study latitude
sample_coal <- sample_coal %>%
  mutate(Lat_difference = Max_lat_of_species-Latitude)
sample_coal$Lat_difference[sample_coal$Lat_difference < 0] <- NA


### reorganize order of column names 
sample_reorder <- sample_coal[, c("User","Pub_number","Pub_info","Study_year_start","File_name",
                                    "Var_type","Var_type_broad","Value_error","Yunits","Func_group","Func_group_broad","Family",
                                    "Genus","Species","Genus_Species","Native_Status","Amount_warmed_C","Amount_warmed_type","Years_warmed",
                                    "Year_round_warm","Latitude","Longitude","Max_lat_of_species","Lat_difference","Site","Tissue_Type","Tissue_Type_broad",
                                    "Lichen_Moss_Type","Warmed_Mean","Warmed_SD","Warmed_N","Ambient_Mean","Ambient_SD","Ambient_N")]


### extracting elevation for coordinates
# selecting out each unique pairing in the lat & long columns
sample_cols <- sample_reorder[,c("Latitude","Longitude")]
sample_cols2 <- unique(sample_cols[,c("Latitude","Longitude")])
sample_cols3 <- sample_cols2 %>%
  relocate(Longitude) %>%
  rename(x = Longitude) %>%
  rename(y = Latitude)
sample_cols3 <- as.data.frame(sample_cols3)
# get elevation
df_elev_aws <- get_elev_point(sample_cols3, prj = 4326, src = "aws")
df_elev_aws$geometry <- as.character(df_elev_aws$geometry)
# manipulate elevation dataframe to re-parse coordinates into separate columns
df_elev_aws$geometry <- str_remove_all(df_elev_aws$geometry, 'c\\(')
df_elev_aws$geometry <- str_remove_all(df_elev_aws$geometry, '\\)')
df_elev_aws[c("Longitude","Latitude")] <- str_split_fixed(df_elev_aws$geometry, ", ", 2)
df_elev_aws$Longitude <- as.numeric(df_elev_aws$Longitude)
df_elev_aws$Latitude <- as.numeric(df_elev_aws$Latitude)
# re-merge elevation with data
sample_elev <- left_join(sample_reorder, df_elev_aws, by=c("Longitude","Latitude"))
# dropping uneeded columns and renaming
sample_elev = subset(sample_elev, select = -c(geometry,elev_units))
colnames(sample_elev)[colnames(sample_elev) == "elevation"] ="Elevation_m"


### getting annual mean temp and precip for each coordinate
# https://stackoverflow.com/questions/76623171/how-to-extract-data-for-coordinates-from-spatraster-class-in-r
# temperature
worldclim_temp <- worldclim_global("tavg",
                          res = 10,
                          version="2.1",
                          path="WorldClimData")
coord_vec <- vect(sample_cols3,
                geom=c("x", "y"),
                crs = "EPSG:4326")
temp_data <- extract(worldclim_temp,
                   coord_vec)
# precipitation
worldclim_precip <- worldclim_global("prec",
                          res = 10,
                          version="2.1",
                          path="WorldClimData")
precip_data <- extract(worldclim_precip,
                     coord_vec)
# getting yearly averages
temp_data2 <- transform(temp_data, Mean_annual_temp = rowMeans(temp_data[,-1], na.rm = TRUE))
precip_data2 <- transform(precip_data, Mean_annual_precip = rowMeans(precip_data[,-1], na.rm = TRUE))
# merging worldclim dataframe
worldclim <- merge(temp_data2, precip_data2, by="ID")
# selecting needed columns
worldclim <- worldclim %>%
  dplyr::select(ID,Mean_annual_temp,Mean_annual_precip)
# merge with lat/long data
worldclim_coord <- bind_cols(sample_cols2, worldclim)
# re-merge with data
worldclim_coord <- worldclim_coord %>%
  dplyr::select(-ID)
sample_climate_data <- left_join(sample_elev, worldclim_coord, by=c("Longitude","Latitude"))
# old method using raster package
# 1970-2000
# https://gis.stackexchange.com/questions/227585/using-r-to-extract-data-from-worldclim
#r <- getData("worldclim",var="bio",res=10)
#r <- r[[c(1,12)]]
#names(r) <- c("Temp","Prec")
#points <- SpatialPoints(sample_cols3, proj4string = r@crs)
#values <- extract(r,points)
#df <- cbind.data.frame(coordinates(points),values)
#plot(r[[1]])
#plot(points,add=T,pch=3,
#     xlab="Longitude",
#     ylab="Latitude")
## fixing scale - WorldClim data has a scaling of 10
#df <- df %>%
#  mutate(Mean_annual_temp = Temp*0.1) %>%
#  mutate(Mean_annual_precip = Prec*0.1)
# re-merge with data
#df <- df %>%
#  rename(Longitude = x) %>%
#  rename(Latitude = y)
#df = subset(df, select = -c(Temp,Prec))
#sample_climate_data <- left_join(sample_elev, df, by=c("Longitude","Latitude"))
## saving just the climate data to google drive 
#write.csv(df, file.path(MA_dir,"L2/worldclim_data_L2.csv"), row.names=F)


### calculating absolute latitude
sample_climate_data <- sample_climate_data %>%
  mutate(Abs_Latitude = abs(Latitude))


### keep max year per study (to remove pseudoreplication)
sample_latest_year <- sample_climate_data %>%
  group_by(Pub_number, File_name, Study_year_start, Var_type, Func_group, Genus, Species, Amount_warmed_C, Latitude, Longitude, Site, Tissue_Type, Lichen_Moss_Type) %>%
  filter(Years_warmed == max(Years_warmed))


### upload csv file to L2 folder
write.csv(sample_latest_year, file.path(MA_dir,"L2/otc_data_cleaned_L2.csv"), row.names=F)



#### calculating effect sizes ###
# https://rfunctions.blogspot.com/2016/10/meta-analysis-in-r.html
esmd <- escalc(measure="SMD", m1i=Warmed_Mean, m2i=Ambient_Mean, # SMD = Hedge's g
               sd1i=Warmed_SD, sd2i=Ambient_SD,
               n1i=Warmed_N, n2i=Ambient_N,
               data=sample_latest_year)
esmd2 <- escalc(measure="SMD", m1i=Warmed_Mean, m2i=Ambient_Mean, # SMD = Hedge's g
               sd1i=Warmed_SD, sd2i=Ambient_SD,
               n1i=Warmed_N, n2i=Ambient_N,
               data=sample_climate_data)

# remove rows with incomplete data
esmd_clean <- esmd %>%
  filter(!is.na(vi))
esmd_clean2 <- esmd2 %>%
  filter(!is.na(vi))

# uploading the effect size data
write.csv(esmd_clean, file.path(MA_dir,"L2/otc_effect_sizes_L2.csv"), row.names=F) # main data
write.csv(esmd_clean2, file.path(MA_dir,"L2/otc_data_cleaned_allyears_L2.csv"), row.names=F) # data with all years

