# TITLE:          L1 data clean-up
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Pat Bills
# DATA INPUT:     L1 sample size data sheet from google drive
# DATA OUTPUT:    cleaned L2 version of the sample size dataframe
# DATE:           Jan 2023


# Clear all existing data
rm(list=ls())

# Load packages
library(tidyverse)

# set working directory
MA_dir<-Sys.getenv("MADIR")
list.files(MA_dir)

# read in data
sample <- read.csv(file.path(MA_dir,"L1/otc_data_sample_sizes (1).csv"))


# remove data with "remove" in a row
# these rows are being removed because they either don't have sample sizes, or don't have SE/SD
sample_rem <- sample %>%
  filter(!(To_Remove == "remove"))

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
sample_rem$Ambient_SD2<-ifelse(sample_rem$Value_error=="mean_SE",
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

# making column for broader variable types
# mainly delineating early vs. late season phenological events here - following Stuble et al. paper 
# from the Kuebbing lab
sample_coal$Var_type_broad <- sample_coal$Var_type
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_leaf_appear'] = 'Phen_early'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_senes'] = 'Phen_late'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_start_male'] = 'Phen_early'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_start_female'] = 'Phen_early'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_seed_set'] = 'Phen_late'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_bud_break'] = 'Phen_early'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_flwr'] = 'Phen_early'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_stem_elong'] = 'Phen_early'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_abscission'] = 'Phen_late'
sample_coal$Var_type_broad[sample_coal$Var_type == 'Phen_emergence'] = 'Phen_early'
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
sample_coal$Tissue_Type[sample_coal$File_name == 'Pub170_Fig1'] = 'Total' # adding tissue type for this row
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
sample_coal$Family[sample_coal$Genus == 'Agropyrum'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Agrostis'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Anaphalis'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Andreae'] = 'Andreaeaceae'
sample_coal$Family[sample_coal$Genus == 'Andromeda'] = 'Ericaceae'
sample_coal$Family[sample_coal$Genus == 'Androsace'] = 'Primulaceae'
sample_coal$Family[sample_coal$Genus == 'Anemone'] = 'Ranunculaceae'
sample_coal$Family[sample_coal$Genus == 'Anthyllis'] = 'Fabaceae'
sample_coal$Family[sample_coal$Genus == 'Arctagrostis'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Arctous'] = 'Ericaceae'
sample_coal$Family[sample_coal$Genus == 'Arenaria'] = 'Caryophyllaceae'
sample_coal$Family[sample_coal$Genus == 'Artemisia'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Asphodedus'] = 'Asphodelaceae'
sample_coal$Family[sample_coal$Genus == 'Aster'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Asterolasia'] = 'Rutaceae'
sample_coal$Family[sample_coal$Genus == 'Aulacomnium'] = 'Aulacomniaceae'
sample_coal$Family[sample_coal$Genus == 'Avicennia'] = 'Acanthaceae'
sample_coal$Family[sample_coal$Genus == 'Bartramia'] = 'Bartramiaceae'
sample_coal$Family[sample_coal$Genus == 'Betula'] = 'Betulaceae'
sample_coal$Family[sample_coal$Genus == 'Bituminaria'] = 'Fabaceae'
sample_coal$Family[sample_coal$Genus == 'Brachyscome'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Carduus'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Carex'] = 'Cyperaceae'
sample_coal$Family[sample_coal$Genus == 'Cassiope'] = 'Ericaceae'
sample_coal$Family[sample_coal$Genus == 'Celmisia'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Cerastium'] = 'Caryophyllaceae'
sample_coal$Family[sample_coal$Genus == 'Cetraria'] = 'Parmeliaceae'
sample_coal$Family[sample_coal$Genus == 'Chorisodontium'] = 'Dicranaceae'
sample_coal$Family[sample_coal$Genus == 'Cladonia'] = 'Cladoniaceae'
sample_coal$Family[sample_coal$Genus == 'Colobanthus'] = 'Caryophyllaceae'
sample_coal$Family[sample_coal$Genus == 'Coris'] = 'Primulaceae'
sample_coal$Family[sample_coal$Genus == 'Craspedia'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Cynodon'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Daphne'] = 'Thymelaeaceae'
sample_coal$Family[sample_coal$Genus == 'Deschampsia'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Diapensia'] = 'Diapensiaceae'
sample_coal$Family[sample_coal$Genus == 'Dicranum'] = 'Dicranaceae'
sample_coal$Family[sample_coal$Genus == 'Distichlis'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Dorycnium'] = 'Fabaceae'
sample_coal$Family[sample_coal$Genus == 'Draba'] = 'Brassicaceae'
sample_coal$Family[sample_coal$Genus == 'Dryas'] = 'Rosaceae'
sample_coal$Family[sample_coal$Genus == 'Dupontia'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Echium'] = 'Boraginaceae'
sample_coal$Family[sample_coal$Genus == 'Elymus'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Empetrum'] = 'Ericaceae'
sample_coal$Family[sample_coal$Genus == 'Erigeron'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Eriophorum'] = 'Cyperaceae'
sample_coal$Family[sample_coal$Genus == 'Festuca'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Flavocetraria'] = 'Parmeliaceae'
sample_coal$Family[sample_coal$Genus == 'Furcraea'] = 'Asparagaceae'
sample_coal$Family[sample_coal$Genus == 'Gentiana'] = 'Gentianaceae'
sample_coal$Family[sample_coal$Genus == 'Geum'] = 'Rosaceae'
sample_coal$Family[sample_coal$Genus == 'Gypsophila'] = 'Caryophyllaceae'
sample_coal$Family[sample_coal$Genus == 'Harpochola'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Hedysarum'] = 'Fabaceae'
sample_coal$Family[sample_coal$Genus == 'Helianthemum'] = 'Cistaceae'
sample_coal$Family[sample_coal$Genus == 'Helictotrichon'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Hennediella'] = 'Pottiaceae'
sample_coal$Family[sample_coal$Genus == 'Heteropogon'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Hierochloe'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Hordeum'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Hylocomium'] = 'Hylocomiaceae'
sample_coal$Family[sample_coal$Genus == 'Kobresia'] = 'Cyperaceae'
sample_coal$Family[sample_coal$Genus == 'Koeleria'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Lagotis'] = 'Plantaginaceae'
sample_coal$Family[sample_coal$Genus == 'Larix'] = 'Pinaceae'
sample_coal$Family[sample_coal$Genus == 'Ledum'] = 'Ericaceae'
sample_coal$Family[sample_coal$Genus == 'Leontodon'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Leptorhynchos'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Loiseleuria'] = 'Ericaceae'
sample_coal$Family[sample_coal$Genus == 'Lonicera'] = 'Caprifoliaceae'
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
sample_coal$Family[sample_coal$Genus == 'Persicaria'] = 'Polygonaceae'
sample_coal$Family[sample_coal$Genus == 'Picea'] = 'Pinaceae'
sample_coal$Family[sample_coal$Genus == 'Pimelea'] = 'Thymelaeaceae'
sample_coal$Family[sample_coal$Genus == 'Plantago'] = 'Plantaginaceae'
sample_coal$Family[sample_coal$Genus == 'Pleurozium'] = 'Hylocomiaceae'
sample_coal$Family[sample_coal$Genus == 'Poa'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Polygonum'] = 'Polygonaceae'
sample_coal$Family[sample_coal$Genus == 'Polytrichastrum'] = 'Polytrichaceae'
sample_coal$Family[sample_coal$Genus == 'Polytrichum'] = 'Polytrichaceae'
sample_coal$Family[sample_coal$Genus == 'Potentilla'] = 'Rosaceae'
sample_coal$Family[sample_coal$Genus == 'Primula'] = 'Primulaceae'
sample_coal$Family[sample_coal$Genus == 'Pseudoroegneria'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Ranunculus'] = 'Ranunculaceae'
sample_coal$Family[sample_coal$Genus == 'Rhododendron'] = 'Ericaceae'
sample_coal$Family[sample_coal$Genus == 'Rubus'] = 'Rosaceae'
sample_coal$Family[sample_coal$Genus == 'Rytidosperma'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Salix'] = 'Salicaceae'
sample_coal$Family[sample_coal$Genus == 'Sanguisorba'] = 'Rosaceae'
sample_coal$Family[sample_coal$Genus == 'Sanionia'] = 'Amblystegiaceae'
sample_coal$Family[sample_coal$Genus == 'Santolina'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Saussurea'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Saxifraga'] = 'Saxifragaceae'
sample_coal$Family[sample_coal$Genus == 'Schoenoplectus'] = 'Cyperaceae'
sample_coal$Family[sample_coal$Genus == 'Selaginella'] = 'Selaginellaceae'
sample_coal$Family[sample_coal$Genus == 'Senecio'] = 'Asteraceae'
sample_coal$Family[sample_coal$Genus == 'Sesleria'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Silene'] = 'Caryophyllaceae'
sample_coal$Family[sample_coal$Genus == 'Sparganium'] = 'Typhaceae'
sample_coal$Family[sample_coal$Genus == 'Spartina'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Sphaerophorus'] = 'Sphaerophoraceae'
sample_coal$Family[sample_coal$Genus == 'Sphagnum'] = 'Sphagnaceae'
sample_coal$Family[sample_coal$Genus == 'Spiraea'] = 'Rosaceae'
sample_coal$Family[sample_coal$Genus == 'Stellaria'] = 'Caryophyllaceae'
sample_coal$Family[sample_coal$Genus == 'Stellera'] = 'Thymelaeaceae'
sample_coal$Family[sample_coal$Genus == 'Stereocaulon'] = 'Stereocaulaceae'
sample_coal$Family[sample_coal$Genus == 'Stiburus'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Stipa'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Thalictrum'] = 'Ranunculaceae'
sample_coal$Family[sample_coal$Genus == 'Teucrium'] = 'Lamiaceae'
sample_coal$Family[sample_coal$Genus == 'Thamnolia'] = 'Icmadophilaceae'
sample_coal$Family[sample_coal$Genus == 'Themeda'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Tofieldia'] = 'Tofieldiaceae'
sample_coal$Family[sample_coal$Genus == 'Trichostema'] = 'Lamiaceae'
sample_coal$Family[sample_coal$Genus == 'Triglochin'] = 'Juncaginaceae'
sample_coal$Family[sample_coal$Genus == 'Tristachya'] = 'Poaceae'
sample_coal$Family[sample_coal$Genus == 'Usnea'] = 'Parmeliaceae'
sample_coal$Family[sample_coal$Genus == 'Vaccinium'] = 'Ericaceae'
sample_coal$Family[sample_coal$Genus == 'Zizania'] = 'Poaceae'
# most common families?
sample_coal %>% 
  count(Family)


# reorganize order of column names 
sample_reorder <- sample_coal[, c("User","Pub_number","Pub_info","Study_year_start","File_name",
                                    "Var_type","Var_type_broad","Value_error","Yunits","Func_group","Func_group_broad","Family",
                                    "Genus","Species","Amount_warmed_C","Amount_warmed_type","Years_warmed",
                                    "Year_round_warm","Latitude","Site","Tissue_Type","Tissue_Type_broad",
                                    "Warmed_Mean","Warmed_SD","Warmed_N","Ambient_Mean","Ambient_SD","Ambient_N")]

# what pubs are still included?
unique(sample_coal$Pub_number)

# upload csv file to L2 folder
write.csv(sample_reorder, file.path(MA_dir,"L2/otc_data_cleaned_L2.csv"), row.names=F)
