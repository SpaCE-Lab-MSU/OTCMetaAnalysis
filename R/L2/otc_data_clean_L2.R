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
unique(sample_coal$Func_group_broad)


# checking that variable types are all spelled the same
unique(sample_coal$Var_type)
sample_coal$Var_type[sample_coal$Var_type == 'Perc_cover'] = 'Percent_cover'


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
unique(sample_coal$Tissue_Type_broad)


# reorganize order of column names 
sample_reorder <- sample_coal[, c("User","Pub_number","Pub_info","Study_year_start","File_name",
                                  "Var_type","Value_error","Yunits","Func_group","Func_group_broad",
                                  "Genus","Species","Amount_warmed_C","Amount_warmed_type","Years_warmed",
                                  "Year_round_warm","Latitude","Site","Tissue_Type","Tissue_Type_broad",
                                  "Warmed_Mean","Warmed_SD","Warmed_N","Ambient_Mean","Ambient_SD","Ambient_N")]


# upload csv file to L2 folder
write.csv(sample_reorder, file.path(MA_dir,"L2/otc_data_cleaned_L2.csv"))

