# TITLE:          L0 data clean-up
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Pat Bills
# DATA INPUT:     L0 data entry sheets from the L0 folder
# DATA OUTPUT:    L1 dataframe
# DATE:           Sep 2022


# Load packages
library(tidyverse)

# set working directory
MA_dir<-Sys.getenv("MADIR")

# read in data
ep <- read.csv(file.path(MA_dir,"L0/otc_data_entry_EP.csv"))
ja <- read.csv(file.path(MA_dir,"L0/otc_data_entry_JA.csv"))
kd <- read.csv(file.path(MA_dir,"L0/otc_data_entry_KD.csv"))

# removing X column from JA
ja <- subset(ja, select = -X)

# binding dataframes
merged <- rbind(ep, ja, kd)

# removing xunits and xvalue columns
merged <- subset(merged, select = -c(Xunits, Xvalue))

## removing data (based on OTC_MA_data_checks document)
# removing shoot_length from pub 38 and 52
merged<-merged[!(merged$Pub_number==38 & merged$Var_type=="Shoot_length"),]
merged<-merged[!(merged$Pub_number==52 & merged$Var_type=="Shoot_length"),]
# removing pub 8 fig 2 and pub 32 fig 2 from EP (duplicated with JA)
merged<-merged[!(merged$User=="EP" & merged$File_name=="Pub8_Fig2"),]
merged<-merged[!(merged$User=="EP" & merged$File_name=="Pub32_Fig2"),]
# removing pub 17 tbl 1 (ANPP data)
merged<-merged[!(merged$File_name=="Pub17_Tbl1"),]
# removing data for all plant lat except plant_lat_1
merged<-merged[!(merged$Site=="Plant_lat_2" |
                   merged$Site=="Plant_lat_3" |
                   merged$Site=="Plant_lat_4" |
                   merged$Site=="Plant_lat_5" |
                   merged$Site=="Plant_lat_6" |
                   merged$Site=="Plant_lat_7"),]
# removing data for pub 54 that isn't needed
merged<-merged[!(merged$Notes=="OTC-GF, 60" |
                 merged$Notes=="OTC-GF, 70" |
                 merged$Notes=="OTC-GF, 75" |
                 merged$Notes=="OTC-GF, 80" |
                 merged$Notes=="OTC-GF, 90" |
                 merged$Notes=="OTC-GF, 100" |
                 merged$Notes=="OTC-GF, 105"),]
# removing pub 66 fig 8 (percent change)
merged<-merged[!(merged$File_name=="Pub66_Fig8"),]
# removing long and short shoot data from pub 67
merged<-merged[!(merged$Pub_number==67 & merged$Tissue_Type=="Long_shoot"),]
merged<-merged[!(merged$Pub_number==67 & merged$Tissue_Type=="Short_shoot"),]
# removing pub 90 fig 3
merged<-merged[!(merged$File_name=="Pub90_Fig3"),]
# removing pub 167 fig 3
merged<-merged[!(merged$File_name=="Pub167_Fig3"),]

# removing notes and comments columns
merged <- subset(merged, select = -c(Notes, Comments))

# re-ordering data by pub #
merged <- merged[order(merged$Pub_number),]

# long to wide format
merged_wider <- merged %>%
  pivot_wider(names_from = Variable, values_from = Yvalue)
