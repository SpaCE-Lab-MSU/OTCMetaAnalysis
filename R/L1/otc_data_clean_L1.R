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

# re-ordering data by pub #
merged <- merged[order(merged$Pub_number),]
