# TITLE:          OTC Meta-Analysis file merge
# AUTHORS:        Kara Dobson, Pat, Bills
# COLLABORATORS:  Phoebe Zarnetske
# DATA INPUT:     Papers that passed three rounds of text mining (in three separate csvs) are input
# DATA OUTPUT:    Outputs each line (which corresponds to an individual paper) that overlaps between each
#                 round of textmining; finds papers which are likely to be relevant
# DATE:           April 2021

# read in packages
library(tidyverse)

# load in files
subset1 <- read.csv("otc_search_clean.csv")
subset2 <- read.csv("passive_search_clean.csv")
subset3 <- read.csv("itex_search_clean.csv")

# merge function - keeps overlapping files
in_all_3 <- merge(merge(subset1, subset2), subset3)

# set arguments
args = commandArgs(trailingOnly=TRUE)

if (length(args) == 1) {
  
  # since there were arguments on the command line use those to run the function
  write.csv(in_all_3, file=args[1])
  
} else {
  # no command line arguments.  Optional but you could print a message about that
  # note this message would be printed every time you 'source' the script inside Rstudio
  
  print("This script requires <path and name of output file>")
}