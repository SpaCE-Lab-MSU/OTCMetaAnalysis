# OTC Meta-Analysis

## Introduction

This repository contains R scripts that organize, clean, analyze, and plot data for an open-top chamber (OTC) meta-analysis.

## Data description

Our data is located within the "Data" folder, which contains three subfolders:

L0: Contains raw data that has not been cleaned. This includes RData files from one paper used in the meta-analysis (Collins et al. 2021). The other csv files contain data extracted from multiple papers, named with the initials of who extracted the data (i.e., KD, EP, or JA).

L1: Contains a cleaned data file, "otc_data_sample_sizes", that has merged together the individual L0 csv files and contains data from the RData files.

L2: Contains the finalized data with calculated effect sizes. The "otc_data_cleaned_allyears" file contains data for all years for studies that had measurements across multiple years, while the "otc_effect_sizes" file only contains the final year of data collection for each study. The "otc_effect_sizes" file is the main data source used in analyses and figures.

We are planning to make an EDI data repository to host this data.

## Workflow

All code for data cleaning, analyzing, and plotting is located in the "R" folder. This folder contains three main sections:

L0: The L0 scripts sort through downloaded PDFs from SCOPUS and perform text-mining functions to select potentially relevant papers. Once this preliminary paper selection step was performed, we manually checked each paper for relevance to our study. After relevance was determined, we manually extracted data for each study, yielding the "otc_data_entry" files in the L0 data folder. We used the publically available data for one study (Collins et al. 2021), which is the RData files in the L0 data folder.

L1: The L1 scripts further organize the data. After data was extracted from each study, these script go in and merge data extraction files prior to calculating effect sizes.

L2: The L2 scripts contain the main code for calculating effect sizes, analyzing, and plotting the data. [otc_data_clean_L2.R](https://github.com/SpaCE-Lab-MSU/OTCMetaAnalysis/blob/main/R/L2/otc_data_clean_L2.R) takes the clean L1 data and calculates effect sizes, yielding our final L2 dataframes. [otc_effectsize_analysis_L2.R](https://github.com/SpaCE-Lab-MSU/OTCMetaAnalysis/blob/main/R/L2/otc_effectsize_analyses_L2.R) and [otc_effectsize_plots](https://github.com/SpaCE-Lab-MSU/OTCMetaAnalysis/blob/main/R/L2/otc_effectsize_plots_L2.R) are the main scripts for figure making and analyses.

## Usage

All analyses were conducted using R (R Core Team 2024).

## Contributors

PI: Phoebe Zarnetske

Collaborators: Kara Dobson, Pat Bills, Emily Parker, Jacklyn Alsbro

Prior collaborators: Kileigh Welshofer

## Funding

Funding for this project is provided by Michigan State University and the National Science Foundation, through the Kellogg Biological Station Long-Term Ecological Research site (NSF DEB # 2224712).

## Contact

For inquiries related to the data and scripts, please contact Phoebe Zarnetske: @plz@msu.edu
