# OTC Meta-Analysis

## Introduction

This repository contains R scripts that organize, clean, analyze, and plot data for an open-top chamber (OTC) meta-analysis.

## Workflow

All code for data cleaning, analyzing, and plotting is located in the "R" folder. This folder contains three main sections:

L0: The L0 scripts sort through downloaded PDFs from SCOPUS and perform text-mining functions to select potentially relevant papers. Once this preliminary paper selection step was performed, we manually checked each paper for relevance to our study.

L1: The L1 scripts further organize the data. After data was extracted from each study, these script go in and merge data extraction files prior to calculating effect sizes.

L2: The L2 scripts contain the main code for calculating effect sizes, analyzing, and plotting the data. [otc_effectsize_analysis_L2.R](https://github.com/SpaCE-Lab-MSU/OTCMetaAnalysis/blob/main/R/L2/otc_effectsize_analyses_L2.R) and [otc_effectsize_plots](https://github.com/SpaCE-Lab-MSU/OTCMetaAnalysis/blob/main/R/L2/otc_effectsize_plots_L2.R) are the main scripts for figure making and analyses.

## Location of data

The clean, L2 data used in analyses and plotting is currently located in the "Data" folder.

## Usage

All analyses were conducted using R (R Core Team 2021)

## Contributors

PI: Phoebe Zarnetske

Collaborators: Kara Dobson

Prior collaborators: Kileigh Welshofer

## Contact

For inquiries related to the data and scripts, please contact Phoebe Zarnetske: @plz@msu.edu
