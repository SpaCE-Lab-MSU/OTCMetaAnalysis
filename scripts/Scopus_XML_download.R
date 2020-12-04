#TITLE: Scopus XML download
# AUTHORS: Kara Dobson
# COLLABORATORS: Pat Bills, Phoebe Zarnetske
# DATA INPUT: 
# DATA OUTPUT: 
# PROJECT: warmXtrophic
# DATE: August 2020

# clear all existing data
rm(list=ls())

# load in packages
for (package in c('rscopus')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages("package")
    library(package, character.only=T)
  }
}


# set API key - Elsevier API key download found online
options("elsevier_api_key" = "key-goes-here")


# attempt to read in a paper - taken from rscopus documentation
# error: resource not found; fails for doi and eid
if (!is.null(api_key)){
  x = article_retrieval("10.1021/jacs.8b10203", identifier = "doi",
                        verbose = FALSE, view = "FULL")
  gen = x$content$`full-text-retrieval-response`
  ot = gen$originalText
} else {
  x = article_retrieval("10.1021/jacs.8b10203",
                        identifier = "doi",
                        api_key_error = FALSE)
}

# simpler attempt
# same error
article_retrieval("10.1038/550S66a",
                  identifier = "doi",
                  view = "FULL")

# another attempt
# same error
object_retrieval("10.1038/550S66a", 
                 identifier = "doi",
                 view = "FULL")