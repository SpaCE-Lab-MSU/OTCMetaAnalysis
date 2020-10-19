## TITLE:         fulltext package pdf download
## AUTHOR:        K. Dobson 
## COLLABORATORS: P. Zarnetske, P. Bills
## PROJECT:       OTC MetaAnalysis
## DATE:          Oct. 2020

## attempting to use the fulltext package for downloading PDFs from scopus ##

# load the package
library(fulltext)

# set API key for elsevier
Sys.setenv(CROSSREF_TDM = "mykey")
ft_get(x = "10.5194/bg-16-4851-2019", from = "elsevier")
# nope