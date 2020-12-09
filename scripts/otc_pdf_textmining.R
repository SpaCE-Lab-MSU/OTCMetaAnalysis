## TITLE:         OTC Meta Analysis PDF analysis
## AUTHOR:        P. Zarnetske 
## COLLABORATORS: K. Dobson, P. Bills, K. Welshofer
## DATA:          Scopus PDFs MSU
## PROJECT:       "OTC MetaAnalysis Paper"
## DATE:          June 7, 2016
##                K. Dobson updated September, 2020

## This file reads in PDFs from a Scopus Search of papers containing "climate change" 
## and similar terms. It then analyzes the PDFs for their content, specifically
## if they contain mention of open top chambers.
## The full search is this:
## SCOPUS Search using following “Article Title, Abstract, Keywords” separated by “OR”
## “climate change”
## “climate-change”
## “climate warm*”
## “climate-warm*”
## “global change”
## “global-change”
## “global warm*”
## “global-warm*”
## “global-climate-change”
## “global-climate-warm*”
## “ITEX”
## AND “experiment”

#!/usr/bin/env Rscript
# clear all existing data
rm(list=ls())

# load packages
library(tm)
library(pdftools)

# function to scan pdfs
scan_pdfs <- function(pdf_folder){
  
  # list all files in the pdf folder
  msu.pdfs <- tryCatch(list.files(path=pdf_folder,pattern="pdf$"), error=function(e) e)
  
  # function to read in PDFs and maintain layout
  Rpdf <- tryCatch(readPDF(control = list(text = "-layout")), error=function(e) e)
  
  # read in the PDFs, convert to text
  msu.pdfs.data <- tryCatch(Corpus(URISource(msu.pdfs), 
                          readerControl = list(reader = Rpdf)), error=function(e) e)
  
  # search through the papers for specific terms
  otc.msu <- tryCatch(DocumentTermMatrix(msu.pdfs.data,
                                list(dictionary = c("chamber","chambers",
                                                    "open-top","open top",
                                                    "warming chamber","warming chambers",
                                                    "warming-chamber","warming chambers",
                                                    "passive","passively","passive-","passively-",
                                                    "temperature","temperatures","ITEX","itex",
                                                    "fan","fans","plant","plants",
                                                    "seedling","seedlings","sapling","saplings",
                                                    "shrub","shrubs","grass","grasses",
                                                    "sedge","sedges","forb","forbs",
                                                    "tree","trees","vegetation"))), error=function(e) e)
  
  otc.msu <- tryCatch(otc.msu[slam::row_sums(otc.msu) > 0,
                     slam::col_sums(otc.msu) > 0], error=function(e) e)
  
  otc.msu <- tryCatch(data.frame(docs = row.names(otc.msu), as.matrix(otc.msu), row.names = NULL), error=function(e) e)
  
  # column headers with spaces become (.) or with dashes become (.1):
  names(otc.msu)
  
  # now some code to determine the high priority papers for using in the meta-analysis:
  # by default, paper is 0 priority unless meets following criteria:
  dim(otc.msu)
  otc.msu$priority<-0
  otc.msu$priority[otc.msu$chamber>0]<-1
  otc.msu$priority[otc.msu$chambers>0]<-1
  otc.msu$priority[otc.msu$open.top>0]<-1
  otc.msu$priority[otc.msu$open.top.1>0]<-1
  otc.msu$priority[otc.msu$warming.chamber>0]<-1
  otc.msu$priority[otc.msu$warming.chamber.1>0]<-1
  otc.msu$passiveotc<-rowSums(cbind(otc.msu$passive, otc.msu$open.top, otc.msu$open.top.1))
  otc.msu$priority[otc.msu$passiveotc>0]<-1
  
  # keep only the paper names + priority columns
  otc.msu <- subset(otc.msu, priority == 1)
  otc.msu <- subset(otc.msu, select = c("docs", "priority"))
  
  return(otc.msu)

}

# set arguments
args = commandArgs(trailingOnly=TRUE)

if (length(args) == 2) {
  
  # since there were arguments on the command line use those to run the function
  otc_results <- scan_pdfs(args[1])
  write.csv(otc_results, file=args[2])
  
} else {
  # no command line arguments.  Optional but you could print a message about that
  # note this message would be printed every time you 'source' the script inside Rstudio
  
  print("This script requires two parameters :otc_pdf_textmining.R  <path to folder with pdfs> <path and name of output file>")
}

