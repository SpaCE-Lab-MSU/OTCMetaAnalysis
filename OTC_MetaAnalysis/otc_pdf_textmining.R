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

# clear all existing data
rm(list=ls())

# load packages
library(tm)
library(pdftools)

####** PDF WORK: MSU **####

# find the pdf folder and list them all
#### make sure this is set for the hpcc ####
pdf_folder <- "/mnt/home/dobsonk2/pdfs"
msu.pdfs<-list.files(path=pdf_folder,pattern="pdf$")

# function to read in PDFs and maintain layout:
Rpdf <- readPDF(control = list(text = "-layout"))

# read in the PDFs, convert to text
msu.pdfs.data <- Corpus(URISource(msu.pdfs), 
                        readerControl = list(reader = Rpdf))

# search through the papers for specific terms
otc.msu <- DocumentTermMatrix(msu.pdfs.data,
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
                                                        "tree","trees","vegetation")))

otc.msu <- otc.msu[slam::row_sums(otc.msu) > 0,
                     slam::col_sums(otc.msu) > 0]

otc.msu <- data.frame(docs = row.names(otc.msu), as.matrix(otc.msu), row.names = NULL)

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
otc.msu$passiveotc<-(otc.msu$passive + otc.msu$open.top + otc.msu$open.top.1)
otc.msu$priority[otc.msu$passiveotc>0]<-1

# keep only the paper names + priority columns
otc.msu <- subset(otc.msu, select = c("docs", "priority"))

# save output
#### make sure this is set for the hpcc ####
write.csv(otc.msu,file="~/otc_papers_msu.csv")
