rm(list=ls())

# load packages
library(tm)
library(pdftools)

 # list all files in the pdf folder
pdf_folder <- "/mnt/home/dobsonk2/test_papers"
msu.pdfs<-list.files(path=pdf_folder,pattern="pdf$")
  
# function to read in PDFs and maintain layout
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
otc.msu$passiveotc<-rowSums(cbind(otc.msu$passive, otc.msu$open.top, otc.msu$open.top.1))
otc.msu$priority[otc.msu$passiveotc>0]<-1

# keep only the paper names + priority columns
otc.msu <- subset(otc.msu, priority == 1)
otc.msu <- subset(otc.msu, select = c("docs", "priority"))


# set arguments
args = commandArgs(trailingOnly=TRUE)

if (length(args) == 1) {
  
  # since there were arguments on the command line use those to run the function
  write.csv(otc.msu, file=args[1])
  
} else {
  # no command line arguments.  Optional but you could print a message about that
  # note this message would be printed every time you 'source' the script inside Rstudio
  
  print("This script requires two parameters :otc_pdf_textmining.R  <path to folder with pdfs> <path and name of output file>")
}
