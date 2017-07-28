#!/usr/bin/env Rscript 

#####################################################################################
rm(list=ls())

rootDir <- "/home_ssd/dag/0-Annat/0-R/customers/mgh"

setwd( rootDir ) 
codeDir   <- file.path(rootDir, "code")
dataDir   <- file.path(rootDir, "data")
outputDir <- file.path(rootDir, "output")
claimDir  <- file.path(rootDir, "find_claim")

source(file.path(codeDir, "utils.r"))
source(file.path(codeDir, "db.r"))

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(tm)
  library(openNLP)
  library(RefManageR)
  library(rorcid)
  library(rcrossref)
})

# need to call this such that we can access correct database

# need to call this such that we can access correct database
SQL_DATABASE_NAME <- 'mgh'

logfilename <- "findblocks.log"

#####################################################################################
find_terms_in_text <- function(lines, jrnl, t1)
{
  all_upper_success <- FALSE
  
  # 1. try all upper case
  t2 <- toupper(t1)
  searchterm <- paste0("^", t2, "[s]*$")
  index <- grep(searchterm, lines, ignore.case=F, value=F)
  if(length(index) > 0) all_upper_success <- TRUE
  
  # 2. then try first letter is upper, the rest is lower
  if(length(index) < 1) {
    t2 <- simpleCap(t1)
    searchterm <- paste0("^", t2, "[s]*$")
    index <- grep(searchterm, lines, ignore.case=T, value=F)
  }
  
  # 3. try ignore case
  if(length(index) < 1) {
    searchterm <- paste0("^", t1, "[s]*$")
    index <- grep(searchterm, lines, ignore.case=T, value=F)
  }
  
  if(length(index) < 1) {
    searchterm <- paste0("^", t1, "[s ]*\\.$")
    index <- grep(searchterm, lines, ignore.case=T, value=F)
  }
  
  if(length(index) < 1) {
    searchterm <- paste0("^[0-9a-zA-Z]{0,2}[\\.)] ", t1) 
    index <- grep(searchterm, lines, ignore.case=T, value=F)
  }
  
  if(length(index) < 1) {
    searchterm <- toupper(t1)
    index <- grep(searchterm, lines, ignore.case=F, value=F)
  }
  
  if(length(index) > 1) {
    if(all_upper_success) {
      # if we found an index already at the first - all upper - try, then simply pick the highest number
    	index <- max(index)
    } else if(grepl('The New England journal of medicine', jrnl, ignore.case=T)) {
    	# if this is NEJM, then assume that the later term is correct term, so pick the highest number
      index <- max(index)
    } else {
      logdata("WARNING WARNING")
      logdata(sprintf("  found more than one entry for term: \"%s\" - row numbers: %s", t1, paste(index, collapse=" ")))
      logdata("we will be using the lowest index")
      logdata("WARNING WARNING")
      index <- min(index)
    }
  }
  
  return(index)
}

#####################################################################################
if(0) {
  doc <- doc1
}
read_in_files_sent <- function(lines, jrnl, term1)
{
  # remove new page
  lines <- gsub("^\f", "", lines)
  
  for(t1 in term1) {
    #logdata(sprintf("t1: %s", t1))
    index <- find_terms_in_text(lines, jrnl, t1)
    if(length(index) > 0) break ; 
  } 

  if(length(index) < 1) index <- 0
  
  return(index)
}

#####################################################################################
read_in_text_blocks <- function(pdfDocs, pdfDir)
{
  logdata("read_in_text_blocks")
  inspect_doc <- FALSE
  
  pdfDocs <- sort(pdfDocs)
  
  #doc <- pdfDocs[1]

  p3 <- data.frame()
  #for(i in 11:20) {
  for(i in seq_len(length(pdfDocs))) {
    doc <- pdfDocs[i]
    docinfo <- calldb(paste0("select docid, pmid, journal from docs where filename = '", doc, "' ;"))

    if(nrow(docinfo) == 0) {
      logdata("WARNING - did not find a document in the database for docname:", doc)
      next ;
    }
    logdata("-------------------------------------------------------------------------")
    logdata(i, " docid:", docinfo$docid, " document in process: ", doc)
    doc1 <- file.path(pdfDir, doc)

    # 1. create VCorpus
    # I could silence the error messages with a text='-raw -q' but I rather see the messages
    # Error that we don't care about
    # 1. Syntax Warning: Invalid Font Weight
    # 2. Syntax Error: Couldn't find trailer dictionary
    # 3. Syntax Error: Catalog object is wrong type (null)
    # 4. Syntax Error (38799): insufficient arguments for Marked Content

    jc <- VCorpus(URISource(doc1, mode = ""), readerControl = list(reader = readPDF(engine = "xpdf", control=list(text='-raw'))))
    if(inspect_doc) inspect_document(jc, topcount=30, stem_terms=T)
    lines <- sapply(jc, function(x) as.character(x))
    
    # http://unicodelookup.com/#latin/1
    
    lines <- clean_all_data(lines) # remove funny characters such as new line, formfeed etc.
    lines <- lines[nchar(lines) > 0] # remove lines with no characters
    
    logdata("write lines to lines1.csv and to database")
    write.csv(lines, file=file.path(codeDir,"lines1.csv"), row.names=F)
    insert_into_text(docinfo$docid, 'lines', text=data.frame(text=lines,stringsAsFactors=F), insert=T)

    # 2. parse abstracts etc from lines
    jrnl <- docinfo$journal
    p2 <- data.frame(doc=doc,
      abstract       = read_in_files_sent(lines, jrnl, term1=c('abstract', 'summary')),
      intro          = read_in_files_sent(lines, jrnl, term1=c('introduction', 'background')),
      method         = read_in_files_sent(lines, jrnl, term1=c('method', 'material and method', 'materials and method', 
                       'subjects and method', 'subject and method', 'experimental procedure')),
      result         = read_in_files_sent(lines, jrnl, term1='results'),
      discussion     = read_in_files_sent(lines, jrnl, term1=c('discussion', 'comment')),
      conclusion     = read_in_files_sent(lines, jrnl, term1='conclusion'),
      acknowledgment = read_in_files_sent(lines, jrnl, term1='acknowledgment'),
      reference      = read_in_files_sent(lines, jrnl, term1=c('references', 'reference list')), stringsAsFactors=F)
    
    p3 <- rbind(p3, p2)
    insert_into_blocks(docinfo$docid, 'lines', p2, insert=T)

    # add a period in the end of each header so to mark it as a new sentence
    # always get the top one
    index <- p2[1,2:ncol(p2)] %>% as.integer()
    
    lines[index] <- paste0(". This is a sentence ", lines[index], ". This is a sentence." )
    
    # 3. create sentences 
    txt1 <- paste(lines, collapse=" ")
    txt1 <- clean_all_data(txt1)
    if(length(grep(paste("^package:", "ggplot2", "$", sep=""), search()))) {detach(package:ggplot2)}
    txt2 <- convert_to_sentence(txt1)
    
    txt2 <- gsub("This is a sentence ", "", txt2)
    txt2 <- gsub("This is a sentence.", "", txt2)
    txt2 <- txt2[nchar(txt2) > 0] # remove lines with no characters
    
    logdata("write sentences to sent1.csv and to database")
    write.csv(txt2, file=file.path(codeDir,"sent1.csv"), row.names=F)
    insert_into_text(docinfo$docid, 'sent', data.frame(text=txt2,stringsAsFactors=F), insert=T)
  
    # 4. parse abstract etc from sentences 
    jrnl <- docinfo$journal
    p2 <- data.frame(doc=doc,
      abstract       = read_in_files_sent(txt2, jrnl, term1=c('abstract', 'summary')),
      intro          = read_in_files_sent(txt2, jrnl, term1=c('introduction', 'background')),
      method         = read_in_files_sent(txt2, jrnl, term1=c('method', 'material and method', 'materials and method', 
                       'subjects and method', 'subject and method', 'experimental procedure')),
      result         = read_in_files_sent(txt2, jrnl, term1='results'),
      discussion     = read_in_files_sent(txt2, jrnl, term1=c('discussion', 'comment')),
      conclusion     = read_in_files_sent(txt2, jrnl, term1='conclusion'),
      acknowledgment = read_in_files_sent(txt2, jrnl, term1='acknowledgment'),
      reference      = read_in_files_sent(txt2, jrnl, term1=c('references', 'reference list')), stringsAsFactors=F)
    p3 <- rbind(p3, p2)
    insert_into_blocks(docinfo$docid, 'sent', p2, insert=T)
  }
  
  logdata("write results to results_sent2.csv file")
  write.csv(p3, file.path(codeDir, "results_sent2.csv"),row.names=F)
}

#####################################################################################
find_rhetorical_blocks <- function(pdfDocs, pdfDir, DELETE_DATABASE=F)
{
  disconnect_all_mysql_connections()
  
  logdata("count from current database")
  get_count_from_tables()
  if(DELETE_DATABASE) { delete_from_tables() }
  
  if(0) {
    pdfDir <- pdfDocs <- NULL
  }

  if(!is.null(pdfDocs)) {
    # pdfDocs is not NULL, then make sure that the file exists in the database before we proceed
    x1 <- calldb(paste0("select filename from docs where filename = '", pdfDocs, "';"))
    if(nrow(x1) == 0) {
      logdata("Error: file ", pdfDocs, "does not exists in the database - exiting")
      return(NULL)
    }
  }

  if(is.null(pdfDir))  pdfDir   <- file.path(dataDir, 'top50alz')
  if(is.null(pdfDocs)) pdfDocs <- calldb("select filename from docs where filename <> ''")$filename 

  read_in_text_blocks(sort(pdfDocs), pdfDir)

  logdata("count from current database")
  get_count_from_tables()
}


#####################################################################################
# MAIN main Main
#

logdata("==================== START OF RUN =================")
t1 <- proc.time()

process_file <- FALSE
if( length( commandArgs() ) == 6 ) {
  filenamex <- commandArgs()[6]
  if(tolower(filenamex) == 'all') {
    logdata("Do all files")
    pdfDocs <- pdfDir <- NULL
    process_file <- TRUE
  } else if(file.exists(filenamex))  {
    logdata("Process file:", filenamex)
    if(0){filenamex <- "/home_ssd/dag/0-Annat/0-R/customers/mgh/data/top50alz/20042704_Petersen_ADNI_Clinical_characterization.pdf"}
    pdfDocs <- basename(filenamex)
    pdfDir <- dirname(filenamex)
    process_file <- TRUE
  } else if(!file.exists(filenamex))  {
    logdata("Error: file", filenamex, "does not exists - exiting")
    logdata("NOTE - we need absolute addresses to files")
    process_file <- FALSE
  }
} else {
  logdata("Input must be either 'all' for all files or a filename with an absolute file path") 
  process_file <- FALSE
}

if(process_file) find_rhetorical_blocks(pdfDocs, pdfDir, DELETE_DATABASE = F)

s <- proc.time()[3] - t1[3]
s <- as.integer(s)
h <- s %/% 3600 ; s <- s - (h*3600) ; 
m <- s %/% 60   ; s <- s - (m*60)   ; 
logdata("")
logdata(sprintf("process time: %d hours %d minutes %d seconds", h, m, s))
logdata("==================== END OF RUN =================")

