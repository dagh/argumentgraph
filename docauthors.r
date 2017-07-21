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
source(file.path(codeDir, "tm_utils.r"))
source(file.path(codeDir, "db.r"))
source(file.path(codeDir, "pbutils.r"))

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(openNLP))
suppressPackageStartupMessages(library(RefManageR))
suppressPackageStartupMessages(library(rorcid))
suppressPackageStartupMessages(library(rcrossref))

# need to call this such that we can access correct database
SQL_DATABASE_NAME <- 'mgh'

logfilename <- "doc_authors.log"

#############################################################################
testay <- function()
{
  d1 <- data.frame()
  for(docid in seq_len(50)) {
    q1 <- paste0("select docid, filename, journal from docs where docid=", docid, " ;")
    d2 <- calldb(q1)
    q1 <- paste0("select type from refs where docid=", docid, " limit 1;")
    type <- calldb(q1) %>% as.character()
    q1 <- paste0("select count(*) from refs where docid=", docid, " ;")
    total_count <- calldb(q1) %>% as.integer()
    q1 <- paste0("select count(*) from refs where docid=", docid, " and bibentry is NULL ;")
    null_count <- calldb(q1) %>% as.integer()
    #d1 <- rbind(d1, data.frame(docid=docid, filename=filename, journal, total_count=total_count, null_count=null_count))
    d1 <- rbind(d1, cbind(d2, data.frame(type=type, total_count=total_count, null_count=null_count)))
  }
  write.csv(d1, file=file.path(codeDir, "refcount.csv"), row.names=F)
  
}

#####################################################################################
find_store_in_pubmed <- function(pdfDocs, pdfDir)
{
  if(is.null(pdfDir)) pdfDir <- file.path(dataDir, 'top50alz')
  if(is.null(pdfDocs)) pdfDocs <- list.files(path=pdfDir, pattern="*.pdf", full.names=F)
  
  i <- 0
  for(docname in pdfDocs) {
    i <- i + 1
    log_data(sprintf("%3d of %2d  %s", i, length(pdfDocs), docname))
    pmid <- substring(docname, 1, 8) %>% as.integer()
    b1 <- GetPubMedByID(pmid)
    if(is.null(b1)) next ;
    df <- as.data.frame(b1)
    if(is.null(df)) next ;
    if(nrow(df) == 0) next ;

    pmid_convert <- convert_pmid(pmid)
    
    names(df)
    d1 <- data.frame(
      type          = 'standard', 
      pmid          = ifelse(!is.null(df$eprint), df$eprint, ''),
      pmcid         = ifelse(!is.null(pmid_convert), pmid_convert$pmcid, ''),
      pmcid_ver     = ifelse(!is.null(pmid_convert), pmid_convert$pmcid_ver, ''),
      filename      = docname,
      bibtype       = ifelse(!is.null(df$bibtype), df$bibtype, ''),
      title         = ifelse(!is.null(df$title), df$title, ''),
      subtitle      = ifelse(!is.null(df$subtitle), df$subtitle, ''),
      year          = ifelse(!is.null(df$year), df$year, ''),
      month         = ifelse(!is.null(df$month), df$month, ''),
      date          = ifelse(!is.null(df$date), df$date, ''),
      publisher     = ifelse(!is.null(df$publisher), df$publisher, ''),
      journal       = ifelse(!is.null(df$journal), df$journal, ''),
      volume        = ifelse(!is.null(df$volume), df$volume, ''),
      number        = ifelse(!is.null(df$number), df$number, ''),
      pages         = ifelse(!is.null(df$pages), df$pages, ''),
      url           = ifelse(!is.null(df$url), df$url, ''),
      doi           = ifelse(!is.null(df$doi), df$doi, ''),
      eprint        = ifelse(!is.null(df$eprint), df$eprint, ''),
      eprinttype    = ifelse(!is.null(df$eprinttype), df$eprinttype, ''),
      language      = ifelse(!is.null(df$language), df$language, ''),
      issn          = ifelse(!is.null(df$issn), df$issn, ''),
      abstract      = ifelse(!is.null(df$abstract), df$abstract, ''),
      score         = ifelse(!is.null(df$score), df$score, ''),
      journal_title = ifelse(!is.null(df$journal_title), df$journal_title, ''),
      stringsAsFactors=F)

    docs1 <- insert_into_docs(d1)
    
    process_authors(docs1, b1, df$doi)
    
  }
  
  # write to spreadsheet
  p1 <- calldb("select * from authors;")
  write.csv(p1, file=file.path(codeDir, "author3.csv"), row.names=F)
  
  p2 <- filter(p1, orcid != '')
  index <- which(duplicated(p2$orcid))
}

#####################################################################################
# MAIN main Main
#

log_data("==================== START OF RUN =================")
t1 <- proc.time()


process_file <- FALSE
if( length( commandArgs() ) == 6 ) {
  filenamex <- commandArgs()[6]
  if(tolower(filenamex) == 'all') {
    log_data("Do all files")
    pdfDocs <- pdfDir <- NULL
    process_file <- TRUE
  } else if(file.exists(filenamex))  {
    log_data("Process file:", filenamex)
    if(0){filenamex <- "/home_ssd/dag/0-Annat/0-R/customers/mgh/data/top50alz/20042704_Petersen_ADNI_Clinical_characterization.pdf"}
    pdfDocs <- basename(filenamex)
    pdfDir <- dirname(filenamex)
    process_file <- TRUE
  } else if(!file.exists(filenamex))  {
    log_data("Error: file", filenamex, "does not exists - exiting")
    log_data("NOTE - we need absolute addresses to files")
    process_file <- FALSE
  }
} else {
  log_data("Input must be either 'all' for all files or a filename with an absolute file path") 
  process_file <- FALSE
}

if(process_file) find_store_in_pubmed(pdfDocs, pdfDir)

s <- proc.time()[3] - t1[3]
s <- as.integer(s)
h <- s %/% 3600 ; s <- s - (h*3600) ; 
m <- s %/% 60   ; s <- s - (m*60)   ; 
log_data("")
log_data(sprintf("process time: %d hours %d minutes %d seconds", h, m, s))
log_data("==================== END OF RUN =================")

