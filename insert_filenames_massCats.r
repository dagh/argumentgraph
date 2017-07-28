#!/usr/bin/env Rscript 

#####################################################################################
# This file simply inserts file names from the 24 massCats / Haggerty documents
# into the database, table docs, based on pmid and then based on title (where pmid
# does not exists)
#
rm(list=ls())

rootDir <- "/home_ssd/dag/0-Annat/0-R/customers/mgh"

setwd( rootDir ) 
codeDir   <- file.path(rootDir, "code")
dataDir   <- file.path(rootDir, "data")
outputDir <- file.path(rootDir, "output")

source(file.path(codeDir, "utils.r"))
source(file.path(codeDir, "db.r"))
source(file.path(codeDir, "pbutils.r"))

library(dplyr)

# need to call this such that we can access correct database
SQL_DATABASE_NAME <- 'mgh'

logfilename <- "insert_filenames_massCats.log"

####################################################################
# haggerty docid
# hag_docid=780
insert_filenames <- function(hag_docid=2480)
{
  file1 <- read.csv(file.path(dataDir, "massCATS_new_docs_jun14_2017.csv"), stringsAsFactors=F)
  db1 <- calldb(paste0("select docid, title, pmid, filename, doi from docs where docid >= ", hag_docid, " ;"))
  
  file1$pmid <- as.character(file1$pmid)
  
  # test different ways to joining the data frames
  #d2 <- full_join(file1, db1, by="docid")
  d3 <- full_join(file1, db1, by="title")
  #d4 <- full_join(file1, db1, by="doi")
  d5 <- full_join(file1, db1, by="pmid")
  
  # note that it is not a good idea to update the database based on docid, because
  # as we update the database, there is a chance that the docids might have changed
  # So: 
  # 1. update database based on PMID. This is the most unique variable
  havepmid <- d5 %>% filter(  !is.na(pmid) & pmid != "")
  
  print("1. use PMID to join filenames")
  for(i in seq_len(nrow(havepmid))) {
  	q1 <- paste0("update docs set filename = '", havepmid$filename.x[i], "' where pmid = '", havepmid$pmid[i], "' ; ")
  	print(q1)
  	calldb(q1)
  }
  
  # 2. Use title on the second round
  havetitle <- d3 %>% filter(!(docid.x %in% havepmid$docid.x)) %>% select(-c(filename.y, pmid.y, doi.x))
  
  print("2. use title to join filenames")
  for(i in seq_len(nrow(havetitle))) {
  	q1 <- paste0("update docs set filename = '", havetitle$filename.x[i], "' where title = '", havetitle$title[i], "' ; ")
  	print(q1)
  	calldb(q1)
  }
}

####################################################################
# main Main MAIN

insert_filenames()
