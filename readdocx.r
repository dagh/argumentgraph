#!/usr/bin/env Rscript 

#####################################################################################
rm(list=ls())

rootDir <- "/home_ssd/dag/0-Annat/0-R/customers/mgh"

setwd( rootDir ) 
codeDir   <- file.path(rootDir, "code")
dataDir   <- file.path(rootDir, "data")
outputDir <- file.path(rootDir, "output")

dataDirHag <- file.path(dataDir, 'haggerty')
imageDir  <- file.path(dataDirHag, "images")
textDir   <- file.path(dataDirHag, "text")
dir.create(imageDir, showWarnings=F)
dir.create(textDir, showWarnings=F)

source(file.path(codeDir, "utils.r"))
source(file.path(codeDir, "db.r"))
source(file.path(codeDir, "pbutils.r"))

library(dplyr)
library(tidyr)
library(xml2)
library(stringr)
library(lubridate)
library(rorcid)

library(RefManageR)
library(rcrossref)
library(stringdist)
library(tm)
library(openNLP)

# need to call this such that we can access correct database
SQL_DATABASE_NAME <- 'mgh'

logfilename <- "readdocx.log"

# useful stackoverflow comments
# https://cran.r-project.org/web/packages/docxtractr/docxtractr.pdf
# https://stackoverflow.com/questions/16065952/how-do-i-create-a-corpus-of-docx-files-with-tm
# for writing word docs: http://www.sthda.com/english/wiki/create-and-format-word-documents-using-r-software-and-reporters-package

####################################################################
read_convert_docx_to_xml <- function(filenamex)
{
  tmpd <- tempdir()
  tmpf <- tempfile(tmpdir=tmpd, fileext=".zip")
  
  file.copy(filenamex, tmpf)
  unzip(tmpf, exdir=sprintf("%s/docdata", tmpd))
  
  doc <- read_xml(sprintf("%s/docdata/word/document.xml", tmpd))
  
  unlink(tmpf)
  unlink(sprintf("%s/docdata", tmpd), recursive=TRUE)

  return(doc)
}

####################################################################
parse_lists <- function(doc)
{
  # parse out list levels
  paragraph <- xml_find_all(doc, ".//w:p", ns=xml_ns(doc))
  list_level <- NA  
  d4 <- data.frame()
  state_inside_list <- FALSE
  parent_list <- rep(0, 10) # no more than 10 list levels
  parent <- 0
  #for(i in 16:25) {
  for(i in seq_len(length(paragraph))) {
    (pPr <- xml_find_all(paragraph[i], ".//w:pPr", ns=xml_ns(doc)))
    if(grepl("ListParagraph", pPr)) {
      #print("state_inside_list == TRUE")
      state_inside_list <- TRUE
      (numPr <- xml_find_all(pPr, ".//w:numPr", ns=xml_ns(doc)))
      if(length(numPr) > 0) {
        (ilvl  <- xml_find_first(numPr, ".//w:ilvl",  ns=xml_ns(doc)) %>% xml_attrs())
        (numId <- xml_find_first(numPr, ".//w:numId", ns=xml_ns(doc)) %>% xml_attrs())
        list_level <- as.integer(unlist(ilvl))
        #print(sprintf("%3d | ilvl: %2d | numId: %2d", i, as.integer(unlist(ilvl)), as.integer(unlist(numId))))
      } else {
        #print("state_inside_list == FALSE")
        state_inside_list <- FALSE
        list_level <- NA
      }
    }
    run_structure <- xml_find_all(paragraph[i], ".//w:r", ns=xml_ns(doc))
    if(length(run_structure) != 0) {
      s1 <- character()
      for(j in seq_len(length(run_structure))) {
        #print(paste(i, j))
        (text1 <- xml_find_all(run_structure[j], ".//w:t"))
        #print(text1)
        text1 <- ifelse(length(text1) != 0, xml_text(text1), "")
        s1 <- paste(s1, text1, sep="")
      }
      s1 <- trim(s1)
      s1 <- gsub("[ ]{2,}", " ", s1)
    } else {
      s1 <- ""
    }
    if(!is.na(list_level) & s1 != "") {
      # use list_level as 1 even if list_level 0 or 1
      
      tmp_list_level <- list_level+1
      #print(sprintf("1. paragraph: %2d list_level: %1d  tmp_list_level: %1d  parent: %2d  parent_list: %2d", i, list_level, tmp_list_level, parent, parent_list[tmp_list_level]))
      #print(parent_list)
      
      # parent is only zero the first time, thus we can use parent as a test for first time in the loop
      if(parent != 0) {
        parent <- parent_list[tmp_list_level]
      } else {
        parent_list[1] <- i
      }
      parent_list[tmp_list_level+1] <- i
      
      #print(parent_list)
      #print(sprintf("2. paragraph: %2d list_level: %1d  tmp_list_level: %1d  parent: %2d  parent_list: %2d", i, list_level, tmp_list_level, parent, parent_list[tmp_list_level]))
      d4 <- rbind(d4, data.frame(paragraph=i, list_level=list_level, parent=parent, text=s1, stringsAsFactors = F))
      #select(d4, paragraph, list_level, parent) %>% print()
      parent <- 1
    }
  }
  return(d4)
}
####################################################################
parse_text <- function(doc)
{
  # parse out list levels
  paragraph <- xml_find_all(doc, ".//w:p", ns=xml_ns(doc))
  d4 <- data.frame()
  for(i in seq_len(length(paragraph))) {
    (pPr <- xml_find_all(paragraph[i], ".//w:pPr", ns=xml_ns(doc)))
    run_structure <- xml_find_all(paragraph[i], ".//w:r", ns=xml_ns(doc))
    if(length(run_structure) != 0) {
      s1 <- character()
      for(j in seq_len(length(run_structure))) {
        #print(paste(i, j))
        (text1 <- xml_find_all(run_structure[j], ".//w:t"))
        #print(text1)
        text1 <- ifelse(length(text1) != 0, xml_text(text1), "")
        s1 <- paste(s1, text1, sep="")
      }
      s1 <- trim(s1)
      s1 <- gsub("[ ]{2,}", " ", s1)
    } else {
      s1 <- ""
    }
    if(s1 != "") {
      d4 <- rbind(d4, data.frame(type='paragraph', linenumber=i, text=s1, stringsAsFactors = F))
    }
  }
  return(d4)
}

####################################################################
parse_metadata <- function(doc)
{
  # good code to parse out full paragraphs and store sentences in a dataframe
  paragraph <- xml_find_all(doc, ".//w:p", ns=xml_ns(doc))
  d2 <- data.frame()
  for(i in seq_len(length(paragraph))) {
    run_structure <- xml_find_all(paragraph[i], ".//w:r", ns=xml_ns(doc))
    if(length(run_structure) != 0) {
      s1 <- character()
      for(j in seq_len(length(run_structure))) {
        #print(paste(i, j))
        (text1 <- xml_find_all(run_structure[j], ".//w:t"))
        #print(text1)
        text1 <- ifelse(length(text1) != 0, xml_text(text1), "")
        s1 <- paste(s1, text1, sep="")
      }
      s1 <- trim(s1)
      s1 <- gsub("[ ]{2,}", " ", s1)
    } else {
      s1 <- ""
    }
    if(grepl("SUMMARY", s1)) break
    d2 <- rbind(d2, data.frame(paragraph=i, text=s1, stringsAsFactors = F))
  }
  
  # we've parsed out the meta data section. Now parse out the keys and values
  #d2 = d2.save
  names(d2)
  d2 <- filter(d2, text != "")
  d3 <- data.frame(key="title", value=d2$text[1], stringsAsFactors = F)
  d2 <- d2[-1,]
  d4 <- separate(d2, text, into=c('key', 'value'), sep=":") %>% select(key, value) 
  d3 <- bind_rows(d3, d4)
  return(d3)
  
}

####################################################################
parse_summary <- function(doc, claim0_start)
{
  # good code to parse out full paragraphs and store sentences in a dataframe
  paragraph <- xml_find_all(doc, ".//w:p", ns=xml_ns(doc))
  d2 <- data.frame()
  for(i in seq_len(length(paragraph))) {
    run_structure <- xml_find_all(paragraph[i], ".//w:r", ns=xml_ns(doc))
    if(length(run_structure) != 0) {
      s1 <- character()
      for(j in seq_len(length(run_structure))) {
        #print(paste(i, j))
        (text1 <- xml_find_all(run_structure[j], ".//w:t"))
        #print(text1)
        text1 <- ifelse(length(text1) != 0, xml_text(text1), "")
        s1 <- paste(s1, text1, sep="")
      }
      s1 <- trim(s1)
      s1 <- gsub("[ ]{2,}", " ", s1)
    } else {
      s1 <- ""
    }
    if(grepl("SUMMARY", s1)) summary_paragraph_number <- i
    if(i > claim0_start) break ; 
    d2 <- rbind(d2, data.frame(paragraph=i, text=s1, stringsAsFactors = F))
  }
  #d2.save = d2
  #d2 <- d2.save
  d3 <- d2[(summary_paragraph_number+1):(claim0_start-1),]
  
  return(d3)
}

####################################################################
parse_concept_terms <- function(doc, verbose=F)
{
  run_structure <- xml_find_all(doc, ".//w:r", ns=xml_ns(doc))
  d1 <- data.frame()
  d2 <- data.frame()
  concept <- character()
  in_paran <- nin_bold <- FALSE
  for(i in seq_len(length(run_structure))) {
    if(length(run_structure[i]) == 0) next

    # figure out if we are bolding or not
    (boldtoken <- xml_find_all(run_structure[i], ".//w:b"))
    if(length(boldtoken) != 0) (in_bold <- is.na(xml_attr(boldtoken, 'w:val', ns=xml_ns(doc))))
    if(length(boldtoken) == 0) (in_bold <- FALSE)
    
    # figure out if we are paran'ing or not
    (texttoken <- xml_find_all(run_structure[i], ".//w:t"))
    (text1 <- ifelse(length(texttoken) != 0, xml_text(texttoken), ""))
    if(!in_paran & grepl('\\(', text1)) (in_paran <- TRUE)
    
    if(verbose) print(sprintf("TEST:  i: %2d  in_bold: %s  in_paran: %s  text: %s", i, in_bold, in_paran, text1))
    if(in_bold & in_paran) {
      if(verbose) print(sprintf("add to concept:  i: %2d  in_bold: %s  in_paran: %s  text: %s", i, in_bold, in_paran, text1))
      concept <- paste(concept, text1, sep="")
    }

    if(in_paran & grepl('\\)', text1)) {
      if(verbose) print(sprintf("save concept:  i: %2d  in_bold: %s  in_paran: %s  text: %s", i, in_bold, in_paran, text1))
      if(length(concept) > 0) {
        concept <- gsub("(\\(|\\))", "", concept)
        d2 <- rbind(d2, data.frame(endblock=i, concept=concept, stringsAsFactors = F))
        concept <- character()
      }
      in_paran <- FALSE
    }
      
    #if(length(text1) == 0) next
    d1 <- rbind(d1, data.frame(run=i, bold=in_bold, paran=in_paran, text=text1, stringsAsFactors = F))
  }
  d3 <- separate_rows(d2, concept, sep=',')
  
  return(d3)
}

####################################################################
#   para <- paragraph[i]
get_references_per_paragraph <- function(doc, para)
{
  (run_structure <- xml_find_all(para, ".//w:r", ns=xml_ns(doc)))
  
  if(length(run_structure) <= 1) {
    # if it is really tiny, then declare it as invalid and return NULL
    return(NULL)
  }
  
  d4 <- data.frame(bibentry=NA, authors=NA, title=NA, year=NA, journal=NA, pmid=NA, pmc=NA, doi=NA, stringsAsFactors=F)
  for(j in seq_len(length(run_structure))) {
    if(j == 1) {
      # the first entry is always the author (date) and title
      (meta <- xml_text(run_structure[j]))
      d4$bibentry <- meta
      d3 <- data.frame(meta=meta, stringsAsFactors=F)
      d3 <- separate(d3, col=meta, into=c('authors', 'title'), sep="\\([0-9]{4}\\)")
      year <- gsub("(\\(|\\))", "", str_extract_all(meta, "\\([0-9]{4}\\)"))
      d4$authors <- d3$authors
      d4$title <- d3$title
      d4$year <- year
    } else {
      d4$bibentry <- paste0(d4$bibentry, xml_text(run_structure[j]))
    }
    
    # check if we can find the name of the journal - always in italics
    if(length(xml_find_all(run_structure[j], ".//w:i", ns=xml_ns(doc))) > 0) {
      # found the italics. Now, get the name of the journal
      (journal <- xml_text(run_structure[j]))
      d4$journal <- journal
    }
    
    # find PMID
    if(length(xml_find_all(run_structure[j], ".//w:b", ns=xml_ns(doc))) > 0) {
      if(grepl("^PMID", xml_text(run_structure[j]))) {
        # ok, we found the pmid entry. Now, extract the 7-8 digit pmid from either this
        # entry or the next entry
        pmid <- str_extract_all(xml_text(run_structure[j]), "[0-9]{7,8}") %>% unlist()
        if(length(pmid) == 0 & j < length(run_structure)) {
          pmid <- str_extract_all(xml_text(run_structure[j+1]), "[0-9]{7,8}") %>% unlist()
        } else {
          pmid <- NA
        }
        d4$pmid <- ifelse(length(pmid) > 0, pmid, NA)
      }
    }
    
    # find PMC
    if(length(xml_find_all(run_structure[j], ".//w:b", ns=xml_ns(doc))) > 0) {
      if(grepl("PMC", xml_text(run_structure[j]))) {
        # found the PMC entry
        pmc <- str_extract_all(xml_text(run_structure[j]), "[0-9]{7,8}") %>% unlist()
        if(length(pmc) == 0 & j < length(run_structure)) {
          pmc <- str_extract_all(xml_text(run_structure[j+1]), "(C|^| )[0-9]{7,8}") %>% unlist()
        } else {
          pmc <- NA
        }
        d4$pmc <- ifelse(length(pmc) > 0, pmc, NA)
      }
    }
    
    # find doi
    if(grepl("doi.org", xml_text(run_structure[j]))) {
      # found the doi
      doi <- str_extract_all(xml_text(run_structure[j]), "doi.org.*$") %>% unlist()
      d4$doi <- ifelse(length(doi) > 0, doi, NA)
    }
  }
  return(d4)
}

####################################################################
# this function starts after the header REFERENCES and finds all the 
# references
parse_reference_list <- function(doc)
{
  paragraph <- xml_find_all(doc, ".//w:p", ns=xml_ns(doc))
  
  # find start of referencs, eg REFERENCES
  for(i in seq_len(length(paragraph))) {
    # loop until we find "REFERENCES"
    (token <- xml_find_all(paragraph[i], ".//w:r//w:t"))
    if(any(sapply(token, function(x) grepl("^<w:t>REFERENCES</w:t>$", x)))) break ;
  }
  start_of_refs <- i  # this is the paragraph number of the start of reference list
  
  # gather reference data
  d1 <- data.frame()
  #for(i in (start_of_refs+1):93) {
  for(i in (start_of_refs+1):length(paragraph)) {
    d4 <- get_references_per_paragraph(doc, paragraph[i])
    if(!is.null(d4)) d1 <- rbind(d1, d4)
  }
  return(d1)
}

####################################################################
find_references_in_text <- function(doc)
{
  paragraph <- xml_find_all(doc, ".//w:p", ns=xml_ns(doc))
  d1 <- data.frame(paragraph=rep(NA, length(paragraph)), 
    reference=rep(NA, length(paragraph)),stringsAsFactors=F)
  
  for(i in seq_len(length(paragraph))) {
    d1$paragraph[i] <- i
    run_structure <- xml_find_all(paragraph[i], ".//w:r", ns=xml_ns(doc))
    if(length(run_structure) == 0) next
    
    for(j in seq_len(length(run_structure))) {
      #print(paste(i, j))
      token <- xml_find_all(run_structure[j], ".//w:fldChar", ns=xml_ns(doc))
      if(length(token) == 0) next
      type <- xml_attr(token, 'w:fldCharType', ns=xml_ns(doc))
      if(type == 'separate') {
        refstring <- xml_find_all(run_structure[j+1], ".//w:t", ns=xml_ns(doc))
        refstring <- xml_text(refstring)
        if(grepl("REFERENCE", refstring)) next
        print(refstring)
        if(is.na(d1$reference[i])) {
          d1$reference[i] <- refstring
        } else {
          d1$reference[i] <- paste(d1$reference[i], refstring, sep=";")
        }
      }
    }
  }
  
  d2 <- filter(d1, !is.na(reference))
  
  return(d2)
}

####################################################################
get_shortname <- function(s1)
{
  # get short name for dlist list
  #dlists$shortname <- NA
  shortname <- rep(NA, length(s1))
  refid <- seq_len(length(s1))

  for(i in refid) {
    #print(i)

    #if(is.na(dlists$ref2[i])) next
    if(is.na(s1[i])) next

    #(dlists$shortname[i] <- str_extract(dlists$ref2[i], "^[A-z]*"))
    (shortname[i] <- str_extract(s1[i], "^[A-z]*"))

    # if the name is two character or less, then add one name
    #if(nchar(dlists$shortname[i]) <= 2) {
    #  dlists$shortname[i] <- str_extract(dlists$ref2[i], "^[A-z]* [A-z]*")
    #}

    if(nchar(shortname[i]) <= 2) {
      shortname[i] <- str_extract(s1[i], "^[A-z]* [A-z]*")
    }
  }
  return(list(shortname, refid))
}

####################################################################
connect_references <- function(dlists, reference_list)
{
  # combine reference list with dlists
  dlists$refid <- NA
  for(i in seq_len(nrow(dlists))) {
    if(is.na(dlists$shortname[i])) next
    for(j in seq_len(nrow(reference_list))) {
      if(dlists$shortname[i] == reference_list$shortname[j] &
         dlists$refyear[i] == reference_list$year[j]) {
        dlists$refid[i] <- reference_list$refid[j]
      }
    }
  }
  return(dlists)
}

####################################################################
store_doc_info <- function(doc, claim0_start)
{
  meta <- parse_metadata(doc)
  summary <- parse_summary(doc, claim0_start)
  summary <- paste(summary$text, collapse=" ")
  
  date1 <- parse_date_time(meta$value[meta$key == 'Date'], c('d m y', 'm d y'))
  (y1 <- year(date1))
  (m1 <- month.abb[month(date1)])
  
  # store in docs
  df <- data.frame(
    type           = 'massCATS', 
    title          = meta$value[meta$key == 'title'], 
    date           = meta$value[meta$key == 'Date'], 
    target         = meta$value[meta$key == 'Target'], 
    targetrational = meta$value[meta$key == 'Target Rationale and Evidence Outline'], 
    priorityscore  = meta$value[meta$key == 'Priority Score'], 
    status         = meta$value[meta$key == 'Status'], 
    projectID      = meta$value[meta$key == 'Project identifier'], 
    nominator      = meta$value[meta$key == 'Nominator'], 
    summary        = summary,
    year           = y1,
    month          = m1,
    stringsAsFactors = F)
  
  print(df$title)
  
  r <- insert_into_docs(df)
  docid <- r$docid[1]
  
  # store in author
  (x1 <- meta$value[meta$key == 'Nominator'])
  x1 <- trim(x1)
  (authname <- strsplit(x1, ",") %>% unlist())
  authname <- authname[1]
  (authname <- gsub("[[:punct:]]", "", authname))
  (x2 <- strsplit(authname, " ") %>% unlist())
  first <- x2[1]
  if(length(x2) == 2) {
    middle <- ""
    last <- x2[2]
  } else {
    middle <- x2[2]
    last <- x2[3]
  }
  
  #first <- 'Mickey'; last <- 'Mouse' ; middle <- 'express'
  print(paste(first, middle, last))
  print(paste(last, middle, last))
  
  # find title
  title <- str_extract(x1, ",.*\\|")
  (title <- gsub("(,[ ]*|[ ]*\\|)", "", title))
  
  # find institution
  (inst <- str_extract(x1, "\\|.*"))
  (inst <- gsub("\\|[ ]*", "", inst))
  
  # find author in database, and update if needed. If not found in database, then 
  # look at orcid
  dauth <- data.frame(last=last, first=first, middle=middle, orcid="", stringsAsFactors = F)
  authors1 <- deduping_authors(dauth)
  
  authors_already_in_database <- authors1 %>% filter(!is.na(authors1$found_auid))
  authors_not_in_database     <- authors1 %>% filter(is.na(authors1$found_auid))
  
  if(nrow(authors_not_in_database) > 0)
  {
    # add these authors to database
    dtmp <- authors_not_in_database %>% select(last, first, middle)
    #authors_not_in_database <- insert_into_authors(authors_not_in_database)
    authors_not_in_database <- insert_into_authors(dtmp)
  }
  # combine both list and insert into authdoc table
  authors_already_in_database$found_auid
  authors_not_in_database$auid
  auid <- c(authors_already_in_database$found_auid, authors_not_in_database$auid)
  
  if(all(!is.na(auid), length(auid) > 0)) {
    r <- calldb(paste0("select * from authors where auid=", auid, " ; "))
    if(nrow(r) == 1) {
      if((r$role == '' | is.na(r$role)) & length(title) > 0) 
        calldb(paste0("update authors set role = '", title, "' where auid = ", r$auid, " ; "))
      if((r$institution == '' | is.na(r$institution)) & length(title) > 0) 
        calldb(paste0("update authors set institution = '", inst, "' where auid = ", r$auid, " ; "))
    }
  }
  
  # insert into authdoc table
  if(length(auid) > 0 & length(docid) > 0) {
    d1 <- data.frame(docid=rep(docid, each=length(auid)),  auid=auid,  stringsAsFactors = F)
    insert_into_authdoc(d1)
  }
  return(docid)
}
  
####################################################################
store_text_block_info <- function(docid, doc)
{
  text1 <- parse_text(doc) %>% select(text, linenumber)
  names(text1)
  head(text1)
  insert_into_text(docid, 'paragraph', text1)
  
  i1 <- grep("SUMMARY", text1$text)
  i2 <- grep("REFERENCE", text1$text)
  df <- data.frame(abstract=0, intro=i1, method=0, result=0,discussion=0, conclusion=0, acknowledgment=0, reference=i2, stringsAsFactors = F)
  insert_into_blocks(docid, 'paragraph', df)
}


####################################################################
# s5 <- bibentry
get_bibentry <- function(s5)
{
  BibOptions(check.entries = FALSE) # sort by name, title, year

  #s5 <- reference_list$bibentry[which(reference_list$refid %in% dlists$refid[i])]
  
  bib <- my.ReadCrossRef(s5)
  if(is.null(bib)) {
    logdata(sprintf("No results found in CrossRef for \"%s\" (string length nchar: %d)", s5, nchar(s5)))
    return(NULL)
  }
  if(length(bib$author) == 0) {
    logdata("Author length is zero, therefore assume this bib is no good, so return")
    return(NULL)
  }
  
  # remove this because at times, it is two entries that causes the 'as.data.frame()' to blow up
  bib$`number.date-parts` <- NULL
  
  dftmp <- as.data.frame(bib)
  
  list1 <- determine_most_likely_bib(s5, dftmp)
  (index <- list1[[1]])
  (dftmp <- list1[[2]])
  # grab the most likely entry
  dftmp <- dftmp[index,]
  
  bib1 <- bib[index]

  return(bib1)
}


####################################################################
store_refs_info <- function(docid, dlists, reference_list)
{
  # 1. store first cut in the database table: refs
  # 2. get pubmed data and store in database
  # 3. from pubmed, store additional stuff in the database
  # 4. if we don't have a pmid, then store what we have in the database
  
  # 1. store first cut in the database table: refs
  no_pmids <- integer()
  for(i in seq_len(nrow(dlists))) {
    print(i)
    if(is.na(dlists$ref2[i])) next ;
    
    refs <- data.frame(docid=docid, sentencesum=dlists$paragraph[i], ref1=dlists$reference[i], refid=dlists$refid[i], type='paran', stringsAsFactors = F) 
    insert_into_refs(docid, refs)
    
    # insert bibentry
    bibentry <- reference_list$bibentry[which(reference_list$refid %in% dlists$refid[i])]
    bibentry <- gsub("'", "''", bibentry)
    (q1 <- paste0("update refs set bibentry = '", bibentry, "' where docid = ", docid, " and refid = '", dlists$refid[i], "';"))
    calldb(q1)
    
    # 2. go to pubmed get data, then store those new documents in the refs table, and in the docs and author and authdoc tables
    (pmid <- reference_list$pmid[which(reference_list$refid %in% dlists$refid[i])])
    
    if(is.na(pmid)) {
      no_pmids <- c(no_pmids, i)
      bib1 <- get_bibentry(bibentry)
      if(is.null(bib1)) next ;
      pbentry <- my.LookupPubMedID(bib1)
      pmid <- pbentry$eprint
    } else {
    	bib1 <- NULL
    }
    #next
    
    # store document and authors in docs and authors and authdoc tables
    #find_store_in_pubmed_pmid(docid, pmid)
    (newdocid <- find_store_in_pubmed_pmid(docid, pmid, bib2=bib1))

    #(d7 <- calldb(paste0("select docid, pmid, doi from docs where pmid = '", pmid, "' ;")))
    (d7 <- calldb(paste0("select docid, pmid, doi from docs where docid = '", newdocid, "' ;")))
  
    if(nrow(d7) == 0) {
      logdata("something really weird because d7 should never be zero rows")
      logdata("     Perhaps there was an error cought in find_store_in_pubmed_pmid()")
      logdata("     newdocid:", newdocid)
      return(NULL)
    }
  
    # update refs table by tying the new document IDs to the reference table
    if(!is.null(d7$docid)) {
      q1 <- paste0("update refs set refdocid=", d7$docid[1], " where docid=", docid, " and sentencenum=", dlists$paragraph[i], " and refid='", dlists$refid[i], "' ;")
      calldb(q1)
    }
    if(!is.null(d7$pmid)) {
      q1 <- paste0("update refs set refpmid=",  d7$pmid[1], " where docid=", docid, " and sentencenum=", dlists$paragraph[i], " and refid='", dlists$refid[i], "' ;")
      calldb(q1)
    }
    if(!is.null(d7$doi)) {
      q1 <- paste0("update refs set refdoi='",  d7$doi[1], "' where docid=", docid, " and sentencenum=", dlists$paragraph[i], " and refid='", dlists$refid[i], "' ;")
      calldb(q1)
    }
  }
}

####################################################################
store_claimtree_info <- function(docid, dlists)
{
  names(dlists)
  d1 <- dlists %>% select(paragraph, parent)
  
  # remove dups
  index <- which(duplicated(d1$paragraph))
  if(length(index) > 0) d1 <- d1[-index,]
  
  d1$docid <- d1$parentdocid <- docid
  d1$type <- d1$parenttype <- 'paragraph'
  d2 <- d1 %>% select(docid, type, linenumber=paragraph, parentdocid, parenttype, parentlinenumber=parent)
  # need to keep the current docid because it is referenced via foreign key constraint
  #d2$parentdocid[d2$parentlinenumber == 0] <- 0
  
  insert_into_claims(d2)
}

####################################################################
read_doc <- function(filenamex)
{
  doc <- read_convert_docx_to_xml(filenamex)
  dlists <- parse_lists(doc)
  claim0_start <- dlists$paragraph[1]    # first entry in claim list
  (docid <- store_doc_info(doc, claim0_start))
  store_text_block_info(docid, doc)
  
  #claim0_start <- which(!is.na(dlists$list_level))[1]    # first entry in claim list
  concepts <- parse_concept_terms(doc)
  reference_list <- parse_reference_list(doc)
  references <- find_references_in_text(doc)
  
  # Combine data lists (numeric bulleted lists inside MS Word docs) and
  # the references inside the numbered lists
  dlists <- full_join(dlists, references, by="paragraph") %>%
    mutate(ref2 = gsub("(\\(|\\))", "", reference)) %>%
    separate_rows(ref2, sep=";") %>%
    mutate(ref2 = trim(ref2)) %>%
    mutate(refyear = str_extract(ref2, "[0-9]{4}$"))
  
  list1 <- get_shortname(dlists$ref2)
  dlists$shortname  <- list1[[1]]
  list1 <- get_shortname(reference_list$authors)
  reference_list$refid <- list1[[2]]
  reference_list$shortname <- list1[[1]]
  
  dlists <- connect_references(dlists, reference_list)
  
  store_refs_info(docid, dlists, reference_list)

  store_claimtree_info(docid, dlists)

  write.csv(dlists, file.path(codeDir, "dlists.csv"), row.names=F)
  write.csv(reference_list, file.path(codeDir, "refs.csv"), row.names=F)
}

####################################################################
# main Main MAIN
filenamex <- file.path(dataDirHag, 'Haggarty.Tau.EvidenceModel_rev1_dh_v2.docx')

read_doc(filenamex)
#
