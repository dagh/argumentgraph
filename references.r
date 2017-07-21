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

library(dplyr)
#library(RNeo4j)
library(stringr)
library(tm)
library(openNLP)
#library(RWeka)
#library(SnowballC)

# need to call this such that we can access correct database
SQL_DATABASE_NAME <- 'mgh'

logfilename <- "references.log"

library(RefManageR)
library(rorcid)
library(rcrossref)
library(stringdist)

#####################################################################################
test_function1 <- function()
{
  # 0. get 100 random references
  p0 <- calldb("select * from bib limit 100 ;")
  write.csv(p0, file=file.path(outputDir, "ref_100_random.csv"), row.names=F)
  
  # 1. get top X documents and store in file
  p1 <- calldb("select count(*) as freq, url, doi, bibtype, title from bib group by url, doi, bibtype, title order by freq desc limit 100 ;")
  write.csv(p1, file=file.path(outputDir, "ref_top_100.csv"), row.names=F)
  
  # 2. get authors for the top 10 documents
  doi1 <- p1$doi[1:10]
  s1 <- paste0(doi1, collapse="','")
  p2 <- calldb(paste0("select * from authorbib where doi in ('", s1, "');"))
  write.csv(p2, file=file.path(outputDir, "ref_authors_of_top_100.csv"), row.names=F)
  
  # 3. get most prolific authors from the references
  p3 <- calldb("select count(*) as freq, last, first from authorbib group by last, first order by freq desc limit 100 ;")
  write.csv(p3, file=file.path(outputDir, "authors_top_100.csv"), row.names=F)
  
  #s5 <- "10.1001/archneur.56.3.303"
  #s5 <- "10.1098/rstb.2003.1358"
  s5 <- "10.1212/WNL.0b013e3181cb3e25"
  bib1 <- my.ReadCrossRef(s5)
  GetBibEntryWithDOI(s5)
  str(bib1)
  bib1$journal
  attributes(bib1$journal)
  attributes(bib1$bibtype)
  
  attr(bib1, bibtype)
  
  bib2 <- GetPubMedByID('20042704')
  str(bib2)
  bib2$journal
  bib2$eprinttype
  attributes(bib2$eprinttype)
  attributes(bib2)
}

#####################################################################################
#####################################################################################
# SECTION 1
#####################################################################################
#####################################################################################
parse_ref_numbers <- function(s1)
{
  #(s1 <- "1,2,3-5,7,9-12, 18:19")
  #(s1 <- "(1,2,3-5,7,9-12, 18:19)")
  #(s1 <- "[1,2,3-5,7,9-12, 18:19]")
  #(s1 <- "{1,2,3-5,7,9-12, 18:19}")
  
  # remove all square brackets, parans and curly brackets
  s1 <- gsub("(\\[|\\])|[(){}]", "", s1)
  
  (s2 <- str_split(s1, ",") %>% unlist() %>% trim())
  (s3 <- str_split(s2, "[-:]"))
  s7 <- integer()
  for(i in seq_len(length(s3))) {
    #print(i)
    if(length(s3[[i]]) > 1) {
      # need the is.na() to ensure we are not storing non-integers in the return string
      if(!is.na(as.integer(s3[[i]][1])) & !is.na(as.integer(s3[[i]][2])))
        s7 <- c(s7, seq( as.integer(s3[[i]][1]), as.integer(s3[[i]][2])))
    } else {
      if(!is.na(as.integer(s3[[i]][1])))
        s7 <- c(s7, as.integer(s3[[i]][1]))
    }
  }
  return(s7)
}

#####################################################################################
create_long_reference_dataframe <- function(d2)
{
  d2$start <- NULL
  x1 <- sapply(d2$refid, parse_ref_numbers)
  d7 <- data.frame()
  for(i in seq_len(nrow(d2))) {
    #print(i) ; print(d2$refid[i]) ; print(x1[i])
    len1 <- length(x1[[i]])
    d8 <- data.frame(docid=rep(d2$docid[i], len1), sentencenum=rep(d2$sentencenum[i], len1), refid=rep(d2$refid[i], len1), ref1=x1[[i]], stringsAsFactors=F)
    d7 <- rbind(d7,d8)
  }
  d7
  # figure that no documents have more than 1000 references so filter away anything more than 1000 (4 chars)
  d7 <- d7 %>% filter(ref1 < 1000)
  
  # remove all lines where the linecount is more than 20. The reason is that when you see 20+ references
  # on one line, then there is something strange. Normally, one line does not carry 20 references
  # a. summarize all lines counts in to a dataframe (d8) and remove all with line count greater than 20
  d8 <- d7 %>% group_by(sentencenum) %>% summarize(linecount=n()) %>% filter(linecount > 20)
  # b. remove all lines for which the linecount is greater than 20
  d9 <- d7 %>% filter(!(sentencenum %in% d8$sentencenum))
  
  names(d9) <- c("docid", "sentencenum", "ref1", "refid")
  
  return(d9)
}

#####################################################################################
parse_ref_name_year <- function(s1)
{
  #(s1 <- "1,2,3-5,7,9-12, 18:19")
  #(s1 <- "(1,2,3-5,7,9-12, 18:19)")
  #(s1 <- "[1,2,3-5,7,9-12, 18:19]")
  #(s1 <- "{1,2,3-5,7,9-12, 18:19}")
  
  (s1 <- "(Ghosh et al., 2005)")
  (s1 <- "(DiNuzzo et al., 2010; Swanson, 1992)")
  (s1 <- "(Brown et al., 2004; Dringen et al., 1993)")
  
  # remove all square brackets, parans and curly brackets
  s1 <- gsub("(\\[|\\])|[(){}]", "", s1)
  s1
  
  (s2 <- str_split(s1, ";") %>% unlist() %>% trim())
  (s2 <- str_split(s1, ";"))
  return(s2)
  s7 <- list()
  for(i in seq_len(length(s2))) {
    print(i)
    if(length(s2[[i]]) > 1) {
      # need the is.na() to ensure we are not storing non-integers in the return string
      s7 <- c(s7, seq( as.integer(s2[[i]][1]), as.integer(s2[[i]][2])))
    } else {
      if(!is.na(as.integer(s2[[i]][1])))
        s7 <- c(s7, as.integer(s2[[i]][1]))
    }
  }
  return(s7)
}

#####################################################################################
create_long_reference_dataframe_name_year <- function(d2)
{
  d2$start <- NULL
  d7 <- data.frame()
  for(i in seq_len(nrow(d2))) {
    (s1 <- gsub("(\\[|\\])|[(){}]", "", d2$refid[i]))
    (s2 <- str_split(s1, ";") %>% unlist() %>% trim())
    len1 <- length(s2)
    d8 <- data.frame(docid=rep(d2$doc[i], len1), sentencenum=rep(d2$sentencenum[i], len1), refid=rep(d2$refid[i], len1), ref1=s2, stringsAsFactors=F)
    d7 <- rbind(d7,d8)
  }
  names(d7) <- c("docid", "sentencenum", "ref1", "refid")
  
  return(d7)
}

#####################################################################################
get_references_dataframe <- function(docid, a1, name_year=F)
{
  # create a data frame
  d1 <- data.frame()
  for(i in seq_len(length(a1))){
    #print(i)
    len1 <- length(a1[[i]])
    #print(i)
    if(len1 > 0) {
      d2 <- data.frame(docid=rep(docid, len1), sentencenum=rep(i, len1), refid=a1[[i]], stringsAsFactors=F)
      # remove references that are probably "year", eg "(2007)"
      d2 <- d2 %>% filter(!(nchar(refid)==6 & substring(refid,2,5) >= 1900 & substring(refid,2,5) <= 2020))
      d1 <- rbind(d1, d2)
      rm(d2)
    }
  }
  
  if(nrow(d1) > 0) {
    # we found references
    # if this is name_year, eg "name, year; name, year...", then don't create the long reference
    if(name_year) {
      d2 <- create_long_reference_dataframe_name_year(d1)
      #d2 <- d1
      #d2$ref1 <- d2$refid
      #names(d2) <- c("docid", "sentencenum", "ref1", "refid")
    } else {
      d2 <- create_long_reference_dataframe(d1)
    }
  }
  
  if(nrow(d1) == 0) {
    # we found no references, so create an empty dataframe
    d2 <- data.frame(docid=as.integer(), sentencenum=as.character(), ref1=as.character(), refid=as.character())
  }
  return(d2)
}

#####################################################################################
get_references_dataframe_sup <- function(docid, a1)
{
  d1 <- data.frame()
  for(i in seq_len(length(a1))) {
    #print(i)
    len1 <- length(a1[[i]])
    if(len1 > 0) {
      d2 <- data.frame(docid=rep(docid, len1), sentencenum=rep(i, len1), refid=a1[[i]], stringsAsFactors=F)
      # remove references that are probably "year", eg "(2007)"
      d2 <- d2 %>% filter(!(nchar(refid)==6 & substring(refid,2,5) >= 1900 & substring(refid,2,5) <= 2020))
      d1 <- rbind(d1, d2)
      rm(d2)
    }
  }
  if(nrow(d1) > 0) {
    # remove letter, periods and anything else in front of the digits
    d1$start <- str_locate(d1$refid, "[0-9]")[,1]
    d1$refid <- substring(d1$refid, d1$start, nchar(d1$refid))
    # trim away spaces
    d1$refid <- trim(d1$refid)
    d1$start <- NULL
    # figure that no documents have more than 1000 references so filter away anything more than 1000 (4 chars)
    #d1 <- d1 %>% select(sentencenum, refid) %>% filter(nchar(refid) <= 3)
  }
  
  if(nrow(d1) > 0) {
    # we found references
    d2 <- create_long_reference_dataframe(d1)
  } else {
    # we found no references, so create an empty dataframe
    d2 <- data.frame(docid=as.integer(), sentencenum=as.character(), ref1=as.character(), refid=as.character())
  }
  
  return(d2)
}
  
#####################################################################################
#docname <- '20061650_McGeer_Neuroinflammation_in_Alzheimers.pdf' 
# docid=37
determine_reference_type <- function(docid)
{
  # because it is very unlikely that a sentence with carry more than 5 separate (each with its own parans) references, 
  # we will delete all those references with more than 5 entries. When you see more than five, that usually means
  # that a table with lots of entries inside parans is to blame
  max_references_per_line <- 5
  
  # get data from the database
  s1 <- calldb(paste0("select * from text where docid = ", docid, " and type = 'sent' order by linenumber ; "))

  # filter out all lines that are in the reference section
  start_of_reference <- calldb(paste0("select reference from blocks where docid = ", docid, " and type = 'sent' ; ")) %>% as.integer()
  
  # check if we need to go up one level in the document to find a start or references
  if(start_of_reference == 0) {
    start_of_reference <- calldb(paste0("select acknowledgment from blocks where docid = ", docid, " and type = 'sent' ; ")) %>% as.integer()
  }
  
  # check if we need to go up one level in the document to find a start or references
  if(start_of_reference == 0) {
    start_of_reference <- calldb(paste0("select conclusion from blocks where docid = ", docid, " and type = 'sent' ; ")) %>% as.integer()
  }

  # note that if we did not find a start of reference, then the reference line number is 0, so therefore, only do this when we have an 
  # actual reference number
  if(start_of_reference > 0) s1 <- s1 %>% filter(linenumber < start_of_reference)
  
  #----- 1. count square brackets references
  pattern <- "\\[[0-9,-]*\\]"
  count_square <- grep("\\[[0-9,-]*\\]", s1$text) %>% length()
  a1 <- str_extract_all(s1$text, pattern)
  # remove lines longer that X references per line
  for(i in seq_len(length(a1))) { if(length(a1[[i]]) > max_references_per_line)  a1[[i]] <- character() }
  dsquare <- get_references_dataframe(docid, a1)
  if(nrow(dsquare) > 0) dsquare$type <- 'square'
  
  #----- 2. count parans references
  pattern <- "\\([0-9,-]*\\)"
  inverse_pattern <- "\\([0-9]{4}\\)"
  index <- grep(pattern, s1$text)
  count_parans <- grep(inverse_pattern, s1$text[index], invert=T) %>% length() # remove all years, eg (2007) or (1989)
  a1 <- str_extract_all(s1$text, pattern)
  # remove lines longer that X references per line
  for(i in seq_len(length(a1))) { if(length(a1[[i]]) > max_references_per_line)  a1[[i]] <- character() }
  dparans <- get_references_dataframe(docid, a1)
  if(nrow(dparans) > 0) dparans$type <- 'paran'
  
  #----- 3. find (name, year; name, year; name, year)
  #pattern <- "\\([A-z]{1}[[:alnum:]\\., ;]*[0-9]{2,}\\)"
  
  # make sure that we are only look at years with 4 digits and that the there are at least
  #  two alpha characters after the first open parans
  #pattern <- "\\([A-z]{2}[[:alnum:]\\., ;]*[0-9]{4}\\)"
  #pattern <- "\\([A-z]{2}[[:alnum:]\\., ;-]*[0-9]{4}\\)" # added a dash to the pattern
  # added the back tick (unicode for accented 'e') adn the dash to the pattern
  #pattern <- "\\([A-z]{2}[[:alnum:]\\., ;\u00c9-]*[0-9]{4}\\)" 
  pattern <- "\\([A-z]{2}[[:alnum:]\\., ;\u00b4-]*[0-9]{4}\\)" 
  a1 <- str_extract_all(s1$text, pattern)
  # remove lines longer that X references per line
  for(i in seq_len(length(a1))) { if(length(a1[[i]]) > max_references_per_line)  a1[[i]] <- character() }
  count_name_year <- sapply(a1, length) %>% sum()
  dname_year <- get_references_dataframe(docid, a1, name_year=T)
  # remove some exceptions
  dname_year <- dname_year %>% filter(!grepl('IIRG', refid)) %>%
                               filter(!grepl('rTg4510', refid))
  
  if(0) {
    #s2.save <- s2
    s2 <- s2.save
    nchar(s2.save)
    (s2 <- substring(s2.save, 504, 662))
    #pattern <- "\\([A-z]{2}[[:alnum:]\\., ; ́e-]*[0-9]{4}\\)" 
    pattern <- "\\([A-z]{2}[[:alnum:]\\., ;é-]*[0-9]{4}\\)" 
    pattern <- "\\([A-z]{2}[[:alnum:]\\., ;\u00b4-]*[0-9]{4}\\)" 
    (s2 <- substring(s2.save, 504, 564))
    str_extract_all(s2, pattern)
    s2 <- paste("cognitive impairments in vitro and in vivo ",
          "(Walsh et al., 2002; Gong et al., 2003; Lesne ́ et al., 2006), and", 
          "this was also true in humans (Kuo et al., 1996; Shankar et al.,  2008; Noguchi et al., 2009). Therefore, ")
    str_extract_all(s2, pattern)
  }
    
    
  if(nrow(dname_year) > 0) dname_year$type <- 'nameyear'
  
  #----- 4. find sup and .sup
  s2 <- s1
  # there are terms that we don't want to use, so mask data that we don't want to match on
  term_mask <- c("amyloid142", "amyloid1-42", "bay94", "beta1", "18 f-thk", "htau40", "awr038", 
                 "apoe2", "apoe3", "apoe4", " e2", " e3", " e4", "/e2", "/e3", "/e4", "[ /]ab4", "9ra5", "[c2 ,]e1", 
                 "tc18", "pet15b", "pd10", "tecnanig2", "at8", "f/3d", "mg2", "ca2", "bl21de3", "\\[1", "3d6", "c57b", 
                 "[ e-]ab[0-9]", "amyloid4", "c99", "c6\\]", "t1/2", "[ ,-]ag[0-9]", "ly411", "hj5\\.", "[ 0]ns0[0-9]", 
                 "/[ ]*ps1", "thk523", "rtg4510", "a4", "kd1", "bmax1", "kd2", "bmax2", "k1828", "k18", "htou40",
                 "alz5", "phf1", "ca1[ ,\\.]", "p301l", "ht7", "5a6", "at180", "s[1-9][a-z),;.]", " ca3", " cp13", "cy3-", " da9",
                 "iba1",
                 "aqp4", "d200", "[-]d[0-9]", "\\[[0-9]", "ng2-", "7ra1", "tie2-", "a594", "-d7", "b14",
                 "e693d", "ad8k", "[1,2,3]e2", "v717l", " nu1", "gse[0-9]", "satb2", "tbr1",
                 "cd33", "rs38", "bv2", "trem2", "[0-9 -][abc]0", "k7r", "de9 ", "aa0", "abca7", "cd2ap", "epha1", "ms4", "[0-9 ]e5",
                 "[( ]m[0-9]", "[ -]u[0-9]", "psen1", "[:;-]d[0-9]", "cdk[0-9]", "[0-9:]e[0-9]", "gsk3", "cxxc1",
                 "mct[0-9]", "sc[0-9]",
                 "k9ja", "s26c", "aw7", "21f12", "div[127]", "ser2", "at270", "pcdh1", "plenti6", "fugene6",
                 "ps[2346]", "pi3k", "p85", "py[169]", "/jci5", "akt[12]", "glut[0-9]", "t2d", "f5,", "erk[0-9]", "[ (p][sy][0-9]",
                 "pt18", "pip3", "jnk[1 ,]")
  # the masking is done by subbing out the "faulty" term and subbing in "=="
  for(tmask in term_mask) s2$text <- gsub(tmask, "==", tolower(s2$text))
  pattern <- "([a-z]|AD|MRI|PET|ADNI|MCI){1}[\\.,]*[0-9]+[-,]*[0-9,-]*( |$)"
  a1 <- str_extract_all(s2$text, pattern, simplify=F)

  # remove lines longer that X references per line
  for(i in seq_len(length(a1))) { if(length(a1[[i]]) > max_references_per_line)  a1[[i]] <- character() }
  count_sup <- sapply(a1, length) %>% sum()
  dsup <- get_references_dataframe_sup(docid, a1)
  if(nrow(dsup) > 0) dsup$type <- 'sup'
  
  #----- 5. log the data
  log_data(sprintf("row-count: %3d  square: %3d  paran: %3d  name_year: %3d  sup: %3d", 
    docid, nrow(dsquare), nrow(dparans), nrow(dname_year), nrow(dsup)))
  log_data(sprintf(" my-count: %3s  square: %3d  paran: %3d  name_year: %3d  sup: %3d", 
    "", count_square, count_parans, count_name_year, count_sup))
  
  s3 <- c(nrow(dsquare), nrow(dparans), nrow(dname_year), nrow(dsup))
  names(s3) <- c("dsquare", "dparans", "dname_year", "dsup")
  namex <- names(s3)[which(s3 == max(s3))]
  
  if(length(namex) > 1) {
    log_data("WARNING - select more than one reference type:\"", paste(namex, collapse=", "), "\" - picking the first one")
    namex <- namex[1]
  }
  
  log_data("selected reference type:", namex)
  if(length(namex) == 1) {
    if(namex == 'dsquare') {
      insert_into_refs(docid, dsquare, insert=T)
    } else  if(namex == 'dparans') {
      insert_into_refs(docid, dparans, insert=T)
    } else  if(namex == 'dname_year') {
      insert_into_refs(docid, dname_year, insert=T)
    } else  if(namex == 'dsup') {
      insert_into_refs(docid, dsup, insert=T)
    } else {
      log_data("Error - namex not found")
    } 
  } else { 
    log_data("Error - more than one reference types found:", namex)
  }

  d2 <- data.frame(docid=docid, dsquare=nrow(dsquare), dparans=nrow(dparans), 
    dname_year=nrow(dname_year), dsup=nrow(dsup), count_square=count_square, 
    count_parans=count_parans, count_name_year=count_name_year, count_sup=count_sup, toptype=namex,
    stringsAsFactors=F)

  return(d2)
}

#####################################################################################
find_references <- function(pdfDocs) 
{
  disconnect_all_mysql_connections()
  
  log_data("count from current database")

  if(!is.null(pdfDocs)) {
    # pdfDocs is not NULL, then make sure that the file exists in the database before we proceed
    x1 <- calldb(paste0("select filename from docs where filename = '", pdfDocs, "';"))
    if(nrow(x1) == 0) {
      log_data("Error: file ", pdfDocs, "does not exists in the database - exiting")
      return(NULL)
    }
  }
  if(is.null(pdfDocs)) pdfDocs <- calldb("select filename from docs where filename <> ''")$filename 

  d3 <- data.frame()
  for(i in seq_len(length(pdfDocs))) {
    # doc <- "23434393_Kondo_Modelling_Alzheimers_Disease_with_IPSCs.pdf"
    doc <- pdfDocs[i]
    docinfo <- calldb(paste0("select docid, pmid, journal from docs where filename = '", doc, "' ;"))

    if(nrow(docinfo) == 0) {
      log_data("WARNING - did not find a document in the database for docname:", doc)
      next ;
    }
    log_data("-------------------------------------------------------------------------")
    log_data(i, " docid:", docinfo$docid, " document in process: ", doc)

    d3 <- rbind(d3, determine_reference_type(docinfo$docid))
  }
  write.csv(d3, file=file.path(codeDir, "d3_8.csv"), row.names=F)

  log_data("count from current database")
}

#####################################################################################
#####################################################################################
# SECTION 2
#####################################################################################
#####################################################################################
store_references_in_db <- function(docid, refid, s3)
{
  # store in database
  for(i in seq_len(length(refid))) {
    bibentry <- gsub("'", "''", s3[i])
    q1 <- paste0("update refs set bibentry = '", bibentry, "' where docid = ", docid, " and refid = '", refid[i], "';")
    calldb(q1)
  }

  d8 <- data.frame(docid=rep(docid, length(s3)), refid=refid, ref=s3, stringsAsFactors = F) %>% filter(refid < 1000)

  d8
}

#####################################################################################
parse_nameyear <- function(docid, d1, start_of_reference)
{
  # Step A. From the reference list in the document, parse out each bibentry
  
  #d1 <- calldb(paste0("select * from text where docid = ", docid, " and type = 'sent' order by linenumber ; "))
  #start_of_reference <- calldb(paste0("select reference from blocks where docid = ", docid, " and type = 'sent' ; ")) %>% as.integer()
  
  d1 <- calldb(paste0("select * from text where docid = ", docid, " and type = 'lines' order by linenumber ; "))
  start_of_reference <- calldb(paste0("select reference from blocks where docid = ", docid, " and type = 'lines' ; ")) %>% as.integer()
  
  # remove some exceptions
  # 1. normally, a bib entry does not contains a 'pipe' symbol
  d1 <- d1 %>% filter(!grepl('\\|', d1$text))
  
  # do lines as a test
  if(length(start_of_reference)) d1 <- d1[c((start_of_reference+1):nrow(d1)),]
  
  head(d1)
  d2 <- data.frame()
  j <- 1
  s1 <- character()
  for(i in seq_len(nrow(d1))) {
    #print(i)
    s2 <- trim(d1$text[i])
    if(grepl("[0-9]{1}\\.$", s2)) {
      s1 <- paste(s1, s2)
      dtmp <- data.frame(i=i, text=s1, stringsAsFactors=F)
      d2 <- rbind(d2, dtmp)
      j <- j + 1
      s1 <- character()
    } else {
      s1 <- paste(s1, s2)
    }
  }
  d2$text <- trim(d2$text)
  
  # d2 now contains the reference list with each bibentry on it's own line
  
  # Step B. Get the bib entries and match those to the references in d2
  d1 <- calldb(paste0("select docid, refid from refs where docid = ", docid, " ;"))
  head(d1)
  head(d2)
  # get first name from both data frames
  d1$first <- sapply(strsplit(d1$refid, "[ ,]"), '[[', 1)
  #d1$year  <- str_extract_all(d1$refid, "[0-9]{4}") %>% unlist()
  d1 <- d1 %>% mutate(y1 = str_extract_all(refid, "[0-9]{4}")) %>% 
    mutate(y2=paste(y1)) %>%
    separate_rows(y2, sep="[ ,]") %>%
    filter(y2 != '') %>%
    mutate(year = gsub("[^0-9]", "", y2)) %>% 
    select(-y2)
  d1$combo <- paste(d1$first, d1$year, sep=":")
  
  d2$first <- sapply(strsplit(d2$text, "[ ,]"), '[[', 1)
  d2$year  <- str_extract_all(d2$text, "[0-9]{4}") %>% gsub("(\\(|\\))", "", .)
  d2$combo <- paste(d2$first, d2$year, sep=":")
  
  if(0) {
    #d2.save <- d2
    d2 <- d2.save
    d2 <- d2.save[2:4,]
    d2$first <- sapply(strsplit(d2$text, "[ ,]"), '[[', 1)
    
    #convert to integer vector
    d2 <- d2 %>% mutate(y1 = str_extract_all(text, "[0-9]{4}")) %>%
      mutate(y2 = map_chr(y1, function(x) {paste(x, collapse=":")})) %>%
      separate_rows(y2, sep=":") %>%
      mutate(y2 = as.integer(y2)) %>%
      filter(between(y2, 1980, 2022))
    
    
    
  }
  d3 <- full_join(d1, d2, by="combo")

  return(list(refid=d3$refid, s3=d3$text))
  
}

#####################################################################################
parse_nameyear_old <- function(d1, start_of_reference)
{
  d2 <- d1[(start_of_reference+1):nrow(d1),]
  a1 <- d2$text
  index <- grep("this is a sentence", tolower(a1))
  if(length(index) > 0) a1 <- a1[-index]
  a1
}


#####################################################################################
#   docname <- "20042704_Petersen_ADNI_Clinical_characterization.pdf"
#   docname <- "20061650_McGeer_Neuroinflammation_in_Alzheimers.pdf"
#   docname <- "20616000_Miller_Divergence_of_human_and_mouse.pdf"
#   docname <- "21436112_Fodero-Tavoletti_18F-ThK523_a_novel_in_vivo_tau_imaging_ligand.pdf"
#   docname <- "21532579_Bero_Neuronal_activity_regulates.pdf"
# docid=13

parse_references <- function(docid)
{
  d1 <- calldb(paste0("select * from text where docid = ", docid, " and type = 'sent' order by linenumber ; "))
  start_of_reference <- calldb(paste0("select reference from blocks where docid = ", docid, " and type = 'sent' ; ")) %>% as.integer()
  reftype <- calldb(paste0("select type from refs where docid = ", docid, " limit 1 ; ")) %>% as.character()
  
  if(reftype == 'nameyear') {
    retval <- parse_nameyear(docid, d1, start_of_reference)
    d8 <- store_references_in_db(docid, retval$refid, retval$s3)
    return(d8)
  }
  
  d2 <- d1[(start_of_reference+1):nrow(d1),]
  a1 <- d2$text
  # remove "this is a sentence" if it exists
  index <- grep("this is a sentence", tolower(a1))
  if(length(index) > 0) a1 <- a1[-index]
  
  if(reftype == 'square') {
    # if there are more than 20% of square brackets, then this is square bracket type of reference list
    # so, if there is a square brackets that is not the first character on a new line, then make 
    # it the first character
    # This is a bit screwy. First we paste all references into one long string (a2)
    # then we split on the open square brackets
    # then add a new first square brackets (the first one is gone during the string split)
    # Then if there is only a square bracket in a string (length == 1), then remove that entry
    # Then trim and set back to a1 again
    a2 <- paste(a1, collapse=" ")
    a3 <- strsplit(a2, '\\[') %>% unlist()
    a4 <- paste0('[', a3)
    index <- which(nchar(a4) == 1)
    if(length(index) > 0) a4 <- a4[-index]
    a4 <- sapply(a4, trim) %>% as.character()
    a1 <- a4
  }
  
  #a1.save <- a1
  #a1 <- a1.save[87:length(a1.save)]
  
  s1 <- s2 <- as.character()
  j <- -1
  for(i in seq_len(length(a1))) {
    #print(i)
    #print(a1[i])
    if(grepl("^([0-9]+|\\[[0-9]+)", a1[i])) {
      # store the old line
      j <- j + 1
      if(j != 0) s2[j] <- s1
      #print(paste("s2: ", s2[j]))
      # new reference line
      (s1 <- a1[i])
    } else {
      s1 <- paste(s1, a1[i])
    }
  }
  j <- j + 1
  s2[j] <- s1
  
  # ------------
  # clean up 
  for(j in seq_len(length(s2))) {
    #print(sprintf("j: %d  nchar(s2): %d", j, nchar(s2[j])))
    max_characters <- 300
    if(nchar(s2[j]) > max_characters) {
      # something weird here. A normal reference is not this large - try to clean up
      # search for a 2 digits followed by a period in the range of say 70 - 300. That should 
      # be the end of the citation.
      min_characters <- 70
      #print(s2[j])
      n1 <- str_locate_all(s2[j], "[0-9]{2}\\.")
      #n1 <- str_locate_all(s2[j], "foobar.")
      
      if(length(n1[[1]]) > 0) {
        m1 <- matrix(unlist(n1), ncol=2, byrow=F)
        # sort the matrix and take the highest number found
        index <- (which((m1[,1] > min_characters & m1[,1] < max_characters)) %>% sort(decreasing=T))[1]
        if(!is.na(index)) s2[j] <- substring(s2[j], 1, m1[index,2])
      }
      if(nchar(s2[j]) > max_characters) {
        # if it still larger than just chop it off
        s2[j] <- substring(s2[j], 1, max_characters)
      }
    }
  }
  
  # ------------
  
  refid <- str_extract_all(s2, "^([0-9]+|\\[[0-9]+)")  %>% unlist()
  refid <- sapply(refid, function(x) gsub("\\[|\\]|\\.", "", x)) %>% as.integer()
  class(refid)

  s3 <- gsub("^([0-9]+|\\[[0-9.]+\\])[ .]*", "", s2)
  s3 <- gsub("- ", "", s3)
  
  d8 <- store_references_in_db(docid, refid, s3)
  
  return(d8)
}

######################################################################################
if(0) { path='tmp'; pmid=3; s5 = s3[i] }
parse_reference_list <- function(pdfDocs)
{
  if(!is.null(pdfDocs)) {
    # pdfDocs is not NULL, then make sure that the file exists in the database before we proceed
    x1 <- calldb(paste0("select filename from docs where filename = '", pdfDocs, "';"))
    if(nrow(x1) == 0) {
      log_data("Error: file ", pdfDocs, "does not exists in the database - exiting")
      return(NULL)
    }
  }
  if(is.null(pdfDocs)) pdfDocs <- calldb("select filename from docs where filename <> ''")$filename 

  d3 <- data.frame()
  for(i in seq_len(length(pdfDocs))) {
    # doc <- "23434393_Kondo_Modelling_Alzheimers_Disease_with_IPSCs.pdf"
    doc <- pdfDocs[i]
    docinfo <- calldb(paste0("select docid, pmid, journal from docs where filename = '", doc, "' ;"))

    if(nrow(docinfo) == 0) {
      log_data("WARNING - did not find a document in the database for docname:", doc)
      next ;
    }
    log_data("-------------------------------------------------------------------------")
    log_data(i, " docid:", docinfo$docid, " document in process: ", doc)

    d3 <- rbind(d3, parse_references(docinfo$docid))
  }
  write.csv(d3, file=file.path(codeDir, "reflist3.csv"), row.names=F)
  unique(d3$docname) 
  df1 <- table(d3$docname) %>% as.data.frame(stringsAsFactors=F) %>% arrange(Var1)
  write.csv(df1, file=file.path(codeDir, "df2.csv"), row.names=F)
}
                                
#####################################################################################
#####################################################################################
# SECTION 3
#####################################################################################
#####################################################################################

#####################################################################################
# find_store_in_bib(p1$docid[i], p1$sentencenum[i], p1$refid[i], p1$bibentry[i])
if(0) {
  docid=p1$docid[i]
  sentencenum = p1$sentencenum[i]
  refid = p1$refid[i]
  s5 = p1$bibentry[i]
}
find_store_in_bib <- function(docid, sentencenum, refid, s5)
{
  BibOptions(check.entries = FALSE) # sort by name, title, year
  
  bib <- my.ReadCrossRef(s5)
  if(is.null(bib)) {
    log_data(sprintf("No results found in CrossRef for \"%s\" (string length nchar: %d)", s5, nchar(s5)))
    return(NULL)
  }
  if(length(bib$author) == 0) {
    log_data("Author length is zero, therefore assume this bib is no good, so return")
    return(NULL)
  }
  
  # remove this because at times, it is two entries that causes the 'as.data.frame()' to blow up
  bib$`number.date-parts` <- NULL
  
  dftmp <- as.data.frame(bib)
  
  list1 <- determine_most_likely_bib(s5, dftmp)
  index <- list1[[1]]
  index
  dftmp <- list1[[2]]
  dftmp
  # grab the most likely entry
  dftmp <- dftmp[index,]
  
  bib1 <- bib[index]
  pbentry <- my.LookupPubMedID(bib1)
  find_store_in_pubmed_pmid(docid, pbentry$eprint, bib2=bib1)
  
  #refpmid <- pbentry$eprint
  #refdoi <- bib1$doi
  (d7 <- calldb(paste0("select docid, pmid, doi from docs where doi = '", bib1$doi, "' ;")))
  
  if(nrow(d7) == 0) {
    log_data("something really weird because d7 should never be zero rows")
    log_data("     Perhaps there was an error cought in find_store_in_pubmed_pmid()")
    log_data("     bib1$doi:", bib1$doi)
    return(NULL)
  }
  
  # update refs table by tying the new document IDs to the reference table
  if(!is.null(d7$docid)) {
    q1 <- paste0("update refs set refdocid=", d7$docid, " where docid=", docid, " and sentencenum=",sentencenum, " and refid='", refid, "' ;")
    calldb(q1)
  }
  if(!is.null(d7$pmid)) {
    q1 <- paste0("update refs set refpmid='", d7$pmid, "' where docid=", docid, " and sentencenum=",sentencenum, " and refid='", refid, "' ;")
    calldb(q1)
  }
  if(!is.null(d7$doi)) {
    q1 <- paste0("update refs set refdoi='", d7$doi, "' where docid=", docid, " and sentencenum=",sentencenum, " and refid='", refid, "' ;")
    calldb(q1)
  }
}


#####################################################################################
# aaaa
find_bibentry <- function(pdfDocs)
{
  if(!is.null(pdfDocs)) {
    q2 <- paste0("select * from refs where bibentry is not NULL and bibentry <> '' ",
                 "and docid in ", 
                 "(select docid from docs where filename = '", pdfDocs, "') ;")
  } else {
    q2 <- "select * from refs where bibentry is not NULL and bibentry <> '' ;"
  }
  
  p1 <- calldb(q2)
  
  d3 <- data.frame()
  for(i in seq_len(nrow(p1))) {
    #log_data("-------------------------------------------------------------------------")
    log_data(i, "of", nrow(p1), "- docid:", p1$docid[i], " sentencenum: ", p1$sentencenum[i], 
      " bibentry:", substring(p1$bibentry[i], 1, 40), " refpmid:", p1$refpmid[i])
    if(!is.na(p1$refpmid[i])) next ;
    find_store_in_bib(p1$docid[i], p1$sentencenum[i], p1$refid[i], p1$bibentry[i])
  }
}

#####################################################################################
usage <- function()
{
  log_data("Error: error in parameters ")
  log_data("Usage: ./references.r all <filename>                  :filename is optional")
  log_data("Usage: ./references.r find_references <filename>      :filename is optional")
  log_data("Usage: ./references.r parse_reference_list <filename> :filename is optional")
  log_data("Usage: ./references.r find_bibentry <filename>        :filename is optional")
  log_data("")
  log_data("Example: ./references.r all /home_ssd/dag/0-Annat/0-R/customers/mgh/data/top50alz/20042704_Petersen_ADNI_Clinical_characterization.pdf")
  log_data("")
}

#####################################################################################
# MAIN main Main
#
# list of test cases for options
#./references.r 
#./references.r find_references
#./references.r donald_duck
#./references.r find_references good-file-name
#./references.r find_references bad-file-name
#./references.r donald_duck good-file-name
#./references.r donald_duck bad-file-name
#

log_data("==================== START OF RUN =================")
t1 <- proc.time()
get_count_from_tables()

option_list <- c('all', 'find_references', 'parse_reference_list', 'find_bibentry')

pdfDir <- pdfDocs <- filenamex <- option1 <- NULL

log_data("command line options - count:", length(commandArgs()))
#print(commandArgs())

# note that when there are no arguments, the length is 4
# but when there are arguments, number 5 is "--args" so
# therefore the first argument is number 6 (a bit weird)

run_process <- TRUE
if( length( commandArgs() ) == 4 ) {         
  option1 <- "all"                           
} else if( length( commandArgs() ) == 6 ) {
  option1 <- commandArgs()[6]
  if(!(option1 %in% option_list)) {
    usage()
    run_process <- FALSE
  } 
} else if( length( commandArgs() ) == 7 ) {
  option1 <- commandArgs()[6]
  if(!(option1 %in% option_list)) {
    usage()
    run_process <- FALSE
  } else {
    filenamex <- commandArgs()[7]
    if(!file.exists(filenamex))  {
      log_data("Error: file", filenamex, "does not exists - exiting")
      log_data("NOTE - we need absolute addresses to files")
      usage()
      run_process <- FALSE
    } else {
      pdfDocs <- basename(filenamex)
      pdfDir <- dirname(filenamex)
    }
  }
}

if(!run_process) option1 <- "bad parameters"

log_data("option1: ", option1)

if(option1 == 'find_references') {
  log_data("1. find references in all documents, and store the first cut in the refs database table")
  find_references(pdfDocs)

} else if(option1 == 'parse_reference_list') {
  log_data("2. parse out the bib entries and store in database")
  parse_reference_list(pdfDocs)

} else if(option1 == 'find_bibentry') {
  log_data("3. find and distribute bibliographic entries")
  find_bibentry(pdfDocs)

} else if(option1 == 'all') {
   
  log_data("4. do all options")
  find_references(pdfDocs)
  parse_reference_list(pdfDocs)
  find_bibentry(pdfDocs)
} 

get_count_from_tables()
s <- proc.time()[3] - t1[3]
s <- as.integer(s)
h <- s %/% 3600 ; s <- s - (h*3600) ; 
m <- s %/% 60   ; s <- s - (m*60)   ; 
log_data("")
log_data(sprintf("process time: %d hours %d minutes %d seconds", h, m, s))
log_data("==================== END OF RUN =================")

