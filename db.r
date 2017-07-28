#!/usr/bin/env Rscript 

#####################################################################################
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(RMySQL))


#######################################################################
calldb <- function(q1, set_utf8=FALSE)
{
  con <- dbConnect(RMySQL::MySQL(), dbname=SQL_DATABASE_NAME, host="127.0.0.1")
  if(set_utf8==TRUE) dbGetQuery(con, "SET NAMES 'utf8'")

  r <- data.frame()
  success = FALSE
  while(success == FALSE) {
    tryCatch( {
      ##### NOTE - supressing warnings because it constantly warns about retrieving numerics from unsigned ints
      r <- suppressWarnings(dbGetQuery(con, q1))
      success <- TRUE
    }, error = function( e ) {
      if(grepl("Duplicate entry", conditionMessage(e))) {
        logdata(paste0("calldb() - trying to insert duplicate entry - this is not a problem for us"))
      } else {
        logdata(paste0("Error in calldb() - error message: \"", conditionMessage(e)))
      }
      success <<- TRUE
    } )
  }

  dbDisconnect(con)
  return(r)
}

#######################################################################
# NOTE - "overwrite" overwrites the whole table - so be careful with that one
#
# use overwrite=F and append=F for new creating new tables
# use overwrite=F and append=T for normal adding records to a table
# use overwrite=T and append=F for overwriting a table - don't use it unless you need to
# overwrite=T AND append=T is illegal
calldbw <- function(d2, table=NA, append=T, set_utf8=FALSE)
{
  con <- dbConnect(RMySQL::MySQL(), dbname=SQL_DATABASE_NAME, host="127.0.0.1")
  if(set_utf8==TRUE) dbGetQuery(con, "SET NAMES 'utf8'")
  
  r <- data.frame()
  success = FALSE
  while(success == FALSE) {
    tryCatch( {
      ##### NOTE - supressing warnings because it constantly warns about retrieving numerics from unsigned ints
      #r <- suppressWarnings(dbGetQuery(con, q1))
      r <- dbWriteTable(con, name=table, value=d2, append=append, row.names=F )
      success <- TRUE
    }, error = function( e ) {
      if(grepl("Duplicate entry", conditionMessage(e))) {
        logdata(paste0("calldb() - trying to insert duplicate entry - this is not a problem for us"))
      } else {
        logdata(paste0("Error in calldb() - error message: \"", conditionMessage(e)))
      }
      success <<- TRUE
    } )
  }
  
  dbDisconnect(con)
  return(r)
}

#######################################################################
disconnect_all_mysql_connections <- function()
{
  all_cons <- dbListConnections(MySQL())
  logdata(paste("Before disconnect - number of connnections:", length(all_cons)))
  for(con in all_cons) dbDisconnect(con)
  all_cons <- dbListConnections(MySQL())
  logdata(paste(" After disconnect - number of connnections:", length(all_cons)))
} 

#######################################################################
delete_from_tables <- function()
{
  for(t in c('text', 'blocks', 'authdoc', 'authors', 'docs')) {
    logdata("delete data from table:", t)
    calldb(paste0("delete from ", t, ";"))
  }
}

#######################################################################
get_count_from_tables <- function()
{
  for(t in c('docs', 'authors', 'authdoc', 'blocks', 'text', 'refs', 'images', 'imagerefs')) {
    countx <- calldb(paste0("select count(*) from ", t, ";")) %>% as.integer()
    logdata(sprintf("table count for %8s is %3d rows", t, countx))
  }
}

#######################################################################
insert_into_docs <- function(d1, insert=T)
{
  logdata(sprintf("insert_into_docs - rows: %s", nrow(d1)))

  if(is.null(d1$type)) d1$type <- 'standard'

  # change single quotes to two single quotes
  d2 <- lapply(d1, function(x) gsub("'", "''", x))  %>% as.data.frame(stringsAsFactors=F)
  r <- calldbw(d2, table='docs')
  
  d4 <- lapply(d2, function(x) gsub("'", "''", x))  %>% as.data.frame(stringsAsFactors=F)

  # need to return the docid for each entry
  d3 <- data.frame()
  for(i in seq_len(nrow(d1))) {
    if(d4$type == 'standard') {
      q1 <- paste0("select * from docs where pmid = '", d4$pmid[i], "' ", "and doi = '", d4$doi[i], "' ", "and title = '", d4$title[i], "' ");
      #q1 <- paste0("select * from docs where pmid = '", d1$pmid[i], "' ;");
    } else {
      # massCATS
      q1 <- paste0("select * from docs ", 
             "where title='", d1$title, "' ",  
             "and nominator='", d1$nominator, "' ",
             "and targetrational='", d1$targetrational, "' ",
             "and status='", d1$status,  "' ;")
    }
    
    d3 <- rbind(d3, suppressWarnings(calldb(q1)))
  }
  return(d3)
}


#######################################################################
insert_into_authors <- function(d1, insert=T)
{
  logdata(sprintf("insert_into_authors - rows: %s", nrow(d1)))

  d2 <- lapply(d1, function(x) gsub("'", "''", x))  %>% as.data.frame(stringsAsFactors=F)

  r <- calldbw(d2, table='authors')
  
  # need to return the authorid for each entry
  d3 <- data.frame()
  for(i in seq_len(nrow(d2))) {
    q1 <- paste0("select * from authors where last = '", d2$last[i], "' ",
      "and first = '", d2$first[i], "' ",
      "and middle = '", d2$middle[i], "' ");
    d3 <- rbind(d3, suppressWarnings(calldb(q1)))
  }
  return(d3)
}

#######################################################################
insert_into_authdoc <- function(d1, insert=T)
{
  logdata(sprintf("insert_into_authdoc - rows: %s  insert: %s", nrow(d1), insert))
  table_fields <- "(docid, auid)"

  insert_replace <- ifelse(insert, 'insert', 'replace')

  d2 <- d1

  s1 <- paste(d2$docid, d2$auid, sep="','")
  s2 <- paste0("('", s1, "')")
  s3 <- paste(s2, collapse=",")
  q1 <- paste0(insert_replace, " into authdoc ", table_fields, "  values ", s3, ";")
  calldb(q1) 
}


#######################################################################
insert_into_blocks <- function(docid, type, df, insert=T)
{
  for(i in seq_len(nrow(df))) {
    logdata(sprintf(
      "insert_into_blocks - docid: %d  type: %s  abstract: %d  intro: %d  method: %d", docid, type, df$abstract[i], df$intro[i], df$method[i])) 
  
    logdata(sprintf(
      "                     result: %d  discussion: %d  conclusion: %d  acknowledgment: %d reference: %d  rows: %d",
      df$result[i], df$discussion[i], df$conclusion[i], df$acknowledgment[i], df$reference[i], nrow(df)))
  }

  table_fields <- "(docid, type, abstract, intro, method, result, discussion, conclusion, acknowledgment, reference)"
  insert_replace <- ifelse(insert, 'insert', 'replace')

  s1 <- paste(docid, type, df$abstract, df$intro, df$method, df$result, df$discussion, df$conclusion, df$acknowledgment, df$reference, sep="','")
  s2 <- paste0("('", s1, "')")
  s3 <- paste(s2, collapse=",")
  q1 <- paste0(insert_replace, " into blocks ", table_fields, "  values ", s3, ";")
  calldb(q1) 
}

#######################################################################
insert_into_refs <- function(docid, refs, insert=T)
{
  logdata(sprintf("insert_into_refs - docid: %d  insert: %s rows: %d", docid, insert, nrow(refs)))
  
  if(nrow(refs) == 0) {
    logdata("WARNING: zero rows in refs. Returning")
    return(NULL)
  }

  names(refs) <- c('docid', 'sentencenum', 'ref1', 'refid', 'type')
  
  # remove duplicate docid:sentencenum:refid combo
  refs1 <- refs %>% mutate(combo=paste(docid, sentencenum, refid, sep=":")) %>%
                          distinct(combo, .keep_all=T) %>%
                          select(-combo)
  
  table_fields <- "(docid, sentencenum, ref1, refid, type)"
  insert_replace <- ifelse(insert, 'insert', 'replace')
  refs1 <- lapply(refs1, function(x) gsub("'", "''", x))  %>% as.data.frame(stringsAsFactors=F)

  s1 <- paste(refs1$docid, refs1$sentencenum, refs1$ref1, refs1$refid, refs1$type, sep="','")
  s1 <- unique(s1)
  s2 <- paste0("('", s1, "')")
  s3 <- paste(s2, collapse=",")
  q1 <- paste0(insert_replace, " into refs ", table_fields, "  values ", s3, ";")
  calldb(q1) 
}

#######################################################################
insert_into_images <- function(docid, df, insert=T)
{
  logdata(sprintf("insert_into_images - docid: %d  insert: %s rows: %d", docid, insert, nrow(df)))
  
  if(nrow(df) == 0) {
    logdata("WARNING: zero rows in images. Returning")
    return(NULL)
  }
  if(any(duplicated(df$imagefile))) {
    logdata("db.r: WARNING - image_meta$imagefile is not unique - will not be able to write to database - docid:", docid)
    return(NULL)
  }

  table_fields <- "(capid, imagefile, docid, page, type, linenumber, caption, checksum)"
  insert_replace <- ifelse(insert, 'insert', 'replace')
  df <- lapply(df, function(x) gsub("'", "''", x))  %>% as.data.frame(stringsAsFactors=F)

  s1 <- paste(df$capid, df$imagefile, df$docid, df$page, df$type, df$linenumber, df$caption, df$checksum, sep="','")

  s2 <- paste0("('", s1, "')")
  s3 <- paste(s2, collapse=",")
  q1 <- paste0(insert_replace, " into images ", table_fields, "  values ", s3, ";")
  calldb(q1) 
}

#######################################################################
update_images <- function(docid, df)
{
  logdata(sprintf("update_images - docid: %d rows: %d", docid, nrow(df)))
  
  if(nrow(df) == 0) {
    logdata("WARNING: update_images() - zero rows in images. Returning")
    return(NULL)
  }

  df <- lapply(df, function(x) gsub("'", "''", x))  %>% as.data.frame(stringsAsFactors=F)

  for(i in seq_len(nrow(df))) {
    imagefile <- df$imagefile[i] 
    calldb(paste0("update images set capid      = ",  df$capid[i],      " where imagefile = '", imagefile, "';"))
    calldb(paste0("update images set type       = '", df$type[i],       "' where imagefile = '", imagefile, "';"))
    calldb(paste0("update images set linenumber = ",  df$linenumber[i], " where imagefile = '", imagefile, "';"))
    calldb(paste0("update images set caption    = '", df$caption[i],    "' where imagefile = '", imagefile, "';"))
  }
}

#######################################################################
insert_into_imagerefs <- function(docid, df, insert=T)
{
  logdata(sprintf("insert_into_imagerefs - docid: %d  insert: %s rows: %d", docid, insert, nrow(df)))
  
  if(nrow(df) == 0) {
    logdata("WARNING: zero rows in imagerefs. Returning")
    return(NULL)
  }

  #table_fields <- "(capid, docid, type, linenumber, imageid, actual_figs)"
  table_fields <- "(capid, docid, type, linenumber, actual_figs)"

  insert_replace <- ifelse(insert, 'insert', 'replace')
  df <- lapply(df, function(x) gsub("'", "''", x))  %>% as.data.frame(stringsAsFactors=F)

  #s1 <- paste(df$capid, df$docid, df$type, df$linenumber, df$imageid, df$actual_figs, sep="','")
  s1 <- paste(df$capid, df$docid, df$type, df$linenumber, df$actual_figs, sep="','")

  s2 <- paste0("('", s1, "')")
  s3 <- paste(s2, collapse=",")
  q1 <- paste0(insert_replace, " into imagerefs ", table_fields, "  values ", s3, ";")
  calldb(q1) 
}

#######################################################################
insert_into_pubmed <- function(doc, path, df, insert=T)
{
  logdata(sprintf("insert_into_pubmed - doc: %s  insert: %s", doc, insert))
  table_fields <- "(pmid, doc, path, bibtype, title, year, month,
    journal, volume, number, pages, eprint, doi, language, issn, abstract)"

  insert_replace <- ifelse(insert, 'insert', 'replace')
  df <- lapply(df, function(x) gsub("'", "''", x))  %>% as.data.frame(stringsAsFactors=F)

  s1 <- paste(df$eprint, doc, path, df$bibtype, df$title,
    df$year, df$month, df$journal, df$volume, df$number,
    df$pages, df$eprint, df$doi, df$language, df$issn, df$abstract, sep="','")
  
  names(df)
  
  s2 <- paste0("('", s1, "')")
  s3 <- paste(s2, collapse=",")
  q1 <- paste0(insert_replace, " into pubmed ", table_fields, "  values ", s3, ";")
  calldb(q1) 
}

#######################################################################
insert_into_authorpb <- function(doc, pmid, df, insert=T)
{
  logdata(sprintf("insert_into_authorpb - doc: %s  pmid: %s  rows: %d  insert: %s", doc, pmid, nrow(df), insert))
  table_fields <- "(pmid, doc, last, first, middle, role, email, comment)"

  insert_replace <- ifelse(insert, 'insert', 'replace')
  df <- lapply(df, function(x) gsub("'", "''", x))  %>% as.data.frame(stringsAsFactors=F)

  s1 <- paste(pmid, doc, df$last, df$first, df$middle, df$role, df$email, df$comment, sep="','")

  s2 <- paste0("('", s1, "')")
  s3 <- paste(s2, collapse=",")
  q1 <- paste0(insert_replace, " into authorpb ", table_fields, "  values ", s3, ";")
  calldb(q1) 
}

#######################################################################
insert_into_bib <- function(doc, pmid, path, df, insert=T)
{
  logdata(sprintf("insert_into_bib - doc: %s  insert: %s", doc, insert))

  table_fields <- "(pmid, doc, bibtype, title, subtitle, date,
    volume, number, pages, url, doi, issn,
    score, journal_title)"

  if(!is.null(df$title)) {
    if(!is.na(df$title)) {
      if(nchar(df$title) > 510) {
        logdata("Warning: title is", nchar(df$title), "characters - chopping")
        df$title <- substring(df$title, 1, 510)
      }
    }
  }

  if(!is.null(df$subtitle)) {
    if(!is.na(df$subtitle)) {
      if(nchar(df$subtitle) > 510) {
        logdata("Warning: subtitle is", nchar(df$subtitle), "characters - chopping")
        df$subtitle <- substring(df$subtitle, 1, 510)
      }
    }
  }

  insert_replace <- ifelse(insert, 'insert', 'replace')
  df <- lapply(df, function(x) gsub("'", "''", x))  %>% as.data.frame(stringsAsFactors=F)

  s1 <- paste(pmid, doc, df$bibtype, df$title, df$subtitle, df$date,
    df$volume, df$number, df$pages, df$url, df$doi, df$issn,
    df$score, df$journal_title, sep="','")
  
  names(df)
  
  s2 <- paste0("('", s1, "')")
  s3 <- paste(s2, collapse=",")
  q1 <- paste0(insert_replace, " into bib ", table_fields, "  values ", s3, ";")
  calldb(q1) 
}

#######################################################################
insert_into_authorbib <- function(doc, pmid, doi, df, insert=T)
{
  logdata(sprintf("insert_into_authorbib - doc: %s  pmid: %s  rows: %d  insert: %s", doc, pmid, nrow(df), insert))
  table_fields <- "(doi, pmid, doc, last, first, middle, role, email, comment)"

  insert_replace <- ifelse(insert, 'insert', 'replace')
  df <- lapply(df, function(x) gsub("'", "''", x))  %>% as.data.frame(stringsAsFactors=F)

  s1 <- paste(doi, pmid, doc, df$last, df$first, df$middle, df$role, df$email, df$comment, sep="','")

  s2 <- paste0("('", s1, "')")
  s3 <- paste(s2, collapse=",")
  q1 <- paste0(insert_replace, " into authorbib ", table_fields, "  values ", s3, ";")
  calldb(q1) 
}

#######################################################################
# insert_into_text(doc, 'lines', lines, insert=F)
insert_into_text <- function(docid, type, text, insert=T)
{
  logdata(sprintf("insert_into_text - docid: %d, type: %s  insert: %s  rows: %d", docid, type, insert, nrow(text)))
	
	# if this is not from readdocx, where type is paragraph, then change column header
	if(type != 'paragraph')  names(text) <- 'text'
  
  # chop of text lines/sentences that are longer than 4096 characters. I think this can be done
  # safely, under the assumptions that no "real" sentences are longer that 4096 characters.
  if(max(nchar(text$text)) > 4095) logdata("WARNING: sentence over 4095 characters")
  text$text <- substring(text$text, 1, 4095)
  
  # if this is not from readdocx, where we already have the paragraph number, then add the line number
  if(is.null(text$linenumber)) text$linenumber <- seq(1:nrow(text))  # need linenumber for primary key
  
  insert_replace <- ifelse(insert, 'insert', 'replace')
  table_fields <- "(docid, type, linenumber, text)"
  
  text$text <- gsub("'", "''", text$text)
  
  start <- 1
  end <- nrow(text)
  step <- 10000
  for(i in seq(start, end, by=step)) {
    j <- min(end, (i + step - 1))
    tmp <- text[i:j,]
    
    s1 <- paste(docid, type, tmp$linenumber, tmp[,1], sep="','")
    s2 <- paste0("('", s1, "')")
    s3 <- paste(s2, collapse=",")
    q1 <- paste0(insert_replace, " into text ", table_fields, "  values ", s3, ";")
    calldb(q1) 
  }
}

#######################################################################
insert_into_claims <- function(d1, insert=T)
{
  logdata(sprintf("insert_into_claims - rows: %s", nrow(d1)))
  r <- calldbw(d1, table='claims')
}

