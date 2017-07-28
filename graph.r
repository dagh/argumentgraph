#!/usr/bin/env Rscript 

#####################################################################################
rm(list=ls())

rootDir <- "/home_ssd/dag/0-Annat/0-R/customers/mgh"

setwd( rootDir ) 
codeDir   <- file.path(rootDir, "code")
dataDir   <- file.path(rootDir, "data")
outputDir <- file.path(rootDir, "output")

source(file.path(codeDir, "utils.r"))
source(file.path(codeDir, "graph_utils.r"))
source(file.path(codeDir, "db.r"))

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(RNeo4j))
suppressPackageStartupMessages(library(RMySQL))
suppressPackageStartupMessages(library(stringr))

logfilename <- "graph.log"

# need to call this such that we can access correct database
SQL_DATABASE_NAME <- 'mgh'
query_sep <- " with count(*) as dummy "

#####################################################################################
build_doc_node <- function(db, dbname, docid)
{
  # use to combine multiple cypher inserts into one
  d1 <- calldb(paste0("select * from docs where docid='", docid, "' ;"))
  
  # remove double single quotes
  d1 <- lapply(d1, function(x) gsub("''", "'", x))  %>% as.data.frame(stringsAsFactors=F)
  
  # manage double quotes
  d1 <- lapply(d1, function(x) gsub("\"", "\\\\\\\"", x))  %>% as.data.frame(stringsAsFactors=F)
  
  # create doc node 
  query_all <- as.character(NULL)
  (q1 <- paste0("merge (n:", dbname, ":", DOCNODE, " {",
               "docid:'", d1$docid, "', ", 
               "pmid:'", d1$pmid, "', ", 
               "pmc:'", d1$pmcid, "', ", 
               "filename:'", d1$filename, "', ", 
               "title:\"", d1$title, "\", ",         # DON'T FORGET THE COMMA AFTER THE 2ND TO LAST PARAMETER
               "year:'", d1$year, "', ", 
               "month:'", d1$month, "', ", 
               "date:'", d1$date, "', ", 
               "journal:\"", d1$journal, "\", ", 
               "volume:'", d1$volume, "', ", 
               "number:'", d1$number, "', ", 
               "pages:'", d1$pages, "', ", 
               "doi:'", d1$doi, "', ", 
               "type:'", d1$type, "', ", 
               "abstract:\"", d1$abstract, "\" ",         # DON'T FORGET THE COMMA AFTER THE 2ND TO LAST PARAMETER
               "}) ",
               "on create set n.count = 1 ",
               "on match set n.count = n.count+1 ;"))
  query_all <- paste0(query_all, query_sep, q1)

  cypher(db, query_all) 
}

#####################################################################################
build_aut_node <- function(db, dbname, docid)
{
  
  # 1. build author node 
  d1 <- calldb(paste0("select a.* ",
    "from authdoc ad ",
    "left join authors a on ad.auid = a.auid ", 
    "left join docs d on ad.docid = d.docid ", 
    "where ad.docid=", docid, "; "))

  if(nrow(d1) == 0) {
    logdata("WARNING - no authors for docid:", docid)
    return(NULL)
  }
  
  d1 <- lapply(d1, function(x) gsub("'", "\\\\'", x))  %>% as.data.frame(stringsAsFactors=F)
  
  for(i in seq_len(nrow(d1))) {
    # 1. create author node
    query_all <- as.character(NULL)
    (q1 <- paste0("merge (n:", dbname, ":", AUTNODE, " {",
                  "auid:'", d1$auid[i], "', ", 
                  "last:'", d1$last[i], "', ", 
                  "first:'", d1$first[i], "', ", 
                  "middle:'", d1$middle[i], "', ", 
                  "orcid:'", d1$orcid[i], "', ", 
                  "orcscore:'", d1$orcscore[i], "', ", 
                  "orcdoiverified:'", d1$orcdoiverified[i], "' ", 
                  "}) ",
                  "on create set n.count = 1 ",
                  "on match set n.count = n.count+1 "))
    query_all <- paste0(query_all, query_sep, q1)

    # 2. find the doc node, then attach author node to doc node

    q1 <- paste0("match (n:", dbname, ":", DOCNODE, "{docid:'",      docid, "'}),", 
                       "(m:", dbname, ":", AUTNODE, "{ auid:'", d1$auid[i], "'})", 
                 "merge (n)-[r:", DOC_AUT_REL, "]->(m) ",
                 "on create set r.count = 1 ")

    query_all <- paste0(query_all, query_sep, q1)
    cypher(db, query_all) 
  }
}

#####################################################################################
build_txt_for_docs_node <- function(db, dbname, docid, refid, type='sent')
{
  
  if(length(refid) == 0) {
    logdata("WARNING - build_txt_for_docs_node - there are no references for docid:", docid)
    return(NULL)
  }
  
  q2 <- paste(refid, collapse=",")
  d1 <- calldb(paste0("select distinct r.refid, t.* ",
                      "from text t ",
                      "inner join refs r on r.sentencenum = t.linenumber ",
                      "where t.docid='", docid, "' and t.type='", type, "' ", 
                      "and r.docid='", docid, "' and r.refid in (", q2, ") ;"))
  
  if(nrow(d1) == 0) {
    logdata("WARNING - no text for docid:", docid)
    return(NULL)
  }
  
  d1 <- lapply(d1, function(x) gsub("'", "\\\\'", x))  %>% as.data.frame(stringsAsFactors=F)

  for(i in seq_len(nrow(d1))) {
    # 1. create rtxt node 
    query_all <- as.character(NULL)
    (q1 <- paste0("merge (n:", dbname, ":", RTXTNODE, " {",
                  "docid:'", d1$docid[i], "', ", 
                  "type:'", d1$type[i], "', ", 
                  "linenumber:'", d1$linenumber[i], "', ", 
                  "text:\"", d1$text[i], "\" ", 
                  "}) ",
                  "on create set n.count = 1 ",
                  "on match set n.count = n.count+1 "))
    query_all <- paste0(query_all, query_sep, q1)
    
    # 2. Connect rtxt node to doc node 
    q1 <- paste0("match (n:", dbname, ":", RTXTNODE, "{docid:'", docid, "', type:'", d1$type[i], "', linenumber:'", d1$linenumber[i], "' }),", 
                 "(m:", dbname, ":", DOCNODE, "{ docid:'", docid, "'})", 
                 "merge (m)-[r:", DOC_RTXT_REL, "]->(n) ",
                 "on create set r.count = 1 ")
    
    query_all <- paste0(query_all, query_sep, q1)
    cypher(db, query_all) 
  }
}

#####################################################################################
build_ref_node <- function(db, dbname, docid, refid, type='sent')
{
  
  if(length(refid) == 0) {
    logdata("WARNING - build_ref_node - there are no references for docid:", docid)
    return(NULL)
  }
  
  q2 <- paste(refid, collapse=",")
  
  d1 <- calldb(paste0("select distinct docid,sentencenum, ref1, refid, ",
                      "bibentry, refdocid, refpmid, refdoi ",
                      "from refs ",
                      "where docid='", docid, "' and refdocid is not NULL and refid in (", q2, ") ;"))
  
  if(nrow(d1) == 0) {
    logdata("WARNING - no references for docid:", docid)
    return(NULL)
  }

  d1 <- lapply(d1, function(x) gsub("'", "\\\\'", x))  %>% as.data.frame(stringsAsFactors=F)
  
  for(i in seq_len(nrow(d1))) {
    # 1. create author node
    query_all <- as.character(NULL)
    (q1 <- paste0("merge (n:", dbname, ":", REFNODE, " {",
                  "docid:'", d1$docid[i], "', ", 
                  "refid:'", d1$refid[i], "', ", 
                  "bibentry:'", d1$bibentry[i], "', ", 
                  "refdocid:'", d1$refdocid[i], "', ", 
                  "refpmid:'", d1$refpmid[i], "', ", 
                  "refdoi:'", d1$refdoi[i], "' ", 
                  "}) ",
                  "on create set n.count = 1 ",
                  "on match set n.count = n.count+1 "))
    query_all <- paste0(query_all, query_sep, q1)
    
    
    q1 <- paste0("match (n:", dbname, ":", RTXTNODE, "{docid:'", docid, "', type:'", type, "', refid:'", d1$refid[i], "' }), ", 
                 "(m:", dbname, ":", REFNODE, "{ docid:'", docid, "', refid:'", d1$refid[i], "' }) ", 
                 "merge (n)-[r:", RTXT_REF_REL, "]->(m) ",
                 "on create set r.count = 1 ")
    
    query_all <- paste0(query_all, query_sep, q1)
    cypher(db, query_all) 
    
    # build doc node for each ref node
    build_doc_node(db, dbname, d1$refdocid[i])
    # find the doc node, then attach author node to doc node
    build_aut_node(db, dbname, d1$refdocid[i])
    
    query_all <- as.character(NULL)
    # attach ref node to new doc node
    q1 <- paste0("match (n:", dbname, ":", REFNODE, "{refdocid:'", d1$refdocid[i], "' }),", 
                 "(m:", dbname, ":", DOCNODE, "{ docid:'", d1$refdocid[i], "'})", 
                 "merge (n)-[r:", REF_DOC_REL, "]->(m) ",
                 "on create set r.count = 1 ")
    query_all <- paste0(query_all, query_sep, q1)
    cypher(db, query_all) 
  }
}

#####################################################################################
build_txt_and_ref <- function(db, dbname, docid, refid, type)
{
  if(length(refid) == 0) {
    logdata("WARNING - build_txt_and_ref - there are no references for docid:", docid)
    return(NULL)
  }
  
  q2 <- paste(refid, collapse=",")
  d1 <- calldb(paste0("select distinct r.sentencenum, r.refid, r.bibentry, r.refdocid, r.refpmid, r.refdoi, t.* ",
                      "from text t ",
                      "inner join refs r on r.sentencenum = t.linenumber ",
                      "where t.docid='", docid, "' and t.type='", type, "' ", 
                      "and r.docid='", docid, "' and r.refid in (", q2, ") ;"))
  
  #write.csv(d1, file.path(codeDir, "d1.csv"),row.names=F)
  
  if(nrow(d1) == 0) {
    logdata("WARNING - no text for docid:", docid)
    return(NULL)
  }
  
  d1 <- lapply(d1, function(x) gsub("'", "\\\\'", x))  %>% as.data.frame(stringsAsFactors=F)
  
  #write.csv(d1, file=file.path(codeDir, "d1.csv"), row.names=F)

  for(i in seq_len(nrow(d1))) {
    if(is.na(d1$refdocid[i])) next ; 

    # 1. create rtxt node
    query_all <- as.character(NULL)
    logdata("----- i:", i, "create rtxt node: linenumber:", d1$linenumber[i])
    (q1 <- paste0("merge (n:", dbname, ":", RTXTNODE, " {",
                  "docid:'", d1$docid[i], "', ", 
                  "type:'", d1$type[i], "', ", 
                  "linenumber:'", d1$linenumber[i], "', ", 
                  "text:\"", d1$text[i], "\" ", 
                  "}) ",
                  "on create set n.count = 1 ",
                  "on match set n.count = n.count+1 "))
    query_all <- paste0(query_all, query_sep, q1)
    # No need for a bibentry in the rtxt node: "bibentry:\"", d1$bibentry[i], "\", ", 

    if(0) {
    # don't build the ref node at all. It is not useful for the graph
    # 2. create the ref node
    logdata("i:", i, "create ref node: linenumber:", d1$refid[i])
    (q1 <- paste0("merge (n:", dbname, ":", REFNODE, " {",
                  "docid:'", d1$docid[i], "', ", 
                  "refid:'", d1$refid[i], "', ", 
                  "bibentry:'", d1$bibentry[i], "', ", 
                  "refdocid:'", d1$refdocid[i], "', ", 
                  "refpmid:'", d1$refpmid[i], "', ", 
                  "refdoi:'", d1$refdoi[i], "' ", 
                  "}) ",
                  "on create set n.count = 1 ",
                  "on match set n.count = n.count+1 "))
    query_all <- paste0(query_all, query_sep, q1)
    }
    
    if(0) {
    # don't build the ref node at all. It is not useful for the graph
    # 3. connect rtxt node with ref node
    logdata("i:", i, "connect rtxt with ref node")
    q1 <- paste0("match (n:", dbname, ":", RTXTNODE, " {docid:'", docid, "',  type:'", d1$type[i],   "', linenumber:'", d1$linenumber[i], "' }),", 
            "(m:", dbname, ":", REFNODE, " {docid:'", docid, "', refid:'", d1$refid[i], "'}) ", 
            "merge (n)-[r:", RTXT_REF_REL, "]->(m) ",
            "on create set r.count = 1 ", 
            "on match set r.count = r.count+1 ")
    query_all <- paste0(query_all, query_sep, q1)
    }

    # 4. connect rtxt to doc node 
    logdata("i:", i, "connect rtxt with doc node")
    q1 <- paste0("match (n:", dbname, ":", RTXTNODE, " {docid:'", docid, "', type:'", d1$type[i], "', linenumber:'", d1$linenumber[i], "' }), ", 
                 "(m:", dbname, ":", DOCNODE, " {docid:'", docid, "'}) ", 
                 "merge (m)-[r:", DOC_RTXT_REL, "]->(n) ",
                 "on create set r.count = 1 ",
                 "on match set r.count = r.count+1 ")
    query_all <- paste0(query_all, query_sep, q1)
    
    logdata("i:", i, " build doc node for refdocid:", d1$refdocid[i])
    build_doc_node(db, dbname, d1$refdocid[i])
    logdata("i:", i, " build author nodes for refdocid:", d1$refdocid[i])
    build_aut_node(db, dbname, d1$refdocid[i])
    
    if(0) {
    # don't build the ref node at all. It is not useful for the graph
    # attach ref node to new doc node
    logdata("i:", i, "connect ref with new doc node - refdocid:", d1$refdocid[i])
    q1 <- paste0("match (n:", dbname, ":", REFNODE, "{refdocid:'", d1$refdocid[i], "' }),", 
                 "(m:", dbname, ":", DOCNODE, "{ docid:'", d1$refdocid[i], "'})", 
                 "merge (n)-[r:", REF_DOC_REL, "]->(m) ",
                 "on create set r.count = 1 ")
    query_all <- paste0(query_all, query_sep, q1)
    }

    # attach rtxt node to new doc node - this is a way of bypassing the ref node
    logdata("i:", i, "connect rtxt with new doc node - refdocid:", d1$refdocid[i])
    q1 <- paste0("match (n:", dbname, ":", RTXTNODE, " {docid:'", docid, "', type:'", d1$type[i], "', linenumber:'", d1$linenumber[i], "' }), ", 
                 "(m:", dbname, ":", DOCNODE, "{ docid:'", d1$refdocid[i], "'})", 
                 "merge (n)-[r:", RTXT_DOC_REL, "]->(m) ",
                 "on create set r.count = 1 ")
    query_all <- paste0(query_all, query_sep, q1)

    cypher(db, query_all) 
  }
}

#####################################################################################
# this is a continuation of the rtxt graph
build_claims_graph <- function(db, dbname, docid, type)
{
  # aaaa
  # we have to union with the zero claim because it does not have a parent but we still need it.
  d1 <- calldb(paste0("select c.docid, c.type, c.linenumber, t1.text as text, ", 
                      "  c.parentdocid, c.parenttype, c.parentlinenumber,  'parenttext' ", 
                      "from claims c ", 
                      "inner join text t1 on c.docid=t1.docid and c.type=t1.type and c.linenumber=t1.linenumber ", 
                      "where c.docid=", docid, " and c.parentlinenumber = 0 ", 
                      "union ", 
                      "select c.docid, c.type, c.linenumber, t1.text as text, ",
                      "  c.parentdocid, c.parenttype, c.parentlinenumber, t2.text as parenttext  ", 
                      "from claims c ", 
                      "inner join text t1 on c.docid=t1.docid and c.type=t1.type and c.linenumber=t1.linenumber ", 
                      "inner join text t2 on c.parentdocid=t2.docid and c.parenttype=t2.type and c.parentlinenumber=t2.linenumber ", 
                      "where c.docid=", docid, " ;"))

  if(nrow(d1) == 0) {
    logdata("WARNING - build_claims_graph() - no data for docid:", docid)
    return(NULL)
  }
  
  d1 <- lapply(d1, function(x) gsub("'", "\\\\'", x))  %>% as.data.frame(stringsAsFactors=F)
  
  # note that we need to create rtxt nodes here. These are nodes that do not have a reference to a document but 
  # they are still needed for the claims graph.

  for(i in seq_len(nrow(d1))) {
    logdata("i:", i, "|docid:", d1$docid[i], "|type:", d1$type[i], "|linenumber:", d1$linenumber[i], "|text:", substring(d1$text[i], 1, 30))
    query_all <- as.character(NULL)

    if(d1$parentlinenumber[i] == 0) {
      # if there is no parent, then make connection to document node
      (q1 <- paste0("merge (n:", dbname, ":", RTXTNODE, " {",
                    "docid:'", d1$docid[i], "', ", 
                    "type:'", d1$type[i], "', ", 
                    "linenumber:'", d1$linenumber[i], "', ", 
                    "text:\"", d1$text[i], "\" ", 
                    "}) ",
                    "on create set n.count = 1 ",
                    "on match set n.count = n.count+1 "))
      query_all <- paste0(query_all, query_sep, q1)
      
      q1 <- paste0("match (n:", dbname, ":", RTXTNODE, " {docid:'", d1$docid[i], "', type:'", d1$type[i], "', linenumber:'", d1$linenumber[i], "' }), ", 
                   "(m:", dbname, ":", DOCNODE, " {docid:'", docid, "'}) ", 
                   "merge (m)-[r:", DOC_RTXT_REL, "]->(n) ",
                   "on create set r.count = 1 ",
                   "on match set r.count = r.count+1 ")
      query_all <- paste0(query_all, query_sep, q1)
        
    } else {
      # we have a parent node, so therefore create/merge and connect to the parent node
      logdata("i:", i, "|docid:", d1$parentdocid[i], "|type:", d1$parenttype[i], "|linenumber:", d1$parentlinenumber[i], 
        "|text:", substring(d1$parenttext[i], 1, 30))
      (q1 <- paste0("merge (n:", dbname, ":", RTXTNODE, " {",
                    "docid:'", d1$docid[i], "', ", 
                    "type:'", d1$type[i], "', ", 
                    "linenumber:'", d1$linenumber[i], "', ", 
                    "text:\"", d1$text[i], "\" ", 
                    "}) ",
                    "on create set n.count = 1 ",
                    "on match set n.count = n.count+1 "))
      query_all <- paste0(query_all, query_sep, q1)
      
      q1 <- paste0("match (n:", dbname, ":", RTXTNODE, " {docid:'", d1$parentdocid[i], "',  type:'", d1$parenttype[i], 
                    "', linenumber:'", d1$parentlinenumber[i], "' }),", 
                   "(m:", dbname, ":", RTXTNODE, " {docid:'", d1$docid[i], "', type:'", d1$type[i], "', linenumber:'", d1$linenumber[i],"' }) ", 
                   "merge (n)-[r:", RTXT_RTXT_REL, "]->(m) ",
                   "on create set r.count = 1 ", 
                   "on match set r.count = r.count+1 ")
      query_all <- paste0(query_all, query_sep, q1)
    }
    cypher(db, query_all)
  } # for...
}



#####################################################################################
build_img_node <- function(db, dbname, docid)
{
  d1 <- calldb(paste0("select distinct * from images where docid='", docid, "' ;" ))
  
  if(nrow(d1) == 0) {
    logdata("WARNING - no images for docid:", docid)
    return(NULL)
  }
  
  for(i in seq_len(nrow(d1))) {
    # 1. create images node
    query_all <- as.character(NULL)
    (q1 <- paste0("merge (n:", dbname, ":", IMGNODE, " {",
                  "imageid:'", d1$imageid[i], "', ", 
                  "capid:'", d1$capid[i], "', ", 
                  "imagefile:'", d1$imagefile[i], "', ", 
                  "docid:'", d1$docid[i], "', ", 
                  "page:'", d1$page[i], "', ", 
                  "type:'", d1$type[i], "', ", 
                  "linenumber:'", d1$linenumber[i], "', ", 
                  "caption:'", d1$caption[i], "' ", 
                  "}) ",
                  "on create set n.count = 1 ",
                  "on match set n.count = n.count+1 "))
    query_all <- paste0(query_all, query_sep, q1)
    
    if(0) {
    # 2. find the doc node, then attach author node to doc node
    q1 <- paste0("match (n:", dbname, ":", ITXTNODE, "{docid:'", docid, "', type:'", d1$type[i], "', linenumber:'", d1$linenumber[i], "' }),", 
                 "(m:", dbname, ":", DOCNODE, "{ docid:'", docid, "'})", 
                 "merge (m)-[r:", DOC_ITXT_REL, "]->(n) ",
                 "on create set r.count = 1 ")
    
    query_all <- paste0(query_all, query_sep, q1)
    }

    cypher(db, query_all) 
  }
}

#####################################################################################
build_txt_for_caps_node <- function(db, dbname, docid, type='sent')
{
  d1 <- calldb(paste0("select distinct ir.capid, t.* ", 
                      "from text t ",
                      "inner join imagerefs ir on ir.linenumber = t.linenumber ",
                      "where t.docid='", docid, "' and t.type='", type, "' ",
                      "and ir.docid='", docid, "' ;")) 
  
  # remove capid and remove duplicates
  d1$capid <- NULL
  index <- which(duplicated(d1$linenumber))
  if(length(index) != 0) d1 <- d1[-index,]
  
  if(nrow(d1) == 0) {
    logdata("WARNING - no image text for docid:", docid)
    return(NULL)
  }
  
  for(i in seq_len(nrow(d1))) {
    # 1. create images node
    #print(i)
    query_all <- as.character(NULL)
    (q1 <- paste0("merge (n:", dbname, ":", ITXTNODE, " {",
                  "capid:'", d1$capid[i], "', ", 
                  "docid:'", d1$docid[i], "', ", 
                  "type:'", d1$type[i], "', ", 
                  "linenumber:'", d1$linenumber[i], "', ", 
                  "text:'\"", d1$text[i], "\"' ", 
                  "}) ",
                  "on create set n.count = 1 ",
                  "on match set n.count = n.count+1 "))
    query_all <- paste0(query_all, query_sep, q1)
    
    # 2. find the doc node, then attach author node to doc node
    
    q1 <- paste0("match (n:", dbname, ":", ITXTNODE, "{docid:'", docid, "', type:'", d1$type[i], "', linenumber:'", d1$linenumber[i], "' }),", 
                 "(m:", dbname, ":", DOCNODE, "{ docid:'", docid, "'})", 
                 "merge (m)-[r:", DOC_ITXT_REL, "]->(n) ",
                 "on create set r.count = 1 ")
    
    query_all <- paste0(query_all, query_sep, q1)
    
    tryCatch( {
      cypher(db, query_all) 
    }, error = function( e ) {
      logdata("ERROR - bad cypher - docid:", docid, "  i:", i)
      logdata("ERROR - cypher error message:", conditionMessage(e))
    }) # tryCatch ...
    
    #cypher(db, query_all) 
  }
}

#####################################################################################
build_cap_node <- function(db, dbname, docid, type='sent')
{
  d1 <- calldb(paste0("select * from imagerefs where docid = '", docid, "' ; "))
  
  ## remove capid and remove duplicates
  #names(d1)
  #d1$type <- d1$actual_figs <- NULL
  #index <- which(duplicated(d1$linenumber))
  #if(length(index) != 0) d1 <- d1[-index,]
  
  if(nrow(d1) == 0) {
    logdata("WARNING - no references for docid:", docid)
    return(NULL)
  }
  
  for(i in seq_len(nrow(d1))) {
    query_all <- as.character(NULL)

    if(1) {
      # don't build and connect the cap node.  Just connect the itxt and the image nodes
      q1 <- paste0("match (n:", dbname, ":", ITXTNODE, "{docid:'", docid, "', type:'", type, "', linenumber:'", d1$linenumber[i], "' }),", 
                   "(m:", dbname, ":", IMGNODE, "{ capid:'", d1$capid[i], "', docid:'", d1$docid[i], "'}) ", 
                   "merge (n)-[r:", ITXT_IMG_REL, "]->(m) ",
                   "on create set r.count = 1 ")
      query_all <- paste0(query_all, query_sep, q1)

    } else {
      # include building and connecting the cap node.
      # 1. create author node
      (q1 <- paste0("merge (n:", dbname, ":", CAPNODE, " {",
                    "docid:'", d1$docid[i], "', ", 
                    #"irefid:'", d1$irefid[i], "', ", 
                    "capid:'", d1$capid[i], "' ", 
                    "}) ",
                    "on create set n.count = 1 ",
                    "on match set n.count = n.count+1 "))
      query_all <- paste0(query_all, query_sep, q1)
      
      # 2. find the doc node, then attach author node to doc node
      
      q1 <- paste0("match (n:", dbname, ":", ITXTNODE, "{docid:'", docid, "', type:'", type, "', linenumber:'", d1$linenumber[i], "' }),", 
                   "(m:", dbname, ":", CAPNODE, "{ docid:'", docid, "', capid:'", d1$capid[i], "' })", 
                   "merge (n)-[r:", ITXT_CAP_REL, "]->(m) ",
                   "on create set r.count = 1 ")
      
      query_all <- paste0(query_all, query_sep, q1)
      cypher(db, query_all) 
      
      query_all <- as.character(NULL)
      
      # attach caption node to image node
      q1 <- paste0("match (n:", dbname, ":", CAPNODE, "{capid:'", d1$capid[i], "', docid:'", d1$docid[i], "' }), ", 
                   "(m:", dbname, ":", IMGNODE, "{ capid:'", d1$capid[i], "', docid:'", d1$docid[i], "'}) ", 
                   "merge (n)-[r:", CAP_IMG_REL, "]->(m) ",
                   "on create set r.count = 1 ")
      query_all <- paste0(query_all, query_sep, q1)
    }
    cypher(db, query_all) 
  }
}

#####################################################################################
build_graph <- function(db, dbname, docid, type='sent')
{
  # build doc and authors
  build_doc_node(db, dbname, docid)
  build_aut_node(db, dbname, docid)

  # find all reference id's for this document
  refid <- calldb(paste0("select distinct cast(refid as signed) as refid from refs where docid = '", docid, 
                "' and refdocid is not NULL order by refid ;"))$refid
  
  # use this system (the following two lines) versus the old system
  build_txt_and_ref(db, dbname, docid, refid, type)
  build_claims_graph(db, dbname, docid, type)
    
  # build image nodes for document
  build_img_node(db, dbname, docid)
  # build text node for captions and attach to document
  build_txt_for_caps_node(db, dbname, docid, type)
  # build caption node and attach to text and image
  build_cap_node(db, dbname, docid, type)
  
  summary1(db, dbname)
}

#####################################################################################
if(0) {
  pdfDocs <- inputdf$pdfDocs
}
loop_on_docs_for_g50 <- function(pdfDocs, dbname='arg3', host='local', delete_db=F)
{
  dblist <- initialize_neo4j(dbname=dbname, host=host, delete_db=delete_db)
  db <- dblist$db
  dbname <- dblist$dbname
  summary1(db, dbname)

  if(!is.na(pdfDocs)) {
    # pdfDocs is not NA, then make sure that the file exists in the database before we proceed
    x1 <- calldb(paste0("select filename from docs where filename = '", pdfDocs, "';"))
    if(nrow(x1) == 0) {
      logdata("Error: file ", pdfDocs, "does not exists in the database - exiting")
      return(NULL)
    }
  }

  if(is.na(pdfDocs)) pdfDocs <- calldb("select filename from docs where filename <> ''")$filename 

  #for(i in 1:5) {
  for(i in seq_len(length(pdfDocs))) {
    doc <- pdfDocs[i]
    docinfo <- calldb(paste0("select docid, pmid, filename, journal from docs where filename = '", doc, "' ;"))
    docid <-  docinfo$docid[1]

    if(nrow(docinfo) == 0) {
      logdata("WARNING - did not find a document in the database for docname:", doc)
      next ;
    }
    logdata("-------------------------------------------------------------------------")
    logdata(i, " docid:", docid, " document in graphing process: ", doc)

    build_graph(db, dbname, docid)
  }
}

#####################################################################################
if(0) {
  docid = inputdf$docid
  delete_db=inputdf$delete_db
  massCATS=inputdf$massCATS
  AWS=inputdf$AWS
}
create_graph_on_docid <- function(docid, delete_db=F, massCATS=F, AWS=F)
{
  logdata("docid    :", docid)
  logdata("delete_db:", delete_db)
  logdata("massCATS :", massCATS)
  logdata("AWS      :", AWS)

  if(AWS) {
    dblist <- initialize_neo4j(dbname='arg2', host='AWS', delete_db=delete_db)
  } else {
    dblist <- initialize_neo4j(dbname='arg2', delete_db=delete_db)
  }

  db <- dblist$db
  dbname <- dblist$dbname
  cypher(db, paste0("match (n:", dbname, ") return count(n);"))
  logdata("neo4j database name:", dbname)

  #  docid=2878; type='paragraph'

  print(paste("create_graph_on_docid:", docid))
  build_graph(db, dbname, docid, type='paragraph')
}

#####################################################################################
usage <- function()
{

  logdata("usage")
  logdata("  ./graph.r <blank>                          : print this message ")
  logdata("  ./graph.r all                              : do all g50-type files normally ")
  logdata("  ./graph.r dir/file                         : do one single g50-type file")
  logdata("  ./graph.r docid=2878                       : do one specific file but use the docid instead")
  logdata("  ./graph.r docid=2878 massCATS              : do one massCATS (Haggerty) type file")
  logdata("  ./graph.r docid=2878 massCATS AWS          : do one massCATS (Haggerty) type file into the AWS installation")
  logdata("  ./graph.r docid=2878 delete_db massCATS AWS: do one massCATS (Haggerty) type file into the AWS installation, delete_db")
}

#####################################################################################
# MAIN main Main
#

logdata("==================== START OF RUN =================")
process_time_total <- proc.time()

inputdf <- data.frame(
  process_type     = NA, 
  docid            = NA, 
  massCATS         = F, 
  AWS              = F, 
  delete_db        = F, 
  pdfDocs          = NA,
  pdfDir           = NA,
  stringsAsFactors = F)

if(1) {
  inputopt <- commandArgs()
} else {
  inputopt <- c("/usr/lib/R/bin/exec/R", "--slave", "--no-restore", "--file=./aa.r", "--args")
  #inputopt <- c(inputopt, "/home_ssd/dag/0-Annat/0-R/customers/mgh/code/graph.r")
  inputopt <- c(inputopt, "docid=2878")
  inputopt <- c(inputopt, "massCATS")
  inputopt <- c(inputopt, "delete_db")
  
  if(0) {
  inputopt <- c(inputopt, "/home_ssd/dag/0-Annat/0-R/customers/mgh/data/top50alz/20042704_Petersen_ADNI_Clinical_characterization.pdf")
  inputopt <- c(inputopt, "/home_ssd/dag/0-Annat/0-R/customers/mgh/data/top50alz/24670762_Lu_REST_and_Stress_resistance.pdf")
  }
  #inputopt <- c(inputopt, "AWS")
}

inputlength <- length(inputopt)
#print(inputopt) ; print(inputlength)

if(length(inputopt) >= 6) {
  if(grepl("docid=", inputopt[6])) {
    # remove "docid=" and the rest should be the actual docid
    (s1 <- inputopt[6])
    inputdf$docid <- substring(s1, nchar('docid=')+1, nchar(s1)) %>% as.integer()
    inputdf$process_type = 'docid'
    for(i in 6:inputlength) {
    	if(tolower(inputopt[i]) == 'masscats')  inputdf$massCATS <- T
    	if(tolower(inputopt[i]) == 'aws')       inputdf$AWS <- T
    	if(tolower(inputopt[i]) == 'delete_db') inputdf$delete_db <- T
    }
  } else {
    filenamex <- inputopt[6]
    if(tolower(filenamex) == 'all') {
      inputdf$process_type <- 'file'
    } else if(!file.exists(filenamex))  {
      logdata("Error: file", filenamex, "does not exists - exiting")
      logdata("NOTE - we need absolute addresses to files")
    } else {
      inputdf$pdfDocs <- basename(filenamex)
      inputdf$pdfDir <- dirname(filenamex)
      inputdf$process_type <- 'file'
    }
  } 
} else if( length( inputopt ) != 6 ) {
  #logdata("Do all files")
  #inputdf$process_type <- 'file'
  usage()
  quit()
}

if(inputdf$process_type == 'docid') {
  if(!is.na(inputdf$docid)) {
    create_graph_on_docid(inputdf$docid, delete_db=inputdf$delete_db, massCATS=inputdf$massCATS, AWS=inputdf$AWS)
  } else {
    logdata("Error: docid is not valid")
  }
} else if(inputdf$process_type == 'file') {
  loop_on_docs_for_g50(inputdf$pdfDocs, dbname='arg3', host='local', delete_db=T)
}

# the following passage splits the rows up in chunks of 100.  This is to 
#   minimize the impact of sucking up memory and cpu.
s <- proc.time()[3] - process_time_total[3]
s <- as.integer(s)
h <- s %/% 3600 ; s <- s - (h*3600) ; 
m <- s %/% 60   ; s <- s - (m*60)   ; 
logdata("")
logdata(sprintf("process time: %d hours %d minutes %d seconds", h, m, s))
logdata("==================== END OF RUN =================")

