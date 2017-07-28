#!/usr/bin/env Rscript 

#####################################################################################
rm(list=ls())

rootDir <- "/home_ssd/dag/0-Annat/0-R/customers/mgh"

setwd( rootDir ) 
codeDir <- file.path(rootDir, "code")
dataDir <- file.path(rootDir, "data")

source(file.path(codeDir, "utils.r"))
source(file.path(codeDir, "graph_utils.r"))
source(file.path(codeDir, "tm_utils.r"))
source(file.path(codeDir, "db.r"))

library(dplyr)
library(RNeo4j)
library(tm)
library(openNLP)

logfilename <- "connectref.log"
logdata("hello")


#################################################################################### 
# NOTE you might see warning messages such as these and that is all right for
# our purposes and does not limit us in any way.
#    Syntax Warning: Invalid Font Weight
#    Syntax Error (38799): insufficient arguments for Marked Content

#################################################################################### 
# text <- p3$text
create_tdm <- function(text, tfidf=F, stem_terms=F, stopword_file=NULL, verbose=F)
{
  # find my stopwords
  my_stopwords = c(stopwords('english'))
  my_stopwords = c(my_stopwords, "better", "high", "much", "need", "offer", "reduce", "rude")

  if(!is.null(stopword_file)) {
    sw <- scan(stopword_file, what="character", comment.char="#")
    my_stopwords = c(my_stopwords, sw) 
  }
  my_stopwords <- my_stopwords[!duplicated(my_stopwords)]
  
  ### prepare corpus
  if(verbose) cat("building index: building corpus\n")

  jc <- Corpus(VectorSource(text))
  #inspect(jc)
  #sapply(jc, function(x) as.character(x))

  # needs to be done prior to removing white spaces
  removeURL = function(x) gsub("http[s]*://[[:alnum:],.,/]*", "", x)
  jc = tm_map(jc, content_transformer(removeURL))
  #jc[[1]][[1]] ; jc[[2]][[1]]

  jc = tm_map(jc, content_transformer(tolower))
  jc = tm_map(jc, content_transformer(removeNumbers))
  jc = tm_map(jc, content_transformer(removePunctuation))
  jc = tm_map(jc, content_transformer(stripWhitespace))
  jc = tm_map(jc, removeWords, my_stopwords)
  if(stem_terms) jc = tm_map(jc, stemDocument)

  if(verbose) cat("building index: creating tdm \n")

  if(tfidf == TRUE) { 
    if(verbose) cat("build tdm based on TfIdf\n")
    tdm = TermDocumentMatrix(jc, control = list(weighting = weightTfIdf, wordLengths=c(2,Inf)))
  } else { 
    if(verbose) cat("build tdm based on TF\n")
    tdm = TermDocumentMatrix(jc, control = list(wordLengths=c(2,Inf))) 
  }

  return( tdm )
} 

######################################################################################
# docs is a character vector where each character strings is its own "doc" (like tweets)
search_docs <- function(docs, query)
{
  number_of_docs <- length(docs)
  (docs1 <- c(docs, query))
  
  tdm <- create_tdm(docs1, tfidf=T, stem_terms=T, stopword_file=NULL)
  
  m1 <- as.matrix(tdm)
  m1 <- scale(m1, center = FALSE, scale = sqrt(colSums(m1^2)))
  
  # break out the query vector
  queryvector <- m1[, (number_of_docs + 1)]
  m1 <- m1[, 1:number_of_docs]
  
  # calculate similarities
  (doc.scores <- t(queryvector) %*% m1)
  return(doc.scores)
}

######################################################################################
pre_query <- function(doc, query, verbose=F)
{
  # 2. create VCorpus
  jc <- VCorpus(URISource(doc, mode = ""), readerControl = list(reader = readPDF(engine = "xpdf", control=list(text='-raw'))))
  if(verbose) inspect_document(jc, topcount=30, stem_terms=T)
  txt1 <- sapply(jc, function(x) as.character(x))
  num_sent1 <- length(convert_to_sentence(txt1))

  if(verbose) head(txt1)
  s1 <- txt1

  # NOTE: in order to count sentences, you have to remove data starting at the top
  # remove all lines to Abstract
  index <- grep("^Abstract", s1)
  if(length(index) > 0) s1 <- s1[index:length(s1)]
  # actually, remove everything down to and including Introduction
  index <- grep("^Introduction", s1, ignore.case=T)
  if(length(index) > 0) s1 <- s1[(index+1):length(s1)]
  if(verbose) head(s1)
  num_sent2 <- length(convert_to_sentence(s1))
  
  # split document in non-reference section and reference section
  (index <- grep("^Reference", s1, ignore.case=T))
  #ref1 <- s1[index:length(s1)] # reference section
  if(length(index) > 0) s1 <- s1[1:(index-1)] # non-reference section
  
  
  # remove new lines
  s1 <- gsub("^\f", "", s1)
  
  txt1 <- paste(s1, collapse=" ")
  txt1 <- clean_all_data(txt1)
  
  if(length(grep(paste("^package:", "ggplot2", "$", sep=""), search()))) {detach(package:ggplot2)}
  txt2 <- convert_to_sentence(txt1)
  if(verbose) head(txt2)
  
  if(verbose) query
  docs <- txt2
  results <- search_docs(docs, query)
  
  # need to keep track of this so that we can display a more accurate sentence count
  sentdiff <- num_sent1 - num_sent2
  
  return(list(results=results, docs=docs, sentdiff=sentdiff))
}

######################################################################################
if(0) {
  docid <- docs_refs$docid[i]
  filename <- docs_refs$filename[i]
  query <- docs_refs$query[i] 
}
search_document_and_query <- function(docid, filename, query, return_results=5)
{
  dataDir1 <- file.path(rootDir, 'code/shiny/www/docs')
  (doc <- file.path(dataDir1, filename))

  # remove the last () from the query. It is only a reference to document and have no 
  # bearing on the actual query
  (index <- regexpr("\\([^\\(]*$", query))
  if(as.integer(index) > 0) query <- substring(query, 1, (as.integer(index)-1)) %>% trim()
  
  dfresults <- pre_query(doc, query)
  results <- dfresults$results
  docs <- dfresults$docs
  sentdiff <- dfresults$sentdiff
  
  total_sentences <- length(docs) + sentdiff

  df <- data.frame(docid=rep(docid, length(docs)), sentence=(seq_len(length(docs))+sentdiff), sentpercentage=((seq_len(length(docs))+sentdiff)/total_sentences),
    score=t(results), text=docs, query=rep(query,length(docs)), stringsAsFactors=F) %>% arrange(desc(score))
  return(df[1:return_results,])
}

######################################################################################
search_all_documents <- function(docid=780)
{
  dblist <- initialize_neo4j(dbname='arg2', delete_db=F)
  db <- dblist$db
  dbname <- dblist$dbname
  cypher(db, paste0("match (n:", dbname, ") return count(n);"))
  
  # 1. get docs and references (references will become queries later)
  #q1 <- paste0("match (d:", dbname, ":doc)<--(ref)<--(rtxt) ",
  #  "where d.docid <> '", docid, "' ", # don't include the haggerty doc
  #  "return d.docid as docid, d.pmid as pmid, d.filename as filename, ",
  #    "rtxt.linenumber as linenumber, rtxt.text as query ",
  #  "order by d.docid, rtxt.linenumber ;")
  
  
  q1 <- paste0("match (d:", dbname, ":doc)<--(rtxt) ",
    "where d.docid <> '", docid, "' ", # don't include the haggerty doc
    "return d.docid as docid, d.pmid as pmid, d.filename as filename, ",
      "rtxt.linenumber as linenumber, rtxt.text as query ",
    "order by d.docid, rtxt.linenumber ;")
  
  
  docs_refs <- cypher(db, q1)

  d1 <- data.frame()
  i=7
  for(i in seq_len(nrow(docs_refs))) {
  	print(i)
    if(docs_refs$filename[i] == "NA") next ;
    if(docs_refs$filename[i] == "") next ;
    d2 <- search_document_and_query(docs_refs$docid[i], docs_refs$filename[i], docs_refs$query[i], return_results=3) 
    d1 <- rbind(d1, d2)
  }
  d1$score <- round(d1$score, digit=2)
  d1$sentpercentage <- paste0(round(d1$sentpercentage, digit=2)*100, "%")
  save(d1, file=file.path(codeDir, 'shiny/connectref3.rdata'))
}

######################################################################################
# main Main MAIN
# do_both_at_the_same_time()
search_all_documents(docid=780)



