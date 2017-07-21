#!/usr/bin/env Rscript 

#####################################################################################

if(Sys.info()['nodename'] == 'hadoop') {
  rootDir <- "/home_ssd/dag/0-Annat/0-R/customers/mgh/code/shiny"
} else {
  rootDir <- "/home/ubuntu/mgh/code/shiny"
}
codeDir <- rootDir
logfilename <- "graph.log"

################################################################################
logdata <- function(...)
{
  dt <- format(Sys.time(), format="%d|%H:%M:%S")
  cat(sprintf("%s %s\n", dt, paste(unlist(list(...)), collapse=" ")), file=stderr())
  cat(sprintf("%s %s\n", dt, paste(unlist(list(...)), collapse=" ")), file=file.path(codeDir, logfilename), append=T)
}

#####################################################################################
initialize_neo4j <- function(dbname=NULL, delete_db=F)
{
  if(is_klurig_machine()) {
    logdata("connecting to Neo4j on Klurig machine")
    db <- startGraph("http://localhost:7474/db/data/")
  } else {
    logdata("connecting to Neo4j on AWS machine")
    db_url <- "http://localhost:7474/db/data/"
    db <- startGraph(db_url, username='neo4j', password='hpr284')
  }

  if(is.null(dbname)) dbname <- "arg3"  # this is the real one

  if(delete_db) delete_nodes_db(db, dbname)
  logdata("Neo4j database name:", dbname)
  
  return(list(db=db, dbname=dbname))
}

################################################################################
# this function returns TRUE if we are on Klurig Analytics primary machine
#   nodename == 'hadoop'
#   and
#   user == 'dag'
#
is_klurig_machine <- function()
{
  (s1 <- Sys.info())
  retval <- ifelse(s1['nodename'] == 'hadoop' & s1['user'] == 'dag', TRUE, FALSE)
  retval <- as.logical(retval)
  return(retval)
}

#################################################################################
if(0) {
  myindex = 105
  y=nodes$year[myindex] 
  m=nodes$month[myindex] 
  d=nodes$date[myindex]
}
parse_date <- function(y, m, d)
{
  # month is only meaningful if year is not empty
  if(is.na(y) | y == 'NA') y <- ''
  if(is.na(m) | m == 'NA') m <- ''
  if(is.na(d) | d == 'NA') d <- ''

  if(y != '' & m != '') {
    date1 <- paste0(m, ", ", y)
  } else if(y != '' & m == '') {
    date1 <- y
  } else if(y == '' & d != '') {
    date1 <- d
  } else {
    date1 = "NA"
  }
  return(date1)
}

