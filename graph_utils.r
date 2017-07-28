#!/usr/bin/env Rscript 

# node types
DOCNODE  <- 'doc'
AUTNODE  <- 'aut'
RTXTNODE <- 'rtxt'
ITXTNODE <- 'itxt'
REFNODE  <- 'ref'
CAPNODE  <- 'cap'
IMGNODE  <- 'img'

# doc to author relationship
DOC_AUT_REL    <- 'has_attribution'

# doc to rtxt and itxt relationships
#RTXTDOCREL   <- 'part_of'
#ITXTDOCREL   <- 'part_of'
DOC_RTXT_REL <- DOC_ITXT_REL  <- 'includes'

# rtxt to rtxt, rtxt to references, itxt to captions relationships
#REFRTXTREL   <- 'supports'
#CAPITXTREL   <- 'supports'
RTXT_RTXT_REL <- RTXT_DOC_REL <- RTXT_REF_REL <- ITXT_CAP_REL <- 'supported_by'

# reference to document, caption to image relationships
#RTXTRTXTREL  <- 'claim_supported_by'
#RTXT1DOCREL  <- 'claim_part_of'
REF_DOC_REL <- CAP_IMG_REL <- ITXT_IMG_REL <- 'references'

db_url <- '' 

#####################################################################################
if(0) {
  # when running this file stand-alone, just flip the 0 to a 1, and all is normal
  rm(list=ls())
  
  rootDir <- "/home_ssd/dag/0-Annat/0-R/customers/mgh"
  
  setwd( rootDir ) 
  codeDir   <- file.path(rootDir, "code")
  dataDir   <- file.path(rootDir, "data")
  outputDir <- file.path(rootDir, "output")
  claimDir  <- file.path(rootDir, "find_claim")
  
  source(file.path(codeDir, "utils.r"))
}
  
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(RNeo4j))

#####################################################################################
# R workhorse to communicate with Neo4J using the REST interface
library(RJSONIO)
library(RCurl)

my_query <- function(querystring, verbose = FALSE) {
  #print("using my_query ===============================================")
  h = basicTextGatherer()
  curlPerform(url=paste0(db_url, "cypher"), 
              postfields=paste('query',curlEscape(querystring), sep='='),
              writefunction = h$update,
              verbose = verbose
  )           
  result <- fromJSON(h$value())
  data <- data.frame(t(sapply(result$data, unlist)))
  if(nrow(data) == 0) return(NULL)
  names(data) <- result$columns
  return(data)
}

#####################################################################################
my_cypher <- function(db, q1, use_curl=F)
{
  if(use_curl) {
    r3 <- my_query(q1, verbose=F)
  } else {
    r3 <- cypher(db, q1)
  }
  
  return(r3)
}

#####################################################################################
# delete all nodes on the database for dbname
delete_nodes_db <- function(db, dbname)
{
  print("deleting graph from database")
  q1 <- paste0("match (n:", dbname, ") detach delete n;")
  my_cypher(db, q1)
}

#####################################################################################
write_to_graph <- function(db, dbname, node1, node2)
{
  # use to combine multiple cypher inserts into one
  query_sep <- " with count(*) as dummy "
  
  # uid = tweet user id
  
  # node1 follows node2, eg; (node1) -[:follows]-> (node2)
  query_all <- as.character(NULL)
  
  # create node1
  q1 <- paste0("merge (n:", dbname,"  {uid:'", node1, "' }) ",
               "on create set n.count = 1 ",
               "on match set n.count = n.count+1 ")
  query_all <- paste0(query_all, query_sep, q1)
  
  # create node2
  q1 <- paste0("merge (n:", dbname,"  {uid:'", node2, "' }) ",
               "on create set n.count = 1 ",
               "on match set n.count = n.count+1 ")
  query_all <- paste0(query_all, query_sep, q1)
  
  # create edge from node to text
  q1 <- paste0("match (n:", dbname, " {uid:'", node1, "'}), (m:", dbname, " {uid:'", node2, "'}) ",
               "merge (n)-[r:follows]->(m) ",
               "on create set r.count = 1 ",
               "on match set r.count = r.count+1 ")
  query_all <- paste0(query_all, query_sep, q1)
  
  my_cypher(db, query_all) 
}

#####################################################################################
write_tweet_to_graph <- function(db, dbname, p1)
{
  # create node for tweet. Need to ensure that the tweet_id is unique. Need a constraint
  # (user1)-[tweeted]->(tweet1)-[before]->(tweet2)<-[tweeted]-(user2)
  userlabel <- 'user'
  tweetlabel <- 'tweet'
  
  query_sep <- " with count(*) as dummy "
  query_all <- as.character(NULL)

  for(i in seq_len(nrow(p1))) {
    # 1. create node
    q1 <- paste0("merge (n:", dbname, ":", tweetlabel," {",
        "tweet_id:'", p1$tweet_id[i],"', ",
        "created:'", p1$created[i],"', ",
        "screenName:'", p1$screenName[i],"', ",
        "retweetCount:'", p1$retweetCount[i],"', ",
        "isRetweeet:'", p1$isRetweeet[i],"', ",
        "retweeted:'", p1$retweeted[i],"', ",
        "text:'", p1$text[i],"', ",
        "rt:'", p1$rt[i],"', ",
        "rtt:'", p1$rtt[i],"' }) ",
      "on create set n.count = 1 ",
      "on match set n.count = n.count+1 ")
    query_all <- paste0(query_all, query_sep, q1)

    # 2. attach node to user if we can
    # create edge from node to text
    q1 <- paste0("match (n:", dbname, ":", userlabel, " {screenName:'", p1$screenName[i], "'}), (m:", dbname, ":", tweetlabel, " {screenName:'", p1$screenName[i], "'}) ",
               "merge (n)-[r:tweets]->(m) ",
               "on create set r.count = 1 ",
               "on match set r.count = r.count+1 ")
    query_all <- paste0(query_all, query_sep, q1)

    # 3. unless this is a first node in the chain, then attach this tweet node
    #    to the previous tweet node
    if(i != 1) {
      q1 <- paste0("match (n:", dbname, ":", tweetlabel, " {tweet_id:'", p1$tweet_id[i-1], "'}), (m:", dbname, ":", tweetlabel, " {tweet_id:'", p1$tweet_id[i], "'}) ",
               "merge (n)-[r:before]->(m) ",
               "on create set r.count = 1 ",
               "on match set r.count = r.count+1 ")
      query_all <- paste0(query_all, query_sep, q1)
    }
  }
  
  cypher(db, query_all) 
  
}

#####################################################################################
build_relationship_between_authors <- function(db, dbname, p1)
{
  # create node for tweet. Need to ensure that the tweet_id is unique. Need a constraint
  # (user1)-[tweeted]->(tweet1)-[before]->(tweet2)<-[tweeted]-(user2)
  userlabel <- 'user'
  
  query_sep <- " with count(*) as dummy "
  query_all <- as.character(NULL)
  
  x <- nrow(p1)
  
  for(from_node in seq_len(x-1)) {
    for(to_node in (from_node+1):x) {
      # connect from_node to to_node
      q1 <- paste0("match (from:", dbname, ":", userlabel, " {screenName:'", p1$screenName[from_node], "'}), ",
                         "(to:", dbname, ":", userlabel, " {screenName:'", p1$screenName[to_node], "'}) ",
                   "merge (from)-[r:tweetbefore]->(to) ",
                   "on create set r.count = 1 ",
                   "on match set r.count = r.count+1 ")
      query_all <- paste0(query_all, query_sep, q1)
    }
    print(paste("load into cypher:", from_node, "out of", x))
    cypher(db, query_all) 
    query_all <- as.character(NULL)
  }
}

#####################################################################################
write_tweet_user_to_graph <- function(db, dbname, p1)
{
  # create node for tweet. Need to ensure that the tweet_id is unique. Need a constraint
  # (user1)-[tweeted]->(tweet1)-[before]->(tweet2)<-[tweeted]-(user2)
  userlabel <- 'user'
  tweetlabel <- 'tweet'
  
  query_sep <- " with count(*) as dummy "
  query_all <- as.character(NULL)

  for(i in seq_len(nrow(p1))) {
    # 1. create node
    q1 <- paste0("merge (n:", dbname, ":", tweetlabel," {",
        "tweet_id:'", p1$tweet_id[i],"', ",
        "created:'", p1$created[i],"', ",
        "screenName:'", p1$screenName[i],"', ",
        "retweetCount:'", p1$retweetCount[i],"', ",
        "isRetweeet:'", p1$isRetweeet[i],"', ",
        "retweeted:'", p1$retweeted[i],"', ",
        "text:'", p1$text[i],"', ",
        "rt:'", p1$rt[i],"', ",
        "rtt:'", p1$rtt[i],"' }) ",
      "on create set n.count = 1 ",
      "on match set n.count = n.count+1 ")
    query_all <- paste0(query_all, query_sep, q1)

    # 2. attach node to user if we can
    # create edge from node to text
    q1 <- paste0("match (n:", dbname, ":", userlabel, " {screenName:'", p1$screenName[i], "'}), (m:", dbname, ":", tweetlabel, " {screenName:'", p1$screenName[i], "'}) ",
               "merge (n)-[r:tweets]->(m) ",
               "on create set r.count = 1 ",
               "on match set r.count = r.count+1 ")
    query_all <- paste0(query_all, query_sep, q1)

    # 3. unless this is a first node in the chain, then attach this tweet node
    #    to the previous tweet node
    if(i != 1) {
      q1 <- paste0("match (n:", dbname, ":", tweetlabel, " {tweet_id:'", p1$tweet_id[i-1], "'}), (m:", dbname, ":", tweetlabel, " {tweet_id:'", p1$tweet_id[i], "'}) ",
               "merge (n)-[r:before]->(m) ",
               "on create set r.count = 1 ",
               "on match set r.count = r.count+1 ")
      query_all <- paste0(query_all, query_sep, q1)
    }
  }
  
  if(i != 1) {
    print("Add nodes to graph database")
    cypher(db, query_all) 
    build_relationship_between_authors(db, dbname, p1)
  } else {
    print("There was only one node, so we did not add the node to the graph database")
  }
}

#####################################################################################
if(0) {
  screenName <- df2$screenName[i] 
  node1 <- df2$id[i] 
  node2 <- friends[j]
  write_db <- T
}
write_to_graph_screenName <- function(db, dbname, screenName, node1, node2, write_db=T)
{
  # use to combine multiple cypher inserts into one
  query_sep <- " with count(*) as dummy "
  
  # uid = tweet user id
  
  # node1 follows node2, eg; (node1) -[:follows]-> (node2)
  query_all <- as.character(NULL)
  
  # create node1
  q1 <- paste0("merge (n:", dbname,"  {screenName:'", screenName, "', uid:'", node1, "' }) ",
               "on create set n.count = 1 ",
               "on match set n.count = n.count+1 ")
  query_all <- paste0(query_all, query_sep, q1)
  
  # create node2
  q1 <- paste0("merge (n:", dbname,"  {uid:'", node2, "' }) ",
               "on create set n.count = 1 ",
               "on match set n.count = n.count+1 ")
  query_all <- paste0(query_all, query_sep, q1)
  
  # create edge from node to text
  q1 <- paste0("match (n:", dbname, " {uid:'", node1, "'}), (m:", dbname, " {uid:'", node2, "'}) ",
               "merge (n)-[r:follows]->(m) ",
               "on create set r.count = 1 ",
               "on match set r.count = r.count+1 ")
  query_all <- paste0(query_all, query_sep, q1)
  
  if(write_db) my_cypher(db, query_all) 

  return(query_all)
}

#####################################################################################
summary1 <- function(db, dbname)
{
  q1 <- paste0("match(n:", dbname, ") return count(*);")
  count_nodes <- my_cypher(db, q1) %>% as.integer()
  
  q1 <- paste0("match(n:", dbname, ")-->(m) return count(*);")
  count_rels <- my_cypher(db, q1) %>% as.integer() 
  
  print(sprintf("node count: %d  relationship count: %d", count_nodes, count_rels))
}

#####################################################################################
query_by_rel_count1 <- function(db, dbname)
{
  q1 <- paste0("MATCH (n:", dbname, ")-[r]-() return n.uid, n.count, count(r) as rel_count order by rel_count desc, n.count desc limit 50;")
  r1 <- my_cypher(db, q1)
  print(r1)
}

#####################################################################################
query_by_rel_count2 <- function(db, dbname)
{
  q1 <- paste0("match(n:", dbname, ")-[r]->(m) return n.uid as follower, type(r), m.uid as followed, r.count order by r.count desc limit 50 ;")
  r1 <- my_cypher(db, q1)
  print(r1)
}

#####################################################################################
store_user_relationships_in_graph <- function()
{
  #### do Neo4j database setup
  print("set up Neo4j database")
  db <- startGraph(db_url)
  
  cypher(db, "match (n) return count(*);") 
  cypher(db, "match (n:vax2) return count(*);") 
  cypher(db, "match (n:vax2:user) return count(*);") 
  cypher(db, "match (n:vax2:tweet) return count(*);") 
  
  
  # ida follows idb, eg; (ida) -[:follows]-> (idb)
  dbname <- "vax1"
  my_cypher(db, paste0("create index on :", dbname, "(uid)"))
  my_cypher(db, paste0("create index on :", dbname, "(screenName)"))
  # check if index is built
  #system(paste0("curl http://localhost:7474/db/data/schema/index/", dbname), ignore.stderr=T, intern=T)
  system(paste0("curl ", db_url, "/schema/index/", dbname), ignore.stderr=T, intern=T)

  ida <- 33 
  idb <- 55 
  idc <- 77 
  idd <- 11 
  # aaaa
  write_to_graph(db, dbname, ida, idb)
  write_to_graph(db, dbname, idb, ida)
  write_to_graph(db, dbname, ida, idc)
  write_to_graph(db, dbname, idc, idd)
  summary1(db, dbname)
  delete_nodes_db(db, dbname)
  query_by_rel_count1(db, dbname)
  query_by_rel_count2(db, dbname)
}

#####################################################################################
build_network_graph <- function()
{
  print("set up Neo4j database")
  db <- startGraph(db_url)
  my_cypher(db, "match (n) return count(*);") 
  dbname <- "vax2"
  
  con <- dbConnect(RMySQL::MySQL(), dbname="twitter", host="127.0.0.1")
  r <- dbGetQuery(con, "select screenName, id from uservax;")
  dbDisconnect(con)
  
  r.save <- r
  #df2 <- r[11:20,]
  df2 <- r
  #for(i in seq_len(nrow(df2))) {
  # bad ones: 907, 2836 or so
  for(i in (2837:(nrow(df2)))) {
    t1 <- proc.time()
    cat(format(Sys.time(), format="%d:%H:%M:%S"), "-- current time\n")
    print(sprintf("%-20s - %-20s - nchar of id: %d", df2$screenName[i], df2$id[i], nchar(df2$id[i])))
    if(nchar(df2$id[i]) > 12) next ;
    u1 <- userFactory$new(screenName=df2$screenName[i], id=df2$id[i])

    # go to Twitter and get friends 
    ratelimit <- getCurRateLimitInfo(c("friends", "application"))
    print(ratelimit)
    friendlimit <- ratelimit[which(grepl("/friends/ids", ratelimit$resource)),]$remaining %>% as.integer()
    if(friendlimit == 0) {
      print("Reached rate limit. Sleeping 30 seconds")
      Sys.sleep(30)
      ratelimit <- getCurRateLimitInfo(c("friends", "application"))
      print(ratelimit)
      friendlimit <- ratelimit[which(grepl("/friends/ids", ratelimit$resource)),]$remaining %>% as.integer()
    }
    
    friends <- u1$getFriendIDs()

    print(sprintf("%4d: %-20s  friend_count: %s  find friends time: %0.0f seconds", 
      i, df2$screenName[i], length(friends), (proc.time()-t1)[3]))

    # if there are no friends, then do the next user
    if(length(friends) == 0) next ;

    # if the user has more than 750 friends, then we assume that this is not a user we are 
    #   looking for, so skip this person and go for the next user instead
    #if(length(friends) < 750) next ;

    # friends.save <- friends
    #friends <- friends.save[1:3005]
    len_friends <- length(friends)
    write_to_db_step = 100
    query_all <- as.character(NULL)
    query_sep <- " with count(*) as dummy "

    t2 <- proc.time()
    for(j in seq_len(length(friends))) {
      #print(sprintf("%4d of %4d", j, len_friends))
      q1 <- write_to_graph_screenName(db, dbname, df2$screenName[i], df2$id[i], friends[j], write_db=F)
      query_all <- paste0(query_all, query_sep, q1)
      if((j %% write_to_db_step) == 0) {
        my_cypher(db, query_all) 
        query_all <- as.character(NULL)
        print(sprintf("%4d of %4d", j, len_friends))
      }
    }
    if(length(query_all) != 0) my_cypher(db, query_all)  # write all that didn't get written inside the loop
    print(sprintf("%4d: %-20s  friend_count: %s  write_to_graph: %0.0f seconds", 
      i, df2$screenName[i], length(friends), (proc.time()-t2)[3]))
    
    #delete_nodes_db(db, dbname)
  }
}

#####################################################################################
remove_duplicates <- function(db)
{
  q1 <- paste0("MATCH (n:vax2) ",
    "WITH n.uid as uid, count(*) as count ",
    "WHERE count > 1 ",
    "RETURN uid, count order by uid limit 10000; ")
  p1 <- cypher(db, q1)
  
  #p1 <- my_cypher(db, q1)

  for(i in seq_len(nrow(p1))) {
    # one by one, delete nodes that have screenName = null
    print(paste(i, p1$uid[i]))
    q1 <- paste0("match (n:vax2 {uid:'", p1$uid[i], "'})-[r]-(m) where n.screenName is null delete r ;")
    my_cypher(db, q1)
    q1 <- paste0("match (n:vax2 {uid:'", p1$uid[i], "'}) where n.screenName is null delete n;")
    my_cypher(db, q1)
  }
}

#####################################################################################
add_screenNames_to_nodes <- function()
{
  print("set up Neo4j database")
  db <- startGraph(db_url)
  my_cypher(db, "match (n) return count(*);") 
  dbname <- "vax2"
  # 1. get top nodes with based on number of followers and that do not have screenNames
  #    say top 100 to start with
  q1 <- paste0("match (n:", dbname, ")-[r]->(m) where m.screenName is null ",
               "return m.uid as followed, m.screenName, count(r) as rel_count order by rel_count desc limit 10000; ")
  g1 <- cypher(db, q1)
  
  # 2. from those uids, go to uservax table and get all records from there
  #    based on those top-100.
  names(g1)
  uid <- paste0(g1$followed, collapse="','")
  
  q1 <- paste0("select distinct id, screenName from uservax ", "where id in ('", uid, "') order by id;")
  con <- dbConnect(RMySQL::MySQL(), dbname="twitter", host="127.0.0.1")
  p1 <- dbGetQuery(con, q1)
  dbDisconnect(con)
    
  # 3. for those that we have the screenName in the uservax table, add those to the 
  #    nodes and write to the graph. Then remove those names from the list.
  for(i in seq_len(nrow(p1))) {
    q1 <- paste0("match (n:", dbname, " {uid:'", p1$id[i], "'}) return n.uid, n.screenName, n.count, keys(n);")
    print(cypher(db, q1))
    q1 <- paste0("match (n:", dbname, " {uid:'", p1$id[i], "'}) set n.screenName = '", p1$screenName[i], "' return n.uid, n.screenName, n.count;")
    print(cypher(db, q1))
  }
  
  # remove from list
  uids <- setdiff(g1$followed, p1$id)
  
  # 4. for those top-100 that we don't have the screenName, do a twitter lookup
  #    to get the screenNames and store those in (a) the uservax table and (b)
  #    in the graph database per node (use the 'set' command to add a new property)
  
  ### --- rate limits
  # go to Twitter and get friends 
  #ratelimit <- getCurRateLimitInfo(c("friends", "application"))
  if(0) {
  ratelimit <- getCurRateLimitInfo()
  print(ratelimit)
  friendlimit <- ratelimit[which(grepl("/friends/ids", ratelimit$resource)),]$remaining %>% as.integer()
  if(friendlimit == 0) {
    print("Reached rate limit. Sleeping 30 seconds")
    Sys.sleep(30)
    ratelimit <- getCurRateLimitInfo(c("friends", "application"))
    print(ratelimit)
    friendlimit <- ratelimit[which(grepl("/friends/ids", ratelimit$resource)),]$remaining %>% as.integer()
  }
  }
  ### --- rate limits
  
  users <- lookupUsers(uids)
  con <- dbConnect(RMySQL::MySQL(), dbname="twitter", host="127.0.0.1")
  register_db_backend(con)
  store_users_db(users, table_name="uservax")
  dbDisconnect(con)
  
  uid <- paste0(uids, collapse="','")
  
  q1 <- paste0("select distinct id, screenName from uservax ", "where id in ('", uid, "') order by id;")
  con <- dbConnect(RMySQL::MySQL(), dbname="twitter", host="127.0.0.1")
  p1 <- dbGetQuery(con, q1)
  dbDisconnect(con)
  
  for(i in seq_len(nrow(p1))) {
    q1 <- paste0("match (n:", dbname, " {uid:'", p1$id[i], "'}) return n.uid, n.screenName, n.count, keys(n);")
    print(cypher(db, q1))
    q1 <- paste0("match (n:", dbname, " {uid:'", p1$id[i], "'}) set n.screenName = '", p1$screenName[i], "' return n.uid, n.screenName, n.count;")
    print(cypher(db, q1))
  }

  # 5. get the next top 100 and do the next 100 and so on. Ensure we don't blow
  #    up twitter's rate limitations.
 
}

#####################################################################################
if(0) {
  p2 <- as.list(p1[i,])
}
write_to_graph_sen_doc <- function(db, dbname, p2, write_db=T)
{
  # use to combine multiple cypher inserts into one
  query_sep <- " with count(*) as dummy "
  
  # uid = tweet user id
  
  # node1 follows node2, eg; (node1) -[:follows]-> (node2)
  query_all <- as.character(NULL)
  
  # create sentence node
  q1 <- paste0("merge (n:", dbname, ":aclaim",
    "  {text:'", p2$text, 
    "', lineno:'", as.integer(p2$line), 
    "', ccit:'", p2$c.cit, 
    "', cfig:'", p2$c.fig, 
    "', ctab:'", p2$c.tab, 
    "' }) ",
    "on create set n.count = 1 ",
    "on match set n.count = n.count+1 ")
  query_all <- paste0(query_all, query_sep, q1)
  
  # create document node
  q1 <- paste0("merge (n:", dbname, ":adoc", 
    "  {name:'", p2$filename, 
    "' }) ",
    "on create set n.count = 1 ",
    "on match set n.count = n.count+1 ")
  query_all <- paste0(query_all, query_sep, q1)
  
  # create edge from node to text
  q1 <- paste0("match (n:", dbname, ":aclaim",  " {text:'", p2$text, "'}), ", 
                     "(m:", dbname, ":adoc", " {name:'", p2$filename, "'}) ",
               "merge (n)-[r:stores]->(m) ",
               "on create set r.count = 1 ",
               "on match set r.count = r.count+1 ")
  query_all <- paste0(query_all, query_sep, q1)
  
  if(write_db) cypher(db, query_all) 

  return(query_all)
}

#####################################################################################
if(0) {
  p2 <- as.list(p1[i,])
}
write_to_graph_sen_ref <- function(db, dbname, p2, write_db=T)
{
  # use to combine multiple cypher inserts into one
  query_sep <- " with count(*) as dummy "
  
  # uid = tweet user id
  
  # node1 follows node2, eg; (node1) -[:follows]-> (node2)
  query_all <- as.character(NULL)
  
  # create sentence node
  q1 <- paste0("merge (n:", dbname, ":aclaim",
    "  {text:'", p2$text, 
    "', lineno:'", as.integer(p2$line), 
    "', ccit:'", p2$c.cit, 
    "', cfig:'", p2$c.fig, 
    "', ctab:'", p2$c.tab, 
    "' }) ",
    "on create set n.count = 1 ",
    "on match set n.count = n.count+1 ")
  query_all <- paste0(query_all, query_sep, q1)
  
  # create document node
  q1 <- paste0("merge (n:", dbname, ":aref", 
    "  {ref:'", p2$ref, 
    "', cid:'", as.integer(p2$cid), 
    "' }) ",
    "on create set n.count = 1 ",
    "on match set n.count = n.count+1 ")
  query_all <- paste0(query_all, query_sep, q1)
  
  # create edge from node to text
  q1 <- paste0("match (n:", dbname, ":aclaim",  " {text:'", p2$text, "'}), ", 
                     "(m:", dbname, ":aref", " {ref:'", p2$ref, "'}) ",
               "merge (n)-[r:supby]->(m) ",
               "on create set r.count = 1 ",
               "on match set r.count = r.count+1 ")
  query_all <- paste0(query_all, query_sep, q1)
  
  if(write_db) cypher(db, query_all) 

  return(query_all)
}


#####################################################################################
if(0) {
  p1 <- df.doc
}
graph_doc <- function(db, dbname, p1)
{
  #p1 <- df.doc %>% filter(line == 22)
  p1 <- data.frame(lapply(p1, function(x) gsub("(’|')", "\\\\'", x)), stringsAsFactors = F)
  
  #delete_nodes_db(db, dbname)

  print("start building graph")
  
  for(i in seq_len(nrow(p1))) {
    #print(i)
    if(is.na(p1$text[i])) next ;
    if(is.na(p1$cid[i])) next ;
    if(p1$c.cit[i]) {
      #print(paste(i, "=============="))
      # aaaa
      # 1. create the sentence node
      write_to_graph_sen_doc(db, dbname, as.list(p1[i,]), write_db=T)
      write_to_graph_sen_ref(db, dbname, as.list(p1[i,]), write_db=T)
    }
  }
}

#####################################################################################
graph_test_function <- function(db, dbname)
{
  #### do Neo4j database setup
  
  cypher(db, "match (n) return count(*);") 
  cypher(db, paste0("match (n:", dbname, ") return count(*);"))
  
  # ida follows idb, eg; (ida) -[:follows]-> (idb)
  #my_cypher(db, paste0("create index on :", dbname, "(uid)"))
  #my_cypher(db, paste0("create index on :", dbname, "(screenName)"))
  # check if index is built
  #system(paste0("curl http://localhost:7474/db/data/schema/index/", dbname), ignore.stderr=T, intern=T)
  system(paste0("curl ", db_url, "/schema/index/", dbname), ignore.stderr=T, intern=T)

  ida <- 33 
  idb <- 55 
  idc <- 77 
  idd <- 11 
  write_to_graph(db, dbname, ida, idb)
  write_to_graph(db, dbname, idb, ida)
  write_to_graph(db, dbname, ida, idc)
  write_to_graph(db, dbname, idc, idd)
  summary1(db, dbname)
  delete_nodes_db(db, dbname)
  query_by_rel_count1(db, dbname)
  query_by_rel_count2(db, dbname)
}

#####################################################################################
initialize_neo4j <- function(dbname=NULL, host='local', delete_db=F)
{
  print("set up Neo4j database")
  if(host == 'local') {
    logdata("Using local host")
    db_url <- "http://localhost:7474/db/data"
    db <- startGraph(db_url)
  } else if(host == 'AWS') {
    logdata("Using AWS host")
    db_url <- "http://mgh.kluriganalytics.com:7474/db/data"
    db <- startGraph(db_url, username='neo4j', password='hpr284')
  } else {
    logdata("ERROR: Unknown host")
  }
  if(is.null(dbname)) {
    #dbname <- "arg3"  # this is the real one
    #dbname <- "arg4"  # testing of new docid functionality from massCATS documents
    #dbname <- "arg5"  # testing of new docid functionality from massCATS documents
    dbname <- "arg1"  # July 13, 2017 
  }
  logdata("Using", dbname, "as database")

  if(delete_db) delete_nodes_db(db, dbname)
  
  return(list(db=db, dbname=dbname))
}


