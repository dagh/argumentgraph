#!/usr/bin/env Rscript 



#################################################################################
assign_hierarchical_levels_2 <- function(nodes, edges)
{
  #save(nodes, edges, file=file.path(codeDir, "debug3.rdata"))
  if(0) {
    load(file=file.path(codeDir, "debug3.rdata"))
    nodes <- nodes %>% filter(id %in% c(72920038, 72920133, 72909924, 72909872, 72909859, 72909903,72909898,72909921))
    edges <- edges %>% filter(from %in% c(72920038, 72920133, 72909924, 72909872, 72909859, 72909903,72909898,72909921))
    edges <- edges %>% filter(to %in% c(72920038, 72920133, 72909924, 72909872, 72909859, 72909903,72909898,72909921))
    #edges <- edges[-3,]
    nodes$shape <- NULL
    nodes$label <- nodes$id
    g1 <- graph_from_data_frame(edges, directed=T, vertices=nodes)
    
    root <- which(degree(g1, mode='in') == 0)
    (l1 <- layout_as_tree(g1, root=root, mode='out'))
    (l1 <- layout_as_tree(g1, mode='out'))
    (l1 <- layout_as_tree(g1, mode='in'))
    plot(g1, layout=l1)
    plot(g1)
    
    summary(g1)
    E(g1)$label <- ''
    V(g1)$label
    tkplot(g1)
    d1 <- distances(g1, mode="out")
    d1[d1 == Inf] <- 0
    d1
    nodes$level <- apply(d1, 2, max) + 1
  }
  
  names(nodes)
  nodes$level <- NULL
  
  g1 <- graph_from_data_frame(edges, directed=T, vertices=nodes)
  
  (d1 <- distances(g1, mode="out"))
  (d1[d1 == Inf] <- 0)
  nodes$level <- apply(d1, 2, max) + 1
  
  return(nodes)
}

#################################################################################
get_node_data <- function(db, dbname, label, unique_nodes = NULL)
{
  if(is.null(unique_nodes)) { 
    s1 <- paste0("(n:", dbname, ":", label, ")")
  } else {
    s1 <- paste0("(n:", dbname, ":", label, ") where id(n) in [", paste(unique_nodes, collapse=','), "] ")
  }

  q1 <- paste0("match ", s1, " return keys(n) as key limit 1;")
  (k1 <- cypher(db,q1) %>% unlist() %>% as.character())
  if(length(k1) == 0) {
    #logdata("query:", q1)
    #logdata("did not find nodes for label:", label, "so returning NULL")
    return(NULL)
  }
  (k2 <- paste("n.", k1, sep="") %>% paste(., collapse=", "))
  
  (q1 <- paste(c("match ", s1, " return id(n),", k2, " ;"), collapse=" "))
  d10 <- cypher(db, q1)
  names(d10) <- c('id', k1)
  d10$group <- label
  
  return(d10)
}

#################################################################################
# this function gets a list of list of relationships. Then breaks the
# lists apart to find each relationship. From each relationship, the function
# gets the start (from) node id, the end (to) node id, and the 
# relationship type.  This is now an edge list, which is uniqued, sorted and returned

if(0) {
  # optimizing the function
  library(profvis)
  
  q2="match (n2:arg7)-[r1*1..10]->(m) where id(n2)=73834351 and not ('aut' in labels(m)) return n2, r1, m"
  q2="match (n2:arg7)-[r1*1..7]->(m) where id(n2)=73834351 and not ('aut' in labels(m)) return n2, r1, m"
  rx <- cypherToList(db, q2)
  length(rx)
  rel_list=rx
  
  profvis({
    if(length(rel_list) == 0) return(NULL)
    datalist = list()
    edge_count <- 0
    for(i in seq_len(length(rel_list))) {
      for(j in seq_len(length(rel_list[[i]]$r))) {
        (rel <- rel_list[[i]]$r[[j]])
        from <- basename(attr(rel, "start"))
        to <- basename(attr(rel, "end"))
        type <- attr(rel, "type")
        tmp <- c(from, to, type)
        edge_count <- edge_count + 1
        datalist[[edge_count]] <- tmp
      }
    }
    e5 <- do.call(rbind, datalist) %>% as.data.frame(stringsAsFactors=F)
    names(e5) <- c('from', 'to', 'type')
    if(!is.null(e5)) e5 <- distinct(e5) %>% arrange(from, to)
  })
}
  
# ------------
relationshiplist2dataframe <- function(rel_list)
{
  #logdata("start of relationshiplist2dataframe")
  if(length(rel_list) == 0) return(NULL)
  datalist = list()
  edge_count <- 0
  for(i in seq_len(length(rel_list))) {
    for(j in seq_len(length(rel_list[[i]]$r))) {
      (rel <- rel_list[[i]]$r[[j]])
      from <- basename(attr(rel, "start"))
      to <- basename(attr(rel, "end"))
      type <- attr(rel, "type")
      tmp <- c(from, to, type)
      edge_count <- edge_count + 1
      datalist[[edge_count]] <- tmp
    }
  }
  e3 <- do.call(rbind, datalist) %>% as.data.frame(stringsAsFactors=F)
  names(e3) <- c('from', 'to', 'type')
  if(!is.null(e3)) e3 <- distinct(e3) %>% arrange(from, to)

  #logdata("end of relationshiplist2dataframe")
  
  return(e3)
}

#################################################################################
# this function removes references nodes and instead connects the rtxt node
# directly to the document node
remove_reference_nodes <- function(refs, nodes, edges)
{
  if(0) {
    #save(refs, nodes, edges, file=file.path(codeDir, "debug2.rdata"))
    load(file=file.path(codeDir, "debug2.rdata"))
    edges <- edges[1:4,1:2] ; nodes <- nodes[1:7,1:2] ; refs <- refs[1:3,]
    edges$from[1] <- 22 ; edges$to[1] <- 33
    edges$from[2] <- 33 ; edges$to[2] <- 44
    edges$from[3] <- 55 ; edges$to[3] <- 66
    edges$from[4] <- 77 ; edges$to[4] <- 88
    nodes$id[1] <- 22 ; nodes$id[2] <- 33 ; nodes$id[3] <- 44 ; nodes$id[4] <- 55 ;
    nodes$id[5] <- 66 ; nodes$id[6] <- 77 ; nodes$id[7] <- 88 ;
    refs$id[1]  <- 33 ; refs$id[2]  <- 66 ; refs$id[3]  <- 77 ;
  }
  
  if(is.null(refs)) refs <- data.frame()
  # remove the (refs) node from the node table
  nodes <- nodes %>% filter(!(id %in% refs$id))

  for(i in seq_len(nrow(refs))) {
    (id1 <- refs$id[i])

    (todoc <- edges$to[edges$from == id1])
    
    # if we find a document, then redirect the edge to the doc
    if(length(todoc) > 0) edges$to[edges$to == id1] <- todoc 
      
    # remove the old link: (refs)-[:references]->(doc) and/or
    #                      (rtxt)-[:suppored_by]->(refs)
    edges <- edges %>% filter(from != id1 & to != id1)
  }
  return(list(nodes = nodes, edges = edges))
}

#################################################################################
if(0) {
  node <- docs
  node <- rtxt
  node <- itxt
  node <- imgs
  node <- auts
}
build_node_structure <- function(node)
{
  if(is.null(node)) return(NULL)
  
  label_max_length <- 250
  title_search_term <- "(.{1,120})(\\s|$)"
  label_search_term <- "(.{1,50})(\\s|$)"
  (grp <- node$group[1])
  names(node)
  
  if(grp == 'doc') {
    abstract_max_length <- 500

    #node$title1 <- sapply(node$title, function(x) gsub('(.{1,90})(\\s|$)', '\\1\n', x))
    node$title1 <- sapply(node$title, function(x) gsub(title_search_term, '\\1\n', x))

    node$abstract1 <- ifelse(nchar(node$abstract) > abstract_max_length, paste0(substring(node$abstract,1,(abstract_max_length-3)), "..."), node$abstract)
    #node$abstract1 <- sapply(node$abstract1, function(x) gsub('(.{1,90})(\\s|$)', '\\1<br>', x))
    node$abstract1 <- sapply(node$abstract1, function(x) gsub(title_search_term, '\\1<br>', x))

    node$pmid1 <- ifelse(node$pmid == '', "NA",  paste0("<a href=\"https://www.ncbi.nlm.nih.gov/pubmed/", node$pmid, "\", target=\"_blank\">", node$pmid, "</a>"))
    
    node$pmc <- ifelse(node$pmc == '', "NA", node$pmc)
    
    node$filename1 <- ifelse(node$filename == '', NA,  paste0("docs/", node$filename))
    node$on_file <- ifelse(is.na(node$filename1), 'No', 'Yes')
   
    node$title <- paste(
      '<b>Id:</b>'   ,   node$docid,  '<br>',
      '<b>Title</b>:',   node$title1, '<br>',
      '<b>Abstract</b>:', node$abstract1,
      '<b>Journal</b>:', node$journal, '<br>',
      '<b>Date</b>:',    mapply(parse_date, y=node$year, m=node$month, d=node$date), '<br>',
      '<b>PMID</b>:',    node$pmid1, '<br>',
      '<b>PMC</b>:',     node$pmc,    '<br>',
      '<b>DOI</b>:',     paste0("<a href=\"http://doi.org/", node$doi, "\", target=\"_blank\">", node$doi, "</a>"), '<br>',
      '<b>On file</b>:', node$on_file,    '<br>')

    node$label <- ifelse(nchar(node$title1) > label_max_length, paste0(substring(node$title1,1,(label_max_length-3)), "..."), node$title1)
    node$label <- sapply(node$label, function(x) gsub(label_search_term, '\\1\n', x))
    
    df <- node %>% select(id, group, label, title, filename=filename1)
    
  } else if(grp %in% c('rtxt', 'itxt')) {
    #node$text1 <- sapply(node$text, function(x) gsub('(.{1,90})(\\s|$)', '\\1<br>', x))
    node$text1 <- sapply(node$text, function(x) gsub(title_search_term, '\\1<br>', x))

    node$title <- paste(
      '<b>Sentence</b>:', node$text1,
      '<b>Sentence Number</b>:', node$linenumber, '<br>')
    node$label <- ifelse(nchar(node$text) > label_max_length, paste0(substring(node$text,1,(label_max_length-3)), "..."), node$text)
    node$label <- sapply(node$label, function(x) gsub(label_search_term, '\\1\n', x))

    df <- node %>% mutate(filename=NA) %>% select(id, group, label, title, filename)
  } else if(grp == 'img') {
    node$filename1 <- ifelse(node$imagefile == '', NA,  paste0("images/", node$imagefile))
    
    node$text <- node$caption
    node$title <- paste(
      '<b>Caption</b>:', node$caption, '<br>',
      '<b>ImageID</b>:', node$imageid, '<br>',
      '<b>Page</b>:', node$page, '<br>')
    node$label <- ifelse(nchar(node$caption) > label_max_length, paste0(substring(node$caption,1,(label_max_length-3)), "..."), node$caption)
    node$label <- sapply(node$label, function(x) gsub(label_search_term, '\\1\n', x))
    
    df <- node %>% select(id, group, label, title, filename=filename1)
  } else if(grp == 'aut') {
    
    # make first letter upper, then the rest lower case
    node$l1 <- paste0(toupper(substring(node$last, 1, 1)), tolower(substring(node$last, 2)))
    node$f1 <- paste0(toupper(substring(node$first, 1, 1)), tolower(substring(node$first, 2)))
    node$m1 <- paste0(toupper(substring(node$middle, 1, 1)), tolower(substring(node$middle, 2)))
    # paste names together
    node$name <- ifelse(node$m1 == '', paste0(node$l1, ", ", node$f1), paste0(node$l1, ", ", node$f1, " ", node$m1))
    # remove all characters that are not letters, commas, spaces and dashes
    node$name <- gsub("[^A-z, -]", "", node$name)
    
    node$orcid1 <- ifelse(node$orcid == '', NA, node$orcid)
    
    head(node)
    node$title <- paste(
      '<b>Id</b>:', node$auid, '<br>',
      '<b>Name</b>:', node$name, '<br>',
      '<b>Orcid</b>:', node$orcid1, '<br>')

    node$label <- ifelse(nchar(node$name) > label_max_length, paste0(substring(node$name,1,(label_max_length-3)), "..."), node$name)
    node$label <- sapply(node$label, function(x) gsub(label_search_term, '\\1\n', x))
    
    df <- node %>% mutate(filename=NA) %>% select(id, group, label, title, filename)
  } else {
    logdata("WARNING - unknown group:", grp)
  }
  
  return(df)
}
  
#################################################################################
if(0) {
  upstreamhops <- 3 
  downstreamhops <- 2
  include_authors <- keep_refs <- F
  nodeid <- NULL
  docid <- 4
}
getarg3 <- function(db, dbname, docid, nodeid=NULL, upstreamhops=1, downstreamhops=1, include_authors=FALSE, max_nodes=100, title=NULL, keep_refs=F)
{
  if(!is.null(title)) {
    title <- gsub("'", "\\\\'", title)
    q1 <- paste0("match (n:", dbname, ":doc {title:'", title, "'}) return id(n) as nodeid;")
    nodeid <- cypher(db, q1)$nodeid[1]
  }

  if(is.null(nodeid)) {
    # nodeid == NULL
    logdata("NOTE - nodeid and title are both NULL")
    
    if(include_authors) {
      q1 <- paste0("match (n:", dbname, ")-[r1]->(m) ", 
        "return id(n) as from, type(r1) as label, id(m) as to limit ", max_nodes*1.5, " ;")
        #"return id(n) as from, type(r1) as label, id(m) as to ;")
    } else {
      q1 <- paste0("match (n:", dbname, ")-[r1]->(m) ",
        "where type(r1) <> 'has_attribution' ",
        "return id(n) as from, type(r1) as label, id(m) as to limit ", max_nodes*1.5, " ;")
        #"return id(n) as from, type(r1) as label, id(m) as to ;")
    }

    #logdata("q1:", q1)
    edges <- cypher(db, q1) 
  } else {
    # nodeid has a read nodeid
    # call for nodes and edges towards the nodeid
    
    if(include_authors) {
      q1 <- paste0("match (n)-[r1*1..", upstreamhops, "]->(n1:", dbname, ") ", 
        "where id(n1)=", nodeid, " return n, r1, n1 limit ", max_nodes*1.5)
    } else {
      q1 <- paste0("match (n)-[r1*1..", upstreamhops, "]->(n1:", dbname, ") ", 
        "where id(n1)=", nodeid, " and not ('aut' in labels(n)) return n, r1, n1 limit ", max_nodes*1.5)
    }
    logdata("q1:", q1)
    r1 <- cypherToList(db, q1)
    e1 <- relationshiplist2dataframe(r1)
    
    # call for nodes and edges from the nodeid
    if(include_authors) {
      q2 <- paste0("match (n2:", dbname, ")-[r1*1..", downstreamhops, "]->(m) ", 
        "where id(n2)=", nodeid, " return n2, r1, m limit ", max_nodes*1.5)
    } else {
      q2 <- paste0("match (n2:", dbname, ")-[r1*1..", downstreamhops, "]->(m) ", 
        "where id(n2)=", nodeid, " and not ('aut' in labels(m)) return n2, r1, m limit ", max_nodes*1.5)
    }
    logdata("q2:", q2)
    r1 <- cypherToList(db, q2)
    logdata("q2 - length(r1):", length(r1))
    e2 <- relationshiplist2dataframe(r1)
    logdata("q2 - nrow(e2):", nrow(e2)) 

    if(is.null(e1) & is.null(e2)) {
      logdata("There are no edges - returning NULL")
      return(NULL)
    }
    
    edges <- rbind(e1, e2) %>% select(from, to, label=type)
    #edges$label <- NA
  }

  if(is.null(edges)) return(NULL)
  if(nrow(edges)==0) return(NULL)
  
  names(edges)

  edges <- edges %>% select(from, to, label) %>% distinct()
  unique_nodes <- unique(c(edges$to, edges$from))
  
  # get all node data
  docs <- get_node_data(db, dbname, "doc", unique_nodes=unique_nodes)
  rtxt <- get_node_data(db, dbname, "rtxt", unique_nodes=unique_nodes)
  itxt <- get_node_data(db, dbname, "itxt", unique_nodes=unique_nodes)
  imgs <- get_node_data(db, dbname, "img", unique_nodes=unique_nodes)
  auts <- get_node_data(db, dbname, "aut", unique_nodes=unique_nodes)
  #refs <- get_node_data(db, dbname, "ref", unique_nodes=unique_nodes)
  #caps <- get_node_data(db, dbname, "cap", unique_nodes=unique_nodes)
  
  docs1 <- build_node_structure(docs)
  rtxt1 <- build_node_structure(rtxt)
  itxt1 <- build_node_structure(itxt)
  imgs1 <- build_node_structure(imgs)
  auts1 <- build_node_structure(auts)
  
  nodes <- rbind(docs1, rtxt1, itxt1, imgs1, auts1)
  
  # remove nodes that does not have an edge
  #unique_nodes <- c(edges$from, edges$to) %>% unique()
  #nodes <- filter(nodes, id %in% unique_nodes)
  
  nodes$shape <- 'box'
  #nodes$label <- substring(nodes$label, 1, 300)
  #nodes$label <- sapply(nodes$label, function(x) gsub('(.{1,50})(\\s|$)', '\\1\n', x))
  #nodes$title <- nodes$label
  
  edges$color <- 'green'
  edges$color[edges$label == 'supported_by'] <- 'salmon'

  #edges$label <- ''
  #nodes$color <- 'lightgreen'
  #nodes$color[nodes$group == 'rtxt'] <- 'salmon'
  #nodes$font.color[nodes$group == 'rtxt'] <- 'white'
  #nodes$color[nodes$group == 'ref']  <- 'yellow'
  #nodes$color[nodes$group == 'aut']  <- 'red'
  #nodes$color[nodes$group == 'itxt'] <- 'lightblue'
  #nodes$color[nodes$group == 'cap']  <- 'gray'
  #nodes$color[nodes$group == 'img']  <- 'salmon'

  #if(!keep_refs) {
  #  list1 <- remove_reference_nodes(refs, nodes, edges)
  #  nodes <- list1$nodes
  #  edges <- list1$edges
  #}

  nodes <- assign_hierarchical_levels_2(nodes, edges)
  nodes$primary <- FALSE
  nodes$primary[nodes$id == nodeid] <- TRUE
  
  set.seed(1)
  g2 <- graph_from_data_frame(edges, directed=T, vertices=nodes)
  return(g2)
  #return(list(nodes=nodes, edges=edges))
}

#################################################################################
fake_main3 <- function()
{
  # start of initialization
  rm(list=ls())

  if(Sys.info()['nodename'] == 'hadoop') {
    rootDir <- "/home_ssd/dag/0-Annat/0-R/customers/mgh/code/shiny"
  } else {
    rootDir <- "/home/ubuntu/mgh/code/shiny"
  }
  codeDir <- rootDir
  source(file.path(codeDir, "utils.r"))

  library(RNeo4j)
  library(dplyr)
  library(igraph)

  dblist <- initialize_neo4j(dbname='arg3', delete_db=F)
  db     <- dblist$db
  dbname <- dblist$dbname
  #dbname <- "arg4"
  logdata("neo4j database name:", dbname)
  docid=17
  # done of initialization
  
  #list1 <- getarg3(db, dbname, docid, nodeid=input$current_node_id3, 
  #g2 <- getarg3(db, dbname, docid, nodeid=10361,
  g2 <- getarg3(db, dbname, docid, nodeid=NULL,
                upstreamhops=3, downstreamhops=2,
                include_authors=F, keep_refs=F) 
  g3 <- delete_vertex_attr(g2, 'shape')
  plot(g3)
  
  
  list1 <- getarg3(db, dbname, docid, keep_refs=T)
  nodes <- list1$nodes
  edges <- list1$edges
  
  visNetwork(nodes, edges, width='100%') %>% visPhysics(enabled=F)
  
  if(0) {
    # just for debugging purposes to be able to view the graph in a pictorial way
    nodes$shape <- NULL
    g2 <- graph_from_data_frame(edges, directed=T, vertices=nodes)
    tkplot(g2)
  }
}
