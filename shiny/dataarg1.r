#!/usr/bin/env Rscript 

#################################################################################
get_node_data <- function(db, dbname, label)
{
  s1 <- paste0("(n:", dbname, ":", label, ")")

  q1 <- paste0("match ", s1, " return keys(n) as key limit 1;")
  (k1 <- cypher(db,q1) %>% unlist() %>% as.character())
  (k2 <- paste("n.", k1, sep="") %>% paste(., collapse=", "))
  
  (q1 <- paste(c("match ", s1, " return id(n),", k2, " ;"), collapse=" "))
  d10 <- cypher(db, q1)
  names(d10) <- c('id', k1)
  d10$group <- label
  
  return(d10)
}
  
#################################################################################
assign_hierarchical_levels <- function(docid, nodes, edges)
{
  # removed this for now
  #library(igraph)
  
  names(nodes)
  nodes$level <- NULL
  
  if(0) {
    s1 <- c(edges$from, edges$to)
    s2 <- unique(s1)
    setdiff(s2, nodes$id)
  }
  
  #e1 <- unique(c(edges$from, edges$to))
  
  g1 <- graph_from_data_frame(edges, directed=T, vertices=nodes)
  
  # root node
  index <- which(V(g1)$group == 'doc' & V(g1)$docid == docid)
  
  # get levels from the number of hops from the root node
  path1 <- shortest_paths(g1, from=index, to=V(g1), mode='all')
  
  # assign levels
  nodes$level <- sapply(path1$vpath, function(x) length(as.character(x)))
  
  # always assign bottom doc to the lowest levels...
  nodes$level[nodes$group == 'doc' & nodes$docid != docid] <- max(nodes$level)
  
  # ... and assign the refs to the second lowest levels
  nodes$level[nodes$group == 'ref'] <- (max(nodes$level)-1)
  
  return(nodes)
}

#################################################################################
if(0) {
  keep_refs=F
}
claim_data_old <- function(db, dbname, docid, keep_refs=F)
{
  (q1 <- paste0("match (n:", dbname, ":rtxt {docid:'", docid, "', type:'paragraph'})-[r1:claim_supported_by]->(m) ",
                "optional match (m)-[r2:supported_by]->(o)-[r3:references]->(n1) ",
                "return id(n) as node1, n.linenumber, type(r1) as edge1, id(m) as node2, m.linenumber, ",
                "type(r2) as edge2, id(o) as node3, type(r3) as edge3, id(n1) as node4 ;"))
  
  edges <- cypher(db, q1) 
  
  names(edges)
  # because how neo4j returns data, and then how we break the data apart, we get 
  # duplicate rows. Using distinct() removes all duplicates
  e1 <- edges %>% select(from=node1, to=node2, label=edge1) %>% distinct()
  e2 <- edges %>% select(from=node2, to=node3, label=edge2) %>% filter(!is.na(to)) %>% distinct()
  e3 <- edges %>% select(from=node3, to=node4, label=edge3) %>% filter(!is.na(to)) %>% distinct()
  edges <- rbind(e1, e2, e3) ; rm(e1, e2, e3)
  
  # find zero node and add to edges list
  #claim0node <- cypher(db, paste0("match (n:", dbname, ":rtxt) where not (n)-[:claim_supports]->() return id(n), n.docid, n.type, n.linenumber, n.text;"))
  # 'from' is the 0claim node, and 'to' is the top document.
  topnodes <- cypher(db, paste0("match (n:", dbname, ":rtxt)-[r:claim_part_of]->(m:", dbname, ":doc) ",
                                "return id(n) as from, n.docid, n.type, n.linenumber, n.text, id(m) as to, m.docid, m.pmid, m.doi, type(r) as label;"))
  edges <- rbind(edges, select(topnodes, from, to, label))
  
  # get all node data
  docs <- get_node_data(db, dbname, "doc")
  rtxt <- get_node_data(db, dbname, "rtxt")
  refs <- get_node_data(db, dbname, "ref")
  
  max_nodes1 <- 10
  names(rtxt)
  #(rt <- rtxt[1:3,c(1,2,4,6,7)])
  #(rt <- rtxt[,c(1,2,4,6,7)])
  #rt <- rtxt[1:max_nodes1,]
  rt <- rtxt
  rt$text1 <- sapply(rt$text, function(x) gsub('(.{1,90})(\\s|$)', '\\1<br>', x))
  #rt$bibentry1 <- sapply(rt$bibentry, function(x) gsub('(.{1,90})(\\s|$)', '\\1<br>', x))

  names(refs)
  #(re <- refs[1:3,c(1,2,5,6,7,9)])
  #(re <- refs[,c(1,2,5,6,7,9)])
  #re <- refs[1:max_nodes1,]
  re <- refs

  names(docs)
  #(do <- docs[1:3,c(1,3,5,6,7,8)])
  #(do <- docs[,c(1,3,5,6,7,8)])
  #do <- docs[1:max_nodes1,]
  do <- docs
  #do$title1 <- do$title
  do$title1 <- sapply(do$title, function(x) gsub('(.{1,90})(\\s|$)', '\\1\n', x))
  do$abstract1 <- ifelse(nchar(do$abstract) > 500, paste0(substring(do$abstract,1,497), "..."), do$abstract)
  do$abstract1 <- sapply(do$abstract1, function(x) gsub('(.{1,90})(\\s|$)', '\\1<br>', x))
  do$type1 <- do$type
  do$pmid[do$pmid == ''] <- "NA"
  
  nodes <- full_join(rt, re, by='id') %>%  
    mutate(group=ifelse(is.na(group.x), group.y, group.x)) %>% 
    #mutate(bibentry=ifelse(is.na(bibentry.x), bibentry.y, bibentry.x)) %>% 
    select(-c(group.x, group.y)) %>%
    full_join(., do, by='id') %>%
    mutate(group=ifelse(is.na(group.x), group.y, group.x)) %>% 
    select(-c(group.x, group.y))
  
  nodes$shape <- nodes$label <- nodes$title <- NA
  nodes$urlmap <- 'http://www.google.com'
  title_max_length <- 150
  
  # ----------- decorate rtxt nodes
  index <- which(nodes$group == 'rtxt')
  
  # a bit convoluted, but adding a <br> after all NAs
  #nodes$bibentry1[index] <- sapply(nodes$bibentry1[index], function(x) {
  #  if(is.na(x)) { paste0(x, '<br>') } else { x } 
    
  # a bit convoluted, but adding a <br> after all NAs
  #nodes$bibentry1[index] <- sapply(nodes$bibentry1[index], function(x) { ifelse(is.na(x), 'NA<br>', x) })
  #  '<b>Reference</b>:', nodes$bibentry1[index],
  
  nodes$title[index] <- paste(
    '<b>Sentence</b>:', nodes$text1[index],
    '<b>Sentence Number</b>:', nodes$linenumber[index], '<br>')
  #nodes$label[index] <- nodes$linenumber[index]
  nodes$label[index] <- substring(nodes$text[index], 1, title_max_length)
  #nodes$shape[index] <- 'ellipse'
  nodes$shape[index] <- 'circle'
  
  #'text:', paste0(substring(nodes$text[index],1,title_max_length),'...'), '<br>',
  
  # ----------- decorate ref nodes
  index <- which(nodes$group == 'ref')
  nodes$title[index] <- paste(
    'internal id:', nodes$id[index], '<br>',
    'type: reference', '<br>', 
    'docid:', nodes$docid[index], '<br>',
    'refdocid:', nodes$refdocid[index], '<br>',
    'refid:', nodes$refid[index], '<br>',
    'bibentry:', paste0(substring(nodes$bibentry[index],1,title_max_length),'...'), '<br>',
    'refpmid:', nodes$refpmid[index], '<br>',
    'refdoi:', paste0("<a href=\"http://google.com\">", nodes$refdoi[index], "</a>"))
  
  nodes$label[index] <- nodes$refid[index]
  nodes$shape[index] <- 'box'
  
  if(0) {
    #index.save <- index
    (index <- index.save)
    index = 105
    s1 <- character(2)
    
    s1 <- character(max(index))
    s1[index] <- paste0(s1[index], '<b>id:</b>'      , nodes$docid[index],  '<br>')
    s1[index] <- paste0(s1[index], '<b>Title</b>:'   , nodes$title1[index], '<br>')
    s1[index] <- paste0(s1[index], '<b>Abstract</b>:', nodes$abstract1[index])
    s1[index] <- paste0(s1[index], '<b>Journal</b>:' , nodes$journal[index], '<br>')
    s1[index] <- paste0(s1[index], '<b>Date</b>:'    , mapply(parse_date, y=nodes$year[index], m=nodes$month[index], d=nodes$date[index]), '<br>')
    s1[index] <- paste0(s1[index], '<b>PMID</b>:'    , paste0("<a href=\"https://www.ncbi.nlm.nih.gov/pubmed/", nodes$pmid[index], "\", target=\"_blank\">", nodes$pmid[index], "</a>"), '<br>')
    s1[index] <- paste0(s1[index], '<b>PMC</b>:'     , nodes$pmc[index], '<br>')
    s1[index] <- paste0(s1[index], '<b>DOI</b>:'     , paste0("<a href=\"http://doi.org/", nodes$doi[index], "\", target=\"_blank\">", nodes$doi[index], "</a>"))
  }
  
  # ----------- decorate doc nodes
  index <- which(nodes$group == 'doc')
  nodes$title[index] <- paste(
    '<b>id:</b>'   ,   nodes$docid[index],  '<br>',
    '<b>Title</b>:',   nodes$title1[index], '<br>',
    '<b>Abstract</b>:', nodes$abstract1[index],
    '<b>Journal</b>:', nodes$journal[index], '<br>',
    '<b>Date</b>:',    mapply(parse_date, y=nodes$year[index], m=nodes$month[index], d=nodes$date[index]), '<br>',
    '<b>PMID</b>:', paste0("<a href=\"https://www.ncbi.nlm.nih.gov/pubmed/", nodes$pmid[index], "\", target=\"_blank\">", nodes$pmid[index], "</a>"), '<br>',
    '<b>PMC</b>:', nodes$pmc[index],    '<br>',
    '<b>DOI</b>:', paste0("<a href=\"http://doi.org/", nodes$doi[index], "\", target=\"_blank\">", nodes$doi[index], "</a>"))
  #nodes$label[index] <- nodes$docid[index]
  nodes$label[index] <- substring(nodes$title1[index], 1, title_max_length)
  nodes$shape[index] <- 'circle'
  nodes$pmid[index] <- ifelse(nodes$pmid[index] == '', NA, nodes$pmid[index])
  nodes$urlmap[index] <- paste0('https://www.ncbi.nlm.nih.gov/pubmed/', nodes$pmid[index])

    #'Title:', paste0(substring(nodes$title1[index],1,title_max_length),'...'), '<br>',

  # aaaaa
  nodes$shape <- 'box'
  nodes$label <- sapply(nodes$label, function(x) gsub('(.{1,50})(\\s|$)', '\\1\n', x))
  
  #edges$color <- 'darkgrey'
  edges$color <- 'salmon'
  edges$label <- ''
#  nodes$level <- NA
#  nodes$level[nodes$group == 'doc'] <- 4
#  nodes$level[nodes$group == 'doc' & nodes$docid == '2878'] <- 1
#  nodes$level[nodes$group == 'rtxt'] <- 2
#  nodes$level[nodes$group == 'ref'] <- 3
  nodes$color <- 'lightgreen'
  nodes$color[nodes$group == 'rtxt'] <- 'salmon'
  nodes$color[nodes$group == 'ref'] <- 'yellow'
  nodes$color[nodes$group == 'doc' & nodes$type1 == 'dataset'] <- 'lightblue'
  
  if(!keep_refs) {
    # this section remove all reference nodes and edges
    #nodes.save <- nodes; edges.save <- edges
    #nodes <- nodes.save ; edges <- edges.save
    
    #edges <- filter(edges, from %in% refs$id[1:3] | to %in% refs$id[1:3])
    #x1 <- unique(c(edges$from, edges$to))
    #nodes <- filter(nodes, id %in% x1)
    #nodes$shape <- NULL
    # g1 <- graph_from_data_frame(edges, directed=F, vertices=nodes) ; plot(g1)
    
    for(i in seq_len(nrow(refs))) {
      (id1 <- refs$id[i])
      # this is what it starts like:
      #   (rtxt-A)-[:supported_by]->(refs-B)
      #   (refs-B)-[:references]->(docid-C)
      # and we want to remove the reference so that it looks like this
      #   (rtxt-A)-[:supported_by]->(docid-C)
      
      # find the document that the reference is referencing: this the todoc
      # (rtxt)-[:supported_by]->(refs)
      # (refs)-[:references]->(docid)
      (todoc <- edges$to[edges$from == id1])
      
      # change the "to" to the new "todoc", so the link looks like this now
      # (rtxt)-[:supported_by]->(docid)
      edges$to[edges$to == id1] <- todoc
      
      # remove the old link: (refs)-[:references]->(doc)
      edges <- edges %>% filter(from != id1)
      
      # remove the (refs) node from the node table
      nodes <- nodes %>% filter(id != id1)
    }
    #g1 <- graph_from_data_frame(edges, directed=F, vertices=nodes) ; plot(g1)
    
  }
  
  # assign levels to the nodes
  nodes <- assign_hierarchical_levels(docid, nodes, edges)
  
  save(nodes, edges, file=file.path(codeDir, "g5.rdata"))
  return(list(nodes=nodes, edges=edges))

}

#################################################################################
if(0) {
  keep_refs=F
}
claim_data <- function(db, dbname, docid, keep_refs=F)
{
  (q1 <- paste0("match (n:", dbname, ":rtxt {docid:'", docid, "', type:'paragraph'})-[r1]->(m) ",
                "return id(n) as node1, n.linenumber, type(r1) as edge1, id(m) as node2, m.linenumber ;"))
  
  edges <- cypher(db, q1) 
  
  zeronode_linenumber <- min(edges$n.linenumber)[1]
  edges <- edges %>% select(from=node1, to=node2, label=edge1) %>% distinct()
  
  # find zero node and add to edges list
  (q1 <- paste0("match (n:", dbname, ":rtxt {linenumber:'", zeronode_linenumber, "'})<-[r1:includes]-(m) ",
                "return id(m) as from, id(n) as to, type(r1) as label ;"))
  e1 <- cypher(db, q1) 
  edges <- rbind(e1, edges)
  
  # get all node data
  docs <- get_node_data(db, dbname, "doc")
  rtxt <- get_node_data(db, dbname, "rtxt")
  #refs <- get_node_data(db, dbname, "ref")
  
  max_nodes1 <- 10
  names(rtxt)
  rt <- rtxt
  rt$text1 <- sapply(rt$text, function(x) gsub('(.{1,90})(\\s|$)', '\\1<br>', x))

  names(docs)
  do <- docs
  do$title1 <- sapply(do$title, function(x) gsub('(.{1,90})(\\s|$)', '\\1\n', x))
  do$abstract1 <- ifelse(nchar(do$abstract) > 500, paste0(substring(do$abstract,1,497), "..."), do$abstract)
  do$abstract1 <- sapply(do$abstract1, function(x) gsub('(.{1,90})(\\s|$)', '\\1<br>', x))
  do$type1 <- do$type
  do$pmid[do$pmid == ''] <- "NA"
  
  nodes <- full_join(rt, do, by='id') %>%  
    mutate(group=ifelse(is.na(group.x), group.y, group.x)) %>%  select(-c(group.x, group.y)) %>%
    mutate(type=ifelse(is.na(type.x), type.y, type.x)) %>%  select(-c(type.x, type.y)) %>%
    mutate(count=ifelse(is.na(count.x), count.y, count.x)) %>%  select(-c(count.x, count.y)) %>%
    mutate(docid=ifelse(is.na(docid.x), docid.y, docid.x)) %>%  select(-c(docid.x, docid.y))
  #write.csv(nodes, file=file.path(codeDir, "nodes.csv"), row.names=F)
  
  nodes$shape <- nodes$label <- nodes$title <- NA
  nodes$urlmap <- 'http://www.google.com'
  title_max_length <- 150
  
  # ----------- decorate rtxt nodes
  index <- which(nodes$group == 'rtxt')
  
  nodes$title[index] <- paste(
    '<b>Sentence</b>:', nodes$text1[index],
    '<b>Sentence Number</b>:', nodes$linenumber[index], '<br>')
  nodes$label[index] <- substring(nodes$text[index], 1, title_max_length)
  nodes$shape[index] <- 'circle'
  
  # ----------- decorate doc nodes
  index <- which(nodes$group == 'doc')
  nodes$title[index] <- paste(
    '<b>id:</b>'   ,   nodes$docid[index],  '<br>',
    '<b>Title</b>:',   nodes$title1[index], '<br>',
    '<b>Abstract</b>:', nodes$abstract1[index],
    '<b>Journal</b>:', nodes$journal[index], '<br>',
    '<b>Date</b>:',    mapply(parse_date, y=nodes$year[index], m=nodes$month[index], d=nodes$date[index]), '<br>',
    '<b>PMID</b>:', paste0("<a href=\"https://www.ncbi.nlm.nih.gov/pubmed/", nodes$pmid[index], "\", target=\"_blank\">", nodes$pmid[index], "</a>"), '<br>',
    '<b>PMC</b>:', nodes$pmc[index],    '<br>',
    '<b>DOI</b>:', paste0("<a href=\"http://doi.org/", nodes$doi[index], "\", target=\"_blank\">", nodes$doi[index], "</a>"))
  #nodes$label[index] <- nodes$docid[index]
  nodes$label[index] <- substring(nodes$title1[index], 1, title_max_length)
  nodes$shape[index] <- 'circle'
  nodes$pmid[index] <- ifelse(nodes$pmid[index] == '', NA, nodes$pmid[index])
  nodes$urlmap[index] <- paste0('https://www.ncbi.nlm.nih.gov/pubmed/', nodes$pmid[index])

  nodes$shape <- 'box'
  nodes$label <- sapply(nodes$label, function(x) gsub('(.{1,50})(\\s|$)', '\\1\n', x))
  
  edges$color <- 'salmon'
  edges$label <- ''
  nodes$color <- 'lightgreen'
  nodes$color[nodes$group == 'rtxt'] <- 'salmon'
  nodes$color[nodes$group == 'ref'] <- 'yellow'
  nodes$color[nodes$group == 'doc' & nodes$type1 == 'dataset'] <- 'lightblue'
  
  # assign levels to the nodes
  nodes <- assign_hierarchical_levels(docid, nodes, edges)
  
  save(nodes, edges, file=file.path(codeDir, "g5.rdata"))
  return(list(nodes=nodes, edges=edges))

}

#################################################################################
author_graph_test <- function(db, dbname, top_authors=NULL)
{
  n1 <- data.frame(a=letters[1:3])
  print(n1)
  logdata(n1)
  e1 <- data.frame(from=c('a', 'b'), to=c('b', 'c'))
  print(e1)
  logdata(e1)

  g1 <- graph_from_data_frame(e1, directed=F, vertices=n1)
  plot(g1)

  n2 <- as_data_frame(g1, what="vertices")
  print(n2)
  e2 <- as_data_frame(g1, what="edges")
  print(32)
  
  return(list(nodes=n2, edges=e2))
}

##############################################################################
centrality_measures <- function(g4, nodes)
{
  g4 <- simplify(g4)
  
  nodes$degree <- NULL
  
  a <- degree(g4)
  dfx <- data.frame(id=names(a), degree=as.vector(a))
  d1 <- full_join(nodes, dfx, by=c("name"="id"))
  
  a <- betweenness(g4)
  dfx <- data.frame(id=names(a), betweenness=as.vector(a))
  d1 <- full_join(d1, dfx, by=c("name"="id"))
  
  a <- closeness(g4)
  dfx <- data.frame(id=names(a), closeness=as.vector(a))
  d1 <- full_join(d1, dfx, by=c("name"="id"))
  
  a <- eigen_centrality(g4)$vector
  dfx <- data.frame(id=names(a), eigen=as.vector(a))
  d1 <- full_join(d1, dfx, by=c("name"="id"))
  
  d1 <- d1 %>% 
    mutate(drank=rank(-degree, ties.method="min"),
           brank=rank(-betweenness, ties.method="min"),
           crank=rank(-closeness, ties.method="min"),
           erank=rank(-eigen, ties.method="min"),
           sumrank=drank+brank+crank+erank,
           totalrank=rank(sumrank, ties.method="min"))
  
  d1$titlelong <- paste('name:', d1$label, "<br>",
                        'Total Rank:', d1$totalrank, "<br>",
                        'degree count:', d1$degree, "<br>",
                        '- degree rank:', d1$drank, "<br>",
                        '- betweenness rank:', d1$brank, "<br>",
                        '- closeness rank:', d1$crank, "<br>",
                        '- eigen rank', d1$erank)
  
  d1 <- d1 %>% select(auid, name, count, size, label, color, coauthor_count=degree, coauthor_rank=drank, betweenness=brank,
                      closeness=crank, eigen=erank, sum=sumrank, totalrank)
  return(d1)
}
  

#################################################################################
author_graph <- function(db, dbname, top_authors=NULL, plot_graph=T)
{
  q1 <- paste0("match (n:", dbname, ":doc)-[r1:has_attribution]->(m:", dbname, ":aut) ",
               "return n.docid as docid, n.title as title, m.auid as auid, m.last as last, m.first as first ",
               "order by n.title, m.last, m.first;")
  #q1 <- paste0("match (n:", dbname, ":doc)-[r1:has_attribution]->(m:arg5:aut) ",
  #             "where n.docid in [ '2887', '2880', '2879', '2909'] ",
  #             "return n.docid as docid, n.title as title, m.auid as auid, m.last as last, m.first as first ",
  #             "order by n.title, m.last, m.first;")
  d1 <- cypher(db, q1) %>% mutate(docidx=paste0("docid.", docid), full=paste0(last, ", ", first)) 
  d2 <- d1 %>% select(docidx, full)
  d3 <- d2
  #(d3 <- d2[1:10,])
  #d3$full[4] <- d3$full[2]
  m1 <- get.adjacency(graph.edgelist(as.matrix(d3), directed=FALSE), sparse=F)
  d4 <- as.data.frame(m1)
  
  # remove authors from column headers
  index <- which(grepl("^docid.", names(d4)))
  if(length(index) > 0)  d4 <- d4[,index]
  
  # remove docs from rows
  index <- which(grepl("^docid.", row.names(d4)))
  if(length(index) > 0)  d4 <- d4[-index,]
  
  if(0) {
    # here we can select which authors and documents we want. 
    dim(d4)
    # 1. give me all authors with a document count of 3 or more
    d5 <- d4[which(rowSums(d4) >= 3),]
    # 2. give me the top 5 authors based on document count
    x1 <- (rowSums(d4) %>% sort(decreasing=T))[1:10]
    d5 <- d4[which(row.names(d4) %in% names(x1)),]
  } 

  if(!is.null(top_authors)) {
    x1 <- (rowSums(d4) %>% sort(decreasing=T))[1:top_authors]
    d5 <- d4[which(row.names(d4) %in% names(x1)),]
  } else {
    d5 <- d4
  }
  
  m2 <- as.matrix(d5)
  a1 <- m2 %*% t(m2)
  
  g1 <- graph_from_adjacency_matrix(a1, mode='undirected', weighted=T, diag=F)
  g1 <- simplify(g1)
  
  if(0) {
    V(g1)$color <- 'red'
    E(g1)$color <- 'darkgray'
    plot(g1)
  }

  nodes <- as_data_frame(g1, what="vertices")
  
  edges <- as_data_frame(g1, what="edges")

  # --- decorate nodes
  rownames(nodes) <- NULL
  nodes$count <- sapply(nodes$name, function(x) rowSums(d5)[rownames(d5) %in% x])
  #nodes$value <- nodes$count * 5
  nodes$size <- nodes$count * 3
  nodes$label <- nodes$name
  nodes$degree <- sapply(nodes$name, function(x) degree(g1)[names(degree(g1)) %in% x])
  nodes$color <- 'red'
  x1 <- d1 %>% select(auid, full) %>% distinct()
  nodes <- left_join(nodes, x1, by=c("name" = "full"))
  # remove any duplicates, if any. There should never be any duplicates but because this is still
  #   in testing mode, we might find duplicates
  index <- which(duplicated(nodes$name))
  if(length(index) > 0) nodes <- nodes[-index,]
  
  nodes$id <- nodes$auid
  ### calc centrality measures
  nodes <- centrality_measures(g1, nodes)
  if(!plot_graph) { 

    # aaaaa
    
    nodes$last  <- sapply(nodes$auid, function(x) d1$last[d1$auid == x][1])
    nodes$first <- sapply(nodes$auid, function(x) substring(d1$first[d1$auid == x][1], 1, 1))
    
    #keyterms <- "(\"vascular dementia\"+OR+amyloid+OR+tau+OR+dementia+OR+alzheimers+OR+neurology+OR+lewy)"
    keyterms <- "(amyloid+OR+tau+OR+dementia+OR+alzheimers+OR+neurology+OR+lewy)"
    nodes$name <- paste0("<a href=\"https://www.ncbi.nlm.nih.gov/pubmed/?term=", keyterms, "%22",
           nodes$last, "+", nodes$first, "%22%5Bauthor%5D\", ",
           "target=\"_blank\">", nodes$name, "</a>")
    #paste0(
    #  "<a href=\"https://www.google.com/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q='", d1$Disorder, "'\"",
    #  ", target=\"_blank\">", d1$Disorder, "</a>")
    nodes <- select(nodes, auid, name, document_count=count, coauthor_count, coauthor_rank, betweenness, closeness, eigen, totalrank) 
    return(list(nodes=nodes, edges=edges)) 
  }
  nodes$title <- paste(
    'Author ID:', nodes$auid, '<br>',
    'Name:', nodes$name, '<br>',
    'Document count:', nodes$count, '<br>',
    'Collaborator count:', nodes$coauthor_count, '<br>',
    'Betweenness rank:', nodes$betweenness, '<br>',
    'Closeness rank:', nodes$closeness, '<br>',
    'Eigen Rank:', nodes$eigen, '<br>',
    'Total Rank:', nodes$totalrank
    )
    #'Aggregated rank:', nodes$total, '<br>',
  nodes$id <- nodes$auid
  nodes <- nodes %>% select(id, c(1:(ncol(nodes)-1)))
  
  # --- decorate edges
  edges$color <- 'darkgray'
  #edges$value <- (1+edges$weight*0.1)
  edges$width <- edges$weight * 0.1
  edges$title <- paste(
        edges$from, ' - ', edges$to, '<br>',
    'Collaboration count:', edges$weight)
  edges$from <- sapply(edges$from, function(x) nodes$auid[nodes$name %in% x])
  edges$to <- sapply(edges$to, function(x) nodes$auid[nodes$name %in% x])
  
  return(list(nodes=nodes, edges=edges))
}

#################################################################################
fake_main <- function()
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
  logdata("neo4j database name:", dbname)
  docid=780
  # done of initialization
  
  list1 <- claim_data(db, dbname, docid, keep_refs=F)
  top_authors=7; plot_graph=T
  list1 <- author_graph(db, dbname, top_authors=30, plot_graph=T)
  top_authors=7; plot_graph=F
  list1 <- author_graph(db, dbname, top_authors=7, plot_graph=F)
  list1 <- author_graph(db, dbname, top_authors=28, plot_graph=T)
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
