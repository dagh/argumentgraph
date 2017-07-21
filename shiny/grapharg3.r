# removed this for now
#library(dplyr)

logdata("---------- start of graph_arg3.r")
first_time <- 0
my.dbname2 <- 'arg3'
db <- NULL
dbname <- NULL
use_title_for_selection <- FALSE
use_double_click_node <- FALSE
use_primary_node <- FALSE
primary_node <- NULL

# -------------------------------------------------------------------------------
# get file names from neo
# -------------------------------------------------------------------------------
get_titles_from_neo <- reactive({
  if(is.null(db)) {
    dblist <- initialize_neo4j(dbname=my.dbname2, delete_db=F)
    db <<- dblist$db
    dbname <<- dblist$dbname
  }

  q1 <- paste0("match (n:", dbname, ":doc) where n.filename <> '' return n.title order by n.title;")
  titles <- cypher(db, q1)$n.title
  titles
})

# -------------------------------------------------------------------------------
output$titles <- renderUI({
  logdata("-- output$titles <- renderUI()")
  selectInput("titles", "Select Title", as.list( get_titles_from_neo() ))
})
# -------------------------------------------------------------------------------
observe({ 
  logdata("-- observe - titles")
  logdata("   input$titles:", input$titles)
  use_title_for_selection <<- TRUE
})

# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
get_data_arg3 <- reactive({
  logdata("-- get_data_arg3()")
  if(is.null(db)) {
    dblist <- initialize_neo4j(dbname=my.dbname2, delete_db=F)
    db <<- dblist$db
    dbname <<- dblist$dbname
  }

  logdata("  2. input$node_dblclicked:", input$node_dblclicked)

  upstreamhops   <- ifelse(input$hop_count[1] < 0, abs(input$hop_count[1]), 0)
  downstreamhops <- ifelse(input$hop_count[2] > 0, input$hop_count[2], 0)
    #upstreamhops=input$hops_upstream, downstreamhops=input$hops_downstream, 
    #upstreamhops=abs(input$hop_count[1]), downstreamhops=abs(input$hop_count[2]), 

  logdata("-------------------------------------------------------------------------")
  logdata("   use_title_for_selection:", use_title_for_selection)
  logdata("   node_dblclicked:", input$node_dblclicked)
  logdata("   use_primary_node:", use_primary_node)
  logdata("   primary_node:", primary_node) 
  logdata("   title:", substring(input$titles, 1, 50))
  logdata("-------------------------------------------------------------------------")

  if(use_title_for_selection) {
    title1 <- input$titles
    primary_node <- NULL
  } else if (use_primary_node) {
    title1 <- NULL
  } else if (!is.null(input$node_dblclicked)) {
    title1 <- NULL
    primary_node <- input$node_dblclicked 
  } else {
    title1 <- NULL
  }

  use_title_for_selection <<- FALSE
  use_primary_node <<- FALSE

  docid <- 2878
  #g2 <- getarg3(db, dbname, docid=4, nodeid=input$node_dblclicked, 
  g2 <- getarg3(db, dbname, docid=4, nodeid=primary_node,
    upstreamhops=upstreamhops, downstreamhops=downstreamhops,
    include_authors=input$include_authors, max_nodes=ifelse(input$max_nodes, 200, 10000), 
    title=title1, keep_refs=F) 

  #list(nodes = nodes, edges = edges)
  return(g2)
})

# -------------------------------------------------------------------------------
output$network_graph_arg3 <- renderVisNetwork({
  logdata("-- output$network_graph_arg3()")
  logdata("  1. input$node_dblclicked:", input$node_dblclicked)

  #list1 <- get_data_arg3()
  #nodes <- list1$nodes
  #edges <- list1$edges

  nodes <- edges <- data.frame(NULL)

  g2 <- get_data_arg3()

  if(is.null(g2)) return(NULL) ;

  #g2 <- delete_vertex_attr(g2, 'color')
  #g2 <- delete_vertex_attr(g2, 'font.color')

  nodes <- as_data_frame(g2, what='vertices')
  #nodes$id <- nodes$name
  #nodes <- nodes %>% select(id, c(2:ncol(nodes)))
  edges <- as_data_frame(g2, what='edges')

  logdata("  3. input$node_dblclicked:", input$node_dblclicked)

  # the 'pri'mary node is the node that has just been double clicked. It is the center node and colored differently.
  V(g2)$orig_group <- V(g2)$group
  V(g2)$group[V(g2)$primary] <- 'pri' 

  logdata(">>>>> primary node:", V(g2)$name[V(g2)$group == 'pri'])
  primary_node <<- V(g2)$name[V(g2)$group == 'pri']
  logdata(">>>>> primary node:", primary_node) 

  visIgraph(g2, idToLabel=F) %>%
    visHierarchicalLayout(enabled=T, levelSeparation=700, nodeSpacing=NULL, direction='LR') %>%
    visPhysics(enabled=F) %>% 
    visInteraction(tooltipDelay=1000) %>%
    visEdges(smooth=F, arrows = "to") %>%
    visOptions(highlightNearest=list(enabled=T, algorithm='hierarchical', degree=list(from=8, to=8))) %>%
    visEvents(doubleClick = "function(nodes){Shiny.onInputChange('node_dblclicked', nodes.nodes);}") %>%
    visEvents(selectNode  = "function(nodes){Shiny.onInputChange('node_selected', nodes.nodes);}") %>%
    visGroups(groupname='pri' , color = list(background = "yellow",     border = "darkblue", highlight = "white"), shadow = list(enabled = T)) %>%
    visGroups(groupname='doc' , color = list(background = "lightgreen", border = "darkblue", highlight = "white"), shadow = list(enabled = T)) %>%
    visGroups(groupname='rtxt', color = list(background = "salmon",     border = "darkblue", highlight = "white"), shadow = list(enabled = T)) %>%
    visGroups(groupname='ref' , color = list(background = "grey",       border = "darkblue", highlight = "white"), shadow = list(enabled = T)) %>%
    visGroups(groupname='aut' , color = list(background = "red",        border = "darkblue", highlight = "white"), shadow = list(enabled = T)) %>%
    visGroups(groupname='itxt', color = list(background = "lightblue",  border = "darkblue", highlight = "white"), shadow = list(enabled = T)) %>%
    visGroups(groupname='cap' , color = list(background = "lightgrey",  border = "darkblue", highlight = "white"), shadow = list(enabled = T)) %>%
    visGroups(groupname='img' , color = list(background = "orange",     border = "darkblue", highlight = "white"), shadow = list(enabled = T))

    #visExport() %>%
})

# -------------------------------------------------------------------------------
#observe({ 
#  logdata("-- observe - visPhysics")
#  visNetworkProxy("network_graph_arg3") %>% visPhysics(enabled=input$physics3) 
#})

# -------------------------------------------------------------------------------
observeEvent(input$node_dblclicked, { 
  logdata("-- observeEvent - double click")
  logdata(paste("   input$node_dblclicked", input$node_dblclicked))
  primary_node <<- input$node_dblclicked
  use_primary_node <- T
  visNetworkProxy("network_graph_arg3") %>% visGetNodes() %>% visGetEdges() %>%
    visSelectNodes(id = input$node_dblclicked)
})

# -------------------------------------------------------------------------------
observeEvent(input$node_selected, { 
  logdata("-- observeEvent - select node")
  logdata(paste("   input$node_selected", input$node_selected))
  visNetworkProxy("network_graph_arg3") %>% visGetNodes() %>% visGetEdges() %>%
    visSelectNodes(id = input$node_selected)
})

# -------------------------------------------------------------------------------
output$view_id <- renderText({
  logdata("-- output - view_id - start")

  info1 <- input$network_graph_arg3_nodes
  found_index=F

  if(length(input$node_dblclicked) > 0) {
    for(i in seq_len(length(info1))) { 
      if(info1[[i]]$id == input$node_dblclicked) { found_index=T; break; } 
  }}

  # get current node and edge count
  g2 <- get_data_arg3()

  if(!is.null(g2)) {
    node_count <- ifelse(is.null(g2), 0, vcount(g2))
    edge_count <- ifelse(is.null(g2), 0, ecount(g2))
    logdata("   node_count:", node_count)
    logdata("   edge_count:", edge_count)
    node_index <- which(V(g2)$name == input$node_dblclicked)
    logdata("   node_index:", node_index)
  } else {
    node_index <- NULL
  }
  
  if(found_index & !is.null(node_index)) {
    paste0(
      "database: ", my.dbname2, 
      "\ndblclick node id: ", input$node_dblclicked, 
      "\nname:   ", V(g2)$name[node_index], 
      "\ngroup:  ", V(g2)$group[node_index], 
      "\nlevel:  ", V(g2)$level[node_index], 
      #"\nlabel: ", V(g2)$label[node_index], 
      #"\ntitle: ", V(g2)$title[node_index], 
      "\nshape:  ", V(g2)$shape[node_index], 
      "\n",
      "\nselected node id: ", input$node_selected,
      "\nnode count: ", node_count, "\nedge count: ", edge_count, 
      "\nprimary node: ", primary_node)
  } else {
    paste0("dblclick node id: ", input$node_dblclicked, 
      "\nselected node id: ", input$node_selected,
      "\nprimary node: ", primary_node)
  }
})

# -------------------------------------------------------------------------------
observe({ 
  logdata("-- observe - visOptions - highlight nearest etc")
  logdata("   Just changed the hops - ensure we use primary node")
  use_primary_node <<- TRUE

  upstreamhops   <- ifelse(input$hop_count[1] < 0, abs(input$hop_count[1]), 0)
  downstreamhops <- ifelse(input$hop_count[2] > 0, input$hop_count[2], 0)

  visNetworkProxy("network_graph_arg3") %>%
    visOptions(highlightNearest=list(enabled=T, algorithm='hierarchical', 
      degree=list(from=upstreamhops, to=downstreamhops)))
})

# -------------------------------------------------------------------------------
observe({
  logdata("-- observe - visSelectNodes")
  nodes_selection <- input$node_selected
  logdata("   nodes_selection:", nodes_selection)
  visNetworkProxy("network_graph_arg3") %>% visSelectNodes(id = nodes_selection)
})

# -------------------------------------------------------------------------------
output$fileviewer1 <- renderText({
  if (!is.null(input$node_selected)) {
    logdata("-- output$fileviewer1")

    # get a list of nodes
    info1 <- input$network_graph_arg3_nodes
    found_index=F

    # find index (i) for the current node in the array of nodes inside 'info1'
    for(i in seq_len(length(info1))) { if(info1[[i]]$id == input$node_selected) {found_index=T; break;} }

    if(found_index) {

      logdata(paste("   id:", info1[[i]]$id))
      logdata(paste("   group:", info1[[i]]$group))
      logdata(paste("   filename:", info1[[i]]$filename))

      #if(info1[[i]]$group %in% c('doc', 'img') & info1[[i]]$filename != '') { 
      if(length(info1[[i]]$filename) > 0) { 
        file_string <- paste('<iframe style="height:800px; width:100%" src="', info1[[i]]$filename, '"></iframe>', sep = "")
        logdata("   file_string:", file_string)
        return(file_string)
      }
    }
  }
})
# -------------------------------------------------------------------------------
output$imgviewer1 <- renderText({
  if (!is.null(input$node_selected) & input$viewfilesimages != 'none') {
    logdata("-- output$imgviewer1")

    # get a list of nodes
    info1 <- input$network_graph_arg3_nodes
    found_index=F

    # find index (i) for the current node in the array of nodes inside 'info1'
    for(i in seq_len(length(info1))) { if(info1[[i]]$id == input$node_selected) {found_index=T; break;} }

    if(found_index) {

      logdata(paste("   id:", info1[[i]]$id))
      logdata(paste("   group:", info1[[i]]$group))
      logdata(paste("   orig_group:", info1[[i]]$orig_group))
      logdata(paste("   viewoption:", input$viewfilesimages))
      logdata(paste("   filename:", info1[[i]]$filename))

      if(input$viewfilesimages == 'both' |
        (input$viewfilesimages == 'img' & info1[[i]]$orig_group == 'img') |
        (input$viewfilesimages == 'doc' & info1[[i]]$orig_group == 'doc'))
      {
        if(length(info1[[i]]$filename) > 0) { 
          file_string <- paste('<iframe style="height:200px; width:100%" src="', info1[[i]]$filename, '"></iframe>', sep = "")
          logdata("   file_string:", file_string)
          return(file_string)
        }
      }
    }
  }
})

# -------------------------------------------------------------------------------
observe({
  level_sep <- ifelse(input$level_separation3 <= 0, 1, input$level_separation3 * 30)
  node_spa  <- ifelse(input$node_spacing3 <= 0, 1, input$node_spacing3 * 2.5)
  visNetworkProxy("network_proxy_options") %>% 
    visHierarchicalLayout(enabled=T, levelSeparation=level_sep, nodeSpacing=node_spa, direction='LR')
})


# -------------------------------------------------------------------------------
observeEvent(input$getNodes,{
  logdata("-- input$getNodes")
  visNetworkProxy("network_graph_arg3") %>% visGetNodes() 
})

observeEvent(input$getEdges, {
  logdata("-- input$getEdges")
  visNetworkProxy("network_graph_arg3") %>% visGetEdges()
})

output$nodes_data_from_shiny <- renderDataTable({
  logdata("-- getNodes renderDataTable")
  if(!is.null(input$network_graph_arg3_nodes)){
    nodes1 <- input$network_graph_arg3_nodes
    info <- data.frame(id=sapply(nodes1, '[[', 'id'),  group=sapply(nodes1, '[[', 'group'), 
      level=sapply(nodes1, '[[', 'level'),  title=sapply(nodes1, '[[', 'title'), stringsAsFactors = F)
    info
  }
}, options=list(paging=F, searching=F), escape=F)

output$edges_data_from_shiny <- renderDataTable({
  logdata("-- getEdges renderDataTable")
  if(!is.null(input$network_graph_arg3_edges)){
    edges1 <- input$network_graph_arg3_edges
    info <- data.frame(from=sapply(edges1, '[[', 'from'),  to=sapply(edges1, '[[', 'to'), stringsAsFactors = F)
    info
  }
}, options=list(paging=F, searching=F), escape=F)
