# removed this for now
#library(dplyr)

logdata("---------- start of graph.r")
first_time <- 0
my.dbname1 <- 'arg2'
db <- NULL
dbname <- NULL


# -------------------------------------------------------------------------------
dataos <- reactive({
  logdata("1. In dataos()")
  if(1) {
    dblist <- initialize_neo4j(dbname=my.dbname1, delete_db=F)
    db <- dblist$db
    dbname <- dblist$dbname

    print("debug 1000")
    docid <- 2480
    print("debug 1100")
    list1 <- claim_data(db, dbname, docid, keep_refs=F) 
    print("debug 1200")
    nodes <- list1$nodes
    print("debug 1300")
    edges <- list1$edges
    print("debug 1400")

  } else {
    load(file="/home_ssd/dag/0-Annat/0-R/customers/mgh/code/g5.rdata")
  }

  list(nodes = nodes, edges = edges)
})

# -------------------------------------------------------------------------------
output$network_proxy_options <- renderVisNetwork({
  # Don't put anything in this section that changes, for instance input$physics, in this
  # section because it forces the graph to be redrawn from scratch every time.
  # Every redraw takes time and delays the executation, specifically when 'physics'
  # is enabled (as we need to recalculate all measurements).

  logdata("output$network_proxy_options()")
  print("debug 100")

  #visNetwork(dataos()$nodes, dataos()$edges) %>% visEdges(arrows = "to") %>% visLegend()
  list1 <- dataos()
  nodes <- list1$nodes
  edges <- list1$edges
  first_node <- min(nodes$id)
  print("debug 200")

  #print(names(nodes))
  #nodes$color <- NULL
  #print(names(nodes))

  print("debug 300")
  # sort the nodes
  nodes <- nodes %>% arrange(label)

  print("debug 400")

  #visNetwork(dataos()$nodes, dataos()$edges) %>% 
  visNetwork(nodes, edges) %>% 
    visPhysics(enabled=F) %>% 
    visInteraction(tooltipDelay=1000) %>%
    #visOptions(collapse = list(enabled = T, fit = T, resetHighlight = T)) %>%
    #visOptions(nodesIdSelection = list(enabled=T, selected=first_node)) %>%
    #visOptions(nodesIdSelection = list(enabled=T)) %>%
    #visOptions(highlightNearest=list(enabled=T, hideColor='darkgray', algorithm='hierarchical', degree=list(from=10, to=10))) %>% 
    #visOptions(nodesIdSelection=list(enabled=T, values=nodes$label, useLabels=F), highlightNearest=list(enabled=T, algorithm='hierarchical', degree=list(from=10, to=10))) %>% 
    visOptions(
      nodesIdSelection=list(enabled=T , 
        values = nodes$id,
        useLabels = T
      ), 
      highlightNearest=list(enabled=T, algorithm='hierarchical', degree=list(from=10, to=10))) %>% 

    visEdges(arrows = "to") %>%
    #visNodes(shadow = T, labelHighlightBold=T, color=list(color='blue', highlight=list(background='red', border='blue'))) %>%
    visNodes(shadow = T, borderWidth=0, borderWidthSelected=10, labelHighlightBold=T, color=list(background='blue', border='black', highlight=list(background='red', border='blue'))) %>%
    #visNodes(shadow = T) %>% 
    visExport() %>%
    visHierarchicalLayout(enabled=T, levelSeparation=1500, nodeSpacing=NULL, direction='LR') %>%
    visEvents(select = "function(nodes){Shiny.onInputChange('current_node_id', nodes.nodes);}")
    #visEvents(
    #  #hold = "function(properties)  { 
    #  doubleClick = "function(properties)  { 
    #    if(this.body.data.nodes._data[properties.nodes[0]].group == 'doc') {
    #      if(this.body.data.nodes._data[properties.nodes[0]].pmid == 'NA') { alert('Not a valid PMID');
    #      } else { window.open(this.body.data.nodes._data[properties.nodes[0]].urlmap, '_blank'); }}}"
    #)
})

# -------------------------------------------------------------------------------
if(0) {
  # just disable cluster technology. The reason is that with the highlight nearest, we have something better
  observe({
    if(input$uncollapse_all > 0){
      visNetworkProxy("network_proxy_options") %>% visUncollapse()
    }
  })
} # if(0)...

# -------------------------------------------------------------------------------
if(0) {
  # just disable cluster technology. The reason is that with the highlight nearest, we have something better
  observe({
    #visOptions(collapse = list(enabled = T, fit = T, resetHighlight = T)) %>%
    if(input$collapse) {
      # enable clustering
      visNetworkProxy("network_proxy_options") %>% 
        visOptions(collapse = list(enabled = input$collapse, fit = T, resetHighlight = T))
    } else {
      # enable highlightNearest, eg thread visualization
      visNetworkProxy("network_proxy_options") %>% 
        visOptions(highlightNearest=list(enabled=T, algorithm='hierarchical', degree=list(from=10, to=10)))
    }
  })
} # if(0)...

# ---------------------------------------------------------
observeEvent(input$current_node_id, {
  visNetworkProxy("network_proxy_options") %>% visGetNodes() 
})

# -------------------------------------------------------------------------------
output$connectref_table <- renderDataTable({
  logdata("connectref_table()")
  if (!is.null(input$current_node_id) && !is.null(input$network_proxy_options_nodes)) {
    #logdata(paste("input$current_node_id", input$current_node_id))

    # find the selected node
    info1 <- input$network_proxy_options_nodes
    found_index=F
    for(i in seq_len(length(info1))) { if(info1[[i]]$id == input$current_node_id) {found_index=T; break;} }

    if(found_index) {
      if(info1[[i]]$group == 'doc') {
        logdata(paste("id:", info1[[i]]$id))
        logdata(paste("docid:", info1[[i]]$docid))

        if(Sys.info()['nodename'] == 'hadoop') {
          load(file="/home_ssd/dag/0-Annat/0-R/customers/mgh/code/shiny/connectref3.rdata")
        } else {
          load(file="/home/ubuntu/mgh/code/shiny/connectref3.rdata")
        }

        d2 <- d1[d1$docid == info1[[i]]$docid,]
        if(nrow(d2) == 0) return() ;
        d2 <- d2[d2$score > 0.2,]   # only show scores that are more than 0.2 probability
        if(nrow(d2) == 0) return() ;
        d2 <- d2 %>% select('Probability Score'=score, 'Reference'=query, 'Text in Document'=text, 'Location in Document'=sentpercentage)
        return(d2)
      }
    }
  }
}, options=list(paging=F, searching=F), escape=F)

# -------------------------------------------------------------------------------
output$pdfviewer1 <- renderText({
  if (!is.null(input$current_node_id) && !is.null(input$network_proxy_options_nodes)) {
    logdata("output$pdfviewer1()")

    #logdata(paste("inside output$pdfviewer1 - input$current_node_id", input$current_node_id))

    # get a list of nodes
    info1 <- input$network_proxy_options_nodes
    found_index=F

    # find index (i) for the current node in the array of nodes inside 'info1'
    for(i in seq_len(length(info1))) { if(info1[[i]]$id == input$current_node_id) {found_index=T; break;} }

    if(found_index) {
      logdata("-----------------------------------")
      logdata(paste("id:", info1[[i]]$id))
      logdata(paste("group:", info1[[i]]$group))
      logdata(paste("filename:", info1[[i]]$filename))
      logdata(paste("type:", info1[[i]]$type1))
      #logdata(paste("journal:", info1[[i]]$journal))

      mytype <- ''
      if(!is.null(info1[[i]]$type1)) {
        if(info1[[i]]$type1 == 'dataset') mytype <- 'dataset'
      }

      logdata(paste("isnull - group:", is.null(info1[[i]]$group)))
      logdata(paste("isnull - filename:", is.null(info1[[i]]$filename)))
      logdata(paste("isnull - type:", is.null(info1[[i]]$type1)))
      logdata(paste("mytype:", mytype)) 

      if(info1[[i]]$group == 'doc' & !is.null(info1[[i]]$filename) & mytype != 'dataset') {
        #return(paste('<iframe style="height:800px; width:100%" src="docs/', info1[[i]]$filename, '"></iframe>', sep = ""))
        return(paste('<iframe src="docs/', info1[[i]]$filename, '"></iframe>', sep = ""))
      }
    }
  }
})


# -------------------------------------------------------------------------------
observe({
  level_sep <- ifelse(input$level_separation <= 0, 1, input$level_separation * 30)
  node_spa  <- ifelse(input$node_spacing <= 0, 1, input$node_spacing * 2.5)
  visNetworkProxy("network_proxy_options") %>% 
    visHierarchicalLayout(enabled=T, levelSeparation=level_sep, nodeSpacing=node_spa, direction='LR')
})

# -------------------------------------------------------------------------------
#observe({
#  visNetworkProxy("network_proxy_options") %>% 
#    visSetTitle(main='main header', submain = 'submain header', footer = 'footer')
#})


# -------------------------------------------------------------------------------
#observe({
#  col <- paste0('rgba(200,200,200,', input$opahigh, ')')
#  visNetworkProxy("network_proxy_options") %>%
#    visOptions(highlightNearest = list(enabled = input$highlightNearest, hover = input$hover,
#                                       algorithm = input$algorithm, degree = input$deg, hideColor = col))
#})

# -------------------------------------------------------------------------------
#observe({
#  visNetworkProxy("network_proxy_options") %>%
#    visOptions(nodesIdSelection = list(enabled = input$nodesIdSelection, selected = 5))
#})

# -------------------------------------------------------------------------------
#observe({ visNetworkProxy("network_proxy_options") %>% visOptions(highlightNearest = input$highlight_nearest) })

# -------------------------------------------------------------------------------
#observe({
#  visNetworkProxy("network_proxy_options") %>% visInteraction(tooltipDelay=input$tooltipdelay*1000)
#})

# -------------------------------------------------------------------------------
#  visHierarchicalLayout(graph, enabled = TRUE, levelSeparation = NULL,
#    nodeSpacing = NULL, treeSpacing = NULL, blockShifting = NULL,
#    edgeMinimization = NULL, parentCentralization = NULL, direction = NULL,
#    sortMethod = NULL)
#    sortMethod(): The possible options are: hubsize, directed.

observe({
  if(first_time < 2) {
    # this is a bit screwy but if we don't disable and enable physics, the graph will lock 
    #   up over time. So this is the workaround.
    first_time <<- first_time + 1 
    visNetworkProxy("network_proxy_options") %>% visPhysics(enabled=F)
    visNetworkProxy("network_proxy_options") %>% visPhysics(enabled=T)
  }
  visNetworkProxy("network_proxy_options") %>% visHierarchicalLayout(enabled=input$hierarchical, direction='LR')
})

# -------------------------------------------------------------------------------
observe({
  visNetworkProxy("network_proxy_options") %>% visPhysics(enabled=input$physics)
})




# -------------------------------------------------------------------------------
#observe({
#  #logdata(paste("selectedBy:", input$selectedBy))
#  #logdata(paste("opasel    :", input$opasel))
#  if(input$selectedby){
#    col <- paste0('rgba(200,200,200,', input$opasel, ')')
#    #logdata(paste("col:", col))
#    visNetworkProxy("network_proxy_options") %>% visOptions(selectedBy = list(variable = "group", hideColor = col))
#  }else{
#    visNetworkProxy("network_proxy_options") %>% visOptions(selectedBy = NULL)
#  }
#})

if(0) {
observe({
  # aaaa
  toggle_switch <- ifelse(input$enable_pubmed, 'on', 'off')
  #toggle_switch <- 'off' 
  logdata('toggle_switch:', toggle_switch)

  visNetworkProxy("network_proxy_options") %>% visEvents(type = toggle_switch, 
    click = "function(properties) { alert('this is a click event')}")

  if(toggle_switch == 'off') {
    logdata("switching off all listeners")
    visNetworkProxy("network_proxy_options") %>% visEvents(type = "off") 
  }

  visNetworkProxy("network_proxy_options") %>% visEvents(type = toggle_switch, 
    doubleClick = "function(properties)  { 
      if(this.body.data.nodes._data[properties.nodes[0]].group == 'doc') {
        if(this.body.data.nodes._data[properties.nodes[0]].pmid != 'NA') { 
          window.open(this.body.data.nodes._data[properties.nodes[0]].urlmap, '_blank'); 
        } else { 
          alert('Not a valid PMID');
        }
      }
   }")
})
} # if(0)...

# -------------------------------------------------------------------------------
if(0) {
  observe({
    if(input$open_collapse){
      if(0) {
        visNetworkProxy("network_proxy_options") %>% visEvents(type = "off", 
          doubleClick = "function(properties)  { 
            if(this.body.data.nodes._data[properties.nodes[0]].group == 'doc') {
              if(this.body.data.nodes._data[properties.nodes[0]].pmid == 'NA') { alert('Not a valid PMID');
              } else { window.open(this.body.data.nodes._data[properties.nodes[0]].urlmap, '_blank'); }}}")
      }
      visNetworkProxy("network_proxy_options") %>% visEvents(type = "on", doubleClick = "networkOpenCluster")
    } else {
      visNetworkProxy("network_proxy_options") %>% visEvents(type = "off", doubleClick = "networkOpenCluster")
  
      if(0) {
        visNetworkProxy("network_proxy_options") %>% visEvents(type = "on", 
          doubleClick = "function(properties)  { 
            if(this.body.data.nodes._data[properties.nodes[0]].group == 'doc') {
              if(this.body.data.nodes._data[properties.nodes[0]].pmid == 'NA') { alert('Not a valid PMID');
              } else { window.open(this.body.data.nodes._data[properties.nodes[0]].urlmap, '_blank'); }}}")
      }
    }
  })
} # if(0)...

# -------------------------------------------------------------------------------
# this section deals with the four check boxes for handling collapse. These check boxes are currently commented out
#observe({
#  visNetworkProxy("network_proxy_options") %>%
#    visOptions(collapse = list(enabled = input$collapse, fit = input$fit_collapse, resetHighlight = input$reset_collapse))
#})

############################################################################################
# -------------------------------------------------------------------------------
output$author_canvas_4 <- renderVisNetwork({
  # Don't put anything in this section that changes, for instance input$physics, in this
  # section because it forces the graph to be redrawn from scratch every time.
  # Every redraw takes time and delays the executation, specifically when 'physics'
  # is enabled (as we need to recalculate all measurements).

  logdata("output$author_canvas_4()")

  dblist <- initialize_neo4j(dbname=my.dbname1, delete_db=F)
  db <- dblist$db
  dbname <- dblist$dbname
  list1 <- author_graph(db, dbname, top_authors=input$author_graph_count, plot_graph=T)
  logdata("nodes:", nrow(list1$nodes), "  edges:", nrow(list1$edges))

  col <- paste0('rgba(200,200,200,', 0.5, ')')

  visNetwork(list1$nodes, list1$edges) %>% 
    #visEdges(smooth=F, color=list(color='darkgray', highlight='red')) %>% 
    visEdges(smooth=F) %>%
    visPhysics(enabled=T) %>% visInteraction(tooltipDelay=1000) %>%
    visOptions(highlightNearest = list(enabled = T))
})

observe({ visNetworkProxy("author_canvas_4") %>% visInteraction(tooltipDelay=input$tooltipdelay4*1000) })
observe({ visNetworkProxy("author_canvas_4") %>% visPhysics(enabled=input$physics4) })

############################################################################################
# -------------------------------------------------------------------------------
output$author_table_4 <- renderDataTable({
  logdata("output$author_table_4()")
  dblist <- initialize_neo4j(dbname=my.dbname1, delete_db=F)
  db <- dblist$db
  dbname <- dblist$dbname
  list1 <- author_graph(db, dbname, top_authors=input$author_table_count, plot=F)
  return(list1$nodes)
}, options=list(paging=F, searching=T), escape=F)

############################################################################################
output$pdfviewer <- renderText({
  logdata("output$pdfviewer()")
  return(paste('<iframe style="height:600px; width:100%" src="', input$pdfurl, '"></iframe>', sep = ""))
})
