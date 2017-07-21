library(shiny)
library(visNetwork)

cat(file=stderr(), "inside ui debug 100", "\n")

shiny::shinyUI(shiny::navbarPage(

  theme = "bootstrap.css",

  ## Application title
  #headerPanel( list( "Gnosis Test Prototype"), windowTitle="Gnosis Interactive Prototype"),
  #headerPanel("Gnosis Test Prototype"),


  #title = paste("MGH", basename(getwd()), sep=" - "),

  titlePanel(title=div(img(src="mgh-logo-neurology-2x.png", height=40, width=140, align='right', style='margin:-30px 1px'), '')),

  shiny::tabPanel(  # tab 1
    title = "Argument Graph",
    fluidRow(
      column(
        width = 2,
        #sliderInput("tooltipdelay", "Tool Tip Delay (sec)", min = 0, max = 10, value = 5),
        sliderInput("node_spacing", "Vertical Node Separation", min = 0, max = 100, value = 50, step=5),
        sliderInput("level_separation", "Horizontal Node Separation", min = 0, max = 100, value = 50, step=5),
        #hr(),
        checkboxInput("physics", "Automatic Resizing", T),
        checkboxInput("hierarchical", "Hierarchical Layout", T)
        #checkboxInput("collapse", "Enable Cluster", F),
        #checkboxInput("enable_reference_nodes", "Enable Reference Nodes", F),
        #checkboxInput("highlight_nearest", "Highlight Nodes", F),
        #hr(),
        #checkboxInput("fit_collapse", "Fit after collapse", FALSE),
        #checkboxInput("reset_collapse", "Reset highlight after collapse", TRUE),
        #checkboxInput("open_collapse", "Enable open cluster", TRUE),
        #hr(),
        #actionButton("uncollapse_all", "Uncollapse All")
      ),
      column(
        width = 10,
        visNetworkOutput("network_proxy_options", height = "500px")
      )
    ), #fluidRow # aaa
    fluidRow(
      column(width = 2, 
        #HTML("<b>Find References in Source Document</b><br>"),
        HTML("<font size=2>"),
        #HTML("<li><b>Probability Score:</b> probability that the reference is found in the text (high is better)"),
        #HTML("<li><b>Reference:</b> reference in source document"),
        #HTML("<li><b>Text in Document:</b> a sentence in the document that has a partial match with the reference"),
        #HTML("<li><b>Location in Document:</b> approximate location of the sentence in the document"),
        HTML("</font>")
      ),
      column(
        width=10,
        div(dataTableOutput("connectref_table"), style="font-size:80%")
      )
    ), #fluidRow # aaa
    fluidRow(
      column(width = 2, 
        HTML("<b>Source and Selected Documents</b><br>"),
        HTML("<font size=2>"),
        HTML("Only showing probability scores > 0.2"),
        HTML("</font>")
        ),
      column( 
        width = 4, 
        #tags$iframe(style="height:800px; width:100%", src="docs/Haggarty.Tau.EvidenceModel_rev1_dh_v2.pdf")
        tags$iframe(src="docs/Haggarty.Tau.EvidenceModel_rev1_dh_v2.pdf")
      ),
      column( 
        width = 6, 
        #tags$iframe(style="height:800px; width:100%", src="docs/Chapuis_2013_PMID_23399914.pdf")
        htmlOutput('pdfviewer1')
      )
    )
  ), #tabPanel
  shiny::tabPanel(  # tab 2
    title = "Author Graph",
    fluidRow(
      column(
        width = 2,
        sliderInput("tooltipdelay4", "Tool Tip Delay (sec)", min = 0, max = 10, value = 2),
        sliderInput("author_graph_count", "Top Author Count (graph)", min = 10, max = 100, value = 20),
        hr(),
        checkboxInput("physics4", "Enable Physicis", T)
      ),
      column(
        width = 10,
        visNetworkOutput("author_canvas_4", height = "400px")
      )
    ), #fluidRow
    hr(),
    fluidRow(
      column(
        width=2,
        sliderInput("author_table_count", "Top Author Count (table)", min = 10, max = 1000, value = 30)
      ),
      column(
        width=10,
        div(h3("Author Data Table"), align = "center"),
        dataTableOutput("author_table_4")
      )
    ) #fluidRow
  ), #tabPanel
  shiny::tabPanel(  # tab 3
    title = "Big Graph",
    fluidRow(
      column(
        width = 2,
        #uiOutput("titles"),
        div(uiOutput("titles"), style="font-size:80%"),

        #sliderInput("hops_upstream",   "Upstream hops", min = 0, max = 10, value = 2, step=1),
        #sliderInput("hops_downstream", "Downstream hops", min = 0, max = 10, value = 2, step=1),
        sliderInput("hop_count", "Up and downstream hops", min=-5, max=5, value=c(-2,2)),
        #sliderInput("node_spacing3", "Vertical Node Separation", min = 0, max = 100, value = 50, step=5),
        #sliderInput("level_separation3", "Horizontal Node Separation", min = 0, max = 100, value = 50, step=5),

        checkboxInput("include_authors", "Include Authors", F),
        #checkboxInput("physics3", "Automatic Resizing", F),
        checkboxInput("max_nodes", "Maximum node count at 200", T),
        radioButtons("viewfilesimages", "Show",
          c('Document' = 'doc', 'Image' = 'img', 'Both' = 'both', 'None' = 'none'),
          selected = 'img'),
        #tags$style(type='text/css', '#view_id {background-color: rgba(0,0,255,0.10); color: blue;}'), 
        tags$style(type='text/css', '#view_id {font-size:80%;}'), 
        verbatimTextOutput('view_id')
        #div(verbatimTextOutput("view_id"), style="font-size:80%")
        #actionButton("getEdges", "Edges"),
        #actionButton("getNodes", "Nodes")
      ),
      #column( width = 2, verbatimTextOutput('view_id')),
      column(width=6, htmlOutput('imgviewer1')),
      column(
        width = 10,
        visNetworkOutput("network_graph_arg3", height = "900px")
      )
    )#,
    #fluidRow(
    #  column(width = 2, 
    #    HTML("Document Viewer")
    #    ),
    #  column( 
    #    width = 8, 
    #    htmlOutput('fileviewer1')
    #  )
    #)
  )
)) #shinyUI

