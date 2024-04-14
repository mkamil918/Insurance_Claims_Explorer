# Front page module

# UI function
tab_rep_pathways_ui <- function(id) {
  
  # Main UI
  fluidPage(
    titlePanel("Representative Pathways"),
    textInput('rep_trace_count', "Representative Trace Count", value = 5),
    tableOutput('representative_traces'),
    plotOutput('representative_traces_visual'),
    plotOutput('representative_traces_visual_legend')
    
  )
  
}

# Server function
tab_rep_pathways_server <- function(input, output, session, vals) {
  
  # Empty, since there is no interactivity on front page
  output$representative_traces <- renderTable({
    representative_sequences <- get_representative_sequences()
  })
  
  output$representative_traces_visual <- renderPlot({
    representative_sequences <- get_representative_sequences()
    plot(representative_sequences)
    #https://www.r-bloggers.com/sequence-of-shopping-carts-in-depth-analysis-with-r/
  })
  
  output$representative_traces_visual_legend <- renderPlot({
    representative_sequences <- get_representative_sequences()
    seqlegend(seqdef(representative_sequences), cex = 1)
  })
  
  get_representative_sequences <- reactive({
    traces_to_analyze <- get_traces_to_analyze()
    
    traces_to_analyze.seq <- seqdef(traces_to_analyze)
    traces_to_analyze.om <- seqdist(traces_to_analyze.seq, method="OM", sm = "CONSTANT")
    rep_count <- input$rep_trace_count
    if (rep_count == ""){
      rep_count <- NULL
    }
    representative_sequences <- seqrep(traces_to_analyze.seq, diss = traces_to_analyze.om, pradius = .025, nrep = rep_count)
  })
  
  
}
