# Front page module

# UI function
tab_transition_analyzer_ui <- function(id) {
  
  # Main UI
  fluidPage(
    titlePanel("Transition Analyzer"),
    fluidRow(
      column(3, selectInput("transition_pattern", "Transition Pattern", c("All", "S33.5XX-M51.26")))
    ),
    sankeywheelOutput('transition_analyzer_pathways'),
    uiOutput('min_transition_count'),
    plotOutput('transition_matrix'),
    dataTableOutput('transition_grid')
    
  )
  
}

# Server function
tab_transition_analyzer_server <- function(input, output, session, vals) {
  
  # Empty, since there is no interactivity on front page
  output$nPath_traces <- DT::renderDataTable({
    if (input$data_source == "Vantage"){
      get_traces_to_analyze_nPath()
    }
    else{
      message <- as.data.frame("no pathways to download - paths created in memory (not pPath)")
      colnames(message) <- c("warning")
      message
    }
  }, options = list(scrollX = TRUE))
  
  get_precedence_matrix <- reactive({
    event_log_object <- bupaR_event_log_object()
    event_log_object %>% precedence_matrix(type = "relative")
  })
  
  output$min_transition_count <- renderUI({
    if (!is.null(vals$claims_data)){
      min_value <- 1
      precedence_matrix <- get_precedence_matrix()
      max_value <- as.integer(max(precedence_matrix$n) / 5)
      start_value <- as.integer(max_value / 10)
      sliderInput('min_transition_count', "Minimum Transition Count", min = min_value, max = max_value, value = start_value)
    }
  })
  
  output$transition_analyzer_pathways <- renderSankeywheel({
    if (!is.null(vals$claims_data)){
      traces_for_analysis <- get_traces_outliers_labeled()
      sankey_input_df <- get_sankey_input_df(traces_for_analysis, input$week_count)
      paid <- traces_dollars(traces_for_analysis)
      claim_count <- traces_claim_count(traces_for_analysis)
      bill_count <- traces_bill_count(traces_for_analysis)
      avg_paid <- paid / claim_count
      avg_paid_per_bill <- paid / bill_count
      sankey_title = paste("Claim Pathways (", dollar(paid), " and ", format(claim_count, big.mark = ","), " claims --> ", dollar(avg_paid), " average paid; ", dollar(avg_paid_per_bill)," average bill)", paste = "")
      sankeywheel(from = sankey_input_df$from, to = sankey_input_df$to, weight = sankey_input_df$weight,
                  type = "sankey", theme = "sandsignika", seriesName = "All Pathways", title = sankey_title)
    }
  })
  
  output$transition_matrix <- renderPlot({
    if (!is.null(vals$claims_data)){
      get_precedence_matrix() %>% dplyr::filter(n >= input$min_transition_count) %>% plot
    }
  })
  
  output$transition_grid <- DT::renderDataTable({
    if (!is.null(vals$claims_data)){
      get_precedence_matrix() %>% arrange(desc(n))
    }
  })
  
  
}
