source("research\\process_mining_research.R")

# Front page module

# UI function
tab_trace_clustering_ui <- function(id) {
  
  # Main UI
  fluidPage(
    titlePanel("Medical Claim Trace Clustering"),
    fluidRow(
      column(4, selectInput("carrier_file_name", "Insurer", c(
        "Travelers" = "data_set.txt", 
        "Texas Mutual" = "data_set2.txt")
      )),
      column(4,
        selectInput("initial_diagnosis", "Initial Diagnosis", c(
          "S33.5XX", "S39.012", "M54.5", "T14.90X"
          )
        ),
      ),
      column(4,
        selectInput("collapse_traces", "Collapse Traces", c("collapsed", "not collapsed"))
      )
    ),
    dataTableOutput("display_event_log"),
    dataTableOutput("show_traces"),
    svgPanZoomOutput('process_map'),
    plotOutput('trace_explorer')
  )
  
}

# Server function
tab_trace_clustering_server <- function(input, output, session, vals) {

  refresh_event_log_object <- reactive({
    full_file_name <- paste("research\\", input$carrier_file_name, sep = "")
    event_log_df <- create_event_log_df(full_file_name)
    event_log_df <- substitute_diagnoses(event_log_df, input$initial_diagnosis)
    event_log_object <- create_event_log_object(event_log_df)
  })
  
  collapse_event_log_object <- function(filtered_event_log_object){
    
    print(filtered_event_log_object)
    print('filtered event log object')
    
    
    event_log_object_collapsed <- filtered_event_log_object %>%
      act_collapse(start = c("start"), 
                   same_chapter = c("same_chapter"),
                   different_chapter = c("different_chapter"),
                   method = "consecutive") # Figure our what act_collapse does and figure out args
    
    print(event_log_object_collapsed)
    print('event_log_object_collapsed line 54')
    
    unspecified_row_count <- event_log_object_collapsed %>%
      filter_rules(activity_id = starts("start")) %>%
      filter(activity_id == "unspecified") %>%
      as.data.frame() %>%
      nrow()
    
    print(event_log_object_collapsed)
    print('event log object collapsed line 69')
    
    print(unspecified_row_count)
    print('unspec row count line 72')
    
    if (unspecified_row_count > 0){
      event_log_object_collapsed <- event_log_object_collapsed %>%
        act_collapse(unspecified = c("unspecified"),
                     method = "consecutive")
    }
    event_log_object_collapsed
  }
  
  
  filtered_event_log_object <- reactive({
    filtered_event_log_object <- refresh_event_log_object () %>%
      filter_rules(activity_id = starts("start"))
    if (input$collapse_traces == "collapsed"){
      filtered_event_log_object <- collapse_event_log_object(filtered_event_log_object)
    }
    filtered_event_log_object
  })
  
  print(head(filtered_event_log_object))
  print('head filtered evnet log object')
  
  
  output$display_event_log <- renderDataTable({
    filtered_event_log_object()
  })
  
  output$show_traces <- renderDataTable({
    filtered_event_log_object() %>%
      traces()
  })
  
  output$process_map <- renderSvgPanZoom({
    filtered_event_log_object() %>% 
      process_map(render = FALSE) %>% generate_dot() %>% 
      grViz(width = 800, height = 1600) %>% 
      export_svg %>% 
      svgPanZoom(height=800, controlIconsEnabled = TRUE)
  })
  
  output$trace_explorer <- renderPlot({
    filtered_event_log_object() %>%
      trace_explorer(coverage = 1)
  })
  
}
