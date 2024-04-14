# Front page module

# UI function
tab_pathways_download_ui <- function(id) {
  
  # Main UI
  fluidPage(
    titlePanel("Pathways Download"),
    downloadButton("download_bupaR_traces", "Download bupaR Traces"),
    dataTableOutput("bupaR_traces"),
    downloadButton("download_nPath_traces", "Download nPath Traces"),
    dataTableOutput("nPath_traces")
    
      )
  
}

# Server function
tab_pathways_download_server <- function(input, output, session, vals) {
  
  # Empty, since there is no interactivity on front page
  output$download_bupaR_traces <- downloadHandler(
    filename = function() {
      paste("bupaR Traces.csv", sep = "")
    },
    content = function(file) {
      write.csv(get_traces_to_analyze_bupaR(), file, row.names = FALSE)
    }
  )
  
  output$bupaR_traces <- DT::renderDataTable({
    if (!is.null(vals$claims_data)){
      get_traces_to_analyze_bupaR()
    }
  }, options = list(scrollX = TRUE, lengthMenu = c(5, 10, 20)))
  
  output$download_nPath_traces <- downloadHandler(
    filename = function() {
      paste("nPath Traces.csv", sep = "")
    },
    content = function(file) {
      write.csv(get_traces_to_analyze_nPath(), file, row.names = FALSE)
    }
  )
  
  
}
