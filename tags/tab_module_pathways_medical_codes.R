# Front page module

# UI function
tab_pathways_med_codes_ui <- function(id) {
  
  # Main UI
  fluidPage(
    titlePanel("Pathways Med. Codes"),
    fluidRow(
      column(2, selectInput("icd_digit_depth_tree_map", "ICD-10 Digit Depth", c("1", "3", "all"), selected = "all")),
      column(2, selectInput("treemap_anomaly_view", "Anomaly View", c("All Pathways" = "All", "Inliers Only" = "Inlier", "Outliers Only" = "Outlier"), selected = "All"))
    ),
    column(10, d3tree3Output('treemap_visual')),
    tableOutput('treemap_data')
  )
  
}

# Server function
tab_pathways_med_codes_server <- function(input, output, session, vals) {
  
  # Empty, since there is no interactivity on front page
  treemap_data <- reactive({
    data <- vals$claims_data
    digit_depth <- local(input$icd_digit_depth_tree_map)
    if (digit_depth == "all"){
      data <- data %>% dplyr::mutate(diagnosis_prefix = First.ICD.9.CM.Diagnosis.Code)
    }
    else{
      data <- data %>%
        dplyr::mutate(diagnosis_prefix = substr(First.ICD.9.CM.Diagnosis.Code, 1, as.numeric(digit_depth)))
    }
    
    claim_numbers <- get_traces_outliers_labeled() %>%
      select(case_id, classification) 
    
    treemap_anomaly_view <- local(input$treemap_anomaly_view)
    if((treemap_anomaly_view == "Outlier")){
      claim_numbers <- claim_numbers %>% filter(classification == "Outlier")
    }
    else if((treemap_anomaly_view == "Inlier")){
      claim_numbers <- claim_numbers %>% filter(classification == "Inlier")
    }
    
    claim_numbers <- claim_numbers %>% select(case_id) %>%
      rename(Claim.Administrator.Claim.Number = case_id)
    
    
    claim_numbers$Claim.Administrator.Claim.Number
    
    data_filtered <- data %>%
      filter(Claim.Administrator.Claim.Number %in% local(claim_numbers$Claim.Administrator.Claim.Number))
    
    
    treemap_tbl <- data_filtered %>%
      group_by(diagnosis_prefix, HCPCS.Line.Procedure.Billed.Code) %>%
      summarize("total_paid" = sum(Total.Amount.Paid.Per.Line))
    treemap_df <- treemap_tbl %>% as.data.frame()
    treemap_df <- treemap_df %>%
      arrange(desc(total_paid)) %>%
      rename("Diagnosis" = diagnosis_prefix, "Procedure" = HCPCS.Line.Procedure.Billed.Code)
    
    print(head(treemap_df))
    print('printing treemap df')
    ## This all works fine
    
    copy_to(vals$db.con, treemap_df,
              in_schema("fis_med_claims_dev", "data_for_treemap_perm"), temporary = FALSE,
            overwrite = TRUE)
  })
  
  output$treemap_visual <- renderD3tree3({
    # if (!is.null(vals$claims_data)){
    
      data <- treemap_data()
      
      print(head(data))
      print('printing line 76')
      
      # dtf is not a data.frame 
      
      p <- treemap(as.data.frame(data), index=c("Diagnosis", "Procedure"), vSize = "total_paid", type = "index",
                   palette = "Set2", #bg.labels=c("white"),
                   align.labels=list( c("center", "center"), c("right", "bottom")))

      d3tree3(p, rootname = "Diagnosis / Procedure Dollars")
    # }
  })

  output$treemap_data <- renderTable({
    # if (!is.null(vals$claims_data)){
      treemap_data()
    # }
  })

  
}
