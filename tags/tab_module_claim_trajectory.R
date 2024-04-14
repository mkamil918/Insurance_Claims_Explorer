# Front page module

# UI function
tab_claim_trajectory_ui <- function(id) {
  
  # Main UI
  fluidPage(
    titlePanel("Claim Trajectory"),
    fluidRow(
      column(4, sliderInput("label_font_size", "Font Size", min = 1, max = 10, value = 2)),
      column(4, textInput("claim_number_to_display", "Claim Number", value = "4841P7N")),
      column(4, selectInput("trajectory_label", "Trajectory Label", 
        choices = c("Diagnosis" = "First.ICD.9.CM.Diagnosis.Code", 
                    "Procedure" = "HCPCS.Line.Procedure.Billed.Code",
                    "Body Part" = "Body.Part"
                   )
        )
      )
    ),
    plotlyOutput("dollars_trajectory"),
    dataTableOutput("claim_detail")
  )
  
}

# Server function
tab_claim_trajectory_server <- function(input, output, session, vals) {
  
  get_velocity_df <- reactive({
    claim_number <- input$claim_number_to_display
    dollar_velocity_df <- vals$claims_data #%>%
    
    print(head(dollar_velocity_df))
    dollar_velocity_df <- dollar_velocity_df %>% # 2822P0N
      filter(Claim.Administrator.Claim.Number == claim_number) %>% 
      group_by(Service.Line.From.Date) %>% 
      summarize(paid_at_service_date = sum(Total.Amount.Paid.Per.Line))
    
    print(head(dollar_velocity_df))
    print('printing dollar velocity df line 37')
    
    dollar_velocity_df <- dollar_velocity_df %>% 
      arrange(Service.Line.From.Date) %>%
      as.data.frame() %>% 
      dplyr::mutate(cumulative_paid = cumsum(paid_at_service_date)) %>%
      inner_join(get_top_dollar_diagnoses(), by = "Service.Line.From.Date") %>% 
      as.data.frame()
  })
  
  get_top_dollar_diagnoses <- reactive({
    claim_number <- input$claim_number_to_display
    trajectory_label <- input$trajectory_label
    get_top_dollar_diagnoses_df <- vals$claims_data %>% 
      filter(Claim.Administrator.Claim.Number == claim_number) %>% 
      group_by(Service.Line.From.Date, !!sym(trajectory_label)) 
    
    print(get_top_dollar_diagnoses_df)
    print('till line 57')
    
    get_top_dollar_diagnoses_df <- get_top_dollar_diagnoses_df %>% 
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) #%>%
    print(get_top_dollar_diagnoses_df)
    print('line 62')
    
    get_top_dollar_diagnoses_df <- get_top_dollar_diagnoses_df %>% 
      arrange(Service.Line.From.Date, desc(total_paid))
      get_top_dollar_diagnoses_df <- get_top_dollar_diagnoses_df %>% 
            as.data.frame() 
    
    print(get_top_dollar_diagnoses_df)
    print('line 70')
    
    get_top_dollar_diagnoses_df <- get_top_dollar_diagnoses_df %>% 
      group_by(Service.Line.From.Date) %>%
      mutate(top_dollar_diagnosis = first(!!sym(trajectory_label))) %>%
      group_by(Service.Line.From.Date, top_dollar_diagnosis) %>%
      summarize()
  })
  
  output$dollars_trajectory <- renderPlotly({
    dollar_velocity_df <- get_velocity_df()
    
    print(head(dollar_velocity_df))
    print('printing velo df 83')
    
    ggplot(dollar_velocity_df, 
           aes(x = Service.Line.From.Date, y = cumulative_paid, label = top_dollar_diagnosis)) +
      geom_area(fill="#69b3a2", alpha=0.4) + geom_text(size = input$label_font_size)
  })
  
  output$claim_detail <- renderDataTable({
    #claim 03623292EC; CPT codes G0483, G0482, and 80307
    claim_number <- input$claim_number_to_display
    trajectory_label <- input$trajectory_label
    vals$claims_data %>% 
      filter(Claim.Administrator.Claim.Number == claim_number) %>% 
      group_by(Service.Line.From.Date, !!as.name(trajectory_label)) %>% 
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% 
      as.data.frame() %>%
      spread(!!as.name(trajectory_label), total_paid) %>%
      inner_join(get_top_dollar_diagnoses(), by = "Service.Line.From.Date")
  }, options = list(scrollX = TRUE))
  
}
