# Front page module

# UI function
tab_cost_outliers_ui <- function(id) {
  # Main UI
  fluidPage(
    titlePanel("Cost Outliers"),
    fluidRow(
      column(4, valueBoxOutput('severity_status',width = "100%")),
      column(4, valueBoxOutput('frequency_status',width = "100%")),
      column(4, valueBoxOutput('combination_status',width = "100%")),
    ),
    fluidRow(
      column(6,div(dataTableOutput("diagnoses_1"), style = "font-size: 85%;")),
      column(6,div(dataTableOutput("top_procedures_1"), style = "font-size: 85%;")),
      style='padding-bottom:20px;'
    ),
    wellPanel(
      fluidRow(
        column(6,sliderTextInput(inputId = "count_range",
                                 label = "Minimum Claim Count for Provider", 
                                 choices = c(0, 5, 10, 15, 20, 25),
                                 grid = TRUE)),
        column(6,sliderTextInput(inputId = "paid_range",
                                 label = "Minimum Paid by Provider",
                                 choices = c(0, 10000, 25000, 50000, 75000, 100000),
                                 grid = TRUE)),
      )
    ),
    fluidRow(
      div(plotOutput("outlier_plot"), style = "font-size: 85%;"),
      style='padding-bottom:20px;'
    ),
    fluidRow(
      div(dataTableOutput("cost_outliers"), style = "font-size: 70%;")
    )
    
  )
}

# Server function
tab_cost_outliers_server <- function(input, output, session, vals) {
  
  cost_vals <- reactiveValues(top_procedures_1_df = NULL)
  
  load_cost_summaries <<- reactive({
    if (!is.null(vals$claims_data)){
      base_data <- vals$claims_data %>%
        select(Billing.Provider.FEIN, First.ICD.9.CM.Diagnosis.Code, Claim.Administrator.Claim.Number, Line.Number,
               HCPCS.Line.Procedure.Billed.Code, Bill.ID, Total.Amount.Paid.Per.Line) %>%
        group_by(Billing.Provider.FEIN, First.ICD.9.CM.Diagnosis.Code, Claim.Administrator.Claim.Number,
                 HCPCS.Line.Procedure.Billed.Code, Bill.ID) %>%
        summarize(tot_paid = sum(Total.Amount.Paid.Per.Line)) %>%
        group_by(Billing.Provider.FEIN, First.ICD.9.CM.Diagnosis.Code, Claim.Administrator.Claim.Number,
                 HCPCS.Line.Procedure.Billed.Code) %>%
        summarize(Frequency = n(), Severity = sum(tot_paid)) %>%
        group_by(Billing.Provider.FEIN, First.ICD.9.CM.Diagnosis.Code,HCPCS.Line.Procedure.Billed.Code) %>%
        summarize(Frequency = mean(Frequency), Severity = sum(Severity)) %>%
        ungroup() %>%
        select(Billing.Provider.FEIN, First.ICD.9.CM.Diagnosis.Code, HCPCS.Line.Procedure.Billed.Code, Frequency,
               Severity)
      
      
      base_data2 <- vals$claims_data %>%
        select(Billing.Provider.FEIN, First.ICD.9.CM.Diagnosis.Code, Line.Number,HCPCS.Line.Procedure.Billed.Code,
               Bill.ID,Total.Amount.Paid.Per.Line) %>%
        group_by(Billing.Provider.FEIN, First.ICD.9.CM.Diagnosis.Code, HCPCS.Line.Procedure.Billed.Code, Bill.ID) %>%
        summarize(tot_paid = sum(Total.Amount.Paid.Per.Line)) %>%
        group_by(Billing.Provider.FEIN, First.ICD.9.CM.Diagnosis.Code, HCPCS.Line.Procedure.Billed.Code) %>%
        summarize(Avg_Severity = mean(tot_paid), Severity = sum(tot_paid)) %>%
        ungroup() %>%
        select(Billing.Provider.FEIN, First.ICD.9.CM.Diagnosis.Code, HCPCS.Line.Procedure.Billed.Code, Avg_Severity, Severity)
      
      sev_thresholds <- tukey_thresholds_1D_multiple(base_data2,
                                        c("First.ICD.9.CM.Diagnosis.Code", "HCPCS.Line.Procedure.Billed.Code"),
                                        Avg_Severity)
      
      severity_outliers <- inner_join(base_data2,sev_thresholds,
                                      by = c("HCPCS.Line.Procedure.Billed.Code" = "HCPCS.Line.Procedure.Billed.Code",
                                             "First.ICD.9.CM.Diagnosis.Code" = "First.ICD.9.CM.Diagnosis.Code")) %>%
        mutate(Status = ifelse(Avg_Severity <= UT,"Inlier", "Outlier")) %>%
        filter(Status == 'Outlier')
      
      copy_to(vals$db.con, severity_outliers, "severity_outliers",
              temporary = TRUE, overwrite = TRUE,
              primary.index = c('Billing.Provider.FEIN','First.ICD.9.CM.Diagnosis.Code',
                                'HCPCS.Line.Procedure.Billed.Code'))
      
      severity_outliers <- tbl(vals$db.con,"severity_outliers")
      
      freq_thresholds <- tukey_thresholds_1D_multiple(base_data,
                                        c("First.ICD.9.CM.Diagnosis.Code", "HCPCS.Line.Procedure.Billed.Code"),
                                        Frequency)
      
      frequency_outliers <- inner_join(base_data,freq_thresholds,
                                       by = c("HCPCS.Line.Procedure.Billed.Code" = "HCPCS.Line.Procedure.Billed.Code",
                                              "First.ICD.9.CM.Diagnosis.Code" = "First.ICD.9.CM.Diagnosis.Code")) %>%
        mutate(Status = ifelse(Frequency <= UT,"Inlier", "Outlier")) %>%
        filter(Status == 'Outlier')
      
      copy_to(vals$db.con, frequency_outliers, "frequency_outliers",
              temporary = TRUE, overwrite = TRUE,
              primary.index = c('Billing.Provider.FEIN','First.ICD.9.CM.Diagnosis.Code',
                                'HCPCS.Line.Procedure.Billed.Code'))
      
      frequency_outliers <- tbl(vals$db.con,"frequency_outliers")
      
      freq_sev_outliers <- inner_join(severity_outliers, frequency_outliers,
                                      by = c("Billing.Provider.FEIN",
                                             "First.ICD.9.CM.Diagnosis.Code",
                                             "HCPCS.Line.Procedure.Billed.Code"))
      
      freq_sev_total_sum <- freq_sev_outliers %>%
        filter(Status.x == 'Outlier' & Status.y == 'Outlier') %>%
        summarise(both = sum(Severity.y)) %>%
        as_tibble()
      
      vals$freq_sev_total_sum <- freq_sev_total_sum$both
      
      sev_total <- severity_outliers %>% 
        summarise(sev_sum = sum(Severity)) %>%
        as_tibble()
      
      vals$sev_total <- sev_total$sev_sum
      
      freq_total <- frequency_outliers %>% 
        summarise(freq_sum = sum(Severity)) %>%
        as_tibble()
      
      vals$freq_total <- freq_total$freq_sum
      
      freq_sev_outliers_FEINs <- freq_sev_outliers %>%
        select(Billing.Provider.FEIN,First.ICD.9.CM.Diagnosis.Code,HCPCS.Line.Procedure.Billed.Code)
      
      severity_outliers_FEINs <- severity_outliers %>%
        select(Billing.Provider.FEIN,First.ICD.9.CM.Diagnosis.Code,HCPCS.Line.Procedure.Billed.Code) %>%
        setdiff(freq_sev_outliers_FEINs) %>%
        mutate(anomaly_type = "severity")
      
      frequency_outliers_FEINs <- frequency_outliers %>%
        select(Billing.Provider.FEIN,First.ICD.9.CM.Diagnosis.Code,HCPCS.Line.Procedure.Billed.Code) %>%
        setdiff(freq_sev_outliers_FEINs) %>%
        mutate(anomaly_type = "frequency")
      
      freq_sev_outliers_FEINs <- freq_sev_outliers_FEINs %>%
        mutate(anomaly_type = "frequency\nand\nseverity")
      
      all_cost_outlier_FEINs <- union_all(freq_sev_outliers_FEINs,frequency_outliers_FEINs) %>%
        union_all(severity_outliers_FEINs)
      
      vals$cost_outlier_data_cube <- inner_join(vals$claims_data,all_cost_outlier_FEINs,
                                                by = c("Billing.Provider.FEIN",
                                                       "First.ICD.9.CM.Diagnosis.Code",
                                                       "HCPCS.Line.Procedure.Billed.Code"))
    }
  })
  
  output$severity_status <- renderValueBox({
    if (!is.null(vals$sev_total)){
      shinydashboard::valueBox(paste('$',format(local(vals$sev_total), big.mark = ","),sep = ""),
                               subtitle = tags$p("Suspicious Payments in Severity",style = "font-size: 150%;"),
                               color = "red",
                               width = NULL)
    }
    else{
      load_cost_summaries()
    }
  })
  
  output$frequency_status <- renderValueBox({
    if (!is.null(vals$freq_total)){
      shinydashboard::valueBox(paste('$',format(local(vals$freq_total), big.mark = ","),sep = ""),
                               subtitle = tags$p("Suspicious Payments in Frequency",style = "font-size: 150%;"),
                               color = "orange",
                               width = NULL)
    }
  })

  output$combination_status <- renderValueBox({
    if (!is.null(vals$freq_sev_total_sum)){
      shinydashboard::valueBox(paste('$',format(local(vals$freq_sev_total_sum), big.mark = ","),sep = ""),
                               subtitle = tags$p("Suspicious Payments in Both Frequency & Severity",style = "font-size: 150%;"),
                               color = "green",
                               width = NULL)
    }
  })

  output$diagnoses_1 <- renderDataTable({
    vals$diagnoses_1_df <- diagnoses_1_df()
    datatable(vals$diagnoses_1_df, options = list(scrollX = TRUE, lengthMenu = c(5, 10, 20), dom = 'ftp'),
              selection = list(target = "row", mode = "single", selected = c("1"))) %>%
      formatCurrency("Total.Paid", "$")
  })
  
  
  top_procedures_1_df <- reactive({
    diagnosis_index <- input$diagnoses_1_rows_selected[1]
    diagnosis <- vals$diagnoses_1_df[diagnosis_index, 1]
    vals$claims_data %>% 
      select(First.ICD.9.CM.Diagnosis.Code, HCPCS.Line.Procedure.Billed.Code,
             Total.Amount.Paid.Per.Line, Claim.Administrator.Claim.Number) %>%
      filter(First.ICD.9.CM.Diagnosis.Code == diagnosis) %>%
      group_by(HCPCS.Line.Procedure.Billed.Code) %>% 
      rename(Procedure.Code = HCPCS.Line.Procedure.Billed.Code) %>%
      summarize(Total.Paid = sum(Total.Amount.Paid.Per.Line),
                Claim.Count = n_distinct(Claim.Administrator.Claim.Number)) %>%
      arrange(desc(Total.Paid)) %>%
      as.data.frame()
  })
  
  output$top_procedures_1 <- renderDataTable({
    cost_vals$top_procedures_1_df <- top_procedures_1_df()
    datatable(cost_vals$top_procedures_1_df, options = list(scrollX = TRUE, lengthMenu = c(5, 10, 20), dom = 'ftp'),
              selection = list(target = "row", mode = "single", selected = c("1"))) %>%
      formatCurrency("Total.Paid", "$")
  })
  
  output$treatment_cell_1 <- renderText({
    diagnosis <- input$diag_select
    procedure_index <- input$top_procedures_1_rows_selected[1]
    top_procedures_1_df <- top_procedures_1_df()
    procedure <- top_procedures_1_df[procedure_index, 1]
    paste(diagnosis, " - ", procedure, sep = "")
  })
  
  cost_outliers_grid <- reactive({
    diagnosis_index <- input$diagnoses_1_rows_selected[1]
    diagnosis <- vals$diagnoses_1_df[diagnosis_index, 1]
    procedure_index <- input$top_procedures_1_rows_selected[1]
    procedure <- cost_vals$top_procedures_1_df[procedure_index, 1]
    
    avg_paid_per_bill <- vals$claims_data %>%
      select(First.ICD.9.CM.Diagnosis.Code, HCPCS.Line.Procedure.Billed.Code,
             Total.Amount.Paid.Per.Line, Billing.Provider.FEIN, Bill.ID) %>%
      filter(First.ICD.9.CM.Diagnosis.Code == diagnosis) %>%
      filter(HCPCS.Line.Procedure.Billed.Code == procedure) %>%
      group_by(Bill.ID, Billing.Provider.FEIN) %>%
      summarize(total_paid_per_bill = sum(Total.Amount.Paid.Per.Line)) %>%
      group_by(Billing.Provider.FEIN) %>%
      summarize(severity = mean(total_paid_per_bill))
    
    avg_pd_per_claim_by_provider <- vals$claims_data %>%
      select(First.ICD.9.CM.Diagnosis.Code, HCPCS.Line.Procedure.Billed.Code,
             Claim.Administrator.Claim.Number, Billing.Provider.FEIN,
             Total.Amount.Paid.Per.Line) %>%
      filter(First.ICD.9.CM.Diagnosis.Code == diagnosis) %>%
      filter(HCPCS.Line.Procedure.Billed.Code == procedure) %>%
      group_by(Claim.Administrator.Claim.Number, Billing.Provider.FEIN) %>%
      summarize(total_paid_by_claim = sum(Total.Amount.Paid.Per.Line)) %>%
      group_by(Billing.Provider.FEIN) %>%
      summarize(avg_paid_per_claim = mean(total_paid_by_claim))
    
    freq <- vals$claims_data %>%
      select(Billing.Provider.FEIN, First.ICD.9.CM.Diagnosis.Code, Claim.Administrator.Claim.Number, Line.Number,
             HCPCS.Line.Procedure.Billed.Code, Bill.ID, Total.Amount.Paid.Per.Line) %>%
      filter(First.ICD.9.CM.Diagnosis.Code == diagnosis) %>%
      filter(HCPCS.Line.Procedure.Billed.Code == procedure) %>%
      group_by(Billing.Provider.FEIN, Claim.Administrator.Claim.Number, Bill.ID) %>%
      summarize(tot_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      group_by(Billing.Provider.FEIN, Claim.Administrator.Claim.Number) %>%
      summarize(frequency = n()) %>%
      group_by(Billing.Provider.FEIN) %>%
      summarize(frequency = mean(frequency)) %>%
      ungroup() %>%
      select(Billing.Provider.FEIN, frequency)
    
    outlier_stats <- inner_join(avg_paid_per_bill, avg_pd_per_claim_by_provider, by = "Billing.Provider.FEIN")
    outlier_stats <- inner_join(outlier_stats, freq, by = "Billing.Provider.FEIN")
    
    provider_activity_overview <- vals$claims_data %>%
      select(First.ICD.9.CM.Diagnosis.Code, HCPCS.Line.Procedure.Billed.Code,
             Billing.Provider.FEIN, Total.Amount.Paid.Per.Line,
             Claim.Administrator.Claim.Number) %>%
      filter(First.ICD.9.CM.Diagnosis.Code == diagnosis) %>%
      filter(HCPCS.Line.Procedure.Billed.Code == procedure) %>%
      group_by(Billing.Provider.FEIN) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line),
                claim_count = n_distinct(Claim.Administrator.Claim.Number)) %>%
      filter(total_paid >= local(input$paid_range) & claim_count >= local(input$count_range))
    
    inner_join(outlier_stats, provider_activity_overview, by = "Billing.Provider.FEIN")
  })
  
  output$outlier_plot <- renderPlot({
    df <- cost_outliers_grid() %>%
      rename(ax_1 = frequency, ax_2 = severity) %>%
      tukey_filter_2D() %>%
      as.data.frame()

    data <- cost_outliers_grid() %>% as.data.frame()
    data$claim_count <- as.numeric(data$claim_count)
    
    p <- ggplot(data, aes(x = frequency, y = severity, size = total_paid, color = claim_count)) +
      geom_point(alpha=0.5) +
      geom_point(mapping = aes(provider = Billing.Provider.FEIN)) +
      scale_size(range = c(.1, 24), name="Total Claims Paid") +
      labs(x='Frequency', y = 'Severity', color='Claim Count') +
      geom_vline(xintercept = local(df$UT_1),linetype="dashed",color="blue") +
      geom_hline(yintercept = local(df$UT_2),linetype="dashed",color="blue") +
      # xlim(min(data$frequency),max(data$frequency)) +
      # ylim(min(data$severity)-200,max(data$severity)+200) +
      coord_cartesian(clip = "off")
    p
    
    # ggplotly(p,tooltip = c("provider","x","y")) %>%
    #   layout(paper_bgcolor='#F5F5F5')
    
  })
  
  output$cost_outliers <- renderDataTable({
    diagnosis_index <- input$diagnoses_1_rows_selected[1]
    diagnosis <- vals$diagnoses_1_df[diagnosis_index, 1]
    procedure_index <- input$top_procedures_1_rows_selected[1]
    procedure <- cost_vals$top_procedures_1_df[procedure_index, 1]
    
    print("printing cost outliers grid below")
    print(head(cost_outliers_grid))
    
    data <- cost_outliers_grid() %>%
      tukey_filter_2D(severity, frequency, operation = 'BOTH') %>%
      mutate(Status = ifelse(STATUS_1 == "Outlier" & STATUS_2 == "Outlier", "Frequency and Severity",
                             ifelse(STATUS_1 == "Outlier","Frequency",
                                    ifelse(STATUS_2 == "Outlier","Severity","Inlier")))) %>%
      select(Billing.Provider.FEIN, frequency, avg_paid_per_claim, severity, claim_count,
             total_paid, Status) %>%
      as.data.frame() %>%
      mutate(round(frequency,digits=2)) %>%
      # pipe_message("mutate round frequency digits done") %>% 
      rename('Provider' = 'Billing.Provider.FEIN', 'Avg.Paid.Per.Claim' = 'avg_paid_per_claim',
             'Claim.Count' = 'claim_count', 'Total.Paid' = 'total_paid', 'Severity' = 'severity',
             'Frequency' = 'frequency') %>%
      # pipe_message('renaming done') %>% 
      mutate(Diagnosis.Code = diagnosis,
             Procedure.Code = procedure)
    
    print(head(data))
    
    datatable(as.data.frame(data) %>%
                select(Diagnosis.Code, Procedure.Code, Provider, Claim.Count,
                       Avg.Paid.Per.Claim, Total.Paid, Severity, Frequency, Status),
              options = list(scrollX = TRUE, lengthMenu = c(5, 10, 20),dom = 'ftp'),
              selection = list(target = "row", mode = "single", selected = c("1"))) %>% 
      formatCurrency(columns = c("Avg.Paid.Per.Claim","Severity","Total.Paid"), "$")
  }, 
  )
}