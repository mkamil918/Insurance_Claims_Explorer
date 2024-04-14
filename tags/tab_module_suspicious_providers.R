# Front page module

# UI function
tab_suspicious_providers_ui <- function(id) {
  
  # Main UI
  fluidPage(
    titlePanel("Procedure Outliers"),
      fluidRow(
        column(3, valueBoxOutput('unlisted_rehabilitation_status',width = "100%")),
        column(3, valueBoxOutput('other_unlisted_status',width = "100%")),
        column(3, valueBoxOutput('additional_procedure_codes_status',width = "100%")),
        column(3, valueBoxOutput('modifiers_status',width = "100%"))
      ),
    fluidRow(
      column(3,wellPanel(uiOutput('cpt_select'),
                         uiOutput('claims_cost_range'),
                         uiOutput('claims_count_range'))),
      column(9,div(dataTableOutput("provider_table"), style = "font-size: 85%;")),
      style='padding-bottom:20px;'
    ),
    fluidRow(
      column(3,wellPanel(uiOutput('cpt_axes'))),
      column(9,plotlyOutput('plot_2D'))
    )
  )
  
}

# Server function
tab_suspicious_providers_server <- function(input, output, session, vals) {
  
  get_2D <- function(data,code_1,code_2){
    provider_costs <- data %>%
      dplyr::select(HCPCS.Line.Procedure.Billed.Code,Total.Amount.Paid.Per.Line,
                    Billing.Provider.FEIN, Claim.Administrator.Claim.Number) %>%
      dplyr::mutate(dummy = 1) %>%
      dplyr::group_by(Billing.Provider.FEIN) %>%
      dplyr::summarise(ax_1 = round((sum(dummy[HCPCS.Line.Procedure.Billed.Code	== code_1])/(sum(dummy)+ 1e-20))*100,digits=2),
                       ax_2 = round((sum(dummy[HCPCS.Line.Procedure.Billed.Code	== code_2])/(sum(dummy)+ 1e-20))*100,digits=2)) %>%
      dplyr::mutate(ax_1 = coalesce(ax_1, 0), ax_2 = coalesce(ax_2, 0)) %>%
      dplyr::filter(ax_1 > 0 | ax_2 > 0)
  }
  
  
  filter_data <- function(data,selected_procs,min_total,min_claims){
    selected_providers <- data %>%
      dplyr::select(Billing.Provider.FEIN, Claim.Administrator.Claim.Number,Total.Amount.Paid.Per.Line) %>%
      dplyr::group_by(Billing.Provider.FEIN) %>%
      dplyr::summarise(n_claims = n_distinct(Claim.Administrator.Claim.Number),
                total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      dplyr::filter(n_claims >= min_claims, total_paid >= min_total) %>%
      dplyr::select(Billing.Provider.FEIN,total_paid) %>%
      dplyr::as_tibble()
    
    data %>%
      dplyr::select(HCPCS.Line.Procedure.Billed.Code,Total.Amount.Paid.Per.Line,
                    Billing.Provider.FEIN) %>%
      dplyr::filter(Billing.Provider.FEIN %in% local(selected_providers$Billing.Provider.FEIN)) %>%
      dplyr::group_by(Billing.Provider.FEIN) %>%
      dplyr::mutate(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      dplyr::filter(HCPCS.Line.Procedure.Billed.Code %in% selected_procs) %>%
      dplyr::group_by(Billing.Provider.FEIN, HCPCS.Line.Procedure.Billed.Code,total_paid) %>%
      dplyr::summarise(portion_paid = sum(Total.Amount.Paid.Per.Line)/(total_paid + 1e-20)) %>%
      dplyr::select(Billing.Provider.FEIN,HCPCS.Line.Procedure.Billed.Code,portion_paid,total_paid)
  }
  
  load_procedure_summaries <<- reactive({
    if (!is.null(vals$claims_data)){
      payments <- vals$claims_data %>%
        dplyr::group_by(Billing.Provider.FEIN) %>%
        dplyr::summarise(ax_1 = sum(Total.Amount.Paid.Per.Line[HCPCS.Line.Procedure.Billed.Code == '97799'])/(sum(Total.Amount.Paid.Per.Line) + 1e-20),
                  paid = sum(Total.Amount.Paid.Per.Line[HCPCS.Line.Procedure.Billed.Code == '97799'])) %>%
        dplyr::mutate(ax_1 = coalesce(ax_1, 0), paid = coalesce(paid, 0))
      
      labeled_payments <- tukey_filter_1D(payments)
      
      suspicious_payments <- labeled_payments %>%
        dplyr::summarise(payments = sum(paid[Status == 'Outlier'])) %>%
        dplyr::as_tibble()
      
      vals$unlisted_rehabalitation_payments <- local(suspicious_payments$payments)
      
      unlisted_rehabilition_FEINs <- labeled_payments %>%
        dplyr::filter(Status == 'Outlier') %>%
        dplyr::select(Billing.Provider.FEIN)
      
      unlisted_rehab_outlier_data_cube <- dplyr::inner_join(vals$claims_data,
                                                            unlisted_rehabilition_FEINs, by = "Billing.Provider.FEIN") %>%
        dplyr::filter(HCPCS.Line.Procedure.Billed.Code == "97799") %>%
        dplyr::mutate(anomaly_type = "unlisted_rehab")
      
      
      print(head(unlisted_rehab_outlier_data_cube))
      
      
      payments <- vals$claims_data %>%
        dplyr::mutate(cpt_suffix = substr(HCPCS.Line.Procedure.Billed.Code, 4, 5)) %>%
        dplyr::group_by(Billing.Provider.FEIN) %>%
        dplyr::mutate(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
        dplyr::filter(cpt_suffix == "99" & HCPCS.Line.Procedure.Billed.Code != '97799') %>%
        dplyr::group_by(Billing.Provider.FEIN, HCPCS.Line.Procedure.Billed.Code, total_paid) %>%
        dplyr::summarize(ax_1 = sum(Total.Amount.Paid.Per.Line)/(total_paid + 1e-20),
                         paid = sum(Total.Amount.Paid.Per.Line)) %>%
        # dplyr::distinct(Billing.Provider.FEIN, HCPCS.Line.Procedure.Billed.Code, ax_1, paid, total_paid) %>%
        dplyr::mutate(ax_1 = coalesce(ax_1, 0), paid = coalesce(paid, 0)) 
      
      
      payments <- vals$claims_data %>%
        dplyr::mutate(cpt_suffix = substr(HCPCS.Line.Procedure.Billed.Code, 4, 5)) %>%
        dplyr::group_by(Billing.Provider.FEIN) %>%
        dplyr::mutate(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
        dplyr::filter(cpt_suffix == "99" & HCPCS.Line.Procedure.Billed.Code != '97799') %>%
        dplyr::group_by(Billing.Provider.FEIN, HCPCS.Line.Procedure.Billed.Code, total_paid) %>%
        dplyr::summarize(ax_1 = sum(Total.Amount.Paid.Per.Line)/(total_paid + 1e-20),
                  paid = sum(Total.Amount.Paid.Per.Line)) %>%
        # dplyr::distinct(Billing.Provider.FEIN, HCPCS.Line.Procedure.Billed.Code, ax_1, paid, total_paid) %>%
        dplyr::mutate(ax_1 = coalesce(ax_1, 0), paid = coalesce(paid, 0))  
      

      print(head(payments))
      
      # Pass HCPS Line proc billed code as string 
      
      thresholds <- tukey_thresholds_1D(payments,
                                        group_column = HCPCS.Line.Procedure.Billed.Code)
      
      print("thresholds")
      print(head(thresholds))
      
      labeled_payments_unlisted_other <- dplyr::inner_join(payments,thresholds,
                                                           by = "HCPCS.Line.Procedure.Billed.Code") %>%
        dplyr::mutate(Status = ifelse(ax_1 <= UT,"INLIER", "OUTLIER"))
      
      
      suspicious_payments <- labeled_payments_unlisted_other %>%
        summarise(payments = sum(paid[Status == 'Outlier'])) %>%
        mutate(payments = coalesce(payments, 0)) %>%
        as.data.frame()
      
      vals$other_unlisted_payments <- sum(suspicious_payments$payments)
      
      #"tab_module_provider_anomalies_action.R" needs supporting data for summary tab
      unlisted_other_FEINs <- labeled_payments_unlisted_other %>%
        dplyr::filter(Status == 'OUTLIER') %>%
        dplyr::ungroup() %>%
        dplyr::select(Billing.Provider.FEIN)
      
      unlisted_other_outlier_data_cube <- inner_join(vals$claims_data,
                                                     unlisted_other_FEINs, by = "Billing.Provider.FEIN") %>%
        dplyr::mutate(cpt_suffix = substr(HCPCS.Line.Procedure.Billed.Code, 4, 5)) %>%
        dplyr::filter(cpt_suffix == "99" & HCPCS.Line.Procedure.Billed.Code != '97799') %>%
        dplyr::select(-cpt_suffix) %>%
        dplyr::mutate(anomaly_type = "unlisted_other")
      
      ##create complete procedure_outlier_data_cube
      vals$procedure_outlier_data_cube <<- dplyr::union_all(unlisted_rehab_outlier_data_cube,
                                                           unlisted_other_outlier_data_cube)
      
    }
  })
  
  output$unlisted_rehabilitation_status <- renderValueBox({
    if (is.null(vals$unlisted_rehabalitation_payments)){
      load_procedure_summaries()
    }
    shinydashboard::valueBox(paste('$',format(vals$unlisted_rehabalitation_payments, big.mark = ","),
                                   sep = ""),
      subtitle = tags$p("Suspicious Payments for CPT 97799",
      style = "font-size: 150%;"),
      color = "red",
      width = NULL)
  })
  
  output$other_unlisted_status <- renderValueBox({
    shinydashboard::valueBox(paste('$',format(vals$other_unlisted_payments, big.mark = ","), sep = ""),
      subtitle = tags$p("Other Unlisted Procedures",
      style = "font-size: 150%;"),
      color = "yellow",
      width = NULL)
  })
  
  output$additional_procedure_codes_status <- renderValueBox({
    if (!is.null(vals$claims_data)){
      payments <- vals$claims_data %>%
        dplyr::group_by(Billing.Provider.FEIN) %>%
        dplyr::mutate(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
        dplyr::group_by(Billing.Provider.FEIN,HCPCS.Line.Procedure.Billed.Code,total_paid) %>%
        dplyr::summarise(ax_1 = sum(Total.Amount.Paid.Per.Line)/(total_paid + 1e-20),
                  paid = sum(Total.Amount.Paid.Per.Line)) %>%
        # dplyr::distinct(Billing.Provider.FEIN,HCPCS.Line.Procedure.Billed.Code,ax_1,paid) %>%
        dplyr::mutate(ax_1 = coalesce(ax_1, 0), paid = coalesce(paid, 0))
      
      thresholds <- tukey_thresholds_1D(payments,HCPCS.Line.Procedure.Billed.Code)
      

      labeled_payments <- dplyr::inner_join(payments,thresholds,
                                     by = "HCPCS.Line.Procedure.Billed.Code") %>%
        dplyr::mutate(Status = ifelse(ax_1 <= UT,"Inlier", "Outlier")) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(row_n = row_number(Billing.Provider.FEIN),dummy=1)
      
      bivariate_labeled_payments <- labeled_payments %>% 
        dplyr::select(Billing.Provider.FEIN, HCPCS.Line.Procedure.Billed.Code, paid, Status) %>%
        dplyr::filter(Status == "Outlier") %>%
        dplyr::group_by(Billing.Provider.FEIN) %>%
        dplyr::filter(n() > 1)


      suspicious_payments <- bivariate_labeled_payments %>%
        dplyr::summarise(payments = sum(paid)) %>%
        dplyr::mutate(payments = coalesce(payments, 0)) %>%
        as.data.frame()
      
      shinydashboard::valueBox(paste('$',format(sum(suspicious_payments$payments), big.mark = ","), sep = ""),
                               subtitle = tags$p("Additional CPT Code Abnormalities",
                                                 style = "font-size: 150%;"),
                               color = "green",
                               width = NULL)
    }
  })
  
  output$modifiers_status <- renderValueBox({
    if (!is.null(vals$claims_data)){
      # suspicious_modifiers <- c('50','51','52') # Unused list
      
      payments <- vals$claims_data %>%
        dplyr::group_by(Billing.Provider.FEIN) %>%
        dplyr::mutate(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
        dplyr::filter(!is.null(First.HCPCS.Modifier.Billed.Code)) %>%
        dplyr::group_by(Billing.Provider.FEIN,First.HCPCS.Modifier.Billed.Code,total_paid) %>%
        dplyr::summarise(ax_1 = sum(Total.Amount.Paid.Per.Line)/(total_paid + 1e-20),
                  paid = sum(Total.Amount.Paid.Per.Line)) %>%
        # dplyr::distinct(Billing.Provider.FEIN,First.HCPCS.Modifier.Billed.Code,ax_1,paid) %>%
        dplyr::mutate(ax_1 = coalesce(ax_1, 0),paid = coalesce(paid, 0))
    
      thresholds <- tukey_thresholds_1D(payments,First.HCPCS.Modifier.Billed.Code)
      
      labeled_payments <- dplyr::inner_join(payments,thresholds,
                                     by = "First.HCPCS.Modifier.Billed.Code") %>%
        mutate(Status = ifelse(ax_1 <= UT,"Inlier", "Outlier"))
      
      
      suspicious_payments <- labeled_payments %>%
        dplyr::summarise(payments = sum(paid[Status == 'Outlier'])) %>%
        dplyr::mutate(payments = coalesce(payments, 0)) %>%
        as.data.frame()
      
      shinydashboard::valueBox(paste('$',format(sum(suspicious_payments$payments), big.mark = ","), sep = ""),
        subtitle = tags$p("Suspicious Payments for Modifiers",
        style = "font-size: 150%;"),
        color = "yellow",
        width = NULL)
    }
  })
  
  output$cpt_select <- renderUI({
    if (!is.null(vals$claims_data)){
      opts = c("ALL",as.character(sort(unique(head(get_local_data(), n = 10000)$HCPCS.Line.Procedure.Billed.Code))))
      selectInput(
        inputId = "cpt_select",
        label = "CPT Codes",
        choices = opts,
        selectize = FALSE,
        size = 10,
        selected = c('95831','97530','97112','97799'),
        multiple = TRUE
      )
    }
  })
  
  output$claims_cost_range <- renderUI({
    if (!is.null(vals$claims_data)){
      sliderTextInput(
        inputId = "claims_cost_range",
        label = "Minimum Paid by Provider", 
        choices = c(0, 10000, 25000, 50000, 75000, 100000),
        grid = TRUE
      )
    }
  })
  
  output$claims_count_range <- renderUI({
    if (!is.null(vals$claims_data)){
      sliderTextInput(
        inputId = "claims_count_range",
        label = "Minimum Claim Count for Provider", 
        choices = c(0, 5, 10, 15, 20, 25),
        grid = TRUE
      )
    }
  })
  
  output$cpt_axes <- renderUI({
    if (!is.null(vals$claims_data)){
      default_choices <- c('95831 vs 99201','95831 vs 99202','95831 vs 99203','95831 vs 99204',
                           '95831 vs 99205','97530 vs 97112')
      if (sum(lengths(input$cpt_select)) >= 2){
        others <- t(combn(input$cpt_select,2))
        more <- paste(others[,1],'vs',others[,2])
        default_choices <- c(default_choices, more)
      }
      selectInput(
        inputId = "cpt_axes",
        label = "CPT Codes",
        choices = default_choices,
        selectize = FALSE,
        size = 10
      )
    }
  })
  
  output$provider_table <- renderDataTable({
    if (length(input$cpt_select) > 0) {
      filtered <- filter_data(vals$claims_data,
                              input$cpt_select,
                              input$claims_cost_range,
                              input$claims_count_range)
      
      if ('97799' %in% input$cpt_select){
        payments_97799 <- vals$claims_data %>%
          dplyr::group_by(Billing.Provider.FEIN) %>%
          dplyr::summarise(portion_paid = sum(Total.Amount.Paid.Per.Line[HCPCS.Line.Procedure.Billed.Code == '97799'])/(sum(Total.Amount.Paid.Per.Line) + 1e-20),
                    total_paid = sum(Total.Amount.Paid.Per.Line[HCPCS.Line.Procedure.Billed.Code == '97799'])) %>%
          dplyr::mutate(portion_paid = coalesce(portion_paid, 0),
                 total_paid = coalesce(total_paid, 0),
                 HCPCS.Line.Procedure.Billed.Code = '97799') %>%
          dplyr::filter(portion_paid == 0)  
        
      print("step 1 - procedure outliers")
      print(head(payments_97799))
        
        filtered <- filtered %>%
          union_all(payments_97799)  
      }
      
      if (input$data_source != "Vantage"){
        filtered <- filtered #%>%
          # dplyr::distinct(Billing.Provider.FEIN, HCPCS.Line.Procedure.Billed.Code,
          #                 total_paid,portion_paid)  
      }
      
      thresholds <- filtered %>%
        dplyr::rename(ax_1 = portion_paid) %>%
        tukey_thresholds_1D(HCPCS.Line.Procedure.Billed.Code)  

      labeled_payments <- dplyr::inner_join(filtered,thresholds,
                                     by = c("HCPCS.Line.Procedure.Billed.Code" = "HCPCS.Line.Procedure.Billed.Code")) %>%
        dplyr::mutate(Status = ifelse(portion_paid <= UT,"Inlier", "Outlier")) %>%
        dplyr::select(Billing.Provider.FEIN, HCPCS.Line.Procedure.Billed.Code, portion_paid,
               total_paid, Status) %>%
        dplyr::rename(Provider = Billing.Provider.FEIN, Procedure = HCPCS.Line.Procedure.Billed.Code,
               Portion = portion_paid, Total.Paid = total_paid) %>%
        dplyr::filter(Portion > 0)  
      
      datatable(as.data.frame(labeled_payments),options = list(scrollX = TRUE, pageLength = 12,
                                        lengthChange = FALSE, dom = 'ftp')) %>% 
        formatPercentage("Portion", 2) %>%
        formatCurrency("Total.Paid", "$")
    }
  })   
   
  output$plot_2D <- renderPlotly({
    if (!(is.na(input$cpt_axes))){
      axes <- strsplit(input$cpt_axes," vs ")[[1]]
      table_2D <- get_2D(vals$claims_data,axes[1],axes[2])
      df <- as.data.frame(tukey_filter_2D(table_2D))
      
      p <- df %>%
        ggplot(aes(x = ax_1, y = ax_2, color = Status)) +
        geom_point(mapping = aes(provider = Billing.Provider.FEIN)) +
        xlab(paste('Proportion of',axes[1])) +
        ylab(paste('Proportion of',axes[2])) +
        xlim(0,100) +
        ylim(0,100) +
        scale_colour_manual(values = c("Outlier" = "red", "Inlier" = "green")) +
        theme_bw()
      ggplotly(p,tooltip = c("provider","x","y")) %>%
        add_segments(x = local(df$UT_1), xend = local(df$UT_1), y = 0, yend = 110,
                     line = list(color = 'blue', dash = "dash", width = 0.4),
                     showlegend = F) %>%
        add_segments(x = 0, xend = 110, y = local(df$UT_2), yend = local(df$UT_2),
                     line = list(color = 'blue', dash = "dash", width = 0.4),
                     showlegend = F) %>%
        layout(paper_bgcolor='#F5F5F5')
    }
   })
}