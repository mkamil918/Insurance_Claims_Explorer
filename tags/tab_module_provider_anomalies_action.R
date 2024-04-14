# Front page module
source("formatting.R")

# UI function
tab_provider_anomalies_action_ui <- function(id) {
  
  ##makes ggplot2 Donut visual not blurry
  trace(grDevices:::png, quote({
    if (missing(type) && missing(antialias)) {
      type <- "cairo-png"
      antialias <- "subpixel"
    }
  }), print = FALSE)
  
  # Main UI
  fluidPage(
    titlePanel("Outlier Executive Summary"),
    ##row of four summary boxes covering Procedure Coding , Claim Cost, and Pathway Outliers
    fluidRow(
      column(3,
             valueBoxOutput('percentage_suspicious_payments',width = "100%"),
             tippy::tippy_this(
               elementId = 'percentage_suspicious_payments',
               tooltip = "Total percentage of anomalous payments made by insurer(s).",
               placement="bottom"
             )
             
      ),
      column(3,
             valueBoxOutput('dollars_suspicious_payments',width = "100%"),
             tippy::tippy_this(
               elementId = 'dollars_suspicious_payments',
               tooltip = "Dollar amount of anomalous payments made by insurer(s).",
               placement="bottom"
              )
      ),
      column(3,
             valueBoxOutput('claim_count_suspicious_payments',width = "100%"),
             tippy::tippy_this(
               elementId = 'claim_count_suspicious_payments',
               tooltip = "Number of unique claims with anomalous bill payments.",
               placement="bottom"
             )
      ),
      column(3,
             valueBoxOutput('provider_count_suspicious_payments',width = "100%"),
             tippy::tippy_this(
               elementId = 'provider_count_suspicious_payments',
               tooltip = "Number of unique providers receiving anomalous bill payments.",
               placement="bottom"
             )
      )
    ),
    
    ##Procedure Coding Outliers
    fluidRow(
      column(3, valueBoxOutput('unlisted_all_gauge_title', width = "100%", height = "40%"),
                tippy::tippy_this(
                  elementId = 'unlisted_all_gauge_title',
                  tooltip = "Unusually high proportion of payments made to unlisted procedure codes such as 97799.",
                  placement="bottom"
                ),
                gaugeOutput("unlisted_all_gauge")
      ),
      column(3, div(style = "height:200px", plotOutput("procedure_coding_outliers_paid"), align = "center")),
      column(3, div(style = "height:200px", plotOutput("procedure_coding_outliers_claims"), align = "center")),
      column(3, div(style = "height:200px", plotOutput("procedure_coding_outliers_providers"), align = "center"))
    ),
    
    
    ##Cost Outliers
    fluidRow(
      column(3,valueBoxOutput('cost_all_gauge_title', width = "100%", height = "40%"),
             gaugeOutput("cost_all_gauge")
      ),
      column(3, div(style = "height:275px", plotOutput("cost_outliers_paid"), align = "center")),
      column(3, div(style = "height:275px", plotOutput("cost_outliers_claims"), align = "center")),
      column(3, div(style = "height:275px", plotOutput("cost_outliers_providers"), align = 'center'))
    ),
    
    fluidRow(
      column(3,valueBoxOutput('pathways_all_gauge_title', width = "100%", height = "40%"),
             gaugeOutput("pathways_all_gauge")
      ),
      column(3, div(style = "height:275px", plotOutput("pathways_outliers_paid"), align = "center")),
      column(3, div(style = "height:275px", plotOutput("pathways_outliers_claims"), align = "center")),
      column(3, div(style = "height:275px", plotOutput("pathways_outliers_providers"), align = 'center'))
    ),
    ##Supporting exploration and reporting
    selectInput("anomaly_type_to_explore", "Explore and Download",
                c("Procedure Coding", "Claim Cost", "Treatment Pathway")),
    fluidRow(
      column(6,
             downloadButton("download_claims_listing", "Download Claims Listing"),
             d3tree3Output("treemap_exploration")),
      column(6, dataTableOutput("detailed_claims_listing"))
    )
  )
  
}

# Server function
tab_provider_anomalies_action_server <- function(input, output, session, vals) {
  
  print("tab_provider_anomalies_action_server...")
  
  anomalies_vals <- reactiveValues(grand_total_paid = NULL, detailed_claims_df = NULL,
                                   claims_count = NULL, providers_count = NULL)
  
  load_count_summaries <- function(){
    print('Loading Count Summaries')
    n_rows <- vals$exec_summaries_tbl %>%
      count() %>%
      as_tibble()
    
    if (n_rows$n > 0){
      counts <- vals$exec_summaries_tbl %>%
        filter(Summary_Key %in% c('claims_count','providers_count')) %>%
        as.data.frame()
      
      claims_count <- counts %>%
        filter(Summary_Key == 'claims_count')
      
      providers_count <- counts %>%
        filter(Summary_Key == 'providers_count')
      
      print('Count summaries loaded')
      
      anomalies_vals$claims_count <- local(claims_count$Summary_Value)
      anomalies_vals$providers_count <- local(providers_count$Summary_Value)
    }
    else{
      print('Count summaries not found, calculating and storing')
      get_counts()
      Insurer_FEIN = c(local(input$insurer_FEIN),local(input$insurer_FEIN))
      Summary_Key = c('claims_count','providers_count')
      Summary_Value = c(anomalies_vals$claims_count,anomalies_vals$providers_count)
      
      df = data.frame(Insurer_FEIN,Summary_Key,Summary_Value)
      
      db_insert_into(con = vals$db.con,
                     table = in_schema("fis_med_claims_dev", "Executive_Summary"),
                     values = df) 
    }
    
  }
  
  load_barchart <- function(data,sel_row,column,row_func){
    print('Loading Barchart')
    unlisted_procedures_summary <- vals$exec_barcharts_tbl %>%
      filter(row == sel_row & col == column) %>%
      as.data.frame()
    
    n_rows <- unlisted_procedures_summary %>%
      count()
    
    if (local(n_rows$n)>0){
      unlisted_procedures_summary_data_cube <- unlisted_procedures_summary %>%
        select(anomaly_type,value) 
      
      print('Barchart loaded')
      
      unlisted_procedures_summary_data_cube
    }
    
    else{
      print('Barchart not found, calculating and storing')
      
      unlisted_procedures_summary_data_cube <- row_func(data)
      
      insert_df <- unlisted_procedures_summary_data_cube %>%
        mutate(Insurer_FEIN = local(input$insurer_FEIN),
               row = sel_row,
               col = column)
      
      db_insert_into(con = vals$db.con,
                     table = in_schema("fis_med_claims_dev", "Executive_Barcharts"),
                     values = insert_df)
      print('Barchart stored')
      
      unlisted_procedures_summary_data_cube
    }
  }
  
  get_counts <- reactive({
    procedure_outlier <- vals$procedure_outlier_data_cube %>%
      select(Billing.Provider.FEIN,Claim.Administrator.Claim.Number) %>%
      distinct(Billing.Provider.FEIN,Claim.Administrator.Claim.Number)
    
    cost_outlier <- vals$cost_outlier_data_cube %>%
      select(Billing.Provider.FEIN,Claim.Administrator.Claim.Number) %>%
      distinct(Billing.Provider.FEIN,Claim.Administrator.Claim.Number)
    
    pathways_outlier <- vals$pathways_outlier_data_cube %>%
      select(Billing.Provider.FEIN,Claim.Administrator.Claim.Number) %>%
      distinct(Billing.Provider.FEIN,Claim.Administrator.Claim.Number)
    
    counts <- dplyr::union_all(procedure_outlier,cost_outlier) %>%
      dplyr::union_all(pathways_outlier) %>%
      dplyr::summarise(provider_count = dplyr::n_distinct(Billing.Provider.FEIN),
                       claim_count = dplyr::n_distinct(Claim.Administrator.Claim.Number)) %>%
      as.data.frame()
    
    anomalies_vals$claims_count <- sum(counts$claim_count)
    anomalies_vals$providers_count <- sum(counts$provider_count)
  })
  
  ##row of four summary boxes covering Procedure Coding , Claim Cost, and Pathway Outliers  
  output$percentage_suspicious_payments <- renderValueBox({
    if (!is.null(vals$claims_data)){
      anomalies_vals$grand_total_paid <- vals$claims_data %>%
        dplyr::summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
        collect %>% .[[1]]
      
      if (is.null(vals$unlisted_rehabalitation_payments)){
        load_procedure_summaries()
      }
      if (is.null(vals$freq_sev_total_sum)){
        load_cost_summaries()
      }
      if (is.null(vals$transition_outliers)){
        load_pathways_summaries()
      }
      
      fraud_portion_raw_value <<- (vals$unlisted_rehabalitation_payments +
                                     vals$other_unlisted_payments +
                                     vals$freq_total +
                                     vals$sev_total +
                                     vals$pathways_payments) / anomalies_vals$grand_total_paid
      
      fraud_portion <- paste(round(fraud_portion_raw_value * 100, 1), " %", sep = "")
      shinydashboard::valueBox(fraud_portion,
                               subtitle = tags$p(" of Payments Labeled As Anomalous",
                                                 style = "font-size: 150%;"),
                               color = "purple",width = NULL)
    }
  })
  
  output$dollars_suspicious_payments <- renderValueBox({
    fraud_portion_raw_value <- (vals$unlisted_rehabalitation_payments +
                                  vals$other_unlisted_payments +
                                  vals$freq_total +
                                  vals$sev_total +
                                  vals$pathways_payments)
    print("typeof fraud_portion_raw_value")
    print(typeof(fraud_portion_raw_value))
    shinydashboard::valueBox(paste('$',format(fraud_portion_raw_value, big.mark = ","), sep = ""),
                             subtitle = tags$p(" of Payments Labeled as Anomalous",
                                               style = "font-size: 150%;"), 
                             color = "purple",width = NULL)
  })
  
  output$claim_count_suspicious_payments <- renderValueBox({
    # procedure_outlier_claims <- vals$procedure_outlier_data_cube %>%
    #   select(Claim.Administrator.Claim.Number) %>%
    #   distinct(Claim.Administrator.Claim.Number)
    # 
    # cost_outlier_claims <- vals$cost_outlier_data_cube %>%
    #   select(Claim.Administrator.Claim.Number) %>%
    #   distinct(Claim.Administrator.Claim.Number)
    # 
    # pathways_outlier_claims<- vals$pathways_outlier_data_cube %>%
    #   select(Claim.Administrator.Claim.Number) %>%
    #   distinct(Claim.Administrator.Claim.Number)
    # 
    # claims <- dplyr::union_all(procedure_outlier_claims,cost_outlier_claims) %>%
    #   dplyr::union_all(pathways_outlier_claims) %>%
    #   dplyr::count() %>%
    #   dplyr::rename(claim_count = n) %>%
    #   as.data.frame()
    # 
    # claim_count <- sum(claims$claim_count)
    
    if (length(anomalies_vals$claims_count)==0){
      if (input$load_summaries){
        load_count_summaries() 
      }
      else{
        get_counts()
      }
    }
    
    shinydashboard::valueBox(paste(format(anomalies_vals$claims_count, big.mark = ",")),
                             subtitle = tags$p(" Claims Labeled as Anomalous",
                                               style = "font-size: 150%;"), 
                             color = "purple",width = NULL)
  })
  
  output$provider_count_suspicious_payments <- renderValueBox({
    # procedure_outlier_providers <- vals$procedure_outlier_data_cube %>%
    #   select(Billing.Provider.FEIN) %>%
    #   distinct(Billing.Provider.FEIN)
    # 
    # cost_outlier_providers <- vals$cost_outlier_data_cube %>%
    #   select(Billing.Provider.FEIN) %>%
    #   distinct(Billing.Provider.FEIN)
    # 
    # pathways_outlier_providers <- vals$pathways_outlier_data_cube %>%
    #   select(Billing.Provider.FEIN) %>%
    #   distinct(Billing.Provider.FEIN)
    # 
    # providers <- dplyr::union_all(procedure_outlier_providers,cost_outlier_providers) %>%
    #   dplyr::union_all(pathways_outlier_providers) %>%
    #   dplyr::count() %>%
    #   dplyr::rename(provider_count = n) %>%
    #   as.data.frame()
    # 
    # prov_count <- sum(providers$provider_count)
    
    shinydashboard::valueBox(paste(format(anomalies_vals$providers_count, big.mark = ",")),
                             subtitle = tags$p(" Providers Receiving Anomalous Payments",
                                               style = "font-size: 150%;"), 
                             color = "purple",width = NULL)
  })
  
  get_gauge <- function(value, gauge_title){
    max_value <- vals$unlisted_rehabalitation_payments + 
      vals$other_unlisted_payments +
      vals$freq_sev_total_sum +
      vals$freq_total + 
      vals$sev_total
    
    billboarder() %>%
      bb_gaugechart(value = value) %>%
      bb_gauge(min = 0, max = max_value, units = "", width = 15) %>%
      bb_legend(show = FALSE) %>%
      bb_title(text = gauge_title) %>%
      bb_color("Orange")
  }
  
  data_row_claims <- function(data){
    claims <- data %>%
      distinct(Claim.Administrator.Claim.Number, anomaly_type) %>%
      dplyr::group_by(anomaly_type) %>%
      dplyr::summarise(value = n()) %>%
      as.data.frame()
  }
  
  data_row_paid <- function(data){
    data %>%
      dplyr::group_by(anomaly_type) %>%
      dplyr::summarize(value = sum(Total.Amount.Paid.Per.Line)) %>%
      as.data.frame()
  }
  
  data_row_providers <- function(data){
    data %>%
      distinct(Billing.Provider.FEIN, anomaly_type) %>%
      dplyr::group_by(anomaly_type) %>%
      dplyr::summarise(value = n()) %>%
      as.data.frame()
  }
  
  donut_wheel_data_row_paid <- function(data, type_of_anomaly_to_analyze){
    paid <- data %>%
      dplyr::filter(anomaly_type == type_of_anomaly_to_analyze) %>%
      dplyr::summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      as.data.frame()
    anomaly_type_df <- data.frame(anomaly_type = c(type_of_anomaly_to_analyze))
    cbind(paid, anomaly_type_df)
  }
  
  donut_wheel_data_row_claims <- function(data, type_of_anomaly_to_analyze){
    claim_count <- data %>%
      dplyr::filter(anomaly_type == type_of_anomaly_to_analyze) %>%
      distinct(Claim.Administrator.Claim.Number) %>%
      dplyr::count() %>%
      dplyr::rename(claim_count = n) %>%
      as.data.frame()
    anomaly_type_df <- data.frame(anomaly_type = c(type_of_anomaly_to_analyze))
    cbind(claim_count, anomaly_type_df)
  }
  
  donut_wheel_data_row_providers <- function(data, type_of_anomaly_to_analyze){
    provider_count <- data %>%
      dplyr::filter(anomaly_type == type_of_anomaly_to_analyze) %>%
      distinct(Billing.Provider.FEIN) %>%
      dplyr::count() %>%
      dplyr::rename(provider_count = n) %>%
      as.data.frame()
    anomaly_type_df <- data.frame(anomaly_type = c(type_of_anomaly_to_analyze))
    cbind(provider_count, anomaly_type_df)
  }
  
  create_summary_donut <- function (labels, values, description){
    width <- 200
    height <- 200
    width <- input$width
    height <- input$height
    show_legend_indicator <- F
    if (description == "Claim\nCount"){
      show_legend_indicator <- T
    }
    fig <- plot_ly(labels = labels, 
                   values = values, 
                   textinfo = 'value',
                   insidetextorientation='radial', width = width, height = height) %>%
      layout(legend = list(orientation = "v", x = 0, y = .5, font = list(size = input$legend_text_size)),
             showlegend = FALSE,annotations = list(text = description, showarrow = F,
                                                   font = list(size = input$text_size, face = "bold"))) %>%
      add_pie(hole = 0.6)
  }
  
  create_summary_donut_updated <- function (labels, values){
    count.data <- data.frame(
      class = labels,
      prop = values
    )
    count.data <- count.data %>%
      arrange(desc(class)) %>%
      mutate(lab.ypos = cumsum(prop) - 0.5*prop)
    
    print(count.data)
    
    mycols <- c("#0073C2FF", "#EFC000FF")#, "#868686FF", "#CD534CFF")
    ggplot(count.data, aes(x = 2, y = prop, fill = class)) +
      geom_bar(stat = "identity", color = "white") +
      coord_polar(theta = "y", start = 0)+
      geom_text(aes(y = lab.ypos, label = prop), color = "white")+
      scale_fill_manual(values = mycols) +
      theme_void() +
      xlim(0.5, 2.5) +
      theme(legend.position = "none")
  }
  
  create_summary_horizontal_bar_chart <- function (labels_input, values_input, bar_colors,
                                                   format_func = scales::comma){
    df <- data.frame(
      labels = labels_input,
      values = values_input
    )
    p <- ggplot(df, aes(x = labels, y = values, fill=labels)) +
      geom_bar(stat='identity',width = 0.7) +
      theme(panel.grid.major = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            legend.position = "none") +
      scale_y_continuous(labels=format_func,expand = expand_scale(mult = c(0, .1))) +
      # scale_fill_brewer(palette="Blues") +
      geom_text(aes(label = labels), vjust = -.5, size = 4)
    p + scale_fill_manual(values=bar_colors)
  }
  
  output$unlisted_all_gauge_title <- renderValueBox({
    shinydashboard::valueBox(value = tags$p("Procedures", style = "font-size: 75%; word-wrap: break-word"),
                             color = "blue", subtitle = "",
                             width = NULL)
  })
  
  output$cost_all_gauge_title <- renderValueBox({
    shinydashboard::valueBox(value = tags$p("Cost", style = "font-size: 75%; word-wrap: break-word"),
                             color = "teal", subtitle = "",
                             width = NULL)
  })
  
  output$pathways_all_gauge_title <- renderValueBox({
    shinydashboard::valueBox(value = tags$p("Pathways", style = "font-size: 75%; word-wrap: break-word"),
                             color = "orange", subtitle = "",
                             width = NULL)
  })
  
  output$unlisted_all_gauge <- renderGauge({
    value <- (vals$unlisted_rehabalitation_payments + vals$other_unlisted_payments) / anomalies_vals$grand_total_paid
    gauge(round(value*100, 3), min = 0, max = fraud_portion_raw_value * 100, symbol = '%', gaugeSectors(
      success = c(0, fraud_portion_raw_value / 5 * 100), 
      warning = c(fraud_portion_raw_value / 5 * 100, 3.5 * fraud_portion_raw_value / 5 * 100), 
      danger = c(3.5 * fraud_portion_raw_value / 5 * 100, fraud_portion_raw_value)
    ))
  })
  
  output$cost_all_gauge <- renderGauge({
    value <- (vals$freq_total + vals$sev_total) / anomalies_vals$grand_total_paid
    gauge(round(value*100, 3), min = 0, max = fraud_portion_raw_value * 100, symbol = '%', gaugeSectors(
      success = c(0, fraud_portion_raw_value / 5 * 100), 
      warning = c(fraud_portion_raw_value / 5 * 100, 3.5 * fraud_portion_raw_value / 5 * 100), 
      danger = c(3.5 * fraud_portion_raw_value / 5 * 100, fraud_portion_raw_value)
    ))
  })
  
  output$pathways_all_gauge <- renderGauge({
    value <- (vals$pathways_payments) / anomalies_vals$grand_total_paid
    gauge(round(value*100, 3), min = 0, max = fraud_portion_raw_value * 100, symbol = '%', gaugeSectors(
      success = c(0, fraud_portion_raw_value / 5 * 100), 
      warning = c(fraud_portion_raw_value / 5 * 100, 3.5 * fraud_portion_raw_value / 5 * 100), 
      danger = c(3.5 * fraud_portion_raw_value / 5 * 100, fraud_portion_raw_value)
    ))
  })
  
  output$unlisted_rehab_gauge <- renderBillboarder({
    gauge_title <- "Unlisted Procedures"
    get_gauge(vals$unlisted_rehabalitation_payments, gauge_title)
  })
  
  output$unlisted_other_gauge <- renderBillboarder({
    gauge_title <- "Procedure Coding\n(Unlisted Other)"
    get_gauge(vals$other_unlisted_payments, gauge_title)
  })
  
  output$procedure_coding_outliers_paid <- renderPlot({
    # unlisted_rehab_summary <- donut_wheel_data_row_paid(vals$procedure_outlier_data_cube,
    #                                                     "unlisted_rehab")
    # unlisted_other_summary <- donut_wheel_data_row_paid(vals$procedure_outlier_data_cube,
    #                                                     "unlisted_other")
    # unlisted_procedures_summary_data_cube <- rbind(unlisted_rehab_summary, unlisted_other_summary) %>%
    #   print()
    
    unlisted_procedures_summary_data_cube <- NULL
    
    if (input$load_summaries){
      unlisted_procedures_summary_data_cube <- load_barchart(data = vals$procedure_outlier_data_cube,
                                                             sel_row = 'procedures',
                                                             column = 'paid',
                                                             row_func = data_row_paid)
    }
    else{
      unlisted_procedures_summary_data_cube <- data_row_paid(vals$procedure_outlier_data_cube)
    }
    
    fig <- create_summary_horizontal_bar_chart(
      unlisted_procedures_summary_data_cube$anomaly_type, 
      unlisted_procedures_summary_data_cube$value,
      c("#79aece", "#4d728d"),
      scales::dollar)
    fig
  }, height = 200, width = 200)
  
  output$procedure_coding_outliers_claims <- renderPlot({
    # unlisted_rehab_summary <- donut_wheel_data_row_claims(vals$procedure_outlier_data_cube,
    #                                                       "unlisted_rehab")
    # unlisted_other_summary <- donut_wheel_data_row_claims(vals$procedure_outlier_data_cube,
    #                                                       "unlisted_other")
    # unlisted_procedures_summary_data_cube <- rbind(unlisted_rehab_summary, unlisted_other_summary)

    unlisted_procedures_summary_data_cube <- NULL
    
    if (input$load_summaries){
      unlisted_procedures_summary_data_cube <- load_barchart(data = vals$procedure_outlier_data_cube,
                                                             sel_row = 'procedures',
                                                             column = 'claims',
                                                             row_func = data_row_claims)
    }
    else{
      unlisted_procedures_summary_data_cube <- data_row_claims(vals$procedure_outlier_data_cube)
    }
    
    fig <- create_summary_horizontal_bar_chart(
      unlisted_procedures_summary_data_cube$anomaly_type,
      as.numeric(unlisted_procedures_summary_data_cube$value),
      c("#79aece", "#4d728d"))
    fig
  }, height = 200, width = 200)
  
  output$procedure_coding_outliers_providers <- renderPlot({
    # unlisted_rehab_summary <- donut_wheel_data_row_providers(vals$procedure_outlier_data_cube,
    #                                                          "unlisted_rehab")
    # unlisted_other_summary <- donut_wheel_data_row_providers(vals$procedure_outlier_data_cube,
    #                                                          "unlisted_other")
    # unlisted_procedures_summary_data_cube <- rbind(unlisted_rehab_summary, unlisted_other_summary) %>%
    #   print()
    
    unlisted_procedures_summary_data_cube <- NULL
    
    if (input$load_summaries){
      unlisted_procedures_summary_data_cube <- load_barchart(data = vals$procedure_outlier_data_cube,
                                                             sel_row = 'procedures',
                                                             column = 'providers',
                                                             row_func = data_row_providers)
    }
    else{
      unlisted_procedures_summary_data_cube <- data_row_providers(vals$procedure_outlier_data_cube)
    }
    
    
    fig <- create_summary_horizontal_bar_chart(unlisted_procedures_summary_data_cube$anomaly_type,
                                               as.numeric(unlisted_procedures_summary_data_cube$value),
                                               c("#79aece", "#4d728d"))
    fig
  }, height = 200, width = 200)
  
  output$cost_outliers_paid <- renderPlot({
    # severity_summary <- donut_wheel_data_row_paid(vals$cost_outlier_data_cube,
    #                                                     "severity")
    # frequency_summary <- donut_wheel_data_row_paid(vals$cost_outlier_data_cube,
    #                                                     "frequency")
    # frequency_severity_summary <- donut_wheel_data_row_paid(vals$cost_outlier_data_cube,
    #                                                         "frequency\nand\nseverity")
    # cost_summary_data_cube <- rbind(severity_summary, frequency_summary,
    #                                                frequency_severity_summary) %>%
    #   print()
    
    cost_summary_data_cube <- NULL
    
    if (input$load_summaries){
      cost_summary_data_cube <- load_barchart(data = vals$cost_outlier_data_cube,
                                              sel_row = 'cost',
                                              column = 'paid',
                                              row_func = data_row_paid)
    }
    else{
      cost_summary_data_cube <- data_row_paid(vals$cost_outlier_data_cube) 
    }
    
    fig <- create_summary_horizontal_bar_chart(cost_summary_data_cube$anomaly_type,
                                               cost_summary_data_cube$value,
                                               c("#66b2b2", "#008080", "#004c4c"),
                                               scales::dollar)
    fig
  }, height = 200, width = 200)
  
  output$cost_outliers_claims <- renderPlot({
    # severity_summary <- donut_wheel_data_row_claims(vals$cost_outlier_data_cube,
    #                                               "severity")
    # frequency_summary <- donut_wheel_data_row_claims(vals$cost_outlier_data_cube,
    #                                                "frequency")
    # frequency_severity_summary <- donut_wheel_data_row_claims(vals$cost_outlier_data_cube,
    #                                                         "frequency\nand\nseverity")
    # cost_summary_data_cube <- rbind(severity_summary, frequency_summary,
    #                                 frequency_severity_summary) %>%
    #   print()
    
    cost_summary_data_cube <- NULL
    
    if (input$load_summaries){
      cost_summary_data_cube <- load_barchart(data = vals$cost_outlier_data_cube,
                                              sel_row = 'cost',
                                              column = 'claims',
                                              row_func = data_row_claims)
    }
    else{
      cost_summary_data_cube <- data_row_claims(vals$cost_outlier_data_cube)
    }
    
    
    fig <- create_summary_horizontal_bar_chart(cost_summary_data_cube$anomaly_type,
                                               as.numeric(cost_summary_data_cube$value),
                                               c("#66b2b2", "#008080", "#004c4c"))
    fig
  }, height = 200, width = 200)
  
  output$cost_outliers_providers <- renderPlot({
    # severity_summary <- donut_wheel_data_row_providers(vals$cost_outlier_data_cube,
    #                                                 "severity")
    # frequency_summary <- donut_wheel_data_row_providers(vals$cost_outlier_data_cube,
    #                                                  "frequency")
    # frequency_severity_summary <- donut_wheel_data_row_providers(vals$cost_outlier_data_cube,
    #                                                           "frequency\nand\nseverity")
    # cost_summary_data_cube <- rbind(severity_summary, frequency_summary,
    #                                 frequency_severity_summary) %>%
    #   print()
    cost_summary_data_cube <- NULL
    
    if (input$load_summaries){
      cost_summary_data_cube <- load_barchart(data = vals$cost_outlier_data_cube,
                                              sel_row = 'cost',
                                              column = 'providers',
                                              row_func = data_row_providers)
    }
    else{
      cost_summary_data_cube <- data_row_providers(vals$cost_outlier_data_cube)
    }
    
    
    fig <- create_summary_horizontal_bar_chart(cost_summary_data_cube$anomaly_type,
                                               as.numeric(cost_summary_data_cube$value),
                                               c("#66b2b2", "#008080", "#004c4c"))
    fig
  }, height = 200, width = 200)  
  
  output$pathways_outliers_paid <- renderPlot({
    pathways_summary_data_cube <- NULL
    
    if (input$load_summaries){
      pathways_summary_data_cube <- load_barchart(data = vals$pathways_outlier_data_cube,
                                                             sel_row = 'pathways',
                                                             column = 'paid',
                                                             row_func = data_row_paid)
    }
    else{
      pathways_summary_data_cube <- data_row_paid(vals$pathways_outlier_data_cube)
    }
    
    fig <- create_summary_horizontal_bar_chart(
      pathways_summary_data_cube$anomaly_type, 
      pathways_summary_data_cube$value,
      c("#fb6134"),
      scales::dollar)
    fig
  }, height = 200, width = 200)
  
  output$pathways_outliers_claims <- renderPlot({
    pathways_summary_data_cube <- NULL
    
    if (input$load_summaries){
      pathways_summary_data_cube <- load_barchart(data = vals$pathways_outlier_data_cube,
                                                  sel_row = 'pathways',
                                                  column = 'claims',
                                                  row_func = data_row_claims)
    }
    else{
      pathways_summary_data_cube <- data_row_claims(vals$pathways_outlier_data_cube)
    }
    
    fig <- create_summary_horizontal_bar_chart(
      pathways_summary_data_cube$anomaly_type,
      as.numeric(pathways_summary_data_cube$value),
      c("#fb6134"))
    fig
  }, height = 200, width = 200)
  
  output$pathways_outliers_providers <- renderPlot({
    pathways_summary_data_cube <- NULL
    
    if (input$load_summaries){
      pathways_summary_data_cube <- load_barchart(data = vals$pathways_outlier_data_cube,
                                                  sel_row = 'pathways',
                                                  column = 'providers',
                                                  row_func = data_row_providers)
    }
    else{
      pathways_summary_data_cube <- data_row_providers(vals$pathways_outlier_data_cube) 
    }
    
    fig <- create_summary_horizontal_bar_chart(pathways_summary_data_cube$anomaly_type,
                                               as.numeric(pathways_summary_data_cube$value),
                                               c("#fb6134"))
    fig
  }, height = 200, width = 200)
  
  
  output$cost_outliers_gauge <- renderBillboarder({
    cost_outliers_total_paid <- (vals$freq_sev_total_sum +
                                   vals$freq_total +
                                   vals$sev_total)
    gauge_title <- "Cost Outliers"
    get_gauge(cost_outliers_total_paid, gauge_title)
  })

  get_reporting_tbl <- reactive({
    input_reporting_cube <- NULL
    if(input$anomaly_type_to_explore == "Procedure Coding"){
      if (!is.null(vals$procedure_outlier_data_cube)){
        input_reporting_cube <- vals$procedure_outlier_data_cube
      }
    } else if (input$anomaly_type_to_explore == "Claim Cost"){
      if (!is.null(vals$cost_outlier_data_cube)){
        input_reporting_cube <- vals$cost_outlier_data_cube
      }
    } else if (input$anomaly_type_to_explore == "Treatment Pathway"){
      if (!is.null(vals$pathways_outlier_data_cube)){
        input_reporting_cube <- vals$pathways_outlier_data_cube
      }
    }
    output_reporting_cube <- input_reporting_cube %>%
      group_by(Claim.Administrator.Claim.Number, 
               Billing.Provider.FEIN,
               HCPCS.Line.Procedure.Billed.Code,
               First.ICD.9.CM.Diagnosis.Code
      )
  })
    
  output$treemap_exploration <- renderD3tree3({
    reporting_table <- get_reporting_tbl()
    claim_index <- input$detailed_claims_listing_rows_selected[1]
    if (length(claim_index)>0){
      claim <- anomalies_vals$detailed_claims_df[claim_index, 1]
      reporting_table <- reporting_table %>%
        filter(Claim.Administrator.Claim.Number==local(claim))
    }
    treemap_df <- reporting_table %>%
      group_by(HCPCS.Line.Procedure.Billed.Code, First.ICD.9.CM.Diagnosis.Code) %>%
      rename("Diagnosis" = First.ICD.9.CM.Diagnosis.Code, "Procedure" = HCPCS.Line.Procedure.Billed.Code) %>%
      summarize(Total_Paid = sum(Total.Amount.Paid.Per.Line)) %>%
      filter(Total_Paid > 0) %>%
      as.data.frame()
    
    p <- treemap(treemap_df, index=c("Diagnosis","Procedure"), vSize="Total_Paid", type="index",
                 palette = "Set2", bg.labels=c("white"),
                 align.labels=list( c("center", "center"), c("right", "bottom")))
    
    d3tree3(p, rootname = input$anomaly_type_to_explore)
  })
  
  get_detailed_claims_listing_df <- reactive({
    get_reporting_tbl() %>%
      group_by(Claim.Administrator.Claim.Number, Billing.Provider.FEIN) %>%
      summarize("Total Paid" = sum(Total.Amount.Paid.Per.Line)) %>%
      rename("Claim #" = Claim.Administrator.Claim.Number, Provider = Billing.Provider.FEIN) %>%
      as.data.frame()
  })
  
  output$download_claims_listing <- downloadHandler(
    filename = function() {
      paste("claims Listing.csv", sep = "")
    },
    content = function(file) {
      write.csv(anomalies_vals$detailed_claims_df, file, row.names = FALSE)
    }
  )
  
  output$detailed_claims_listing <- renderDataTable({
    anomalies_vals$detailed_claims_df <- get_detailed_claims_listing_df()
    anomalies_vals$detailed_claims_df
  }, options = list(scrollX = TRUE),
  selection = list(target = "row", mode = "single"))
  
}
