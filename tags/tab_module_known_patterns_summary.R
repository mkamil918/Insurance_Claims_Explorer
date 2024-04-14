# Front page module
source("formatting.R")

# UI function
tab_pattern_summary_ui <- function(id) {
  
  # Main UI
  fluidPage(
    titlePanel("Known Patterns Summary"),
    tableOutput('selected_summary_stats'),
    valueBoxOutput('fraud_portion_value'),
    fluidRow(
      column(2, tags$div(style = "background-color: royalblue; word-wrap:break-word;",
                         billboarderOutput('gauge_unlisted_rehab_portion', width = "100%", height = "75%"))),
      column(2, tags$div(style = "background-color: #C233FF; word-wrap:break-word;",
                         billboarderOutput('gauge_nova_healthcare_portion', width = "100%", height = "75%"))),
      column(2, tags$div(style = "background-color: #28B463; word-wrap:break-word;",
                         billboarderOutput('gauge_homelink_medrisk_portion', width = "100%", height = "75%")))
    ),
    fluidRow(
      column(5,
        selectInput('cost_plot_diagnosis', "Diagnosis for Benchmark",
                    c("Sprain of ligaments of lumbar spine" = "S33.5XX",
                      "Strain of muscle, fascia and tendon of lower back" = "S39.012")),
        checkboxGroupInput('log_axes', "Select axes for log display",
                           c("X (horizontal)" = "x", "Y (vertical)" = "y")),
        rHandsontableOutput('business_case_table') #https://www.youtube.com/watch?v=BzE1JmC0F6Q
      ),
      column(7, plotOutput('cost_plot'))
    )
    
  )
  
}

# Server function
tab_pattern_summary_server <- function(input, output, session, vals) {

  output$selected_summary_stats <- renderTable({
    if (!is.null(vals$claims_data)){
      data <- vals$claims_data
      
      #bill line item count
      bill_line_count_tbl <- count(data)
      bill_line_count_vector <- bill_line_count_tbl %>% select(n) %>% collect %>% .[[1]]
      bill_line_count <- as.integer(bill_line_count_vector[1])
      bill_line_count <- format(bill_line_count_vector[1], big.mark = ",", nsmall = 0)
      bill_line_count <- format_number(bill_line_count_vector[1])
      
      #bill count
      bill_count_vector <- data %>% distinct(Bill.ID) %>% count() %>% select(n) %>%
        collect %>% .[[1]]
      bill_count <- as.integer(bill_count_vector[1])
      bill_count <- format_number(bill_count)
      
      #claim count
      claim_count_vector <- data %>% distinct(Claim.Administrator.Claim.Number) %>%
        count() %>% select(n) %>% collect %>% .[[1]]
      claim_count <- as.integer(claim_count_vector[1])
      claim_count <- format_number(claim_count)
      
      #dollars paid
      dollars_paid_tbl <- data %>% summarize(total_paid = sum(Total.Amount.Paid.Per.Line))
      dollars_paid_vector <- dollars_paid_tbl %>% select(total_paid) %>% collect %>% .[[1]]
      total_paid <- as.integer(dollars_paid_vector[1])
      total_paid <- dollar(total_paid)
      
      summary_df <- data.frame(total_paid, claim_count, bill_count, bill_line_count)
      names(summary_df) <- c("Total Paid", "Claim Count", "Bill Count", "Bill Line Count")
      summary_df
    }
  })
  
  output$fraud_portion_value <- renderValueBox({
    if (!is.null(vals$claims_data)){
      fraud_portion <- paste(round(fraud_portion() * 100, 1), " %", sep = "")
      shinydashboard::valueBox(fraud_portion,
                               subtitle = tags$p("Portion of Dollars Paid to Suspicous Providers",
                                                 style = "font-size: 150%;"), color = "orange")
    }
  })
  
  output$gauge_excluded_portion <- renderBillboarder({
    if (!is.null(vals$claims_data)){
      excluded_FEINs_grand_total_paid <- excluded_FEINs_grand_total_paid()
      total_paid_all_bills <- total_paid_all_bills()
      excluded_FEINs_portion <- round((excluded_FEINs_grand_total_paid / total_paid_all_bills) * 100, 1)
      fraud_portion <- round(fraud_portion() * 100, 1)
      
      billboarder() %>%
        bb_gaugechart(value = excluded_FEINs_portion) %>%
        bb_gauge(min = 0, max = fraud_portion, units = "", width = 15) %>%
        bb_legend(show = FALSE) %>% bb_title(text = paste("Excluded Providers\n",
                                                          excluded_FEINs_portion, "% of ",
                                                          fraud_portion, "%", sep = ""))
    }
  })
  
  output$gauge_unlisted_rehab_portion <- renderBillboarder({
    if (!is.null(vals$claims_data)){
      unlisted_rehab_grand_total_paid <- unlisted_rehab_grand_total_paid()
      total_paid_all_bills <- total_paid_all_bills()
      unlisted_rehab_portion <- round((unlisted_rehab_grand_total_paid / total_paid_all_bills) * 100, 1)
      fraud_portion <- round(fraud_portion() * 100, 1)
      
      billboarder() %>%
        bb_gaugechart(value = unlisted_rehab_portion) %>%
        bb_gauge(min = 0, max = fraud_portion, units = "", width = 15) %>% bb_legend(show = FALSE) %>%
        bb_title(text = paste("Unlisted Rehab\n", unlisted_rehab_portion, "% of ",
                              fraud_portion, "%", sep = "")) %>% bb_color("Orange")
    }
  })
  
  output$gauge_nova_healthcare_portion <- renderBillboarder({
    if (!is.null(vals$claims_data)){
      nova_healthcare_FEINs_grand_total_paid <- nova_healthcare_FEINs_grand_total_paid()
      total_paid_all_bills <- total_paid_all_bills()
      nova_healthcare_portion <- round((nova_healthcare_FEINs_grand_total_paid / total_paid_all_bills) * 100, 1)
      fraud_portion <- round(fraud_portion() * 100, 1)
      
      billboarder() %>%
        bb_gaugechart(value = nova_healthcare_portion) %>%
        bb_gauge(min = 0, max = fraud_portion, units = "", width = 15) %>% bb_legend(show = FALSE) %>%
        bb_title(text = paste("NOVA Healthcare\n", nova_healthcare_portion, "% of ",
                              fraud_portion, "%", sep = "")) %>% bb_color("Orange")
    }
  })
  
  output$gauge_homelink_medrisk_portion <- renderBillboarder({
    if (!is.null(vals$claims_data)){
      homelink_medrisk_FEINs_grand_total_paid <- homelink_medrisk_grand_total_paid()
      total_paid_all_bills <- total_paid_all_bills()
      homelink_medrisk_portion <- round((homelink_medrisk_FEINs_grand_total_paid / total_paid_all_bills) * 100, 1)
      fraud_portion <- round(fraud_portion() * 100, 1)
      
      billboarder() %>%
        bb_gaugechart(value = homelink_medrisk_portion) %>%
        bb_gauge(min = 0, max = fraud_portion, units = "", width = 15) %>% bb_legend(show = FALSE) %>%
        bb_title(text = paste("Homelink & \nMedrisk\n", homelink_medrisk_portion, "% of ",
                              fraud_portion, "%", sep = "")) %>% bb_color("Orange")
    }
  })
  
  get_data_values <- reactive({
    Total_Paid = c(unlisted_rehab_grand_total_paid(), nova_healthcare_FEINs_grand_total_paid(),
                   homelink_medrisk_grand_total_paid())
    Reduction = c(0.25, 0.25, 0.25)
    Dollar_Impact = Total_Paid * Reduction
    
    df = data.frame(Total_Paid = Total_Paid, Reduction = Reduction, Dollar_Impact = Dollar_Impact)
    
    business_case_values <- reactiveValues(data=df)
  })
  
  output$business_case_table <- renderRHandsontable({
    if (!is.null(vals$claims_data)){
      business_case_values <- get_data_values()
      rhandsontable(business_case_values$data, rowHeaders = c("Unlisted", "Nova", "Homelink_Medrisk"),
                    width = 400, rowHeaderWidth = 125) %>% hot_col("Total_Paid", format = "$0,0") %>%
        hot_col("Dollar_Impact", format = "$0,0") %>% hot_col("Reduction", format = "0%")
    }
  })
  
  observeEvent(
    input$business_case_table$changes$changes, # observe if any changes to the cells of the rhandontable
    {
      business_case_values <- get_data_values()
      
      xi=input$business_case_table$changes$changes[[1]][[1]] # capture the row which is changed
      business_case_values$data <- hot_to_r(input$business_case_table) # convert the rhandontable to R data frame object so manupilation / calculations could be done
      
      #update values within business_case_table
      business_case_values$data[xi+1,3] = business_case_values$data[xi+1,1] + business_case_values$data[xi+1,2] # calculate column varibale C values based on cell values in column variable a and b
      business_case_values$data[xi+1,3] = business_case_values$data[xi+1,1] * business_case_values$data[xi+1,2]
    }
  )
  
  output$cost_plot <- renderPlot({
    if (!is.null(vals$claims_data)){
      data <- vals$claims_data
      benchmark_FEIN <- "040482976"
      diagnosis <- local(input$cost_plot_diagnosis)
      
      benchmark_cost_per_claim <- data %>% dplyr::filter(Billing.Provider.FEIN == benchmark_FEIN,
                                                        First.ICD.9.CM.Diagnosis.Code == diagnosis,
                                                        Line.Number == 1) %>%
        dplyr::group_by(Claim.Administrator.Claim.Number) %>%
        dplyr::summarize(total_paid_by_claim = sum(Total.Amount.Paid.Per.Line)) %>%
        dplyr::summarize(benchmark_avg_paid_per_claim = mean(total_paid_by_claim)) %>%
        as.data.frame() %>% as.numeric
      benchmark_visits_per_claim <- data %>% dplyr::filter(Billing.Provider.FEIN == benchmark_FEIN,
                                                           First.ICD.9.CM.Diagnosis.Code == diagnosis,
                                                           Line.Number == 1) %>%
        dplyr::group_by(Claim.Administrator.Claim.Number) %>% dplyr::summarize(num_visits = n()) %>%
        dplyr::summarize(benchmark_avg_num_visits = mean(num_visits)) %>% as.data.frame() %>% as.numeric
      benchmark_df <- data.frame("Provider(s)" = "Benchmark (Concentra)",
                                 "Avg_Cost_Per_Claim" = benchmark_cost_per_claim,
                                 "Avg_Visits_Per_Claim" = benchmark_visits_per_claim)
      
      unlisted_rehab_FEINs <<- unlisted_rehab_FEINs_vector()
      unlisted_rehab_cost_per_claim <- data %>% dplyr::filter(Billing.Provider.FEIN %in% unlisted_rehab_FEINs,
                                                              First.ICD.9.CM.Diagnosis.Code == diagnosis,
                                                              Line.Number == 1) %>%
        dplyr::group_by(Claim.Administrator.Claim.Number) %>%
        dplyr::summarize(total_paid_by_claim = sum(Total.Amount.Paid.Per.Line)) %>%
        dplyr::summarize(unlisted_rehab_avg_paid_per_claim = mean(total_paid_by_claim)) %>% as.data.frame() %>%
        as.numeric
      unlisted_rehab_visits_per_claim <- data %>% dplyr::filter(Billing.Provider.FEIN %in% unlisted_rehab_FEINs,
                                                                First.ICD.9.CM.Diagnosis.Code == diagnosis,
                                                                Line.Number == 1) %>%
        dplyr::group_by(Claim.Administrator.Claim.Number) %>% dplyr::summarize(num_visits = n()) %>%
        dplyr::summarize(unlisted_rehab_avg_num_visits = mean(num_visits)) %>% as.data.frame() %>% as.numeric
      unlisted_rehab_df <- data.frame("Provider(s)" = "Unlisted Rehab Providers",
                                      "Avg_Cost_Per_Claim" = unlisted_rehab_cost_per_claim,
                                      "Avg_Visits_Per_Claim" = unlisted_rehab_visits_per_claim)
      
      nova_healthcare_FEIN <- nova_healthcare_FEIN()
      nova_healthcare_cost_per_claim <- data %>% dplyr::filter(Billing.Provider.FEIN %in% nova_healthcare_FEIN,
                                                               First.ICD.9.CM.Diagnosis.Code == diagnosis,
                                                               Line.Number == 1) %>%
        dplyr::group_by(Claim.Administrator.Claim.Number) %>%
        dplyr::summarize(total_paid_by_claim = sum(Total.Amount.Paid.Per.Line)) %>%
        dplyr::summarize(avg_paid_per_claim = mean(total_paid_by_claim)) %>% as.data.frame() %>% as.numeric
      nova_healthcare_visits_per_claim <- data %>% dplyr::filter(Billing.Provider.FEIN %in% nova_healthcare_FEIN,
                                                                 First.ICD.9.CM.Diagnosis.Code == diagnosis,
                                                                 Line.Number == 1) %>%
        dplyr::group_by(Claim.Administrator.Claim.Number) %>% dplyr::summarize(num_visits = n()) %>%
        dplyr::summarize(avg_num_visits = mean(num_visits)) %>% as.data.frame() %>% as.numeric
      nova_healthcareb_df <- data.frame("Provider(s)" = "Nova Healthcare",
                                        "Avg_Cost_Per_Claim" = nova_healthcare_cost_per_claim,
                                        "Avg_Visits_Per_Claim" = nova_healthcare_visits_per_claim)
      
      homelink_medrisk_FEIN <- homelink_medrisk_FEIN()
      homelink_medrisk_cost_per_claim <- data %>% dplyr::filter(Billing.Provider.FEIN %in% homelink_medrisk_FEIN,
                                                                First.ICD.9.CM.Diagnosis.Code == diagnosis,
                                                                Line.Number == 1) %>%
        dplyr::group_by(Claim.Administrator.Claim.Number) %>%
        dplyr::summarize(total_paid_by_claim = sum(Total.Amount.Paid.Per.Line)) %>%
        dplyr::summarize(avg_paid_per_claim = mean(total_paid_by_claim)) %>% as.data.frame() %>% as.numeric
      homelink_medrisk_visits_per_claim <- data %>% dplyr::filter(Billing.Provider.FEIN %in% homelink_medrisk_FEIN,
                                                                  First.ICD.9.CM.Diagnosis.Code == diagnosis,
                                                                  Line.Number == 1) %>%
        dplyr::group_by(Claim.Administrator.Claim.Number) %>% dplyr::summarize(num_visits = n()) %>%
        dplyr::summarize(avg_num_visits = mean(num_visits)) %>% as.data.frame() %>% as.numeric
      homelink_medrisk_df <- data.frame("Provider(s)" = "Homelink / Medrisk",
                                        "Avg_Cost_Per_Claim" = homelink_medrisk_cost_per_claim,
                                        "Avg_Visits_Per_Claim" = homelink_medrisk_visits_per_claim)
      
      data_points <- rbind(benchmark_df, unlisted_rehab_df, nova_healthcareb_df, homelink_medrisk_df)
      legend_text <- c("Benchmark (Concentra Health Systems)",
                       paste("Unlisted Rehab Providers ", currency(unlisted_rehab_cost_plot_paid()), sep = ""),
                       paste("NOVA Healthcare ", currency(nova_healthcare_cost_plot_paid()), sep = ""),
                       paste("Homelink & Medrisk ", currency(homelink_medrisk_cost_plot_paid()), sep = ""))
      
      colors <- c("black", "royalblue", "#C233FF", "#28B463")
      plot(data_points$Avg_Visits_Per_Claim, data_points$Avg_Cost_Per_Claim,
           log = paste(input$log_axes,collapse = ""), col = rep(colors), pch = 19, cex = 3,
           xlab = "", ylab = "", xact = "n", yaxt="n", main = "Medical Claims Total Cost and Encounter Frequency")
      legend("topleft", legend = legend_text, col = rep(colors), pch = 19, cex=1)
      axis(2, at=axTicks(2), labels=sprintf("$%s", axTicks(2), sep = ","))
      mtext(side=2, line=2, "Average Paid Per Claim", col="black", font=2,cex=1.2)
      mtext(side=1, line=2, "Average Visit Count Per Claim", col="black", font=2,cex=1.2)
      #https://rstudio-pubs-static.s3.amazonaws.com/297778_5fce298898d64c81a4127cf811a9d486.html
    }
  })
  
  excluded_FEINs_grand_total_paid <- function(){
    excluded_FEINs_grand_total_paid_vector <- excluded_FEINs_paid() %>%
      summarize(grand_total_paid_excluded = sum(total_paid_excluded)) %>%
      select(grand_total_paid_excluded) %>% collect %>% .[[1]]
    excluded_FEINs_grand_total_paid <- as.numeric(excluded_FEINs_grand_total_paid_vector[1])
  }
  
  unlisted_rehab_grand_total_paid <- function(){
    data <- vals$claims_data
    unlisted_rehab_FEINs <<- unlisted_rehab_FEINs_vector()
    unlisted_rehab_vector <- data %>% dplyr::filter(Billing.Provider.FEIN %in% unlisted_rehab_FEINs) %>%
      dplyr::summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% dplyr::select(total_paid) %>%
      collect %>% .[[1]]
    unlisted_rehab <- as.numeric(unlisted_rehab_vector[1])
  }
  
  nova_healthcare_FEINs_grand_total_paid <- function(){
    nova_healthcare_FEINs_grand_total_paid_vector <- nova_healthcare_paid() %>%
      dplyr::summarize(grand_total_paid_cost_outlier = sum(total_paid_cost_outlier)) %>%
      dplyr::select(grand_total_paid_cost_outlier) %>% collect %>% .[[1]]
    nova_healthcare_FEINs_grand_total_paid <- as.numeric(nova_healthcare_FEINs_grand_total_paid_vector[1])
  }
  
  homelink_medrisk_grand_total_paid <- function(){
    data <- vals$claims_data
    homelink_medrisk_FEIN <- homelink_medrisk_FEIN()
    homelink_medrisk_vector <- data %>% dplyr::filter(Billing.Provider.FEIN == homelink_medrisk_FEIN) %>%
      dplyr::summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% dplyr::select(total_paid) %>%
      collect %>% .[[1]]
    homelink_medrisk <- as.numeric(homelink_medrisk_vector[1])
  }
  
  total_paid_all_bills <- function(){
    total_paid_all_bills_vector <- vals$claims_data %>%
      dplyr::summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% dplyr::select(total_paid) %>%
      collect %>% .[[1]]
    total_paid_all_bills <- as.numeric(total_paid_all_bills_vector[1])
  }
  
  ##########reactive functions that must reside within server.R########
  fraud_portion <- reactive({
    total_potential_fraud_identified <- sum(unlisted_rehab_grand_total_paid(),
                                            nova_healthcare_FEINs_grand_total_paid(),
                                            homelink_medrisk_grand_total_paid())
    total_paid_all_bills <- total_paid_all_bills()
    fraud_portion <- (total_potential_fraud_identified / total_paid_all_bills)
  })
  
  nova_healthcare_FEIN <- reactive({
    c("244835136")
  })
  
  homelink_medrisk_FEIN <- reactive({
    c("406276249")
  })
  
  nova_healthcare_paid <- reactive({
    nova_healthcare_vector <- nova_healthcare_FEIN()
    data <- vals$claims_data
    nova_healthcare_paid <- data %>% dplyr::filter(Billing.Provider.FEIN %in% nova_healthcare_vector) %>%
      dplyr::group_by(Billing.Provider.FEIN) %>%
      dplyr::summarize(total_paid_cost_outlier = sum(Total.Amount.Paid.Per.Line))
  })
  
  unlisted_rehab_cost_plot_paid <- reactive({
    data <- vals$claims_data
    unlisted_rehab_FEINs <<- unlisted_rehab_FEINs_vector()
    unlisted_rehab_vector <- data %>%
      dplyr::filter(Billing.Provider.FEIN %in% unlisted_rehab_FEINs,
                    First.ICD.9.CM.Diagnosis.Code == local(input$cost_plot_diagnosis)) %>%
      dplyr::summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% dplyr::select(total_paid) %>%
      collect %>% .[[1]]
    unlisted_rehab <- as.numeric(unlisted_rehab_vector[1])
  })
  
  nova_healthcare_cost_plot_paid <- reactive({
    nova_healthcare_vector <- nova_healthcare_FEIN()
    data <- vals$claims_data
    nova_healthcare_paid_vector <- data %>%
      dplyr::filter(Billing.Provider.FEIN %in% nova_healthcare_vector,
                    First.ICD.9.CM.Diagnosis.Code == local(input$cost_plot_diagnosis)) %>%
      dplyr::group_by(Billing.Provider.FEIN) %>%
      dplyr::summarize(total_paid_cost_outlier = sum(Total.Amount.Paid.Per.Line)) %>%
      dplyr::select(total_paid_cost_outlier) %>% collect %>% .[[1]]
    nova_healthcare_paid <- as.numeric(nova_healthcare_paid_vector[1])
  })
  
  homelink_medrisk_cost_plot_paid <- reactive({
    data <- vals$claims_data
    homelink_medrisk_FEIN <- homelink_medrisk_FEIN()
    homelink_medrisk_vector <- data %>%
      dplyr::filter(Billing.Provider.FEIN == homelink_medrisk_FEIN,
                    First.ICD.9.CM.Diagnosis.Code == local(input$cost_plot_diagnosis)) %>%
      dplyr::summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% dplyr::select(total_paid) %>%
      collect %>% .[[1]]
    homelink_medrisk <- as.numeric(homelink_medrisk_vector[1])
  })
  
  
}
