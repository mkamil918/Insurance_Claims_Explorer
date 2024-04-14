# Front page module

# UI function
tab_guided_exploration_ui <- function(id) {
  
  # Main UI
  fluidPage(
    titlePanel("Guided Exploration"),
    fluidRow(
      column(12,
             valueBoxOutput('anomalous_providers_title', width = "100%"),
             tippy::tippy_this(
               elementId = 'anomalous_providers',
               tooltip = "Flagged providers based on several anomaly detection techniques",
               placement="bottom"
             )
      )
    ),
    fluidRow(
      column(4,
        dataTableOutput("anomalous_providers"),
        textOutput("report_text")
      ),
      column(4,
        uiOutput("scenario_options"),
        dataTableOutput("scenario_output")
      )
    ),
    fluidRow(
      column(12,
        valueBoxOutput('diagnosis_procedure_title',width = "100%"),
        tippy::tippy_this(
          elementId = 'diagnosis_procedure_title',
          tooltip = "Analysis of common diagnosis + procedure combinations",
          placement="bottom"
        ),
        dataTableOutput("diagnosis_procedure_pivot")
      )
    )
  )
}

# Server function
tab_guided_exploration_server <- function(input, output, session, vals) {
  
  output$anomalous_providers_title <- renderValueBox({
    shinydashboard::valueBox("", subtitle = tags$p("Provider Anomalies (Across **ALL INSURERS** Regardless of Selected Data)",
      style = "font-size: 150%;"), color = "purple", width = NULL)
  })

  anomalous_providers_data <- reactive({
    file_name <- "C:\\Users\\tk250052\\Documents\\OneDrive - Teradata\\Eminence IC Work\\WC Medical Claims\\github\\InsuranceClaimsExplorer\\data\\anomaly_detection.txt"
    anomalous_providers <- read.table(file_name, header = TRUE, sep = "\t")
    anomalous_providers <- anomalous_providers %>%
      replace(., is.na(.), "")
    anomalous_providers$anomaly_count <- rowSums(anomalous_providers == "x")
    anomalous_providers %>%
      relocate(Billing.Provider.FEIN, anomaly_count) %>%
      arrange(desc(anomaly_count))
  })

  output$anomalous_providers <- renderDataTable({
    anomalous_providers <- anomalous_providers_data()
  }, options = list(scrollX = TRUE, lengthMenu = c(5, 10, 25)))

  output$scenario_options <- renderUI({
    scenario_options <- colnames(anomalous_providers_data() %>%
                                    select(-Billing.Provider.FEIN, -anomaly_count)
                                 )
    selectInput("scenario_options", "Select Scenario to Explore", scenario_options)
  })
  
  get_provider_total_spend <- function(diagnosis, procedure){
    provider_total_spend <- vals$claims_data %>%
      filter(First.ICD.9.CM.Diagnosis.Code == diagnosis, 
             HCPCS.Line.Procedure.Billed.Code == procedure
      ) %>%
      group_by(Billing.Provider.FEIN) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line))
  }
  
  create_severity_MRD <- function(diagnosis, procedure){
    provider_total_spend <- get_provider_total_spend(diagnosis, procedure)
    provider_cost <- vals$claims_data %>%
      filter(First.ICD.9.CM.Diagnosis.Code == diagnosis, 
             HCPCS.Line.Procedure.Billed.Code == procedure
      ) %>%
      group_by(Billing.Provider.FEIN, Claim.Administrator.Claim.Number, Service.Line.From.Date) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      group_by(Billing.Provider.FEIN, Claim.Administrator.Claim.Number) %>%
      summarize(average_paid_per_visit = mean(total_paid)) %>%
      group_by(Billing.Provider.FEIN) %>%
      summarize(provider_average = mean(average_paid_per_visit))
    
    visit_severity_df <- inner_join(provider_cost, provider_total_spend, by = "Billing.Provider.FEIN") %>%
      arrange(desc(total_paid)) %>%
      as.data.frame()
  }
  
  output$scenario_output <- renderDataTable({
    selected_scenario <- input$scenario_options
    #scenario_components <- str_split(selected_scenario, "_")
    scenario_components <- strsplit(selected_scenario, "_")
    type <- scenario_components[[1]][1]
    diagnosis <- scenario_components[[1]][2]
    procedure <- scenario_components[[1]][3]
    create_severity_MRD(diagnosis, procedure)
    # vals$claims_data %>%
    #   filter(First.ICD.9.CM.Diagnosis.Code == diagnosis, HCPCS.Line.Procedure.Billed.Code == procedure) %>%
    #   group_by(Billing.Provider.FEIN) %>%
    #   summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
    #   arrange(desc(total_paid)) %>%
    #   as.data.frame()
    
  })
  
  get_address_text <- function(billing_provider_FEIN){
    addresses_df <- vals$claims_data %>%
      dplyr::filter(Billing.Provider.FEIN == as.character(billing_provider_FEIN)) %>%
      dplyr::group_by(Billing.Provider.Primary.Address) %>%
      dplyr::summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      dplyr::arrange(desc(total_paid)) %>%
      as.data.frame()
    primary_address <- addresses_df[1, "Billing.Provider.Primary.Address"]
    primary_address
  }

  get_diagnosis_information <- function(billing_provider_FEIN){
    diagnoses_df <- vals$claims_data %>%
      dplyr::filter(Billing.Provider.FEIN == billing_provider_FEIN) %>%
      dplyr::group_by(First.ICD.9.CM.Diagnosis.Code) %>%
      dplyr::summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      dplyr::arrange(desc(total_paid)) %>%
      as.data.frame()
    primary_diagnosis <- diagnoses_df[1, "First.ICD.9.CM.Diagnosis.Code"]
  }

  get_selected_FEIN <- reactive({
    selected_row <- input$anomalous_providers_rows_selected[1]
    if (is.null(selected_row)){
      selected_row <- 1
    }

    anomalous_providers <- anomalous_providers_data()
    selected_FEIN <- anomalous_providers[selected_row, "Billing.Provider.FEIN"]
  })

  generate_explanation_text <- function(scenarios_list){
    #outlier_scenarios <- str_split(scenarios_list, ",")
    outlier_scenarios <- strsplit(scenarios_list, ",")
    outlier_scenarios_count <- length(outlier_scenarios[[1]])
    cumulative_explanation <- NULL
    for (i in 1:outlier_scenarios_count){
      #scenario_components <- str_split(outlier_scenarios[[1]][i], "_")
      scenario_components <- strsplit(outlier_scenarios[[1]][i], "_")
      type <- scenario_components[[1]][1]
      diagnosis <- scenario_components[[1]][2]
      procedure <- scenario_components[[1]][3]
      behavior_description <- NULL
      if (type == "fre"){
        behavior_description <- "bills more frequently for "
      }
      else if (type == "sev"){
        behavior_description <- "charges more per visit for "
      }
      incremental_text <- paste(behavior_description, diagnosis, "diagnosis /", procedure, "procedure;")
      if (is.null(cumulative_explanation)){
        cumulative_explanation <- incremental_text
      } else{
        cumulative_explanation <- paste(cumulative_explanation, " ", incremental_text, sep = "")
      }
    }
    return(cumulative_explanation)
  }
  
  output$report_text <- renderText({
    #opening statement
    billing_provider_FEIN <- get_selected_FEIN()
    report_text <- paste("Provider FEIN (encrypted) ", billing_provider_FEIN, " is located at ", sep = "")

    #add address information
    address_information <- get_address_text(billing_provider_FEIN)
    report_text <- paste(report_text, address_information, sep = "")

    #add information about typical diagnoses and procedures
    diagnosis_information <- get_diagnosis_information(billing_provider_FEIN)
    report_text <- paste(report_text,
      " and typtically treats patients with diagnosis ",
      diagnosis_information,
      ".",
      sep = "")

    #add why the provider was flagged
    #--> need to revise grid such that cell values are the procedure / diagnosis pairs
    transposed_anomaly_df <- anomalous_providers_data() %>%
      filter(Billing.Provider.FEIN == billing_provider_FEIN) %>%
      select(-Billing.Provider.FEIN, -anomaly_count) %>%
      t() %>%
      as.data.frame()
    
    colnames(transposed_anomaly_df) <- c("flagged")
    transposed_anomaly_df$type_diagnosis_procedure <- rownames(transposed_anomaly_df)
    rownames(transposed_anomaly_df) <- NULL
    
    #create 'flagged_scenario_string' by concatenating across rows
    type_diagnosis_procedure <- transposed_anomaly_df %>%
      filter(flagged == "x") %>%
      group_by(flagged) %>%
      summarize(outlier_descriptions = paste(type_diagnosis_procedure, collapse = ",")) %>%
      as.data.frame() %>%
      select(outlier_descriptions)
    
    explanation_text <- generate_explanation_text(type_diagnosis_procedure$outlier_descriptions)
    report_text <- paste(report_text, explanation_text, sep = "")
  })
  
  output$diagnosis_procedure_title <- renderValueBox({
    shinydashboard::valueBox("", subtitle = tags$p("Diagnosis & Procedure Combinations - All Providers",
      style = "font-size: 150%;"), color = "purple", width = NULL)
  })

  output$diagnosis_procedure_pivot <- renderDataTable({
    file_name <- "C:\\Users\\tk250052\\Documents\\OneDrive - Teradata\\Eminence IC Work\\WC Medical Claims\\github\\InsuranceClaimsExplorer\\data\\diagnosis_procedure_data_cube.txt"
    read.table(file_name, header = TRUE, sep = "\t")
  }, options = list(scrollX = TRUE))
  
}
