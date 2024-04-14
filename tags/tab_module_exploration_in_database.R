# Front page module

# UI function
tab_exploration_in_database_ui <- function(id) {
  
  # remove <NONE>
  
  custom_explore_options <<- c("Claim Number" = "Claim.Administrator.Claim.Number", 
    "Provider ID" = "Billing.Provider.FEIN", "Diagnosis" = "First.ICD.9.CM.Diagnosis.Code", 
    "Procedure" = "HCPCS.Line.Procedure.Billed.Code", "Date of Bill" = "Date.of.Bill", 
    "Start Service Date" = "Service.Line.From.Date", "Insurer ID" = "Insurer.FEIN", 
    "Claim Admin ID" = "Claim.Administrator.FEIN", "Claim Admin Name" = "Claim.Administrator.Name",
    "Bill ID" = "Bill.ID", 
    "Rendering Provider (Last) Name" = "Rendering.Bill.Provider.Last.Name.or.Group",
    "Rendering Provider First Name" = "Rendering.Bill.Provider.First.Name", 
    "Billing Provider Address" = "Billing.Provider.Primary.Address", 
    "Body Part" = "Body.Part", "Employee Mailing City" = "Employee.Mailing.City", 
    "Employee Mailing Postal Code" = "Employee.Mailing.Postal.Code", 
    "Employee Date of Injury" = "Employee.Date.of.Injury", "<NONE>")
  
  # Main UI
  fluidPage(
    titlePanel("Exploration (in-database)"),
    fluidRow(
      column(2, selectInput("summary_type", "Summary Type", c("Top Diagnoses", "Top Insurers",
        "Modifiers", "Top Procedures", "Top Providers", "Custom"), selected = "Custom")),
      column(2, uiOutput("custom_explore_filter1")),
      column(3, uiOutput("filter_value1")),
      column(2, uiOutput("custom_explore_filter2")),
      column(3, uiOutput("filter_value2"))
    ),
    fluidRow(
      column(4, uiOutput("custom_explore_group1")),
      column(4, uiOutput("custom_explore_group2")),
      column(4, uiOutput("custom_explore_group3"))
    ),
    fluidRow(
      column(4, uiOutput("custom_explore_group4")),
      column(4, uiOutput("custom_explore_group5")),
      column(4, uiOutput("custom_explore_group6"))
    ),
    fluidRow(
      column(4, uiOutput("submit_button")),
      column(4, downloadButton("download_exploration", "Download Claims Listing"))
    ),
    fluidRow(
      column(12, dataTableOutput("display_summary"))
    )
  )
  
}

# Server function
tab_exploration_in_database_server <- function(input, output, session, vals) {
  
  output$custom_explore_filter1 <- renderUI({
    if (input$summary_type == "Custom"){
      selectInput("custom_explore_filter1", "Filter Field 1", custom_explore_options, 
                  selected = "Claim.Administrator.Claim.Number")
    }
  })
  
  output$filter_value1 <- renderUI({
    if (input$summary_type == "Custom"){
      textInput("filter_value1", "',' separated value(s) w/o spaces")
    }
  })
  
  output$custom_explore_filter2 <- renderUI({
    if (input$summary_type == "Custom"){
      selectInput("custom_explore_filter2", "Filter Field 2",
                  custom_explore_options, selected = "Claim.Administrator.Claim.Number")
    }
  })
  
  output$filter_value2 <- renderUI({
    if (input$summary_type == "Custom"){
      textInput("filter_value2", "',' separated value(s) w/o spaces")
    }
  })
  
  output$submit_button <- renderUI({
    #if (input$summary_type == "Custom"){
      actionButton("submit_button", "Display Data")
    #}
  })
  
  output$custom_explore_group1 <- renderUI({
    if (input$summary_type == "Custom"){
      selectInput("custom_explore_group1", "Group Level 1",
                  custom_explore_options, selected = "Claim.Administrator.Claim.Number")
    }
  })
   
  output$custom_explore_group2 <- renderUI({
    if (input$summary_type == "Custom"){
      selectInput("custom_explore_group2", "Group Level 2", custom_explore_options,
                  selected = "<NONE>")
    }
  })
  
  output$custom_explore_group3 <- renderUI({
    if (input$summary_type == "Custom"){
      selectInput("custom_explore_group3", "Group Level 3", custom_explore_options,
                  selected =  "<NONE>")
    }
  })
  
  output$custom_explore_group4 <- renderUI({
    if (input$summary_type == "Custom"){
      selectInput("custom_explore_group4", "Group Level 4", custom_explore_options,
                  selected =  "<NONE>")
    }
  })
  
  output$custom_explore_group5 <- renderUI({
    if (input$summary_type == "Custom"){
      selectInput("custom_explore_group5", "Group Level 5", custom_explore_options, 
                  selected =  "<NONE>")
    }
  })
  
  output$custom_explore_group6 <- renderUI({
    if (input$summary_type == "Custom"){
      selectInput("custom_explore_group6", "Group Level 6", custom_explore_options, 
                  selected =  "<NONE>")
    }
  })
  
  observeEvent(input$submit_button, {
    filtered_data <- NULL
    if (input$filter_value1 == ""){
      print("filter_value1 is blank")
      filtered_data <- vals$claims_data
      print(head(filtered_data))
    }
    else{
      print("filter_value1 is not blank")
      print(input$filter_value1)
      filter_values1_as_vector <- unlist(strsplit(input$filter_value1, split=","))
      filtered_data <- vals$claims_data %>% 
        filter(!!as.name(input$custom_explore_filter1) %in% filter_values1_as_vector)
    }
    if (input$filter_value2 != ""){
      filter_values2_as_vector <- unlist(strsplit(input$filter_value2, split=","))
      filtered_data <- filtered_data %>% 
        filter(!!as.name(input$custom_explore_filter2) %in% filter_values2_as_vector)
      print(head(filtered_data))
      print('first if cond line 141')
    }
    
    # Replace multiple if conditions with just 1 check (if the input is not none)
    
    # else if (input$custom_explore_group5 != "<NONE>"){
    #   grouped_data <- filtered_data %>%
    #     group_by(!!as.name(input$custom_explore_group1), !!as.name(input$custom_explore_group2), 
    #       !!as.name(input$custom_explore_group3), !!as.name(input$custom_explore_group4),
    #       !!as.name(input$custom_explore_group5))
    # }
    # else if (input$custom_explore_group4 != "<NONE>"){
    #   grouped_data <- filtered_data %>%
    #     group_by(!!as.name(input$custom_explore_group1), !!as.name(input$custom_explore_group2), 
    #              !!as.name(input$custom_explore_group3), !!as.name(input$custom_explore_group4))
    # }
    # else if (input$custom_explore_group3 != "<NONE>"){
    #   grouped_data <- filtered_data %>%
    #     group_by(!!as.name(input$custom_explore_group1), !!as.name(input$custom_explore_group2),
    #              !!as.name(input$custom_explore_group3))
    # }
    # else if (input$custom_explore_group2 != "<NONE>"){
    #   grouped_data <- filtered_data %>%
    #     group_by(!!as.name(input$custom_explore_group1), !!as.name(input$custom_explore_group2))
    # }
    # # else if (input$custom_explore_group2 == "<NONE>" & input$custom_explore_group3 != "<NONE>" & 
    #          input$custom_explore_group4 != "<NONE>" & input$custom_explore_group5 != "<NONE>" & 
    #          input$custom_explore_group6 != "<NONE>"){
    #   grouped_data <- filtered_data %>%
    #     group_by(!!as.name(input$custom_explore_group1), !!as.name(input$custom_explore_group3), 
    #              !!as.name(input$custom_explore_group4), !!as.name(input$custom_explore_group5),
    #                                        !!as.name(input$custom_explore_group6))
    #   print("2 is none rest are selected")
    # }
      if (input$custom_explore_group1 != "<NONE>"){
        grouping_list <- c(input$custom_explore_group1, input$custom_explore_group2, 
                           input$custom_explore_group3,
                           input$custom_explore_group4, 
                           input$custom_explore_group5, input$custom_explore_group6)
        grouping_list <- grouping_list[grouping_list != "<NONE>"]
        grouping_list <- grouping_list[!duplicated(grouping_list)]
      grouped_data <- filtered_data %>%
        group_by_at(grouping_list)
      print(head(grouped_data))
      print('grouped data head line 198 exp in data')
    }

    else {
       grouping_list <- c(input$custom_explore_group1, input$custom_explore_group2, 
                           input$custom_explore_group3,
                           input$custom_explore_group4, 
                           input$custom_explore_group5, input$custom_explore_group6)
        grouping_list <- grouping_list[grouping_list != "<NONE>"]
        grouping_list <- grouping_list[!duplicated(grouping_list)]
        grouped_data <- filtered_data %>%
          group_by_at(grouping_list)
        print(head(grouped_data))
        print('grouped data head line 198 exp in data')
      }
      
    vals$custom_data <- grouped_data %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line))
  })
  
  output$download_exploration <- downloadHandler(
    filename = function() {
      paste("Claims Activity.csv", sep = "")
    },
    content = function(file) {
      write.csv(vals$exploration_result, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$submit_button, {
    result <- NULL
    if (input$summary_type == "Top Diagnoses"){
      result <- vals$top_diagnoses
    }
    else if (input$summary_type == "Top Insurers"){
      result <- vals$top_insurers
    }
    else if (input$summary_type == "Modifiers"){
      result <- vals$modifiers
    }
    else if (input$summary_type == "Top Procedures"){
      result <- vals$top_procedures
    }
    else if (input$summary_type == "Top Providers"){
      result <- vals$top_providers
    }
    else if (input$summary_type == "Custom"){
      result <- vals$custom_data
    }
    
    if (!is.null(result)){
      result <- result %>%
        filter(!is.na(total_paid)) %>%
        mutate(rank = rank(-total_paid)) %>%
        filter(rank <= 5000) %>%
        select(-rank)
    }
    vals$exploration_result <<- result
  })
  
  output$display_summary <- renderDataTable({
    if (!is.null(vals$exploration_result)){
      vals$exploration_result %>%
        as.data.frame()
    }
  }, options = list(scrollX = TRUE))
  
}