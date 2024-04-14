# Global (for this .R file) variables
SV1_Columns <- c("Bill.Type", "First.ICD.9.CM.Diagnosis.Code", "Date.of.Bill", "Billing.Provider.FEIN",
                 "Line.Number", "Claim.Administrator.Claim.Number", "Total.Amount.Paid.Per.Line",
                 "Service.Line.From.Date", "HCPCS.Line.Procedure.Billed.Code", "Bill.ID",
                 "Claim.Administrator.FEIN", "Claim.Administrator.Name", "Insurer.FEIN",
                 "First.HCPCS.Modifier.Billed.Code", "Employee.Date.of.Birth", "Employee.Mailing.City", 
                 "Employee.Mailing.Postal.Code", "Service.Line.To.Date", "Employer.FEIN",
                 "Billing.Provider.Last.Name.or.Group", "Billing.Provider.First.Name", 
                 "Billing.Provider.Primary.Address",
                 "Rendering.Bill.Provider.Last.Name.or.Group", 
                 "Rendering.Bill.Provider.First.Name", "Employee.Date.of.Injury")
SV4_Columns <- c("Bill.ID", "Bill.Type", "Claim.Administrator.Claim.Number", "Date.of.Bill",
                 "Line.Number", "Drug.Name", "Service.Line.From.Date", "Total.Amount.Paid.Per.Line",
                 "NDC.Billed.Code")

# Front page module

# UI function
tab_data_selection_ui <- function(id) {
  
  # Main UI
  fluidPage(
    titlePanel("Data Selection"),
    fluidRow(
      #column(2, selectInput("data_source", "Data Source", c("Local CSV", "Vantage", "Upload CSV", "COVID"))),
      #as of Feb 24 2021, limiting the options and redfining terms for simplicity and clarity
      column(2, selectInput("data_source", "Scope", c("Triangles", "Targeted" = "Vantage", "Broad" = "COVID"))),
      column(2, uiOutput("minimum_bill_date")),
      column(2, uiOutput("maximum_bill_date")),
      column(2, uiOutput("selected_bill_type")),
      column(3, uiOutput("insurer_selection"))
    ),
    fluidRow(
      column(2, uiOutput("user_name")),
      column(3, uiOutput("user_password")),
      column(3, uiOutput("load_summaries_switch"))
      #column(3, uiOutput("host_name")),
      #column(4, uiOutput("host_name_free_form"))
    ),
    fluidRow(
      column(4, uiOutput("login_button"))
    ),
    textOutput("summary_stats_title"),
    dataTableOutput("summary_stats")
  )
  
}


# Server function
tab_data_selection_server <- function(input, output, session, vals) {
  
  ####START DATA REACTIVES#####################################################
  get_data_reactive <- reactive({
    connect_to_database(input$data_source)
    data <- NULL
      data <- get_COVID_data()
      data <- data %>% dplyr::select(all_of(append(SV1_Columns, c("DRG", 
                                                                  "Long.Description.First", "Body.Part"))))
      if (input$load_summaries){
        vals$exec_summaries_tbl <- tbl(vals$db.con,
                                       in_schema("fis_med_claims_dev", "Executive_Summary")) %>%
          filter(Insurer_FEIN == local(input$insurer_FEIN)) %>%
          as.data.frame()
        
        vals$exec_barcharts_tbl <- tbl(vals$db.con,
                                      in_schema("fis_med_claims_dev", "Executive_Barcharts")) %>%
          filter(Insurer_FEIN == local(input$insurer_FEIN))# %>%
          #as.data.frame()
      }
    data
  })  
  
  get_local_data <<- reactive({
    data <- read.csv(paste(getwd(), "/data/", "SV1_848446466.csv", sep = ""))
    map <- read.csv(paste(getwd(), "/data/", "ICD10_DRG_MAP.csv", sep = ""))
    data <- dplyr::left_join(data, map, by = 'First.ICD.9.CM.Diagnosis.Code')
    data <- data %>%
      dplyr::mutate(DRG = coalesce(DRG, substr(First.ICD.9.CM.Diagnosis.Code, 1, 1)))
    data
  })
  
  connect_to_database <- function(data_source){
    if (input$user_name != "" && input$user_password != ""){
      print('before context creation...')
      vals$db.con <<- td_create_context(
        host="tdprd.td.teradata.com", uid=input$user_name, pwd=input$user_password, 
        dType = "native", logmech = "LDAP")
      
      print('after context creation...')
    }
  }
  
  get_COVID_data <- reactive({
    con <- vals$db.con
    insurer_FEIN <- local(input$insurer_FEIN)
    
    print('line 97')
    
    #OUTPATIENT
    SV1DETAIL <- tbl(con, in_schema("fis_med_claims_dev", "COVID_SV1_Detail")) %>%
      dplyr::mutate(Service.Line.From.Date = as.integer(Service.Line.From.Date))
    SV1HEADER <- tbl(con, in_schema("fis_med_claims_dev", "COVID_SV1_Header")) %>%
      dplyr::filter(Date.of.Bill >= as.integer(local(input$minimum_bill_date)),
                    Date.of.Bill < as.integer(local(input$maximum_bill_date))
                   )
    if (insurer_FEIN != "All"){
      SV1HEADER <- SV1HEADER %>%
        dplyr::filter(Insurer.FEIN == insurer_FEIN)
    }
    print('line 110')
    SV1HEADER <- SV1HEADER %>% dplyr::rename_all(list(~gsub("-", ".", .)))
    SV1HEADER <- SV1HEADER %>% dplyr::mutate(Date.of.Bill = as.integer(Date.of.Bill)) %>%
      dplyr::mutate(Bill_Year = trunc(Date.of.Bill / 10000))
    SV1 <- dplyr::inner_join(SV1DETAIL, SV1HEADER, by = "Bill.ID") %>%
      dplyr::select(all_of(SV1_Columns))

    print('line 117')
    #INPATIENT
    SV2DETAIL <- tbl(con, in_schema("fis_med_claims_dev", "COVID_SV2_Detail"))
    SV2HEADER <- tbl(con, in_schema("fis_med_claims_dev", "COVID_SV2_Header")) %>%
      dplyr::filter(Date.of.Bill >= as.integer(local(input$minimum_bill_date)),
                    Date.of.Bill < as.integer(local(input$maximum_bill_date))
                   )
    print('line 124')
    SV2HEADER <- SV2HEADER %>%
      dplyr::rename_all(list(~gsub("-", ".", .)))
    SV2 <- dplyr::inner_join(SV2DETAIL, SV2HEADER, by = "Bill.ID")
    print('line 127')
    SV2 <- SV2 %>%
      dplyr::select(-First.ICD.9.CM.Diagnosis.Code) %>%
      dplyr::mutate(First.ICD.9.CM.Diagnosis.Code = Principal.Diagnosis.Code) %>%
      dplyr::select(all_of(SV1_Columns))
    SV_1_and_2 <- dplyr::full_join(SV1, SV2)
  print('line 134')
    ##PRESCRIPTION DRUGS
    SV4DETAIL <- tbl(con, in_schema("fis_med_claims_dev", "COVID_SV4_Detail"))
    SV4HEADER <- tbl(con, in_schema("fis_med_claims_dev", "COVID_SV4_Header")) %>%
      dplyr::filter(Date.of.Bill >= as.integer(local(input$minimum_bill_date)),
                    Date.of.Bill < as.integer(local(input$maximum_bill_date))
                   )
    SV4HEADER <- SV4HEADER %>% dplyr::rename_all(list(~gsub("-", ".", .)))
    SV4 <- dplyr::inner_join(SV4DETAIL, SV4HEADER, by = "Bill.ID") %>% dplyr::select(SV4_Columns)
    SV_1_and_2_and_4 <- SV4 %>% dplyr::full_join(SV_1_and_2)
    print('144')
    #define claims_data based on what Bill Types user selected
    claims_data <- NULL
    if (local(input$selected_bill_type == "All")){
      claims_data <- SV_1_and_2_and_4
    }
    else if (local(input$selected_bill_type == "SV1")){
      claims_data <- SV1
    }
    
    #join with DRG_MAP to get the DRG values
    DRG_MAP <- tbl(con, in_schema("fis_med_claims_dev", "ICD10_DRG_MAP"))
    DRG_MAP <- DRG_MAP %>% dplyr::rename_all(list(~gsub("_", ".", .)))
    DRG_MAP <- DRG_MAP %>% dplyr::rename_all(list(~gsub("-", ".", .)))
    claims_data <- dplyr::left_join(claims_data, DRG_MAP, by = 'First.ICD.9.CM.Diagnosis.Code')
    claims_data <- claims_data %>%
      dplyr::mutate(DRG = coalesce(DRG, substr(First.ICD.9.CM.Diagnosis.Code, 1, 1)))
    print('161')
    #add medical coding descriptions
    descriptions_tbl <- tbl(vals$db.con, in_schema("fis_med_claims_dev", "icd_10_descriptions"))
    claims_data <- claims_data %>%
      dplyr::mutate(ICD.Code = gsub("\\.", "", First.ICD.9.CM.Diagnosis.Code))
    claims_data <- left_join(claims_data, descriptions_tbl %>% 
                       dplyr::select(Long.Description.First, Body.Part, ICD.Code), by = "ICD.Code") %>%
      dplyr::select(-ICD.Code)
    print('169')
    claims_data_return <- claims_data
    #return the temp table if SV1 only
    if (local(input$selected_bill_type == "SV1")){
      copy_to(con, claims_data, "claims_data_temp", 
                                          temporary = TRUE, overwrite = TRUE)
      claims_data_return <- tbl(con, "claims_data_temp")
    }
    claims_data_return
  })
  print('179')
  claims_data <- observeEvent(input$login_button, {
    vals$claims_data <<- get_data_reactive()
    vals$top_diagnoses <<- vals$claims_data %>%
      dplyr::group_by(First.ICD.9.CM.Diagnosis.Code) %>%
      dplyr::summarize(total_paid = sum(Total.Amount.Paid.Per.Line))
    vals$top_insurers <<- vals$claims_data %>%
      dplyr::group_by(Claim.Administrator.FEIN, Insurer.FEIN, Claim.Administrator.Name) %>%
      dplyr::summarize(total_paid = sum(Total.Amount.Paid.Per.Line))
    vals$modifiers <<- vals$claims_data %>%
      dplyr::group_by(First.HCPCS.Modifier.Billed.Code) %>%
      dplyr::summarize(total_paid = sum(Total.Amount.Paid.Per.Line))
    vals$top_procedures <<- vals$claims_data %>%
      dplyr::group_by(HCPCS.Line.Procedure.Billed.Code) %>%
      dplyr::summarize(total_paid = sum(Total.Amount.Paid.Per.Line))
    vals$top_providers <<- vals$claims_data %>%
      dplyr::group_by(Billing.Provider.FEIN, Billing.Provider.Last.Name.or.Group, Billing.Provider.Primary.Address) %>%
      dplyr::summarize(total_paid = sum(Total.Amount.Paid.Per.Line))
  })
  print('198')
  ####END DATA REACTIVES#####################################################
  
  ####START UI ELEMENTS######################################################
  output$minimum_bill_date <- renderUI({
    date_options <- c("2000" = "20000000", "2017" = "20170000", 
                      "2018" = "20180000", "2019" = "20190000", "2020" = "20200000")
    if (input$data_source == "Vantage"){
      selectInput("minimum_bill_date", "Minimum Bill Date", date_options, selected = "20180000")
    }
    else if (input$data_source == "COVID"){
      selectInput("minimum_bill_date", "Minimum Bill Date", date_options, selected = "20170000")
    }
    else if (input$data_source == "Triangles"){
      selectInput("minimum_bill_date", "Minimum Bill Date", date_options, selected = "20170000")
    }
  })
  
  output$maximum_bill_date <- renderUI({
    date_options <- c("2019 Q1" = "20190400", "2020" = "20210000")
    if (input$data_source == "Vantage"){
      selectInput("maximum_bill_date", "Maximum Bill Date", date_options, selected = "20190400")
    }
    else if (input$data_source == "COVID"){
      selectInput("maximum_bill_date", "Maximum Bill Date", date_options, selected = "20210000")
    }
    else if (input$data_source == "Triangles"){
      selectInput("maximum_bill_date", "Maximum Bill Date", date_options, selected = "20210000")
    }
  })
  
  output$selected_bill_type <- renderUI({
    bill_type_options <- c("Outpatient Only" = "SV1", "Out + In + Drug" = "All")
    if (input$data_source == "Vantage"){
      selectInput("selected_bill_type", "Bill Type", bill_type_options, selected = "Sv1")
    }
    else if (input$data_source == "COVID"){
      selectInput("selected_bill_type", "Bill Type", bill_type_options, selected = "All")
    }
    else if (input$data_source == "Triangles"){
      selectInput("selected_bill_type", "Bill Type", bill_type_options, selected = "SV1")
    }
  })
  
  output$insurer_selection <- renderUI({
    insurer_options <- c("Travelers" = "848446466", "TASB" = "539579033",
                         "Texas Mutual" = "726579180", "Texas Municipal" = "631079340",
                         "State Office of Risk Mgmt" = "941579809", "Liberty" = "762442160",
                         "Farmers" = "358538187", "All")
    if (input$data_source == "Vantage"){
      selectInput("insurer_FEIN", "Insurer", insurer_options, selected = "848446466")
    }
    else if (input$data_source == "COVID"){
      selectInput("insurer_FEIN", "Insurer", insurer_options, selected = "All")
    }
    else if (input$data_source == "Triangles"){
      selectInput("insurer_FEIN", "Insurer", insurer_options, selected = "848446466")
    }
  })
  outputOptions(output, "insurer_selection", suspendWhenHidden = FALSE)
  
  output$user_name <- renderUI({
    textInput("user_name", "User Name")
  })
  
  output$user_password <- renderUI({
    passwordInput("user_password", "User Password")
  })
  
  output$login_button <- renderUI({
    actionButton("login_button", "Create Dynamic SQL Pipeline")
  })
 
  output$load_summaries_switch <- renderUI({
    prettySwitch('load_summaries','Load Stored Summaries',value = FALSE)
  })
   
  output$summary_stats_title <- renderText({
    "**SV1 (Outpatient) Only**"
  })
  
  output$summary_stats <- renderDataTable({
    if (!is.null(vals$claims_data)){
      data <- vals$claims_data %>%
        dplyr::filter(Bill.Type == "SV1")
      print('283')
      #bill line item count
      bill_line_count_tbl <- count(data)
      bill_line_count_vector <- bill_line_count_tbl %>% select(n) %>% collect %>% .[[1]]
      bill_line_count <- as.integer(bill_line_count_vector[1])
      bill_line_count <- format(bill_line_count_vector[1], big.mark = ",", nsmall = 0)
      bill_line_count <- format_number(bill_line_count_vector[1])
      print('290')
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
      print('308')
      summary_df <- data.frame(total_paid, claim_count, bill_count, bill_line_count)
      names(summary_df) <- c("Total Paid", "Claim Count", "Bill Count", "Bill Line Count")
      summary_df
    }
  }, options = list(scrollX = TRUE))
  ####END UI ELEMENTS######################################################
  
}