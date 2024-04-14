# Front page module

# UI function
tab_covid_overview_ui <- function(id) {
  
  # Main UI
  fluidPage(
    titlePanel("COVID Overview"),
    fluidRow(
      column(2, 
             selectInput("analysis_type", "Chart Analysis", choices = c("Procedure", "Diagnosis", "Drug")),
             uiOutput("code_values")
      ),
      column(10, plotOutput("month_over_month_plot",
                            dblclick = "plot1_dblclick",
                            brush = brushOpts(id = "plot1_brush", resetOnNew = TRUE)
      )
      )
    ),
    fluidRow(
      column(3,
             selectInput("survey_analysis_type", "Analysis Type", choices = c("Procedure", "Diagnosis", "Drug")),
             uiOutput("hierarchy_selection")
      ),
      column(9, dataTableOutput("split_shifts"))
    ),
    fluidRow(
      column(6, tableOutput("diagnosis_procedure_pre_lockdown")),
      column(6, tableOutput("diagnosis_procedure_post_lockdown"))
    )
    #tableOutput('initial_diagnostic')
  )
  # print(code_values)
  # print('code values from ui')
}

# Server function
tab_covid_overview_server <- function(input, output, session, vals) {
  
  TEXT_SIZE <- 16
  
  output$code_values <- renderUI({
    if (input$analysis_type == "Procedure"){
      code_descriptions_file <- read.csv(paste(getwd(), "\\data\\code_descriptions.csv", sep = ""), sep = ",")
      drug_names <- code_descriptions_file %>% 
        filter(type == "Procedure") %>%
        select(code_value)
      values <- drug_names$code_value
      
      user_text <- "Select Procedures"
    }
    else if (input$analysis_type == "Diagnosis"){
      code_descriptions_file <- read.csv(paste(getwd(), "\\data\\code_descriptions.csv", sep = ""), sep = ",")
      drug_names <- code_descriptions_file %>% 
        filter(type == "Diagnosis") %>%
        select(code_value)
      values <- drug_names$code_value
      user_text <- "Select Diagnoses"
    }
    else if (input$analysis_type == "Drug"){
      code_descriptions_file <- read.csv(paste(getwd(), "\\data\\code_descriptions.csv", sep = ""), sep = ",")
      drug_names <- code_descriptions_file %>% 
        filter(type == "Drug") %>%
        select(code_description) %>%
        group_by(code_description) %>%
        summarize()
      values <- drug_names$code_description
      user_text <- "Select Drug"
    }

    checkboxGroupInput(
      "selected_code_values",
      user_text,
      choices = values,
      selected = values
    )
    print(values)
    print('values')
    print(code_values)
    print('code values')
  })
  
  lockdown_analysis <- function(code_type, values, timing_type, split_month) {
    if (code_type %in% c("diagnosis_all_digits", "diagnosis_chapter", "procedure")){
      filtered_claims <- vals$claims_data %>% filter(Bill.Type %in% c("SV1", "SV2"))
    }
    else if (code_type == "prescription"){
      filtered_claims <- vals$claims_data %>% filter(Bill.Type %in% c("SV4"))
    }
    
    if (code_type == "diagnosis_all_digits"){
      code_name <- "First.ICD.9.CM.Diagnosis.Code"
    } 
    else if (code_type == "diagnosis_chapter"){
      code_name <- "diagnosis_chapter"
      filtered_claims <- filtered_claims %>% 
        dplyr::mutate(diagnosis_chapter = substr(First.ICD.9.CM.Diagnosis.Code, 1, 1))
    }
    else if (code_type == "procedure"){
      code_name <- "HCPCS.Line.Procedure.Billed.Code"
    } else if (code_type == "prescription"){
      code_name <- "NDC.Billed.Code"
    }
    
    print(code_type)
    print('code type')
    print(code_name)
    print('code name')
    
    ##lookup against code description values:  join "filtered claims_ with code descriptions, output is "data"
    code_descriptions_file <- read.csv(paste(getwd(), "\\data\\code_descriptions.csv", sep = ""), sep = ",") %>%
      select(code_value, code_description)
    colnames(code_descriptions_file) <- c(code_name, "code_description")
    copy_to(vals$db.con, code_descriptions_file, "code_descriptions", temporary = TRUE, overwrite = TRUE)
    code_descriptions_tbl <- tbl(vals$db.con, "code_descriptions")
    data <- left_join(filtered_claims, code_descriptions_tbl, by = code_name)
    
    print(head(data))
    print('printing data line 108')
    
    if (code_type != "prescription"){
      data <- data %>%
        mutate(code_description = paste(!!sym(code_name), ": ", code_description, sep = ""))
    }
    print(head(data))
    print('data$code description ')
    
    print(code_values)
    print('printing code values')
    
    # Hard code code_values, try again 
    code_values = "all"
    
    if (code_values == "all"){
      data_filtered_for_codes <- filtered_claims %>%
        mutate(code_description = !!sym(code_name))
    } else {
      data_filtered_for_codes <- data %>%
        filter(!!sym(code_name) %in% values)
    }

    # data_filtered_for_codes <- data %>% filter(!!sym(code_name) %in% code_values)
    
    print(head(data_for_filtered_codes))
    
    if (timing_type == "split"){
      lockdown_date <- split_month
      
      pre_lockdown_grand_total_paid_vector <- data %>% 
        filter(Date.of.Bill <= lockdown_date, Date.of.Bill >= 20200000) %>%
        summarize(grand_total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
        collect %>% .[[1]]
      pre_lockdown_grand_total_paid <- as.numeric(pre_lockdown_grand_total_paid_vector[1])
      pre_lockdown_activity <- data_filtered_for_codes %>% 
        filter(Date.of.Bill <= lockdown_date, Date.of.Bill >= 20200000) %>% 
        mutate(grand_total_paid = pre_lockdown_grand_total_paid) %>% 
        group_by(code_description, grand_total_paid) %>% 
        summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% 
        mutate(portion_dollars_pre_lockdown = total_paid / grand_total_paid)
      
      post_lockdown_grand_total_paid_vector <- data %>% 
        filter(Date.of.Bill > lockdown_date) %>%
        summarize(grand_total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
        collect %>% .[[1]]
      post_lockdown_grand_total_paid <- as.numeric(post_lockdown_grand_total_paid_vector[1])
      post_lockdown_activity <- data_filtered_for_codes %>% 
        filter(Date.of.Bill > lockdown_date) %>% 
        mutate(grand_total_paid = post_lockdown_grand_total_paid) %>% 
        group_by(code_description, grand_total_paid) %>% 
        summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% 
        mutate(portion_dollars_post_lockdown = total_paid / grand_total_paid)
      
      return(inner_join(
        pre_lockdown_activity %>% select(code_description, portion_dollars_pre_lockdown), 
        post_lockdown_activity %>% select(code_description, portion_dollars_post_lockdown),
        by = "code_description"
      ))
    }
    else if(timing_type == "month_over_month"){
      grand_total_paid_by_month <- data %>%
        filter(Date.of.Bill >= 20200000) %>%
        mutate(Date.of.Bill = as.integer(Date.of.Bill)) %>%
        mutate(Date.of.Bill = as.character(Date.of.Bill)) %>%
        mutate(month = substr(Date.of.Bill, 5, 6)) %>%
        group_by(month) %>%
        summarize(grand_total_paid_by_month = sum(Total.Amount.Paid.Per.Line))
      
      monthly_analysis <- data_filtered_for_codes %>%
        filter(Date.of.Bill >= 20200000) %>%
        mutate(Date.of.Bill = as.integer(Date.of.Bill)) %>%
        mutate(Date.of.Bill = as.character(Date.of.Bill)) %>%
        mutate(month = substr(Date.of.Bill, 5, 6)) %>%
        group_by(code_description, month) %>%
        summarize(total_paid = sum(Total.Amount.Paid.Per.Line))
      
      inner_join(monthly_analysis, grand_total_paid_by_month, by = "month") %>%
        mutate(portion_dollars = total_paid / grand_total_paid_by_month) %>%
        arrange (code_description, month)
    }
  }
  
  clean_plot_variables <- function(plot_df){
    month_name <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun")
    month <- c("01", "02", "03", "04", "05", "06")
    month_lookup_df <- data.frame(month_name, month)
    return <- inner_join(plot_df, month_lookup_df, by = "month")
    #https://rstudio-pubs-static.s3.amazonaws.com/7433_4537ea5073dc4162950abb715f513469.html
    #return$month_name <- factor(return$month_name, levels = unique(return$month))
    return$month_name <- factor(return$month_name, levels = return$month_name[order(unique(return$month))])
    return
  }
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  plot_analysis <- function(analysis_output, legend_text){
    #https://www.r-graph-gallery.com/239-custom-layout-legend-ggplot2.html
    #http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software
    analysis_output_df <- analysis_output %>%
      as.data.frame() %>%
      clean_plot_variables() %>%
      filter(month <= "05")
    colnames(analysis_output_df)[colnames(analysis_output_df) == 'code_description'] <- legend_text
    ggplot(data = analysis_output_df, 
           aes(x = month_name, y = portion_dollars, group = !!as.name(legend_text), color = !!as.name(legend_text))) +
      ggtitle("Residual Impact of COVID-19 on Work Comp Medical Claims") +
      theme(plot.title = element_text(hjust = 0.5, size = TEXT_SIZE + 2, face = "bold")) +
      theme(legend.title = element_text(size = TEXT_SIZE, face = "bold")) +
      theme(legend.text = element_text(size = (TEXT_SIZE - 2))) +
      theme(legend.position = "bottom") + 
      theme(axis.title.x = element_text(size = TEXT_SIZE, face = "bold")) +
      theme(axis.title.y = element_text(size = TEXT_SIZE, face = "bold")) +
      theme(axis.text.x = element_text(size = (TEXT_SIZE - 4))) +
      theme(axis.text.y = element_text(size = (TEXT_SIZE - 4))) +
      xlab("Month (2020)") +
      ylab("Portion Dollars Spend") +
      scale_y_continuous(limits = c(0, .1), breaks = c(.025, .05, .075, .1)) +
      scale_y_continuous(labels = scales::percent) +
      geom_line(linetype = "solid", size = 2) +
      geom_point(size = 4) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  }
  
  output$month_over_month_plot <- renderPlot({
    if (input$analysis_type == "Procedure"){
      procedure_analysis <- lockdown_analysis("procedure", input$selected_code_values, "month_over_month", 20200400)
      plot_analysis(procedure_analysis, "Procedures")
    }
    else if (input$analysis_type == "Diagnosis"){
      diagnosis_analysis <- lockdown_analysis("diagnosis_all_digits", input$selected_code_values, "month_over_month", 20200400)
      plot_analysis(diagnosis_analysis, "Diagnoses")
    }
    else if (input$analysis_type == "Drug"){
      code_descriptions_file <- read.csv(paste(getwd(), "\\data\\code_descriptions.csv", sep = ""), sep = ",")
      drug_NDCs <- code_descriptions_file %>% 
        filter(code_description %in% input$selected_code_values) %>%
        select(code_value)
      drug_NDCs_vector <- drug_NDCs$code_value
      
      prescription_analysis <- lockdown_analysis("prescription", drug_NDCs_vector, "month_over_month", 20200400)
      plot_analysis(prescription_analysis, "Drug")
    }
  })
  
  output$hierarchy_selection <- renderUI({
    if (input$survey_analysis_type == "Procedure"){
      selectInput("selected_hierarchy", "Procedure Hierarchy", c("All Digits" = "procedure"))
    }else if (input$survey_analysis_type == "Diagnosis"){
      selectInput("selected_hierarchy", "Diagnosis Hierarchy", c("All Digits" = "diagnosis_all_digits", "Chapter (first digit)" = "diagnosis_chapter", "DRG"))
    }
  })
  
  output$split_shifts <- renderDataTable({
    lockdown_analysis_result <- NULL
    if (input$selected_hierarchy == "diagnosis_chapter"){
      analysis_type <- "diagnosis_chapter"
      diagnosis_chapters <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "V", "W", "X", "Y")
      lockdown_analysis_result <- lockdown_analysis(analysis_type, diagnosis_chapters, "split", 20200400)
    } else if(input$selected_hierarchy %in% c("procedure", "diagnosis_all_digits")){
      lockdown_analysis_result <- lockdown_analysis(input$selected_hierarchy, "all", "split", 20200400)
    }
    
    if (!is.null(lockdown_analysis_result)){
      lockdown_analysis_result %>%
        as.data.frame() %>%
        arrange(desc(portion_dollars_pre_lockdown))
    } else{
    }
  })
  
  output$diagnosis_procedure_pre_lockdown <- renderTable({
    analysis <- vals$claims_data %>% 
      filter(Bill.Type %in% c("SV1")) %>%
      filter(Date.of.Bill < 20200400, Date.of.Bill >= 20200000) %>%
      group_by(HCPCS.Line.Procedure.Billed.Code, First.ICD.9.CM.Diagnosis.Code) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      filter(total_paid > 100000) %>%
      arrange(desc(total_paid))
    head(analysis %>% as.data.frame(), n = 50)
  })
  
  output$diagnosis_procedure_post_lockdown <- renderTable({
    analysis <- vals$claims_data %>% 
      filter(Bill.Type %in% c("SV1")) %>%
      filter(Date.of.Bill >= 20200400) %>%
      group_by(HCPCS.Line.Procedure.Billed.Code, First.ICD.9.CM.Diagnosis.Code) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      filter(total_paid > 100000) %>%
      arrange(desc(total_paid))
    head(analysis %>% as.data.frame(), n = 50)
  })
  
  #https://www.beckershospitalreview.com/finance/cdc-releases-new-icd-10-codes-for-covid-19-related-illnesses.html#:~:text=The%20new%20ICD%2D10%20codes,to%20COVID%2D19%20(Z20.
  #1. Encounter for screening for COVID-19 (Z11.52)
  #2. Contact with and suspected exposure to COVID-19 (Z20.822)
  #3. Personal history of COVID-19 (Z86.16)
  #4. Multisystem inflammatory syndrome (M35.81)
  #5. Other specified systemic involvement of connective tissue (M35.89)
  #6. Pneumonia due to COVID-19 (J12.82)
  
  #https://www.who.int/classifications/icd/COVID-19-coding-icd10.pdf
  #U07.1:  positive test result
  #U07.2:  suspected, probable case
  #Z29.0:  isolation
  #Z01.7:  laboratory examination
  #Z11.5:  Special screening examination
  
  output$initial_diagnostic <- renderTable({
    SV_1_and_2 <- vals$claims_data %>% filter(Bill.Type %in% c("SV1", "SV2"))
    
    SV_1_and_2 %>%
      filter(First.ICD.9.CM.Diagnosis.Code == "U07.1") %>%
      group_by(HCPCS.Line.Procedure.Billed.Code) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      arrange(desc(total_paid))
    
    SV_1_and_2 %>%
      filter(First.ICD.9.CM.Diagnosis.Code == "U07.1") %>%
      group_by(Claim.Administrator.Claim.Number, HCPCS.Line.Procedure.Billed.Code) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      arrange(desc(total_paid))
    
    covid_diagnoses <- c("Z11.52", "Z20.822", "Z86.16", "J12.82", "U07.1", "U07.2", "Z29.0", "Z01.7", "Z11.5")
    
    #total paid, split by SV1 and SV2
    SV_1_and_2 %>% filter(First.ICD.9.CM.Diagnosis.Code %in% covid_diagnoses) %>% group_by(Bill.Type, First.ICD.9.CM.Diagnosis.Code) %>% summarize(total_paid = sum(Total.Amount.Paid.Per.Line))
    
    #individual claim numbers for SV2
    SV_1_and_2 %>% filter(First.ICD.9.CM.Diagnosis.Code %in% covid_diagnoses, Bill.Type == "SV2") %>% 
      group_by(Claim.Administrator.Claim.Number) %>% 
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line))
    
    #complete patient journey
    #for the SV1 variant - G0481?  drug tests?  claim 630255920.  example of being labeled as "COVID"
    #(SV1 also) FP51512399999398 also "ends" with COVID diagnosis
    #nothing earth shattering otherwise small $$ (expected to change in Q12021 w/ more data)
    SV2_covid_claim_numbers <- SV_1_and_2 %>% 
      filter(First.ICD.9.CM.Diagnosis.Code %in% covid_diagnoses, Bill.Type == "SV2") %>%
      group_by(Claim.Administrator.Claim.Number) %>%
      summarize() %>%
      select(Claim.Administrator.Claim.Number) %>%
      as.data.frame()
    SV2_covid_claim_numbers_list <- SV2_covid_claim_numbers$Claim.Administrator.Claim.Number
    SV_1_and_2 %>% filter(Claim.Administrator.Claim.Number %in% SV2_covid_claim_numbers_list) %>%
      group_by(Claim.Administrator.Claim.Number, Service.Line.From.Date, First.ICD.9.CM.Diagnosis.Code, HCPCS.Line.Procedure.Billed.Code) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% arrange(desc(total_paid))
    
    #Demographics of Large dollar SV2 claims - three of them; 2 of 3 have no CPT codes
    #out of a total of 45,042 total unique claim numbers for 2020 Q2
    #<$100,000 out of over $37 million, even before prescription drugs
    #all three under age of 50 ...
    large_dollar_SV2_claims <- SV_1_and_2 %>% 
      filter(Claim.Administrator.Claim.Number %in% SV2_covid_claim_numbers_list) %>%
      group_by(Claim.Administrator.Claim.Number) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      filter(total_paid > 10000) %>% 
      select(Claim.Administrator.Claim.Number) %>%
      as.data.frame()
    large_dollar_SV2_claims_list <- large_dollar_SV2_claims$Claim.Administrator.Claim.Number
    SV_1_and_2 %>% 
      filter(Claim.Administrator.Claim.Number %in% large_dollar_SV2_claims_list) %>%
      group_by(Claim.Administrator.Claim.Number, Employee.Date.of.Birth, Employee.Mailing.City, Employer.FEIN) %>%
      summarize()
    
    #complete patient journey for an individual claim
    SV_1_and_2 %>% filter(Claim.Administrator.Claim.Number == "38399996999393") %>%
      group_by(Bill.Type, Service.Line.From.Date, Service.Line.To.Date, First.ICD.9.CM.Diagnosis.Code, HCPCS.Line.Procedure.Billed.Code, Bill.ID) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% arrange(Service.Line.From.Date)
    
    #SV1 covid claim numbers
    SV1_covid_claim_numbers <- SV_1_and_2 %>% 
      filter(First.ICD.9.CM.Diagnosis.Code %in% covid_diagnoses, Bill.Type == "SV1") %>%
      group_by(Claim.Administrator.Claim.Number) %>%
      summarize() %>%
      select(Claim.Administrator.Claim.Number) %>%
      as.data.frame()
    SV1_covid_claim_numbers_list <- SV1_covid_claim_numbers$Claim.Administrator.Claim.Number
    
    #prescription drug activity for COVID claims
    SV4 <- vals$claims_data %>% filter(Bill.Type == "SV4")
    SV4 %>% filter(Claim.Administrator.Claim.Number %in% SV1_covid_claim_numbers_list) %>%
      group_by(Drug.Name) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      arrange(desc(total_paid))
    
    #NIH paper - indirect impacts of COVID
    #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7445106/
    
    #drop off in independent medical examinations - 99456
    #PT up?  per 97110 and 97530
    #99199 about triple "Other Medicine Services and Procedures" - however still small $
    pre_lockdown_procedures <- SV_1_and_2 %>% 
      filter(Bill.Type == "SV1", Date.of.Bill <= 20200400) %>% 
      group_by(HCPCS.Line.Procedure.Billed.Code) %>% 
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% 
      mutate(grand_total_paid = sum(total_paid)) %>% 
      mutate(portion_dollars_pre_lockdown = total_paid / grand_total_paid)
    
    post_lockdown_procedures <- SV_1_and_2 %>% 
      filter(Bill.Type == "SV1", Date.of.Bill > 20200400) %>% 
      group_by(HCPCS.Line.Procedure.Billed.Code) %>% 
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% 
      mutate(grand_total_paid = sum(total_paid)) %>% 
      mutate(portion_dollars_post_lockdown = total_paid / grand_total_paid)
    
    inner_join(pre_lockdown_procedures %>% 
                 select(HCPCS.Line.Procedure.Billed.Code, portion_dollars_pre_lockdown),
               post_lockdown_procedures %>% 
                 select(HCPCS.Line.Procedure.Billed.Code, portion_dollars_post_lockdown),
               by = "HCPCS.Line.Procedure.Billed.Code"
    ) %>%
      arrange(desc(portion_dollars_pre_lockdown))
    
    #shift in ICD-10 diagnoses, DRG's
    #small uptick in DRG 951 (includes nicotine dependence):  0.0176 to 0.0244
    #G89.4 drop in half - "Chronic Pain Syndrome"
    DRGs <- tbl(vals$db.con, in_schema("fis_med_claims_dev", "ICD10_DRG_MAP"))
    
    pre_lockdown_diagnoses <- SV_1_and_2 %>% 
      filter(Bill.Type == "SV1", Date.of.Bill <= 20200400) %>% 
      group_by(First.ICD.9.CM.Diagnosis.Code) %>% 
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% 
      mutate(grand_total_paid = sum(total_paid)) %>% 
      mutate(portion_dollars_pre_lockdown = total_paid / grand_total_paid)
    
    post_lockdown_diagnoses <- SV_1_and_2 %>% 
      filter(Bill.Type == "SV1", Date.of.Bill > 20200400) %>% 
      group_by(First.ICD.9.CM.Diagnosis.Code) %>% 
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% 
      mutate(grand_total_paid = sum(total_paid)) %>% 
      mutate(portion_dollars_post_lockdown = total_paid / grand_total_paid)
    
    inner_join(pre_lockdown_diagnoses %>% 
                 select(First.ICD.9.CM.Diagnosis.Code, portion_dollars_pre_lockdown),
               post_lockdown_diagnoses %>% 
                 select(First.ICD.9.CM.Diagnosis.Code, portion_dollars_post_lockdown),
               by = "First.ICD.9.CM.Diagnosis.Code"
    ) %>%
      arrange(desc(portion_dollars_pre_lockdown))
    
    pre_lockdown_DRGs <- inner_join(pre_lockdown_diagnoses, DRGs, by = "First.ICD.9.CM.Diagnosis.Code")
    post_lockdown_DRGs <- inner_join(post_lockdown_diagnoses, DRGs, by = "First.ICD.9.CM.Diagnosis.Code")
    
    pre_lockdown_paid_by_DRG <- pre_lockdown_DRGs %>% group_by(DRG) %>% summarize(sum_portion_dollars_pre_lockdown = sum(portion_dollars_pre_lockdown))
    post_lockdown_paid_by_DRG <- post_lockdown_DRGs %>% group_by(DRG) %>% summarize(sum_portion_dollars_post_lockdown = sum(portion_dollars_post_lockdown))
    inner_join(pre_lockdown_paid_by_DRG, post_lockdown_paid_by_DRG, by = "DRG") %>%
      arrange(desc(sum_portion_dollars_pre_lockdown))
    
    #first diagnosis digit
    #F - dependencies e.g. alcohol upward trend; still very small, watch out for this in data update
    pre_lockdown_diagnosis_digit <- SV_1_and_2 %>% 
      filter(Bill.Type == "SV1", Date.of.Bill <= 20200400) %>% 
      mutate(diagnosis_digit = substr(First.ICD.9.CM.Diagnosis.Code, 1, 3)) %>%
      group_by(diagnosis_digit) %>% 
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% 
      mutate(grand_total_paid = sum(total_paid)) %>% 
      mutate(portion_dollars_pre_lockdown = total_paid / grand_total_paid)
    
    post_lockdown_diagnosis_digit <- SV_1_and_2 %>% 
      filter(Bill.Type == "SV1", Date.of.Bill > 20200400) %>% 
      mutate(diagnosis_digit = substr(First.ICD.9.CM.Diagnosis.Code, 1, 3)) %>%
      group_by(diagnosis_digit) %>% 
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% 
      mutate(grand_total_paid = sum(total_paid)) %>% 
      mutate(portion_dollars_post_lockdown = total_paid / grand_total_paid)
    
    inner_join(pre_lockdown_diagnosis_digit %>% 
                 select(diagnosis_digit, portion_dollars_pre_lockdown),
               post_lockdown_diagnosis_digit %>% 
                 select(diagnosis_digit, portion_dollars_post_lockdown),
               by = "diagnosis_digit"
    ) %>%
      arrange(desc(portion_dollars_pre_lockdown))
    
    #shift in prescription drug data; Lyrica down, Lidothol down, Pregabalin up,
    # DULOXETINE HCL - ~ 3%
    #however, "Lyrica" found in many entries some of which go up
    #--> redo this with "NDC.Billed.Code"
    pre_lockdown_drugs <- SV4 %>% 
      filter(Date.of.Bill <= 20200400) %>% 
      group_by(Drug.Name) %>% 
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% 
      mutate(grand_total_paid = sum(total_paid)) %>% 
      mutate(portion_dollars_pre_lockdown = total_paid / grand_total_paid)
    
    post_lockdown_drugs <- SV4 %>% 
      filter(Date.of.Bill > 20200400) %>% 
      group_by(Drug.Name) %>% 
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% 
      mutate(grand_total_paid = sum(total_paid)) %>% 
      mutate(portion_dollars_post_lockdown = total_paid / grand_total_paid)
    
    inner_join(pre_lockdown_drugs %>% 
                 select(Drug.Name, portion_dollars_pre_lockdown),
               post_lockdown_drugs %>% 
                 select(Drug.Name, portion_dollars_post_lockdown),
               by = "Drug.Name"
    ) %>%
      arrange(desc(portion_dollars_pre_lockdown))
    
    #for developing logical drug groupings
    #https://dailymed.nlm.nih.gov/dailymed/lookup.cfm?setid=60185c88-ecfd-46f9-adb9-b97c6b00a553
    lyrica_NDCs <- c("00071101268", "00071101341", "00071101368", "00071101441", "00071101468", "00071101541", "00071101568", "00071101641", "00071101668", "00071101768", "00071101868", "00071101968", "00071102001")
    pre_lockdown_lyrica_paid <- SV4 %>% 
      filter(Date.of.Bill <= 20200400) %>% 
      filter(NDC.Billed.Code %in% lyrica_NDCs) %>%
      summarize(pre_lyrica = sum(Total.Amount.Paid.Per.Line))
    pre_lockdown_total_paid <- SV4 %>% 
      filter(Date.of.Bill <= 20200400) %>% 
      summarize(pre_total = sum(Total.Amount.Paid.Per.Line))
    post_lockdown_lyrica_paid <- SV4 %>% 
      filter(Date.of.Bill > 20200400) %>% 
      filter(NDC.Billed.Code %in% lyrica_NDCs) %>%
      summarize(post_lyrica = sum(Total.Amount.Paid.Per.Line))
    post_lockdown_total_paid <- SV4 %>% 
      filter(Date.of.Bill > 20200400) %>% 
      summarize(post_total = sum(Total.Amount.Paid.Per.Line))
    cbind(as.data.frame(pre_lockdown_lyrica_paid), 
          as.data.frame(pre_lockdown_total_paid),
          as.data.frame(post_lockdown_lyrica_paid),
          as.data.frame(post_lockdown_total_paid)
    ) %>%
      mutate(pre_ratio = pre_lyrica / pre_total) %>%
      mutate(post_ratio = post_lyrica / post_total)
  })
  
}