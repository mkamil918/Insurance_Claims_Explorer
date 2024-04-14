# Front page module

# UI function
tab_pathways_viz_ui <- function(id) {
  
  # Main UI
  fluidPage(
    titlePanel("Pathways Visualization"),
    fluidRow(
      column(4, uiOutput('suspicious_transitions'))
      ),
    wellPanel(
      fluidRow(
        column(6,div(dataTableOutput("top_diagnoses"), style = "font-size: 85%;")),
        column(6,div(dataTableOutput("top_providers"), style = "font-size: 85%;")),
        style='padding-bottom:20px;'
      ),
      fluidRow(
        # column(5, selectInput("initial_diagnosis", "Endpoint Diagnosis",
        #                       c("Sprain of ligaments of lumbar spine" = "S33.5XX",
        #                         "Strain of muscle, fascia and tendon of lower back" = "S39.012",
        #                         "Injury, unspecified" = "T14.90X", "Low Back Pain" = "M54.5",
        #                         "Chronic Pain Syndrome" = "G89.4",
        #                         "Complete rotator cuff tear or rupture of right shoulder" = "M75.121",
        #                         "Sprain of ligaments of cervical spine" = "S13.4XX"))),
        column(2, uiOutput('endpoint_type')),
        column(2, selectInput("week_count", "Weeks to Display", c(3, 4, 5, 6, 7, 8), selected = 6)),
        column(2, selectInput("pathway_digit_depth", "ICD-10 Hierarchy", c("3", "DRG", "all"),
                              selected = "all")),
        column(3, selectInput("complete_paths_only", "Complete Paths Only", c("no" = FALSE,
                                                                              "yes" = TRUE),
                              selected = FALSE)),
        column(3, uiOutput("pathing_source"))
      )
    ),
    sankeywheelOutput('all_pathways_visual'),
    sankeywheelOutput('inlier_pathways_visual'),
    sankeywheelOutput('outlier_pathways_visual')
    
  )
  
}

# Server function
  tab_pathways_viz_server <- function(input, output, session, vals) {
    
    pathviz_vals <- reactiveValues(top_providers_df = NULL,
                                   length_outliers = NULL)
    
    # Empty, since there is no interactivity on front page
    output$endpoint_type <- renderUI({
      options <- c("Start")
    try(    
      if (input$pathing_source == "bupaR"){
          options <- c("Start", "End")
        },
      silent = TRUE
    )
    selectInput("endpoint_type", "Endpoint Type", options)
  })

  output$top_diagnoses <- renderDataTable({
    if (is.null(vals$diagnoses_1_df)){
      vals$diagnoses_1_df <- diagnoses_1_df()
    }
    datatable(vals$diagnoses_1_df, options = list(scrollX = TRUE, lengthMenu = c(5, 10, 20),
                                                  dom = 'ftp'),
              selection = list(target = "row", mode = "single", selected = c("1"))) %>%
      formatCurrency("Total.Paid", "$")
  })

  load_pathways_summaries <<- reactive({
    data <- vals$claims_data
    insurers <- Dict$new("848446466"="Travelers","539579033"="TASB",
                         "726579180"="Texas Mutual","631079340"="Texas Municipal",
                         "941579809"="State Office of Risk Mgmt","762442160"="Liberty",
                         "All"="All")
      
    table_name = paste('KMeansOutput_',insurers[local(input$insurer_FEIN)],sep='')
      
    table_name <- stringi::stri_replace(table_name, replacement = '_', regex=' ')
      
    claims_clusters <- tryCatch(
      expr = {tbl(vals$db.con,in_schema("fis_med_claims_dev", table_name))},
      error = function(error){
        print(paste("Looking for",table_name))
        data_for_pathing <- data %>%
          dplyr::select(Claim.Administrator.Claim.Number, First.ICD.9.CM.Diagnosis.Code, DRG,
                        Service.Line.From.Date, Total.Amount.Paid.Per.Line) %>%
          dplyr::group_by(Claim.Administrator.Claim.Number) %>%
          dplyr::mutate(first_service_date = min(Service.Line.From.Date)) %>%
          dplyr::mutate(Service.Line.From.Date = as.integer(Service.Line.From.Date)) %>%
          dplyr::mutate(first_service_date = as.integer(first_service_date)) %>%
          dplyr::mutate(week = trunc((as_date(Service.Line.From.Date) - as_date(first_service_date)) / 7))
          
        data_for_pathing <- data_for_pathing %>%
          dplyr::group_by(Claim.Administrator.Claim.Number, First.ICD.9.CM.Diagnosis.Code, DRG, week) %>%
          dplyr::summarise(Total.Amount.Paid.Per.Line = sum(Total.Amount.Paid.Per.Line)) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(Claim.Administrator.Claim.Number, week) %>%
          dplyr::mutate(diagnosis_count = n()) %>%
          dplyr::ungroup() %>%
          dplyr::rename(Service.Line.From.Date = week)
          
        data_for_pathing <- data_for_pathing %>%
          dplyr::mutate(First.ICD.9.CM.Diagnosis.Code = ifelse(diagnosis_count > 1, "Many",
                                                               as.character(First.ICD.9.CM.Diagnosis.Code)),
                        DRG = ifelse(diagnosis_count > 1, "Many",as.character(DRG))) %>%
          dplyr::group_by(Claim.Administrator.Claim.Number, First.ICD.9.CM.Diagnosis.Code, DRG,
                          Service.Line.From.Date) %>%
          dplyr::summarise(Total.Amount.Paid.Per.Line = sum(Total.Amount.Paid.Per.Line)) %>%
          dplyr::ungroup()
          
        data_for_pathing_tbl <- data_for_pathing %>%
          dplyr::mutate(Diagnosis = First.ICD.9.CM.Diagnosis.Code,
                        DRG = DRG,
                        Claim_Number = Claim.Administrator.Claim.Number,
                        Date = Service.Line.From.Date,
                        Total_Paid = Total.Amount.Paid.Per.Line) %>%
          select(Diagnosis, DRG, Claim_Number, Date, Total_Paid)
        
        # copy_to(vals$db.con, data_for_pathing_tbl,
        #         in_schema("fis_med_claims_dev", "data_for_pathing_tbl_perm_drg"),
        #         temporary = FALSE, overwrite = TRUE)
        
        print(head(data_for_pathing_tbl))
        print('printing data for pathing tbl line 123')
       
        transitions_result <- td_npath_sqle (
          data1 = data_for_pathing_tbl,
          mode = "OVERLAPPING",
          pattern = "B.B",
          symbols =  c("true as B"),
          result = c("ACCUMULATE (CDISTINCT \"Claim_Number\" OF ANY (B) DELIMITER '$$') AS claim_numbers",
                       "ACCUMULATE (\"DRG\" of any(B) ) AS path",
                     "FIRST(\"Date\" of ANY (B)) as week"),
          data1.partition.column = "Claim_Number",
          data1.order.column = "Date"
        )
          
        transitions_result <- transitions_result$result
        
        print(transitions_result)
          
        final_pairs <- transitions_result %>%
          rename(trace = path, case_id = claim_numbers) %>%
          mutate(case_id = gsub('[\\[\\]]','',case_id),
                 evnt1 = gsub(', .+|\\[','',trace),
                 evnt2 = gsub('\\[[a-zA-Z0-9\\.\\-]+, |\\]','',trace)) %>%
          select(case_id, evnt1, evnt2, week)
          
        copy_to(vals$db.con, final_pairs, "final_pairs",temporary = TRUE, overwrite = TRUE,
                primary.index = c('evnt1','evnt2'))
          
        final_pairs <- tbl(vals$db.con,"final_pairs")
          
        collapsed_transition_probs <- tbl(vals$db.con,in_schema("fis_med_claims_dev",
                                                                "Collapsed_Transition_Probs"))
          
        path_probs <- inner_join(collapsed_transition_probs,final_pairs,by = c('evnt1','evnt2'))
          
        pivotted_cases <- tdplyr::td_pivot(
          data = path_probs,
          data.partition.column = 'case_id',
          data.order.column = 'week',
          partition.columns = 'case_id',
          num.rows = 103,
          target.columns = 't_prob'
        )
          
        pivotted_cases <- pivotted_cases$result
          
        print(pivotted_cases)
          
        coalesce_pivotted <- pivotted_cases %>%
          mutate_all(funs(z=coalesce(.,0)))
          
        coalesce_pivotted <- coalesce_pivotted %>%
          select(contains('_z'))
          
        print('coalesce_pivotted')
          
        copy_to(vals$db.con,coalesce_pivotted,in_schema("fis_med_claims_dev", "KMeansInput"), 
                temporary = FALSE, overwrite = TRUE)
          
        try(DBI::dbExecute(vals$db.con,"DROP TABLE fis_med_claims_dev.KMeansOutput"), silent = TRUE)
          
        # query <- "CREATE TABLE fis_med_claims_dev.KMeansOutput as (
        #           SELECT * FROM TD_KMeansPredict(
        #            ON fis_med_claims_dev.KMeansInput AS InputTable1 PARTITION BY ANY
        #            ON fis_med_claims_dev.Cluster_Centroids AS InputTable2 DIMENSION
        #           ) AS dt
        #           ) WITH DATA"
        
        query <- "SELECT * FROM fis_med_claims_dev.KMeansOutput"
        
        query <- stringi::stri_replace(query, replacement = table_name, regex = 'KMeansOutput')
          
        DBI::dbExecute(vals$db.con,query)
          
        print('KMeansOutput')
      }
    )
      
    claims_clusters <- tbl(vals$db.con,in_schema("fis_med_claims_dev", table_name)) %>%
      select(case_id_z,clusterid)
    
    print(claims_clusters)
    print('claims clusters')
      
    cluster.centroids <- tbl(vals$db.con,in_schema("fis_med_claims_dev","Cluster_Centroids"))
    print(cluster.centroids)
    print('cluster.centroids')
      
    smallest_cluster <- cluster.centroids %>%
      mutate(min_size = min(size)) %>%
      filter(size == min_size) %>%
      as_tibble()
    print(smallest_cluster)  
    print('smallest_cluster')
      
    transition_outliers <- claims_clusters %>%
      filter(clusterid == local(smallest_cluster$clusterid[[1]])) %>%
      inner_join(data,
                 by = c("case_id_z"="Claim.Administrator.Claim.Number"))
    print(transition_outliers)  
    print('transition_outliers')
      
    # copy_to(vals$db.con, transition_outliers,
    #         in_schema("fis_med_claims_dev", "transition_outliers_perm"), temporary = FALSE, overwrite = TRUE)
    
    
    vals$pathways_outlier_data_cube <- transition_outliers %>%
      mutate(anomaly_type = 'claim_pathways',
             Claim.Administrator.Claim.Number = case_id_z)
    print(vals$pathways_outlier_data_cube)
    print('vals $ payhthways outlier data cube')

    

    # npath_merge <- vals$pathways_outlier_data_cube %>% 
    #   select(case_id_z) %>%
    #   mutate(dummy = 1) %>%
    #   distinct(case_id_z)
    # print(npath_merge)
    # print('npath merge line 240')
    
        score_payment <- transition_outliers %>%
      select(Total.Amount.Paid.Per.Line) %>%
      summarise(total = sum(Total.Amount.Paid.Per.Line)) %>%
      as_tibble()
      
    vals$pathways_payments <- local(score_payment$total)
      
    print('score_payment')
  })
  
  output$suspicious_transitions <- renderValueBox({
    print(input$data_source)
    if (input$data_source %in% c("Vantage", "COVID")){
      load_pathways_summaries()
      
      print(vals$pathways_outlier_data_cube)
      
      transition_outliers <- vals$pathways_outlier_data_cube
      
      print(transition_outliers)
      print('transition outliers line 260 pathways viz')
      
      
      vals$transition_outliers <- transition_outliers %>%
        select(case_id_z) %>%
        mutate(dummy = 1) %>%
        distinct(case_id_z, dummy)
    print(vals$transition_outliers)
    
    print('npath module pathways viz line 268 transition outlier')
      
      shinydashboard::valueBox(paste('$',format(vals$pathways_payments, big.mark = ","),
                                     sep = ""),
                               subtitle = tags$p("Suspicious Payments for Anomalous Transitions",
                                                 style = "font-size: 150%;"),
                               color = "green",
                               width = NULL)
    }
  })
  
  top_providers_df <- reactive({
    diagnosis_index <- input$top_diagnoses_rows_selected[1]
    diagnosis <- vals$diagnoses_1_df[diagnosis_index, 1]

    all_prov <- vals$claims_data %>%
      select(Billing.Provider.FEIN, First.ICD.9.CM.Diagnosis.Code,
             Total.Amount.Paid.Per.Line, Claim.Administrator.Claim.Number,
             Service.Line.From.Date) %>%
      group_by(Billing.Provider.FEIN,Claim.Administrator.Claim.Number) %>%
      mutate(row_n = row_number(Service.Line.From.Date),
             total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      filter(row_n == 1) %>%
      ungroup() %>%
      filter(First.ICD.9.CM.Diagnosis.Code == diagnosis) %>%
      summarize(Total.Paid = sum(total_paid),
                Claim.Count = n()) %>%
      mutate(Provider = 'ALL')

    providers <- vals$claims_data %>%
      select(First.ICD.9.CM.Diagnosis.Code, Billing.Provider.FEIN,
             Total.Amount.Paid.Per.Line, Claim.Administrator.Claim.Number,
             Service.Line.From.Date) %>%
      group_by(Billing.Provider.FEIN,Claim.Administrator.Claim.Number) %>%
      mutate(row_n = row_number(Service.Line.From.Date),
             total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      filter(row_n == 1) %>%
      ungroup() %>%
      filter(First.ICD.9.CM.Diagnosis.Code == diagnosis) %>%
      group_by(Billing.Provider.FEIN) %>%
      summarize(Total.Paid = sum(total_paid),
                Claim.Count = n()) %>%
      filter(Claim.Count >= 5) %>%
      rename(Provider = Billing.Provider.FEIN)

    providers$Provider <- as.character(providers$Provider)

    union_all(providers, all_prov) %>%
      arrange(desc(Total.Paid)) %>%
      as.data.frame()
  })

  output$top_providers <- renderDataTable({
    pathviz_vals$top_providers_df <- top_providers_df()
    datatable(pathviz_vals$top_providers_df, options = list(scrollX = TRUE,
                                                            lengthMenu = c(5, 10, 20), dom = 'ftp'),
              selection = list(target = "row", mode = "single", selected = c("1"))) %>%
      formatCurrency("Total.Paid", "$")
  })
  
    bupaR_event_log_object <<- reactive({
      data <- vals$claims_data
      provider_index <- input$top_providers_rows_selected[1]
      sankey_billing_provider_FEIN <- pathviz_vals$top_providers_df[provider_index, 1]
      
      # sankey_billing_provider_FEIN <- local(input$sankey_billing_provider_FEIN)
      
      if (!(sankey_billing_provider_FEIN == "ALL")){
        if (sankey_billing_provider_FEIN == "Unlisted Rehabilitation Providers"){
          sankey_billing_provider_FEIN <<- unlisted_rehab_FEINs_vector()
        }
        data <- data %>% dplyr::filter(Billing.Provider.FEIN %in% sankey_billing_provider_FEIN)
      }
      
      data_for_claim_numbers <- data
      
      # print(head(data_for_claim_numbers))
      print('testing 1910 data for claim numbers')
      
      diagnosis_index <- input$top_diagnoses_rows_selected[1]
      diagnosis_in_trace <- vals$diagnoses_1_df[diagnosis_index, 1]
      # diagnosis_in_trace <- local(input$initial_diagnosis)
      digit_depth <- local(input$pathway_digit_depth)
      
      sel_diagnosis <- diagnosis_in_trace
      
      # print(sel_diagnosis)
      print("printing sel diag")
      
      # print(head(vals$claims_data))
      print('printing vals$claims_data')
      
      # print(diagnosis_in_trace)
      print('printing diag in trace')

      ### DIAGNOSIS IN TRACE IS  NULL ### 
            
      if (digit_depth == "DRG"){
        target_col <- "DRG"
        diagnosis_drg <- vals$claims_data %>%
          select(First.ICD.9.CM.Diagnosis.Code, DRG) %>%
          filter(First.ICD.9.CM.Diagnosis.Code == diagnosis_in_trace) %>%
          # distinct(First.ICD.9.CM.Diagnosis.Code,DRG) %>%
          as_tibble()
        sel_diagnosis <- local(diagnosis_drg$DRG)
      }
      else{
        target_col <- "First.ICD.9.CM.Diagnosis.Code"
      }
    
      # print(sel_diagnosis)
      print('printing new sel diagnosis from diagnosis_drg line 341')
  
      
      if (!is.na(as.numeric(digit_depth))){
        digit_depth <- as.numeric(local(input$pathway_digit_depth))
        diagnosis_in_trace <- substr(diagnosis_in_trace, 1, digit_depth)
        data_for_claim_numbers <- data_for_claim_numbers %>%
          dplyr::mutate(First.ICD.9.CM.Diagnosis.Code = substr(First.ICD.9.CM.Diagnosis.Code, 1, digit_depth))
      }
      
      # print(data_for_claim_numbers)
      print('printing data_for_claim_numbers')
      
 ############### CLAIM_NUMBERS (BELOW) RETURNS NULL ##################
      
      claim_numbers <- data_for_claim_numbers %>%
        dplyr::filter(!!sym(target_col) == sel_diagnosis) %>%
        dplyr::select("Claim.Administrator.Claim.Number")
      # revisit this 
      
      # print(claim_numbers)
      print("printing claim numbers")
      
      ### CODE RUNS FINE TILL HERE ### 
      
      claim_numbers_vector <- as.data.frame(claim_numbers)$Claim.Administrator.Claim.Number
      
      # print(claim_numbers_vector)
      print("printing claim numbers vector")
      ## This print statement runs, claim_numbers_vector returns NULL ###########
      
      data_for_event_log <- data %>% 
        dplyr::filter(Claim.Administrator.Claim.Number %in% claim_numbers_vector)
      
      # print(head(data))
      print('printing data line 387')
      
      data_for_event_log <- data_for_event_log %>%
        dplyr::select("Claim.Administrator.Claim.Number", {{target_col}},
                       "Service.Line.From.Date", "Billing.Provider.FEIN") %>% 
              dplyr::group_by(Claim.Administrator.Claim.Number) %>%
        dplyr::mutate(first_service_date = min(Service.Line.From.Date)) %>%
        dplyr::mutate(week = trunc((as_date(Service.Line.From.Date) - as_date(first_service_date)) / 7))

      # print(head(data_for_event_log))
      print('printing data for event log line 395')
      
            data_for_event_log <- data_for_event_log %>%
        # dplyr::distinct(Claim.Administrator.Claim.Number, {{target_col}}, week) %>%
        dplyr::group_by(Claim.Administrator.Claim.Number, week) %>%
        dplyr::mutate(diagnosis_count = n()) %>%
        dplyr::ungroup() #%>%
        # dplyr::rename(Service.Line.From.Date = week)

      # print(head(data_for_event_log))
      print('testing 1910 line 408.....')
      
      # Printed till here ## CHECK BELOW FOR CODE BREAKING ## 
      
      if (digit_depth == "DRG")
        {
          data_for_event_log <- data_for_event_log %>%
            dplyr::mutate(DRG = ifelse(diagnosis_count > 1, "Many", as.character(DRG))) #%>%
            # dplyr::distinct(Claim.Administrator.Claim.Number, {{target_col}}, Service.Line.From.Date)
          
          print(head(data_for_event_log))
          print('printing first block of if condition line 412')
        }
      else
        {
          data_for_event_log <- data_for_event_log %>%
            dplyr::mutate(First.ICD.9.CM.Diagnosis.Code = ifelse(diagnosis_count > 1, "Many", 
                                         as.character(First.ICD.9.CM.Diagnosis.Code))) #%>%
            # group_by(Claim.Administrator.Claim.Number, First.ICD.9.CM.Diagnosis.Code,
            #          Service.Line.From.Date) %>%
            # summarize(dummy = 1) #%>% 
         # grouped by here instead of in process_mining.R    
            # dplyr::distinct(Claim.Administrator.Claim.Number, 
            #                 !!sym(target_col), Service.Line.From.Date, .keep_all = TRUE) %>% show_query()
          print(head(data_for_event_log))
          print('printing else block of if cond line 434')
        }

      copy_to(vals$db.con, data_for_event_log,
              in_schema("fis_med_claims_dev", "data_event_log_perm"), temporary = FALSE, overwrite = TRUE)
      
            # WORKS FINE TILL HERE, BELOW FUNC RETURNS ERROR 
      
      event_log_object <- get_event_log_object(data_for_event_log, digit_depth) 
        # dplyr::arrange(case_id)
      
      print(head(event_log_object))
      print('printing event log object line 428')
      
      if (!is.na(as.numeric(digit_depth)))({
        event_log_object <- event_log_object %>%
          mutate_at(.vars = "activity_id", .funs = gsub, pattern = "Man", replacement = "Many")
      })
      
      if (!is.null(input$endpoint_type)){
        if (input$endpoint_type == "Start"){
          event_log_object <- event_log_object %>% filter_rules(starts(sel_diagnosis))
        }
        else{
          event_log_object <- event_log_object %>% filter_rules(ends(sel_diagnosis))
        }
        
      }
      
      event_log_object
    })

    output$all_pathways_visual <- renderSankeywheel({
      if (!is.null(vals$claims_data)){
        traces_for_analysis <- get_traces_outliers_labeled()
        sankey_input_df <- get_sankey_input_df(traces_for_analysis, input$week_count)
        paid <- traces_dollars(traces_for_analysis)
        claim_count <- traces_claim_count(traces_for_analysis)
        avg_paid <- paid / claim_count

        bill_count <- traces_bill_count(traces_for_analysis)
        print(paste('All Bills',bill_count))
        avg_paid_per_bill <- paid / bill_count

        sankey_title = paste("All Pathways (", dollar(paid), " and ", format(claim_count, big.mark = ","),
                             " claims --> ", dollar(avg_paid), " average paid; ", dollar(avg_paid_per_bill),
                             " average bill)", paste = "")
        sankeywheel(from = sankey_input_df$from, to = sankey_input_df$to, weight = sankey_input_df$weight,
                    type = "sankey", theme = "sandsignika", seriesName = "All Pathways", title = sankey_title)
      }
    })
    
    output$inlier_pathways_visual <- renderSankeywheel({
      if (!is.null(vals$claims_data)){
        traces_for_analysis <- get_traces_outliers_labeled() %>% filter(classification == "Inlier")
        sankey_input_df <- get_sankey_input_df(traces_for_analysis, input$week_count)
        paid <- traces_dollars(traces_for_analysis)
        claim_count <- traces_claim_count(traces_for_analysis)
        avg_paid <- paid / claim_count
        
        bill_count <- traces_bill_count(traces_for_analysis)
        print(paste('Inlier Bills',bill_count))
        avg_paid_per_bill <- paid / bill_count
        
        sankey_title = paste("Inlier Pathways (", dollar(paid), " and ", format(claim_count, big.mark = ","),
                             " claims --> ", dollar(avg_paid), " average paid; ", dollar(avg_paid_per_bill),
                             " average bill)", paste = "")
        sankeywheel(from = sankey_input_df$from, to = sankey_input_df$to, weight = sankey_input_df$weight,
                    type = "sankey", theme = "sandsignika", seriesName = "Inlier Pathways", title = sankey_title)
      }
    })
    
    output$outlier_pathways_visual <- renderSankeywheel({
      if (!is.null(vals$claims_data)){
        traces_for_analysis <- get_traces_outliers_labeled() %>% filter(classification == "Outlier")
        sankey_input_df <- get_sankey_input_df(traces_for_analysis, input$week_count)
        sankey_title = paste("Outlier Pathways (", dollar(traces_dollars(traces_for_analysis)), " and ",
                             format(traces_claim_count(traces_for_analysis), big.mark = ","), " claims)", paste = "")
        paid <- traces_dollars(traces_for_analysis)
        claim_count <- traces_claim_count(traces_for_analysis)
        avg_paid <- paid / claim_count
        
        bill_count <- traces_bill_count(traces_for_analysis)
        print(paste('Outlier Bills',bill_count))
        avg_paid_per_bill <- paid / bill_count
        
        sankey_title = paste("Outlier Pathways (", dollar(paid), " and ", format(claim_count, big.mark = ","),
                             " claims --> ", dollar(avg_paid), " average paid; ", dollar(avg_paid_per_bill),"
                           average bill)", paste = "")
        sankeywheel(from = sankey_input_df$from, to = sankey_input_df$to, weight = sankey_input_df$weight,
                    type = "sankey", theme = "sandsignika", seriesName = "Outlier Pathways", title = sankey_title)
      }
    })
    
    get_data_for_pathing_tbl <<- reactive({
      if (!is.null(vals$claims_data)){
        data <- vals$claims_data
        
        provider_index <- input$top_providers_rows_selected[1]
        sankey_billing_provider_FEIN <- pathviz_vals$top_providers_df[provider_index, 1]
        digit_depth <- local(input$pathway_digit_depth)
        
        if (digit_depth == "DRG"){
          target_col <- "DRG"
          # target_col_at <- c("Claim.Administrator.Claim.Number", "DRG", "week")
        }
        else{
          target_col <- "First.ICD.9.CM.Diagnosis.Code"
          # target_col_at <- c("Claim.Administrator.Claim.Number", "First.ICD.9.CM.Diagnosis.Code", "week")
        }
        
        # sankey_billing_provider_FEIN <- local(input$sankey_billing_provider_FEIN)
        if (!(sankey_billing_provider_FEIN == "ALL")){
          if (sankey_billing_provider_FEIN == "Unlisted Rehabilitation Providers"){
            sankey_billing_provider_FEIN <<- unlisted_rehab_FEINs_vector()
          }
          data <- data %>% dplyr::filter(Billing.Provider.FEIN %in% sankey_billing_provider_FEIN)
        }
        
        if (!is.na(as.numeric(digit_depth))){
          data <- data %>%
            dplyr::mutate(First.ICD.9.CM.Diagnosis.Code = substr(First.ICD.9.CM.Diagnosis.Code,
                                                                 1, as.numeric(digit_depth)))
        }
        
        # print(head(data))
        # 
        data_for_pathing <- data %>%
          dplyr::select(Claim.Administrator.Claim.Number, {{target_col}}, Service.Line.From.Date) %>%
          dplyr::group_by(Claim.Administrator.Claim.Number) %>%
          dplyr::mutate(first_service_date = min(Service.Line.From.Date)) %>%
          dplyr::mutate(Service.Line.From.Date = as.integer(Service.Line.From.Date)) %>%
          dplyr::mutate(first_service_date = as.integer(first_service_date)) %>%
          dplyr::mutate(week = trunc((as_date(Service.Line.From.Date) - as_date(first_service_date)) / 7)) %>% 
          show_query()
        
        # print(head(data_for_pathing))
        # 
        data_for_pathing <- data_for_pathing %>%
          dplyr::group_by(Claim.Administrator.Claim.Number, !!sym(target_col), week) %>%
          # pipe_message("group_by_completed") %>% 
          dplyr::summarize(dummy = 1) %>% 
          dplyr::select(-dummy) %>%
          dplyr::group_by(Claim.Administrator.Claim.Number, week) %>%
          dplyr::mutate(diagnosis_count = n()) %>%
          dplyr::ungroup() %>%
          dplyr::rename(Service.Line.From.Date = week) %>% show_query()
        
        if (digit_depth == "DRG")
        {
          data_for_pathing <- data_for_pathing %>%
            dplyr::mutate(DRG = ifelse(diagnosis_count > 1, "Many", as.character(DRG)))
        }
        else
        {
          data_for_pathing <- data_for_pathing %>%
            dplyr::mutate(First.ICD.9.CM.Diagnosis.Code = ifelse(diagnosis_count > 1, "Many",
                                                   as.character(First.ICD.9.CM.Diagnosis.Code)))
        }
        
        # 
        # data_for_pathing <- data_for_pathing %>%
        #   dplyr::mutate(First.ICD.9.CM.Diagnosis.Code = ifelse(diagnosis_count > 1, "Many",
        #                                                        First.ICD.9.CM.Diagnosis.Code))
        
        copy_to(vals$db.con, data_for_pathing,
                in_schema("fis_med_claims_dev", "data_for_pathing_pathways_vis_perm"),
                temporary = FALSE, overwrite = TRUE)
        
        data_for_pathing <- data_for_pathing %>%
          dplyr::mutate(Diagnosis = !!sym(target_col), Claim_Number = Claim.Administrator.Claim.Number,
                        Date = Service.Line.From.Date)
        
        copy_to(vals$db.con, data_for_pathing, "data_for_pathing_tmp_table", temporary = TRUE, overwrite = TRUE)

        copy_to(vals$db.con, data_for_pathing,
                in_schema("fis_med_claims_dev", "data_for_pathing_tbl_perm"),
                temporary = FALSE, overwrite = TRUE)
        
        print("data printed to database data_for_pathing_tbl_perm")
        
        data_for_pathing_tbl <- tbl(vals$db.con, "data_for_pathing_tmp_table")
      }
    })
    
    output$pathing_source <- renderUI({
      if (input$data_source %in% c("Vantage", "COVID")){
        selectInput("pathing_source", "Pathing Source", c("bupaR (R in-memory)" = "bupaR",
                                                          "npath (Teradata)" = "nPath"), selected = "nPath")
      }
      else if (input$data_source == "Local CSV"){
        selectInput("pathing_source", "Pathing Source", c("bupaR (R in-memory)" = "bupaR"), selected = "nPath")
      }
      else{
        selectInput("pathing_source", "Pathing Source", c("bupaR (R in-memory)" = "bupaR",
                                                          "npath (Teradata)" = "nPath"), selected = "nPath")
      }
    })
    
    
    
  }