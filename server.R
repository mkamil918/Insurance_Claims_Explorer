source("formatting.R")
source("process_mining.R")
source("rename_columns.R")

shinyServer(function(input, output, session) {
  
  # reactiveValues object for storing global values
  vals <- reactiveValues(claims_data = NULL, db.con = NULL, top_diagnoses = NULL,
                         procedure_outlier_analysis = NULL, cost_outlier_FEINs = NULL,
                         top_insurers = NULL, modifiers = NULL, unlisted_rehabalitation_payments = NULL,
                         other_unlisted_payments = NULL, sev_total = NULL, freq_total = NULL,
                         freq_sev_total_sum = NULL, pathways_payments = NULL, diagnoses_1_df = NULL,
                         transition_outliers = NULL, procedure_outlier_data_cube = NULL,
                         cost_outlier_data_cube = NULL, pathways_outlier_data_cube = NULL, 
                         top_providers = NULL, custom_data = NULL, exec_barcharts_tbl = NULL,
                         exec_summaries_tbl = NULL, exploration_result = NULL,
                         triangle_source_data = NULL)
  
  traces_dollars <<- function(traces_outliers_labeled){
    claim_numbers <- traces_outliers_labeled %>% select(case_id) %>% as_vector()
    data <- vals$claims_data
    data %>% filter(Claim.Administrator.Claim.Number %in% claim_numbers) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% as.data.frame() %>% as.numeric()
  }
  
  traces_claim_count <<- function(traces_outliers_labeled){
    claim_numbers <- traces_outliers_labeled %>% select(case_id) %>% as_vector() %>% length()
  }
  
  traces_bill_count <<- function(traces_outliers_labeled){
    claim_numbers <- traces_outliers_labeled %>% select(case_id) %>% as_vector()
    data <- vals$claims_data
    count <- data %>% filter(Claim.Administrator.Claim.Number %in% claim_numbers) %>%
      filter(Line.Number == 1) %>%
      summarise(len = n()) %>%
      as.data.frame()
    count$len
  }
  
  unlisted_rehab_FEINs_vector <<- reactive({
    c("511833936", "251247223", "762151001", "940842484", "984493084", "398205976", "505878903",
      "881292823", "682227284", "839400776", "856868284", "699802377", "206984284", "980164284",
      "386834484")
  })
  
  tukey_thresholds_1D <<- function(data, group_column, outlier_column = ax_1){
    thresholds <- data %>% 
      group_by({{group_column}}) %>% 
      summarise(q25 := quantile({{outlier_column}}, probs = c(0.25)),
                q75 := quantile({{outlier_column}}, probs = c(0.75))) %>%
      mutate(tf = 1.5 * (q75 - q25)) %>%
      mutate(LT = q25 - tf) %>%
      mutate(UT = q75 + tf) %>%
      select({{group_column}}, LT, UT)
  }
  
  tukey_thresholds_1D_multiple <<- function(data, group_columns, outlier_column = ax_1){
    thresholds <- data %>% 
      group_by_at(group_columns) %>% 
      summarise(q25 := quantile({{outlier_column}}, probs = c(0.25)),
                q75 := quantile({{outlier_column}}, probs = c(0.75))) %>%
      mutate(tf = 1.5 * (q75 - q25)) %>%
      mutate(LT = q25 - tf) %>%
      mutate(UT = q75 + tf) %>%
      select(group_columns, LT, UT)
  }
  
  tukey_filter_1D <<- function(data, outlier_column = ax_1){
    thresholds <- data %>% 
      summarise(q25 := quantile({{outlier_column}}, probs = c(0.25)),
                q75 := quantile({{outlier_column}}, probs = c(0.75))) %>%
      mutate(tf = 1.5 * (q75 - q25)) %>%
      mutate(LT = q25 - tf) %>%
      mutate(UT = q75 + tf) %>%
      select(LT, UT) %>% as_tibble()
    
    data %>%
      mutate(Status := ifelse({{outlier_column}} <= local(thresholds$UT),
                             "Inlier", "Outlier"))
  }
  
  tukey_filter_2D <<- function(data, outlier_column_1 = ax_1, outlier_column_2 = ax_2,
                               operation = 'OR'){
    thresholds <- data %>% 
      summarise(q25_1 := quantile({{outlier_column_1}}, probs = c(0.25)),
                q75_1 := quantile({{outlier_column_1}}, probs = c(0.75)),
                q25_2 := quantile({{outlier_column_2}}, probs = c(0.25)),
                q75_2 := quantile({{outlier_column_2}}, probs = c(0.75))) %>%
      mutate(tf_1 = 1.5 * (q75_1 - q25_1),
             tf_2 = 1.5 * (q75_2 - q25_2)) %>%
      mutate(LT_1 = q25_1 - tf_1, LT_2 = q25_2 - tf_2) %>%
      mutate(UT_1 = q75_1 + tf_1, UT_2 = q75_2 + tf_2) %>%
      select(LT_1,UT_1,LT_2, UT_2) %>% as_tibble()
    
    
    comb <- data %>%
      mutate(STATUS_1 := ifelse({{outlier_column_1}} <= local(thresholds$UT_1),
                               "Inlier", "Outlier"),
             STATUS_2 := ifelse({{outlier_column_2}} <= local(thresholds$UT_2),
                               "Inlier", "Outlier"),
             UT_1 = local(thresholds$UT_1), UT_2 = local(thresholds$UT_2))
    
    if (operation == 'OR'){
      comb %>%
        mutate(Status = ifelse(STATUS_1 == "Outlier" | STATUS_2 == "Outlier", "Outlier", "Inlier"))
    }
    else if (operation == 'AND'){
      comb %>%
        mutate(Status = ifelse(STATUS_1 == "Outlier" & STATUS_2 == "Outlier", "Outlier", "Inlier"))
    }
    else{
      comb
    }
  }
  
  diagnoses_1_df <<- reactive({
    vals$claims_data %>%
      select(First.ICD.9.CM.Diagnosis.Code, Total.Amount.Paid.Per.Line,
             Claim.Administrator.Claim.Number) %>%
      group_by(First.ICD.9.CM.Diagnosis.Code) %>% 
      rename(Diagnosis.Code = First.ICD.9.CM.Diagnosis.Code) %>%
      summarize(Total.Paid = sum(Total.Amount.Paid.Per.Line),
                Claim.Count = n_distinct(Claim.Administrator.Claim.Number)) %>%
      arrange(desc(Total.Paid)) %>%
      as.data.frame()
  })
  
  get_npath_result <<- reactive({
    digit_depth <- local(input$pathway_digit_depth)
    data_for_pathing_tbl <<- get_data_for_pathing_tbl()
    
    print(head(data_for_pathing_tbl))
    print('data for pathing tbl server line 133')
    
    # copy_to(vals$db.con, data_for_pathing_tbl,
    #         in_schema("fis_med_claims_dev", "data_for_pathing_tbl_perm_default"),
    #         temporary = FALSE, overwrite = TRUE)
    # 
    ## saving data_for_pathing_tbl as a perm table in the database caused the npath to work
    ## even though the table is not being called anywhere else in the code
    print('data copied to db data for pathing tbl perm default')
    
    diagnosis_index <- input$top_diagnoses_rows_selected[1]
    diagnosis <- vals$diagnoses_1_df[diagnosis_index, 1]
    
    print(diagnosis_index)
    print(diagnosis)
    print('diagnosis and diag index server line 141')
    
    print(digit_depth)
    print('digit depth')
    # diagnosis <- local(input$initial_diagnosis)
    if (!is.na(as.numeric(digit_depth))){
      diagnosis <- substr(diagnosis, 1, as.numeric(digit_depth))
    }
    
    sel_diagnosis <- diagnosis
    
    print(sel_diagnosis)
    print('sel diagnosis server line 153')
    
    if (digit_depth == "DRG"){
      diagnosis_drg <- vals$claims_data %>%
        select(First.ICD.9.CM.Diagnosis.Code,DRG) %>%
        filter(First.ICD.9.CM.Diagnosis.Code == Diagnosis) %>%
        # distinct(First.ICD.9.CM.Diagnosis.Code,DRG) %>%
        as_tibble()
      sel_diagnosis <- local(diagnosis_drg$DRG)
      print(head(data_for_pathing_tbl))
      print('printing data for pathing tbl')
      

    }
    
    print(input$transition_pattern)
    print('input $ trans pattern')
    
    if (input$transition_pattern == "All"){
      symbols_string <- paste("\"Diagnosis\" like '", sel_diagnosis, "' as O", sep = "")
      dynamic_pattern <- "^O.B*"
      npath_result <- td_npath_sqle (
        data1 = data_for_pathing_tbl,
        mode = "NONOVERLAPPING",
        pattern = dynamic_pattern,
        symbols =  c(symbols_string, "true as B"),
        result = c("ACCUMULATE (CDISTINCT \"Claim_Number\" OF ANY (O,B) DELIMITER '$$') AS claim_numbers",
                   "COUNT (* of any(O,B)) AS week_count", "ACCUMULATE (\"Diagnosis\" of any(O,B) ) AS path"),
        data1.partition.column = "Claim_Number",
        data1.order.column = "Date"
      )
      ## Run this as a query 
      # Need the pattern for both O and B 
      # Something is missing for "B" 
      # run as query in vantage using documentation 
      npath_result
      # print(head(npath_result))
      # print('printing npath result when input $ trans patterns == all')
    }
    else{
      symbols_string <- paste("\"Diagnosis\" like '", sel_diagnosis, "' as O, ",
                              "\"Diagnosis\" like 'M51.26' as A",sep = "")
      dynamic_pattern <- "^O.A.B"
      npath_result <- td_npath_sqle (
        data1 = data_for_pathing_tbl,
        mode = "NONOVERLAPPING",
        pattern = dynamic_pattern,
        symbols =  c("true as B", symbols_string),
        result = c("ACCUMULATE (CDISTINCT \"Claim_Number\" OF ANY (O,A,B) DELIMITER '$$') AS claim_numbers",
                   "COUNT (* of any(O,A,B)) AS week_count", "ACCUMULATE (\"Diagnosis\" of any(O,A,B) ) AS path"),
        data1.partition.column = "Claim_Number",
        data1.order.column = "Date"
      )
      npath_result
      print('npath result else condition line 209')
    }
  })
  
  get_traces_outliers_labeled_npath <<- reactive({
    con <- vals$db.con
    diagnosis_index <- input$top_diagnoses_rows_selected[1]
    diagnosis <- vals$diagnoses_1_df[diagnosis_index, 1]
    
    print(diagnosis)
    print('diagnosis server line 204')

    # Comment below lines 2nd november
        
    npath_result <- get_npath_result()
    print(npath_result)
    print('printing npath result server line 200')
    #
    npath_result <- npath_result$result
    print(npath_result)
    print('printing npath_result$result')
    
    print(vals$transition_outliers)
    print('vals $ trans outliers')
    
    labelled_paths <- npath_result %>%
      rename(trace = path, case_id = claim_numbers) %>%
      mutate(case_id = gsub('[\\[\\]]','',case_id)) 
    print(labelled_paths)
    print('labelled paths line 240')
    
  my_query <- "SELECT DISTINCT case_id_z, 1.0 AS dummy FROM fis_med_claims_dev.transition_outliers_perm"
  merge_transition_outliers <- tbl(con, sql(my_query))
  
  print(merge_transition_outliers)
  print('merge transition outliers')
    
    # ERROR IS HERE BECAUSE VALS$transition_outliers is NULL 
    labelled_paths <- labelled_paths %>%
      left_join(merge_transition_outliers,
                 by = c("case_id"="case_id_z")) %>%
      mutate(classification = ifelse(is.na(dummy),"Inlier", "Outlier"))
    
    npath_result_df <- labelled_paths %>%
      as.data.frame()
    
    npath_result_df <- npath_result_df %>%
      mutate_all(.funs = gsub, pattern = "[", replacement = "", fixed = TRUE) %>%
      mutate_all(.funs = gsub, pattern = "]", replacement = "", fixed = TRUE)
    
    npath_result_df <- cSplit(npath_result_df, 'trace', sep = ",", type.convert = FALSE)
    
    if (input$complete_paths_only){
      npath_result_df <- npath_result_df %>% 
        tidyr::drop_na(sankey_trace_columns(colnames(npath_result_df), input$week_count))
    }
    else {
      npath_result_df <- npath_result_df %>% replace(is.na(.), "NO_ACTIVITY")
    }
    npath_result_df
    
  })
  
  get_traces_outliers_labeled <<- reactive({
    if (local(input$pathing_source) == "bupaR"){
      traces_to_analyze <- get_traces_to_analyze()
      weeks_for_segmentation <- 2    
      
      # traces_to_analyze %>% View()
      
      traces.seq <- seqdef(traces_to_analyze %>% dplyr::select(contains("trace")),
                           var = 1:weeks_for_segmentation)
      
      # traces.seq %>% View()
      
      ccost <- seqsubm(traces.seq, method = "CONSTANT", cval = 2)
      traces.distances <- seqdist(traces.seq, method = "CHI2")
      
      # traces.distances %>% View()
      
      #traces.distances <- seqdist(traces.seq, method = "OM", norm = TRUE, sm = ccost, indel = 1)
      outlier_labels <- DB(traces.distances, d = 1, fraction = .25)
      cbind(outlier_labels %>% as.data.frame(), traces_to_analyze)
      
      ####code for calculating distances using the nomclust package    
      #hca.object <- nomclust(traces_to_analyze, measure = "iof", method = "average", clu.high = 4)
      #hca.object$prox
      #DB(hca.object$prox, d = 1, fraction = .25)
    }
    else{
      outlier_labels <- get_traces_outliers_labeled_npath()
    }
  })
  
  get_traces_to_analyze_bupaR <<- reactive({
    diagnosis_index <- input$top_diagnoses_rows_selected[1]
    diagnosis_in_trace <- vals$diagnoses_1_df[diagnosis_index, 1]
    # diagnosis_in_trace <- local(input$initial_diagnosis)
    if (!is.na(as.numeric(local(input$pathway_digit_depth)))){
      diagnosis_in_trace <- substr(diagnosis_in_trace, 1, as.numeric(local(input$pathway_digit_depth)))
    }
    
    event_log_object <<- bupaR_event_log_object()
    
    traces <- cSplit(event_log_object %>% edeaR::trace_coverage(level = "case"),
                     'trace', sep = ",", type.convert = FALSE)
    
    if (input$complete_paths_only){
      traces <- traces %>% tidyr::drop_na(sankey_trace_columns(colnames(traces), input$week_count))
    }
    else {
      traces <- traces %>% replace(is.na(.), "NO_ACTIVITY")
    }
    traces
  })
  
  get_traces_to_analyze_nPath <<- reactive({
    if (!is.null(vals$claims_data)){
      npath_result <- get_npath_result()
      npath_result_df <- npath_result$result %>%
        rename(trace = path, case_id = claim_numbers) %>%
        as.data.frame()
      npath_result_df <- npath_result_df %>%
        mutate_all(.funs = gsub, pattern = "[", replacement = "", fixed = TRUE) %>%
        mutate_all(.funs = gsub, pattern = "]", replacement = "", fixed = TRUE)
      npath_result_df <- cSplit(npath_result_df, 'trace', sep = ",", type.convert = FALSE)
      
      if (input$complete_paths_only){
        npath_result_df <- npath_result_df %>% 
          tidyr::drop_na(sankey_trace_columns(colnames(npath_result_df), input$week_count))
      }
      else {
        npath_result_df <- npath_result_df %>% replace(is.na(.), "NO_ACTIVITY")
      }
      npath_result_df
    }
  })
  
  get_traces_to_analyze <<- reactive({
    if (local(input$pathing_source) == "bupaR"){
      get_traces_to_analyze_bupaR()
    }
    else{
      get_traces_to_analyze_nPath()
    }
  })
  
  
  get_sankey_input_df <<- function(traces_to_analyze, week_count){
    traces_to_analyze <- traces_to_analyze %>% 
      dplyr::select(sankey_trace_columns(colnames(traces_to_analyze), week_count))
    traces_to_analyze_df <- head(traces_to_analyze, n = 1) %>%
      as.data.frame()
    column_count <- ncol(traces_to_analyze_df)
    sankey_input_df <- NULL
    
    for (i in 1:(column_count - 1)){
      from_column_name <- colnames(traces_to_analyze_df)[i]
      to_column_name <- colnames(traces_to_analyze_df)[i + 1]
      from_appendix <- paste("(", unlist(strsplit(from_column_name, "_"))[2], ")", sep = "")
      to_appendix <- paste("(", unlist(strsplit(to_column_name, "_"))[2], ")", sep = "")
      sankey_input_addition <- traces_to_analyze %>% dplyr::select(from_column_name, to_column_name) %>%
        dplyr::mutate({{from_column_name}} := paste0(from_appendix, !!as.name(from_column_name), sep = "")) %>%
        dplyr::mutate({{to_column_name}} := paste(to_appendix, !!as.name(to_column_name), sep = "")) %>%
        dplyr::rename(from = from_column_name, to = to_column_name)
      sankey_input_df <- rbind(sankey_input_df, sankey_input_addition %>% as.data.frame())
    }
    
    sankey_input_df <- sankey_input_df %>%
      dplyr::group_by(from, to) %>%
      dplyr::summarize(weight = n())
  }
  
  sankey_trace_columns <<- function(column_names, week_count){
    column_list <- NULL
    
    ##put in logic for the sprintf string to handle the variable number of 
    zero_digits_string <- '%02d'
    if ("trace_1" %in% column_names){
      zero_digits_string <- '%01d'
    }
    else if ("trace_01" %in% column_names){
      zero_digits_string <- '%02d'
    }
    else if ("trace_001" %in% column_names){
      zero_digits_string <- '%03d'
    }
    
    for (i in 1:week_count){
      column_list <- c(column_list, paste("trace_", sprintf(zero_digits_string, i), sep = ""))
    }

    column_list
  }
  
  
  tab_data_selection_server(input, output, session, vals)
  tab_suspicious_providers_server(input, output, session, vals)
  tab_cost_outliers_server(input, output, session, vals)
  tab_provider_anomalies_action_server(input, output, session, vals)
  tab_pattern_summary_server(input, output, session, vals)
  tab_pathways_download_server(input, output, session, vals)
  tab_pathways_med_codes_server(input, output, session, vals)
  tab_pathways_viz_server(input, output, session, vals)
  tab_physical_therapy_pathsways_server(input, output, session, vals)
  tab_transition_analyzer_server(input, output, session, vals)
  tab_rep_pathways_server(input, output, session, vals)
  tab_exploration_server(input, output, session, vals)
  tab_exploration_in_database_server(input, output, session, vals)
  tab_claim_trajectory_server(input, output, session, vals)
  tab_covid_overview_server(input, output, session, vals)
  tab_diagnosis_explorer_server(input, output, session, vals)
  tab_guided_exploration_server(input, output, session, vals)
  tab_triangles_server(input, output, session, vals)
  tab_trace_clustering_server(input, output, session, vals)
  
})