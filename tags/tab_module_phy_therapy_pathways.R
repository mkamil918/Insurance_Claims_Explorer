# Front page module

# UI function
tab_physical_therapy_pathsways_ui <- function(id) {
  
  # Main UI
  fluidPage(
    titlePanel("Physical Therapy Pathways"),
    selectInput("pt_pathway_digit_depth", "Diagnosis Digit Depth", c("1", "2", "3", "all"), selected = "2"),
    sankeywheelOutput('physical_therapy_pathways'),
    sankeywheelOutput('physical_therapy_pathways_high_cost'),
    sankeywheelOutput('physical_therapy_pathways_low_cost')
  )
  
}

# Server function
tab_physical_therapy_pathsways_server <- function(input, output, session, vals) {
  
  # Empty, since there is no interactivity on front page
  get_pt_traces_to_analyze <- function(min_trace_count_to_display, segment){
    if (input$data_source == "Local CSV" || input$data_source == "Upload CSV"){
      data <- read.csv(paste(getwd(), "/data/", "pt_claims.csv", sep = ""))
    }
    else{
      data <- vals$claims_data
    }
    
    head(data)
    
    digit_depth <- local(input$pt_pathway_digit_depth)
    
    claim_numbers_vector <- NULL
    if (segment == "all"){
      claim_numbers_vector <- c("0095UZG", "0253L1O", "0284PNG", "0297P0N", "0354P0N", "0473OUG","0485XUG", "0497OUG", "0503XUG", "0579WZG", "0659POG", "0702XUG", "0722SSG", "0833P0N", "0898UZG", "1063POG", "1081XUG", "1102XUG", "1109PNG", "1186OUG", "1203OUG", "1208OUG", "1680XUG", "1683POG", "1686OUG", "1783XUG", "1816XZG", "2230XUG", "2246XZG", "2299XZG", "2345UZG", "2395XOG", "2487UZG", "2651PNG", "2656XUG", "2779BGN", "2801POG", "2843BGN", "3023UZG", "3073XUG", "3365XZG", "3421XUG", "3454A5N", "3457XUG", "3479OUG", "3608UZG", "3616XZG", "3630UZG", "3693UZG", "3730VSG", "3783POG", "3786JEG", "3790VSG", "4143BGN", "4143UZG", "4181SWN", "4189XZG", "4258UZG", "4263BGN", "4267P0N", "4295UZG", "4356PNG", "4439UZG", "4443BGN", "4451UZG", "5162O1O", "5167XUG", "5310PNG", "5733XUG", "5846UZG", "5966P0N", "6018XUG", "6032VSG", "6150UZG", "6153PNG", "6270XUG", "6370A5N", "6433UZG", "6437OUG", "6451XUG", "6527UZG", "6716REG", "6733UZG", "6740OUG", "6805PNG", "6817VSG", "6982POG", "7140PNG", "7173XUG", "7224XUG", "7299XZG", "7405VSG", "7458XZG", "7474OUG", "7549XUG", "7617VWN", "7829PNG", "8045WZG", "8259XZG", "8263BGN", "8266OUG", "8351UZG", "8384XUG", "8689P0N", "8808XZG", "8815PNG", "8826XZG", "8973UZG", "9034SSG", "9036SSG", "9133WZG", "9195UZG", "9246XUG", "9329XUG", "9335P7N", "9353OUG", "9451P0N", "9462OUG", "9507OUG", "9514XUG", "9770XUG", "9899UZG", "0019UZG", "0053XZG", "0218PNG", "0236UZG", "0265XUG", "0414UZG", "0477OUG", "0499XEG", "0516UZG", "0565XUG", "0632XUG", "0743BGN", "0758XZG", "0797JEG", "0845UZG", "0853XZG", "0884OUG", "0884XUG", "0931XZG", "1160UZG", "1196UZG", "1215OUG", "1219JEG", "1296UZG", "1317K6N", "1436UZG", "1440P7N", "1686XUG", "1731XZG", "1734OUG", "1746XUG", "1774MKG", "1799XUG", "1809JBO", "1827XUG", "1831VSG", "1889POG", "1965Z8N", "2006XUG", "2049PNG", "2091UZG", "2128XZG", "2167UZG", "2208XUG", "2353XZG", "2395UZG", "2430UZG", "2430XZG", "2542VSG", "2546UZG", "2583OUG", "2590OUG", "2622XZG", "2648UZG", "2781SSG", "2883XUG", "2886UZG", "2986OUG", "3018UZG", "3107PNG", "3126XZG", "3135XUG", "3263PNG", "3270UZG", "3330XUG", "3335UZG", "3343BGN", "3443BGN", "3503XZG", "3533XZG", "3567XUG", "3695UZG", "3768SSG", "3813XUG", "4034WZG", "4035XUG", "4078P0N", "4340UZG", "4392XUG", "4397UZG", "4399XUG", "4441XUG", "4473XZG", "4518NFO", "4593OUG", "4786UZG", "4949XUG", "5040OUG", "5127UZG", "5231POG", "5239UZG", "5301XZG", "5404JZK", "5417OUG", "5425OUG", "5435OUG", "5545PNG", "5547XZG", "5609POG", "5651UZG", "5694PNG", "5703POG", "5735WZG", "5742WZG", "5816UZG", "5825OUG", "5854LIO", "5964VSG", "6124XZG", "6209WZG", "6280VSG", "6396UZG", "6532P0N", "6587PNG", "6589Z8N", "6608OUG", "6622P7N", "6663BGN", "6712XZG", "6900P0N", "6979UZG", "7058P0N", "7137OUG", "7182UZG", "7267UZG", "7274XZG", "7291XZG", "7424XZG", "7440UZG", "7512WZG", "7514VSG", "7519POG", "7570XZG", "7608XUG", "7620XZG", "7667UZG", "7708XZG", "7762XUG", "7791UZG", "7869UZG", "7878XUG", "8046OUG", "8144XUG", "8194UZG", "8271OUG", "8486XUG", "8542XUG", "8580UZG", "8631POG", "8768VSG", "8911POG", "8959WZG", "9151UZG", "9221WZG", "9237OUG", "9302OUG", "9306XUG", "9306XZG", "9328XZG", "9337UZG", "9344SSG", "9348OUG", "9523BGN", "9631VSG", "9703XUG", "9849PNG", "9896WZG", "9913JEG", "9958XZG")
    }
    else if (segment == "high_cost"){
      claim_numbers_vector <- c("0019UZG", "0053XZG", "0218PNG", "0236UZG", "0265XUG", "0414UZG", "0477OUG", "0499XEG", "0516UZG", "0565XUG", "0632XUG", "0743BGN", "0758XZG", "0797JEG", "0845UZG", "0853XZG", "0884OUG", "0884XUG", "0931XZG", "1160UZG", "1196UZG", "1215OUG", "1219JEG", "1296UZG", "1317K6N", "1436UZG", "1440P7N", "1686XUG", "1731XZG", "1734OUG", "1746XUG", "1774MKG", "1799XUG", "1809JBO", "1827XUG", "1831VSG", "1889POG", "1965Z8N", "2006XUG", "2049PNG", "2091UZG", "2128XZG", "2167UZG", "2208XUG", "2353XZG", "2395UZG", "2430UZG", "2430XZG", "2542VSG", "2546UZG", "2583OUG", "2590OUG", "2622XZG", "2648UZG", "2781SSG", "2883XUG", "2886UZG", "2986OUG", "3018UZG", "3107PNG", "3126XZG", "3135XUG", "3263PNG", "3270UZG", "3330XUG", "3335UZG", "3343BGN", "3443BGN", "3503XZG", "3533XZG", "3567XUG", "3695UZG", "3768SSG", "3813XUG", "4034WZG", "4035XUG", "4078P0N", "4340UZG", "4392XUG", "4397UZG", "4399XUG", "4441XUG", "4473XZG", "4518NFO", "4593OUG", "4786UZG", "4949XUG", "5040OUG", "5127UZG", "5231POG", "5239UZG", "5301XZG", "5404JZK", "5417OUG", "5425OUG", "5435OUG", "5545PNG", "5547XZG", "5609POG", "5651UZG", "5694PNG", "5703POG", "5735WZG", "5742WZG", "5816UZG", "5825OUG", "5854LIO", "5964VSG", "6124XZG", "6209WZG", "6280VSG", "6396UZG", "6532P0N", "6587PNG", "6589Z8N", "6608OUG", "6622P7N", "6663BGN", "6712XZG", "6900P0N", "6979UZG", "7058P0N", "7137OUG", "7182UZG", "7267UZG", "7274XZG", "7291XZG", "7424XZG", "7440UZG", "7512WZG", "7514VSG", "7519POG", "7570XZG", "7608XUG", "7620XZG", "7667UZG", "7708XZG", "7762XUG", "7791UZG", "7869UZG", "7878XUG", "8046OUG", "8144XUG", "8194UZG", "8271OUG", "8486XUG", "8542XUG", "8580UZG", "8631POG", "8768VSG", "8911POG", "8959WZG", "9151UZG", "9221WZG", "9237OUG", "9302OUG", "9306XUG", "9306XZG", "9328XZG", "9337UZG", "9344SSG", "9348OUG", "9523BGN", "9631VSG", "9703XUG", "9849PNG", "9896WZG", "9913JEG", "9958XZG")
    }
    else if (segment == "low_cost"){
      claim_numbers_vector <- c("0095UZG", "0253L1O", "0284PNG", "0297P0N", "0354P0N", "0473OUG", "0485XUG", "0497OUG", "0503XUG", "0579WZG", "0659POG", "0702XUG", "0722SSG", "0833P0N", "0898UZG", "1063POG", "1081XUG", "1102XUG", "1109PNG", "1186OUG", "1203OUG", "1208OUG", "1680XUG", "1683POG", "1686OUG", "1783XUG", "1816XZG", "2230XUG", "2246XZG", "2299XZG", "2345UZG", "2395XOG", "2487UZG", "2651PNG", "2656XUG", "2779BGN", "2801POG", "2843BGN", "3023UZG", "3073XUG", "3365XZG", "3421XUG", "3454A5N", "3457XUG", "3479OUG", "3608UZG", "3616XZG", "3630UZG", "3693UZG", "3730VSG", "3783POG", "3786JEG", "3790VSG", "4143BGN", "4143UZG", "4181SWN", "4189XZG", "4258UZG", "4263BGN", "4267P0N", "4295UZG", "4356PNG", "4439UZG", "4443BGN", "4451UZG", "5162O1O", "5167XUG", "5310PNG", "5733XUG", "5846UZG", "5966P0N", "6018XUG", "6032VSG", "6150UZG", "6153PNG", "6270XUG", "6370A5N", "6433UZG", "6437OUG", "6451XUG", "6527UZG", "6716REG", "6733UZG", "6740OUG", "6805PNG", "6817VSG", "6982POG", "7140PNG", "7173XUG", "7224XUG", "7299XZG", "7405VSG", "7458XZG", "7474OUG", "7549XUG", "7617VWN", "7829PNG", "8045WZG", "8259XZG", "8263BGN", "8266OUG", "8351UZG", "8384XUG", "8689P0N", "8808XZG", "8815PNG", "8826XZG", "8973UZG", "9034SSG", "9036SSG", "9133WZG", "9195UZG", "9246XUG", "9329XUG", "9335P7N", "9353OUG", "9451P0N", "9462OUG", "9507OUG", "9514XUG", "9770XUG", "9899UZG")
    }
    
    data_for_event_log <- data %>% dplyr::filter(Claim.Administrator.Claim.Number %in% claim_numbers_vector)
    data_for_event_log <- data_for_event_log %>%
      dplyr::select("Claim.Administrator.Claim.Number", "First.ICD.9.CM.Diagnosis.Code",
                     "Service.Line.From.Date", "Billing.Provider.FEIN",
                     "HCPCS.Line.Procedure.Billed.Code") %>%
      dplyr::group_by(Claim.Administrator.Claim.Number) %>%
      dplyr::mutate(first_service_date = min(Service.Line.From.Date)) %>%
      dplyr::mutate(week = trunc((as_date(Service.Line.From.Date) - as_date(first_service_date)) / 7))
    
    #this assumes we are showing Sankey's by ICD-10 code
    data_for_event_log <- data_for_event_log %>% dplyr::group_by(Claim.Administrator.Claim.Number, First.ICD.9.CM.Diagnosis.Code, week) %>% dplyr::summarize(dummy = 1) %>% dplyr::select(-dummy) %>% dplyr::group_by(Claim.Administrator.Claim.Number, week) %>% dplyr::mutate(diagnosis_count = n()) %>% dplyr::ungroup() %>% dplyr::rename(Service.Line.From.Date = week)
    data_for_event_log <- data_for_event_log %>% dplyr::mutate(First.ICD.9.CM.Diagnosis.Code = ifelse(diagnosis_count > 1, "*", First.ICD.9.CM.Diagnosis.Code))
    event_log_object <- get_event_log_object(data_for_event_log, digit_depth) %>% dplyr::arrange(case_id)
    
    #this assumes we are showing Sankey's by procedure code
    #data_for_event_log <- data_for_event_log %>% dplyr::group_by(Claim.Administrator.Claim.Number, HCPCS.Line.Procedure.Billed.Code, week) %>% dplyr::summarize(dummy = 1) %>% dplyr::select(-dummy) %>% dplyr::group_by(Claim.Administrator.Claim.Number, week) %>% dplyr::mutate(procedure_count = n()) %>% dplyr::ungroup() %>% dplyr::rename(Service.Line.From.Date = week)
    ##data_for_event_log <- data_for_event_log %>% dplyr::mutate(HCPCS.Line.Procedure.Billed.Code = ifelse(procedure_count > 1, "Many", HCPCS.Line.Procedure.Billed.Code))
    #event_log_object <- get_event_log_object(data_for_event_log, "procedure") %>% dplyr::arrange(case_id)
    
    if ((digit_depth == "3"))({
      event_log_object <- event_log_object %>% mutate_at(.vars = "activity_id", .funs = gsub, pattern = "Man", replacement = "Many")
    })
    else if ((digit_depth == "2"))({
      event_log_object <- event_log_object %>% mutate_at(.vars = "activity_id", .funs = gsub, pattern = "Ma", replacement = "Many")
    })
    
    #traces <- event_log_object %>% filter_rules(starts(diagnosis_in_trace)) %>% edeaR::trace_coverage(level = "case")
    traces <- event_log_object %>% edeaR::trace_coverage(level = "case")
    traces <- cSplit(traces, 'trace', sep = ",", type.convert = FALSE) %>% replace(is.na(.), "NO_ACTIVITY")
    
    traces %>% dplyr::filter(absolute >= min_trace_count_to_display)
    #uncomment this if showing Sankey's by procedure code
    #traces_to_analyze <- traces %>% dplyr::rename(trace_01 = trace_001, trace_02 = trace_002, trace_03 = trace_003, trace_04 = trace_004, trace_05 = trace_005, trace_06 = trace_006, trace_07 = trace_007, trace_08 = trace_008, trace_09 = trace_009)
  }
  
  output$physical_therapy_pathways <- renderSankeywheel({
    traces_for_analysis <- get_pt_traces_to_analyze(2, "all")
    sankey_input_df <- get_sankey_input_df(traces_for_analysis, input$week_count)
    sankey_title = paste("All Pathways ($1,802 average paid)")
    sankeywheel(from = sankey_input_df$from, to = sankey_input_df$to, weight = sankey_input_df$weight,
                type = "sankey", theme = "sandsignika", seriesName = "All Pathways", title = sankey_title)
  })
  
  output$physical_therapy_pathways_high_cost <- renderSankeywheel({
    traces_for_analysis <- get_pt_traces_to_analyze(2, "high_cost")
    sankey_input_df <- get_sankey_input_df(traces_for_analysis, input$week_count)
    sankey_title = paste("High Cost Pathways ($2,054 average paid):  2+ PT Visits in First Week")
    sankeywheel(from = sankey_input_df$from, to = sankey_input_df$to, weight = sankey_input_df$weight,
                type = "sankey", theme = "sandsignika", seriesName = "High Cost Pathways", title = sankey_title)
  })
  
  output$physical_therapy_pathways_low_cost <- renderSankeywheel({
    traces_for_analysis <- get_pt_traces_to_analyze(1, "low_cost")
    sankey_input_df <- get_sankey_input_df(traces_for_analysis, input$week_count)
    sankey_title = paste("Low Cost Pathways ($1,452 average paid):  Max 1 PT Visits in First Week")
    sankeywheel(from = sankey_input_df$from, to = sankey_input_df$to, weight = sankey_input_df$weight,
                type = "sankey", theme = "sandsignika", seriesName = "Low Cost Pathways",
                title = sankey_title)
  })
  
  
}
