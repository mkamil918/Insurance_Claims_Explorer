get_event_log_object <- function(HEADER_join_DETAIL, digit_count){
  activity_column <- ""
  if (digit_count == "all"){
    activity_column <- "First.ICD.9.CM.Diagnosis.Code"
    activity_column_at <- c("Claim.Administrator.Claim.Number", "First.ICD.9.CM.Diagnosis.Code",
                            "Service.Line.From.Date")
  }
  else if (digit_count == "procedure"){
    activity_column <- "HCPCS.Line.Procedure.Billed.Code"
    activity_column_at <- c("Claim.Administrator.Claim.Number", "HCPCS.Line.Procedure.Billed.Code",
                            "Service.Line.From.Date")

  }
  else if (digit_count == "DRG"){
    activity_column <- "DRG"
    activity_column_at <- c("Claim.Administrator.Claim.Number", "DRG",
                            "Service.Line.From.Date")
  }
  else {
    HEADER_join_DETAIL <- HEADER_join_DETAIL %>%
      dplyr::mutate(ICD_10_substring = substr(First.ICD.9.CM.Diagnosis.Code, 1, as.numeric(digit_count)))
    activity_column <- "ICD_10_substring"
    activity_column_at <- c("Claim.Administrator.Claim.Number", "ICD_10_substring",
                            "Service.Line.From.Date")

  }

  # print("before before something I")
  # print(HEADER_join_DETAIL$`First.ICD.9.CM.Diagnosis.Code`)
  # print("before before something II")


  # Kamil commented below code, revert if not work

  data_grouped <- HEADER_join_DETAIL %>%
    dplyr::group_by(Claim.Administrator.Claim.Number, !!sym(activity_column),
                    Service.Line.From.Date) %>%
    dplyr::summarize(dummy = 1) %>%
    show_query()

  print(head(data_grouped))
  print('printing activity column at data grouped')
    # print("before something I")

  #
  # print("before something II")

  event_log_tbl <- data_grouped %>%
    dplyr::select("Claim.Administrator.Claim.Number", {{activity_column}}, "Service.Line.From.Date") %>%
    dplyr::mutate(lifecycle_id = "start", Billing.Provider.FEIN = "unknown")

  event_log_tbl <- tbl_df(event_log_tbl) %>%
    dplyr::arrange(Service.Line.From.Date) %>%
    dplyr::mutate(activity_instance_id = row_number())

  # print("before something I")
  # print(event_log_tbl)
  # print("before something II")


  #this line could be commented out because Service.Line.From.Date is technically not a date but an integer representing a week number
  event_log_tbl <- event_log_tbl %>%
    dplyr::mutate(Service.Line.From.Date = as.integer(Service.Line.From.Date)) %>%
    dplyr::mutate(Service.Date = as_date(Service.Line.From.Date)) %>%
    dplyr::rename(case_id = "Claim.Administrator.Claim.Number", activity_id = activity_column,
                  timestamp = "Service.Date", resource_id = "Billing.Provider.FEIN")

  # print("something 1")
  # print(event_log_tbl)
  # print("something II")

  event_log_object <- event_log_tbl %>%
    eventlog(case_id = "case_id", activity_id = "activity_id",
             activity_instance_id = "activity_instance_id", lifecycle_id = "lifecycle_id",
             timestamp = "timestamp", resource_id = "resource_id")

  # event_log_object %>% View()
  # print("something III")

  event_log_object
}





create_diagnosis_summary_rows <- function(HEADER_join_DETAIL, event_log_object, input, con, code_substring_to_model){
  summary_table <- NULL
  for (diagnosis in code_substring_to_model){
    new_row <- create_diagnosis_summary_row (HEADER_join_DETAIL, event_log_object, input, diagnosis, input$end_ICD_codes, con)
    new_row$start_diagnosis <- diagnosis
    summary_table <- rbind(summary_table, new_row)
  }
  summary_table
}

create_diagnosis_summary_row <- function(HEADER_join_DETAIL, event_log_object, input, diagnosis, end_ICD_codes, con){
  cumulative_table <- full_join(
    get_summary(HEADER_join_DETAIL, event_log_object, input, diagnosis, end_ICD_codes, TRUE, "avg_per_claim", con) %>% rename("avg_pd_per_claim_with_shift" = 1) %>% dplyr::mutate(join_column = "123"),
    get_summary(HEADER_join_DETAIL, event_log_object, input, diagnosis, end_ICD_codes, TRUE, "avg_per_bill", con) %>% rename("avg_pd_per_bill_with_shift" = 1) %>% dplyr::mutate(join_column = "123"),
    by = "join_column"
  )
  cumulative_table <- full_join(
    cumulative_table,
    get_summary(HEADER_join_DETAIL, event_log_object, input, diagnosis, end_ICD_codes, FALSE, "avg_per_claim", con) %>% rename("avg_pd_per_claim_without_shift" = 1) %>% dplyr::mutate(join_column = "123"),
    by = "join_column"
  )
  cumulative_table <- full_join(
    cumulative_table,
    get_summary(HEADER_join_DETAIL, event_log_object, input, diagnosis, end_ICD_codes, FALSE, "avg_per_bill", con) %>% rename("avg_pd_per_bill_without_shift" = 1) %>% dplyr::mutate(join_column = "123"),
    by = "join_column"
  ) %>% select(-(join_column))
}

get_summary <- function(HEADER_join_DETAIL, event_log_object, input, diagnosis, end_ICD_codes, Jump, SummaryType, con){
  has_diagnosis_jump <- select(get_diagnosis_jumps(event_log_object, input, diagnosis, end_ICD_codes) %>% filter(diagnosis_jump == Jump), case_id)
  if (SummaryType == "total_per_claim"){
    inner_join(select(has_diagnosis_jump, case_id) %>% 
                 rename(Claim.Administrator.Claim.Number = case_id),
               HEADER_join_DETAIL, by = "Claim.Administrator.Claim.Number", copy = TRUE) %>% 
      group_by(Claim.Administrator.Claim.Number) %>% 
      dplyr::mutate(Total.Amount.Paid.Per.Line = as.numeric(Total.Amount.Paid.Per.Line)) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line))
  }
  else if (SummaryType == "avg_per_claim"){
    inner_join(select(has_diagnosis_jump, case_id) %>% 
                 rename(Claim.Administrator.Claim.Number = case_id),
               HEADER_join_DETAIL, by = "Claim.Administrator.Claim.Number", copy = TRUE) %>%
      group_by(Claim.Administrator.Claim.Number) %>%
      dplyr::mutate(Total.Amount.Paid.Per.Line = as.numeric(Total.Amount.Paid.Per.Line)) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>% 
      summarize(avg_paid_per_claim = mean(total_paid))
  }
  else if (SummaryType == "avg_per_bill"){
    inner_join(select(has_diagnosis_jump, case_id) %>% 
                 rename(Claim.Administrator.Claim.Number = case_id), 
               HEADER_join_DETAIL, by = "Claim.Administrator.Claim.Number", copy = TRUE) %>%
      group_by(Bill.ID) %>%
      dplyr::mutate(Total.Amount.Paid.Per.Line = as.numeric(Total.Amount.Paid.Per.Line)) %>% 
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      summarize(avg_paid_per_bill = mean(total_paid))
  }
}

get_diagnosis_jumps <- function (event_log_object, input, diagnosis, end_ICD_codes){
  filter_event_log_object(event_log_object, input, diagnosis, end_ICD_codes) %>%
    group_by(case_id) %>%
    dplyr::mutate(had_other = any(activity_id != diagnosis)) %>% 
    arrange(case_id) %>% 
    summarize(diagnosis_jump = first(had_other))
}

filter_event_log_object <- function(event_log_object, input, diagnosis, end_ICD_codes){
  if (input$end_ICD_codes == "ANY"){
    event_log_object %>% filter_endpoints(start_activities = diagnosis)
  }
  else {
    event_log_object %>% filter_endpoints(start_activities = diagnosis, end_activities = diagnosis)
  }
}