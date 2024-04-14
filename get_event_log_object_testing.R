get_event_log_object <- function(HEADER_join_DETAIL, digit_count){
  activity_column <- ""
  print(activity_column)
  print('printing start of function')
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

  print("before before something I")
  print(HEADER_join_DETAIL$`First.ICD.9.CM.Diagnosis.Code`)
  print("before before something II")

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
