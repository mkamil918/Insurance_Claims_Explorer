rename_columns_treatment_cluster_MRD <- function(tbl){
  return_value <- tbl %>% rename("Diagnosis Code" = First.ICD.9.CM.Diagnosis.Code,
                                 "Procedure Code" = HCPCS.Line.Procedure.Billed.Code)
  return_value
}

rename_bill_details_selected <- function(tbl){
  tbl <- tbl %>% rename("Bill ID" = Bill.ID,
                        "Date of Bill" = Date.of.Bill,
                        "Paid Per Bill" = Total.Amount.Paid.Per.Bill,
                        "Diagnosis Code" = First.ICD.9.CM.Diagnosis.Code,
                        "Claim Number" = Claim.Administrator.Claim.Number,
                        "Line" = Line.Number,
                        "Paid Per Line" = Total.Amount.Paid.Per.Line,
                        "Procedure Code" = HCPCS.Line.Procedure.Paid.Code,
                        "Provider" = Billing.Provider.FEIN,
                        "Service Date" = Service.Line.From.Date)
  tbl %>% select("Date of Bill",
                 "Line",
                 "Service Date",
                 "Paid Per Line",
                 "Paid Per Bill",
                 "Procedure Code",
                 "Diagnosis Code",
                 "Provider", everything())
}

rename_claim_traces <- function(tbl){
  tbl <- tbl %>% rename("Claim Number" = Claim.Administrator.Claim.Number,
                        "Total Paid" = "total_paid",
                        "Unique Diagnoses" = number_of_activities,
                        "Diagnosis Count" = trace_length)
  #tbl <- tbl %>% rename("Claim Number" = Claim.Administrator.Claim.Number, "Total Paid" = "total_paid", "Diagnosis Count" = trace_length)
  tbl %>% select("Claim Number", "Total Paid", "Unique Diagnoses", "Diagnosis Count", everything())
}

rename_claim_pathway_data_foundation <- function(tbl){
  tbl %>% dplyr::rename("Procedure Code" = HCPCS.Line.Procedure.Billed.Code,
                        "Claim Number" = Claim.Administrator.Claim.Number,
                        "Bill ID"= Bill.ID, Provider = Billing.Provider.FEIN,
                        "Date of Service" = Service.Line.From.Date,
                        Paid = Total.Amount.Paid.Per.Line)
}