library(tdplyr)
library(dplyr)
library(lubridate)
library(pracma)
library(ggplot2)
library(hrbrthemes)
library(getPass)
library(dbplyr)

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

USER_ID <- "TK250052"
INSURER_FEIN <- "848446466" # Travelers
INSURER_FEIN <- "726579180" # Texas Mutual
MINIMUM_BILL_DATE <- "20180000"
INJURY_YEAR <- 2018
INJURY_MONTH <- 1
MAXIMUM_BILL_DATE <- "20200000"

#connect to Teradata
con <- td_create_context(
  host="tdprd.td.teradata.com", uid = USER_ID, pwd = getPass(), 
  dType = "native", logmech = "LDAP")

#create table ojects
SV1DETAIL <- tbl(con, in_schema("fis_med_claims_dev", "COVID_SV1_Detail")) %>%
  dplyr::mutate(Service.Line.From.Date = as.integer(Service.Line.From.Date))
SV1HEADER <- tbl(con, in_schema("fis_med_claims_dev", "COVID_SV1_Header")) %>%
  dplyr::filter(Date.of.Bill >= as.integer(local(MINIMUM_BILL_DATE)),
                Date.of.Bill < as.integer(local(MAXIMUM_BILL_DATE))
  )

#filter for insurer
if (INSURER_FEIN != "All"){
  SV1HEADER <- SV1HEADER %>%
    dplyr::filter(Insurer.FEIN == INSURER_FEIN)
}

#data type, column name cleanup
SV1 <- dplyr::inner_join(SV1DETAIL, SV1HEADER, by = "Bill.ID")
SV1 <- SV1 %>% dplyr::rename_all(list(~gsub("_", ".", .)))
SV1 <- SV1 %>% dplyr::rename_all(list(~gsub("-", ".", .)))
SV1 <- SV1 %>% 
  dplyr::mutate(Claim.Administrator.Claim.Number = as.character(Claim.Administrator.Claim.Number), 
                First.ICD.9.CM.Diagnosis.Code = as.character(First.ICD.9.CM.Diagnosis.Code), 
                Service.Line.From.Date = as.character(Service.Line.From.Date)) %>%
  dplyr::select(all_of(SV1_Columns))

#filter for injury date
SV1 <- SV1 %>% mutate(InjuryYear = as.integer(Employee.Date.of.Injury / 100), 
                      InjuryMonth = Employee.Date.of.Injury - InjuryYear * 100) %>%
               filter(InjuryYear == INJURY_YEAR, InjuryMonth == INJURY_MONTH)

SV1_df <- SV1 %>% as.data.frame()
write.table(SV1_df, file = "data_set2.txt", sep = "\t", row.names = FALSE)
