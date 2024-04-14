library(dbplyr)
library(dplyr)
library(tdplyr)
library(getPass)

con <- td_create_context(host = "tdprd.td.teradata.com", uid="fr250022", 
                         pwd=getPass(), dType = "native", logmech = "LDAP")

############ Joining Tables

MIN_BILL_DATE <- 20180000

SV1HEADER <- tbl(con, in_schema("fis_med_claims_dev", "SV1HEADER"))
SV1HEADER <- SV1HEADER %>% rename_all(list(~gsub("_", ".", .)))
SV1HEADER <- SV1HEADER %>% rename_all(list(~gsub("-", ".", .)))
SV1HEADER <- SV1HEADER %>% rename(First.ICD.9.CM.Diagnosis.Code = First.ICD9.CM.Diagnosis.Code)
SV1HEADER <- SV1HEADER %>% mutate(Date.of.Bill = as.integer(Date.of.Bill)) %>% mutate(Bill_Year = trunc(Date.of.Bill / 10000))

SV1DETAIL <- tbl(con, in_schema("fis_med_claims_dev", "SV1DETAIL"))
SV1DETAIL <- SV1DETAIL %>% rename_all(list(~gsub("_", ".", .)))
SV1DETAIL <- SV1DETAIL %>% rename_all(list(~gsub("-", ".", .)))

DRG_MAP <- tbl(con, in_schema("fis_med_claims_dev", "ICD10_DRG_MAP"))
DRG_MAP <- DRG_MAP %>% rename_all(list(~gsub("_", ".", .)))
DRG_MAP <- DRG_MAP %>% rename_all(list(~gsub("-", ".", .)))

SV1HEADER_filtered <- SV1HEADER %>% filter(Date.of.Bill >= MIN_BILL_DATE)

SV1 <- inner_join(SV1HEADER_filtered, SV1DETAIL, by = "Bill.ID")
SV1 <- left_join(SV1, DRG_MAP, by = 'First.ICD.9.CM.Diagnosis.Code')
SV1 <- SV1 %>%
  dplyr::mutate(DRG = coalesce(DRG, substr(First.ICD.9.CM.Diagnosis.Code, 1, 1)))

copy_to(con, SV1, "SV1_temp", temporary = TRUE, overwrite = TRUE)

############ Preparing Table

SV1_data <- tbl(con,"SV1_temp")

data_for_pathing <- SV1_data %>%
  dplyr::select(Claim.Administrator.Claim.Number,Insurer.FEIN, First.ICD.9.CM.Diagnosis.Code,
                DRG, Service.Line.From.Date) %>%
  dplyr::mutate(Claim.Administrator.Claim.Number = paste0(Insurer.FEIN,Claim.Administrator.Claim.Number)) %>%
  dplyr::group_by(Claim.Administrator.Claim.Number) %>%
  dplyr::mutate(first_service_date = min(Service.Line.From.Date)) %>%
  dplyr::mutate(Service.Line.From.Date = as.integer(Service.Line.From.Date)) %>%
  dplyr::mutate(first_service_date = as.integer(first_service_date)) %>%
  dplyr::mutate(week = trunc((as_date(Service.Line.From.Date) - as_date(first_service_date)) / 7))

data_for_pathing <- data_for_pathing %>%
  dplyr::distinct(Claim.Administrator.Claim.Number, First.ICD.9.CM.Diagnosis.Code, DRG, week) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Claim.Administrator.Claim.Number, week) %>%
  dplyr::mutate(diagnosis_count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::rename(Service.Line.From.Date = week)

data_for_pathing <- data_for_pathing %>%
  dplyr::mutate(DRG = ifelse(diagnosis_count > 1, "Many",as.character(DRG)),
                First.ICD.9.CM.Diagnosis.Code = ifelse(diagnosis_count > 1,
                                                       "Many",
                                                       as.character(First.ICD.9.CM.Diagnosis.Code))) %>%
  dplyr::distinct(Claim.Administrator.Claim.Number, First.ICD.9.CM.Diagnosis.Code,
                  DRG, Service.Line.From.Date)

data_for_pathing_tbl <- data_for_pathing %>%
  dplyr::rename(Diagnosis = First.ICD.9.CM.Diagnosis.Code,
                DRG = DRG,
                Claim_Number = Claim.Administrator.Claim.Number,
                Date = Service.Line.From.Date) %>%
  select(Diagnosis,DRG,Claim_Number,Date)

############ Probability Calculation

npath_result <- td_npath_sqle (
  data1 = data_for_pathing_tbl,
  mode = "NONOVERLAPPING",
  pattern = "^B+",
  symbols =  c("true as B"),
  result = c("ACCUMULATE (\"Diagnosis\" of any(B) ) AS path",
             "ACCUMULATE (\"DRG\" of any(B) ) AS drg_path",
             "FIRST_NOTNULL (Diagnosis OF ANY(B)) AS init_diag",
             "FIRST_NOTNULL (DRG OF ANY(B)) AS init_drg"),
  data1.partition.column = c("Claim_Number"),
  data1.order.column = "Date"
)

npath_result <- npath_result$result

print(npath_result)

npath_result <- npath_result %>%
  rename(trace = path, drg_trace = drg_path) %>%
  mutate(fjourney = gsub('\\[','Start, ',trace),
         drg_journey = gsub('\\[','Start, ',drg_trace)) %>%
  mutate(fjourney = gsub('\\]','',fjourney),
         drg_journey = gsub('\\]','',drg_journey))

copy_to(con, npath_result, "npath_result",temporary = TRUE, overwrite = TRUE)

npath_result <- tbl(con,"npath_result")

icd_pairs <- tdplyr::td_ngramsplitter_sqle(
  data = npath_result,
  text.column = 'fjourney',
  grams = '2',
  overlapping = TRUE,
  to.lower.case = FALSE,
  punctuation = '$',
  reset = '$',
  delimiter = ', '
)

icd_pairs <- icd_pairs$result

print(icd_pairs)

final_icd_pairs <- icd_pairs %>%
  select(trace, init_diag, fjourney, ngram, frequency) %>%
  mutate(evnt1 = gsub(' .+','',ngram),
         evnt2 = gsub('[a-zA-Z0-9\\.\\-]+ ','',ngram)) %>%
  select(trace, init_diag, evnt1, evnt2, frequency)

drg_pairs <- tdplyr::td_ngramsplitter_sqle(
  data = npath_result,
  text.column = 'drg_journey',
  grams = '2',
  overlapping = TRUE,
  to.lower.case = FALSE,
  punctuation = '$',
  reset = '$',
  delimiter = ', '
)

drg_pairs <- drg_pairs$result

print(drg_pairs)

final_drg_pairs <- drg_pairs %>%
  select(trace, init_drg, fjourney, ngram, frequency) %>%
  mutate(evnt1 = gsub(' .+','',ngram),
         evnt2 = gsub('[a-zA-Z0-9\\.\\-]+ ','',ngram)) %>%
  select(trace, init_drg, evnt1, evnt2, frequency) %>%
  rename(init_diag = init_drg)

########## Accounting for Initial Diagnosis

transition_counts <- union_all(final_icd_pairs,final_drg_pairs) %>%
  group_by(init_diag,evnt1,evnt2) %>%
  summarise(b_count = sum(as.double(frequency)))

copy_to(con, transition_counts, "transition_counts",temporary = FALSE, overwrite = TRUE,
        primary.index = c('init_diag','evnt1'))

transition_counts <- tbl(con,"transition_counts")

transition_counts

evnt1_cnts <- union_all(final_icd_pairs,final_drg_pairs) %>%
  group_by(evnt1,init_diag) %>%
  summarise(tot_cnt = sum(frequency)) %>%
  ungroup()

copy_to(con, evnt1_cnts, "evnt1_cnts",temporary = FALSE, overwrite = TRUE,
        primary.index = c('init_diag','evnt1'))

evnt1_cnts <- tbl(con,"evnt1_cnts")

transition_probs <- inner_join(transition_counts, evnt1_cnts, by=c("init_diag","evnt1")) %>%
  group_by(init_diag,evnt1,evnt2,tot_cnt,b_count) %>%
  summarise(t_prob = (as.double(b_count)/as.double(tot_cnt)))


######## Storing Transitions in Database

copy_to(con, transition_probs, in_schema("fis_med_claims_dev","Transition_Probs"),
        temporary = FALSE, overwrite = TRUE, primary.index = c('evnt1','evnt2'))

########## Collapsing Transitions

collapsed_transition_counts <- union_all(final_icd_pairs,final_drg_pairs) %>%
  group_by(evnt1,evnt2) %>%
  summarise(b_count = sum(as.double(frequency)))

copy_to(con, collapsed_transition_counts, "collapsed_transition_counts",temporary = FALSE, overwrite = TRUE,
        primary.index = c('evnt1'))

collapsed_transition_counts <- tbl(con,"collapsed_transition_counts")

collapsed_transition_counts

collapsed_evnt1_cnts <- union_all(final_icd_pairs,final_drg_pairs) %>%
  group_by(evnt1) %>%
  summarise(tot_cnt = sum(frequency)) %>%
  ungroup()

copy_to(con, collapsed_evnt1_cnts, "collapsed_evnt1_cnts",temporary = FALSE, overwrite = TRUE,
        primary.index = c('evnt1'))

collapsed_evnt1_cnts <- tbl(con,"collapsed_evnt1_cnts")

collapsed_transition_probs <- inner_join(collapsed_transition_counts, collapsed_evnt1_cnts,
                                        by="evnt1") %>%
  group_by(evnt1,evnt2,tot_cnt,b_count) %>%
  summarise(t_prob = (as.double(b_count)/as.double(tot_cnt)))


######## Storing Transitions in Database

copy_to(con, collapsed_transition_probs, in_schema("fis_med_claims_dev","Collapsed_Transition_Probs"),
        temporary = FALSE, overwrite = TRUE, primary.index = c('evnt1','evnt2'))