library(dbplyr)
library(dplyr)
library(tdplyr)
library(getPass)

con <- td_create_context(host = "tdprd.td.teradata.com", uid="fr250022", 
                         pwd=getPass(), dType = "native", logmech = "LDAP")

data <- tbl(con,in_schema("fis_med_claims_dev", "SV1_Travelers"))

icd_drg_map <- tbl(con,in_schema("fis_med_claims_dev", "ICD10_DRG_MAP"))

mapped_data <- data %>%
  dplyr::select(Claim.Administrator.Claim.Number, First.ICD.9.CM.Diagnosis.Code,
                Service.Line.From.Date, Total.Amount.Paid.Per.Line) %>%
  left_join(icd_drg_map, by = 'First.ICD.9.CM.Diagnosis.Code') %>%
  dplyr::mutate(DRG = coalesce(DRG, substr(First.ICD.9.CM.Diagnosis.Code, 1, 1)))

data_for_pathing <- mapped_data %>%
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

copy_to(con, final_pairs, "final_pairs",temporary = TRUE, overwrite = TRUE,
        primary.index = c('evnt1','evnt2'))

final_pairs <- tbl(con,"final_pairs")

collapsed_transition_probs <- tbl(con,in_schema("fis_med_claims_dev","Collapsed_Transition_Probs"))

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

########### Elbow Method

total_ss <- c()

for (i in sequence(from = 2, nvec = 10,by = 2)){
  kmeans_clusters <- tdplyr::td_kmeans(
    data = coalesce_pivotted,
    centers = i,
    iter.max = 20
  )
  
  print(kmeans_clusters)
  
  tss <- kmeans_clusters$clusters.centroids %>%
    select(withinss) %>%
    summarise(total_ss = sum(withinss)) %>%
    as_tibble()
  
  total_ss <- append(total_ss,tss$total_ss)
  
}

total_ss

plot(x=sequence(from = 2, nvec = 10,by = 2),y = total_ss)

########### Final Clusters (Vantage)

kmeans_clusters <- tdplyr::td_kmeans(
  seed = 42,
  data = coalesce_pivotted,
  centers = 8,
  iter.max = 100
)

print(kmeans_clusters)

smallest_cluster <- kmeans_clusters$clusters.centroids %>%
  mutate(min_size = min(size)) %>%
  filter(size == min_size) %>%
  as_tibble()


kmeans_clusters$clustered.output %>%
  filter(clusterid == local(smallest_cluster$clusterid[[1]])) %>%
  inner_join(data,
             by = c("case_id_z"="Claim.Administrator.Claim.Number")) %>%
  select(Total.Amount.Paid.Per.Line) %>%
  summarise(total = sum(Total.Amount.Paid.Per.Line)) %>%
  as_tibble()

copy_to(con, kmeans_clusters$clusters.centroids,
        in_schema("fis_med_claims_dev","Cluster_Centroids"),
        temporary = FALSE, overwrite = TRUE)