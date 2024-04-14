OUTLOOK_ID <- "MK255125"

#################global#########################################################
#if(!require(rpivotTable)) {install.packages('rpivotTable', dependencies = TRUE); require(rpivotTable)}
#if(!require(shinydashboard)) {install.packages('shinydashboard', dependencies = TRUE); require(shinydashboard)}
if(!require(tdplyr)) {install.packages('tdplyr', dependencies = TRUE); require(tdplyr)}
if(!require(dplyr)) {install.packages('dplyr', dependencies = TRUE); require(dplyr)}
#if(!require(bupaR)) {install.packages('bupaR', dependencies = TRUE); require(bupaR)}
if(!require(dbplyr)) {install.packages('dbplyr', dependencies = TRUE); require(dbplyr)}
if(!require(askpass)) {install.packages('askpass', dependencies = TRUE); require(askpass)}
#if(!require(tidyr)) {install.packages('tidyr', dependencies = TRUE); require(tidyr)}
#if(!require(hablar)) {install.packages('hablar', dependencies = TRUE); require(hablar)}
# if(!require(shinybusy)) {install.packages('shinybusy', dependencies = TRUE); require(shinybusy)}
# if(!require(shinyWidgets)) {install.packages('shinyWidgets', dependencies = TRUE); require(shinyWidgets)}
# if(!require(tippy)) {install.packages('tippy', dependencies = TRUE); require(tippy)}
# if(!require(Dict)) {install.packages('Dict', dependencies = TRUE); require(Dict)}
# if(!require(RColorBrewer)) {install.packages('RColorBrewer', dependencies = TRUE); require(RColorBrewer)}

# if(!require(petrinetR)) {install.packages('petrinetR', dependencies = TRUE); require(petrinetR)}
# if(!require(heuristicsmineR)) {install.packages('heuristicsmineR', dependencies = TRUE); require(heuristicsmineR)}
# 
# if(!require(DiagrammeR)) {install.packages('DiagrammeR', dependencies = TRUE); require(DiagrammeR)}
# if(!require(plotly)) {install.packages('plotly', dependencies = TRUE); require(plotly)}
# if(!require(ggplot2)) {install.packages('ggplot2', dependencies = TRUE); require(ggplot2)}
# if(!require(eventInterval)) {install.packages('eventInterval', dependencies = TRUE); require(eventInterval)}
# if(!require(TraMineR)) {install.packages('TraMineR', dependencies = TRUE); require(TraMineR)}
# if(!require(splitstackshape)) {install.packages('splitstackshape', dependencies = TRUE); require(splitstackshape)}
# if(!require(DT)) {install.packages('DT', dependencies = TRUE); require(DT)}
# 
# if(!require(scales)) {install.packages('scales', dependencies = TRUE); require(scales)}
# if(!require(purrr)) {install.packages('purrr', dependencies = TRUE); require(purrr)}
# if(!require(cluster)) {install.packages('cluster', dependencies = TRUE); require(cluster)}
# if(!require(sortable)) {install.packages('sortable', dependencies = TRUE); require(sortable)}
# if(!require(ggthemes)) {install.packages('ggthemes', dependencies = TRUE); require(ggthemes)}
# if(!require(RColorBrewer)) {install.packages('RColorBrewer', dependencies = TRUE); require(RColorBrewer)}
# #if(!require(tidyverse)) {install.packages('tidyverse', dependencies = TRUE); require(tidyverse)}
# if(!require(shinyWidgets)) {install.packages('shinyWidgets', dependencies = TRUE); require(shinyWidgets)}
# if(!require(esquisse)) {install.packages('esquisse', dependencies = TRUE); require(esquisse)}
# 
# if(!require(d3treeR)) {install.packages("remotes"); remotes::install_github("d3treeR/d3treeR"); require(d3treeR)}
# 
# if(!require(treemap)) {install.packages('treemap', dependencies = TRUE); require(treemap)}
# if(!require(svgPanZoom)) {install.packages('svgPanZoom', dependencies = TRUE); require(svgPanZoom)}
# if(!require(DiagrammeRsvg)) {install.packages('DiagrammeRsvg', dependencies = TRUE); require(DiagrammeRsvg)}
# if(!require(flexdashboard)) {install.packages('flexdashboard', dependencies = TRUE); require(flexdashboard)}
# if(!require(billboarder)) {install.packages('billboarder', dependencies = TRUE); require(billboarder)}
# if(!require(DDoutlier)) {install.packages('DDoutlier', dependencies = TRUE); require(DDoutlier)}
# if(!require(processcheckR)) {install.packages('processcheckR', dependencies = TRUE); require(processcheckR)}
# if(!require(shinyjs)) {install.packages('shinyjs', dependencies = TRUE); require(shinyjs)}
# if(!require(formattable)) {install.packages('formattable', dependencies = TRUE); require(formattable)}
# if(!require(rhandsontable)) {install.packages('rhandsontable', dependencies = TRUE); require(rhandsontable)}
# if(!require(googleVis)) {install.packages('googleVis', dependencies = TRUE); require(googleVis)}
# if(!require(sankeywheel)) {install.packages('sankeywheel', dependencies = TRUE); require(sankeywheel)}
if(!require(getPass)) {install.packages('getPass', dependencies = TRUE); require(getPass)}
# if(!require(lubridate)) {install.packages('lubridate', dependencies = TRUE); require(lubridate)}
# if(!require(gapminder)) {install.packages('gapminder', dependencies = TRUE); require(gapminder)}
# if(!require(hrbrthemes)) {install.packages('hrbrthemes', dependencies = TRUE); require(hrbrthemes)}
# if(!require(viridis)) {install.packages('viridis', dependencies = TRUE); require(viridis)}
# if(!require(ChainLadder)) {install.packages('ChainLadder', dependencies = TRUE); require(ChainLadder)}

#################server#########################################################
tukey_filter_1D <- function(data, outlier_column = "ax_1"){
  thresholds <- data %>% 
    summarise(q25 = quantile(!!as.name(outlier_column), probs = c(0.25)),
              q75 = quantile(!!as.name(outlier_column), probs = c(0.75))) %>%
    mutate(tf = 1.5 * (q75 - q25)) %>%
    mutate(LT = q25 - tf) %>%
    mutate(UT = q75 + tf) %>%
    select(LT, UT) %>% as_tibble()
  
  data %>%
    mutate(Status = ifelse(!!as.name(outlier_column) <= local(thresholds$UT),
                           "Inlier", "Outlier"))
}

#################tab_module_data_selection######################################
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
SV4_Columns <- c("Bill.ID", "Bill.Type", "Claim.Administrator.Claim.Number", "Date.of.Bill",
                 "Line.Number", "Drug.Name", "Service.Line.From.Date", "Total.Amount.Paid.Per.Line",
                 "NDC.Billed.Code")

password <- askpass()
db.con <- td_create_context(
  host="tdprd.td.teradata.com", uid=OUTLOOK_ID, pwd=password, 
  dType = "native", logmech = "LDAP")
con <- db.con

insurer_FEIN <- "848446466"
minimum_bill_date <- "20180000"
maximum_bill_date <- "20210000"
selected_bill_type <- "SV1"

#OUTPATIENT
SV1DETAIL <- tbl(con, in_schema("fis_med_claims_dev", "COVID_SV1_Detail")) %>%
  dplyr::mutate(Service.Line.From.Date = as.integer(Service.Line.From.Date))
SV1HEADER <- tbl(con, in_schema("fis_med_claims_dev", "COVID_SV1_Header")) %>%
  dplyr::filter(Date.of.Bill >= as.integer(minimum_bill_date),
                Date.of.Bill < as.integer(maximum_bill_date)
  )
if (insurer_FEIN != "All"){
  SV1HEADER <- SV1HEADER %>%
    dplyr::filter(Insurer.FEIN == insurer_FEIN)
}
SV1HEADER <- SV1HEADER %>% dplyr::rename_all(list(~gsub("-", ".", .)))
SV1HEADER <- SV1HEADER %>% dplyr::mutate(Date.of.Bill = as.integer(Date.of.Bill)) %>%
  dplyr::mutate(Bill_Year = trunc(Date.of.Bill / 10000))
SV1 <- dplyr::inner_join(SV1DETAIL, SV1HEADER, by = "Bill.ID") %>%
  dplyr::select(all_of(SV1_Columns))

#INPATIENT
SV2DETAIL <- tbl(con, in_schema("fis_med_claims_dev", "COVID_SV2_Detail"))
SV2HEADER <- tbl(con, in_schema("fis_med_claims_dev", "COVID_SV2_Header")) %>%
  dplyr::filter(Date.of.Bill >= as.integer(local(minimum_bill_date)),
                Date.of.Bill < as.integer(local(maximum_bill_date))
  )
SV2HEADER <- SV2HEADER %>%
  dplyr::rename_all(list(~gsub("-", ".", .)))
SV2 <- dplyr::inner_join(SV2DETAIL, SV2HEADER, by = "Bill.ID")
SV2 <- SV2 %>%
  dplyr::select(-First.ICD.9.CM.Diagnosis.Code) %>%
  dplyr::mutate(First.ICD.9.CM.Diagnosis.Code = Principal.Diagnosis.Code) %>%
  dplyr::select(all_of(SV1_Columns))
SV_1_and_2 <- dplyr::full_join(SV1, SV2)

##PRESCRIPTION DRUGS
SV4DETAIL <- tbl(con, in_schema("fis_med_claims_dev", "COVID_SV4_Detail"))
SV4HEADER <- tbl(con, in_schema("fis_med_claims_dev", "COVID_SV4_Header")) %>%
  dplyr::filter(Date.of.Bill >= as.integer(local(minimum_bill_date)),
                Date.of.Bill < as.integer(local(maximum_bill_date))
  )
SV4HEADER <- SV4HEADER %>% dplyr::rename_all(list(~gsub("-", ".", .)))
SV4 <- dplyr::inner_join(SV4DETAIL, SV4HEADER, by = "Bill.ID") %>% dplyr::select(SV4_Columns)
SV_1_and_2_and_4 <- SV4 %>% dplyr::full_join(SV_1_and_2)

#define claims_data based on what Bill Types user selected
claims_data <- SV1

#join with DRG_MAP to get the DRG values
DRG_MAP <- tbl(con, in_schema("fis_med_claims_dev", "ICD10_DRG_MAP"))
DRG_MAP <- DRG_MAP %>% dplyr::rename_all(list(~gsub("_", ".", .)))
DRG_MAP <- DRG_MAP %>% dplyr::rename_all(list(~gsub("-", ".", .)))
claims_data <- dplyr::left_join(claims_data, DRG_MAP, by = 'First.ICD.9.CM.Diagnosis.Code')
claims_data <- claims_data %>%
  dplyr::mutate(DRG = coalesce(DRG, substr(First.ICD.9.CM.Diagnosis.Code, 1, 1)))

#add medical coding descriptions
descriptions_tbl <- tbl(db.con, in_schema("fis_med_claims_dev", "icd_10_descriptions"))
claims_data <- claims_data %>%
  dplyr::mutate(ICD.Code = gsub("\\.", "", First.ICD.9.CM.Diagnosis.Code))
claims_data <- left_join(claims_data, descriptions_tbl %>% 
                           dplyr::select(Long.Description.First, Body.Part, ICD.Code), by = "ICD.Code") %>%
  dplyr::select(-ICD.Code)

claims_data_return <- claims_data
#return the temp table if SV1 only
if (selected_bill_type == "SV1"){
  copy_to(con, claims_data, "claims_data_temp", temporary = TRUE, overwrite = TRUE)
  claims_data_return <- tbl(con, "claims_data_temp")
}
claims_data_return

#################tab_module_suspicious_providers################################
claims_data <- claims_data_return
payments <- claims_data %>%
  dplyr::group_by(Billing.Provider.FEIN) %>%
  dplyr::summarise(ax_1 = sum(Total.Amount.Paid.Per.Line[HCPCS.Line.Procedure.Billed.Code == '97799'])/(sum(Total.Amount.Paid.Per.Line) + 1e-20),
                   paid = sum(Total.Amount.Paid.Per.Line[HCPCS.Line.Procedure.Billed.Code == '97799'])) %>%
  dplyr::mutate(ax_1 = coalesce(ax_1, 0), paid = coalesce(paid, 0))

labeled_payments <- tukey_filter_1D(payments)

suspicious_payments <- labeled_payments %>%
  dplyr::summarise(payments = sum(paid[Status == 'Outlier'])) %>%
  dplyr::as_tibble()

unlisted_rehabalitation_payments <- suspicious_payments$payments

unlisted_rehabilition_FEINs <- labeled_payments %>%
  dplyr::filter(Status == 'Outlier') %>%
  dplyr::select(Billing.Provider.FEIN)

unlisted_rehab_outlier_data_cube <- dplyr::inner_join(claims_data,
                                                      unlisted_rehabilition_FEINs, by = "Billing.Provider.FEIN") %>%
  dplyr::filter(HCPCS.Line.Procedure.Billed.Code == "97799") %>%
  dplyr::mutate(anomaly_type = "unlisted_rehab")

##**error here
payments <- claims_data %>%
  dplyr::mutate(cpt_suffix = substr(HCPCS.Line.Procedure.Billed.Code, 4, 5)) %>%
  dplyr::group_by(Billing.Provider.FEIN) %>%
  dplyr::mutate(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
  dplyr::filter(cpt_suffix == "99" & HCPCS.Line.Procedure.Billed.Code != '97799') %>%
  dplyr::group_by(Billing.Provider.FEIN,HCPCS.Line.Procedure.Billed.Code,total_paid) %>%
  dplyr::summarise(ax_1 = sum(Total.Amount.Paid.Per.Line)/(total_paid + 1e-20),
                   paid = sum(Total.Amount.Paid.Per.Line)) %>%
  dplyr::distinct(Billing.Provider.FEIN,HCPCS.Line.Procedure.Billed.Code,ax_1,paid) %>% #error
  dplyr::mutate(ax_1 = coalesce(ax_1, 0), paid = coalesce(paid, 0))

thresholds <- tukey_thresholds_1D(payments,HCPCS.Line.Procedure.Billed.Code)

labeled_payments_unlisted_other <- dplyr::inner_join(payments,thresholds,
                                                     by = "HCPCS.Line.Procedure.Billed.Code") %>%
  dplyr::mutate(Status = ifelse(ax_1 <= UT,"INLIER", "OUTLIER"))


suspicious_payments <- labeled_payments_unlisted_other %>%
  summarise(payments = sum(paid[Status == 'Outlier'])) %>%
  mutate(payments = coalesce(payments, 0)) %>%
  as.data.frame()

vals$other_unlisted_payments <- sum(suspicious_payments$payments)

#"tab_module_provider_anomalies_action.R" needs supporting data for summary tab
unlisted_other_FEINs <- labeled_payments_unlisted_other %>%
  dplyr::filter(Status == 'OUTLIER') %>%
  dplyr::ungroup() %>%
  dplyr::select(Billing.Provider.FEIN)

unlisted_other_outlier_data_cube <- inner_join(vals$claims_data,
                                               unlisted_other_FEINs, by = "Billing.Provider.FEIN") %>%
  dplyr::mutate(cpt_suffix = substr(HCPCS.Line.Procedure.Billed.Code, 4, 5)) %>%
  dplyr::filter(cpt_suffix == "99" & HCPCS.Line.Procedure.Billed.Code != '97799') %>%
  dplyr::select(-cpt_suffix) %>%
  dplyr::mutate(anomaly_type = "unlisted_other")

##create complete procedure_outlier_data_cube
vals$procedure_outlier_data_cube <<- dplyr::union_all(unlisted_rehab_outlier_data_cube,
                                                      unlisted_other_outlier_data_cube)