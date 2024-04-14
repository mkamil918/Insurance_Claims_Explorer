library(dplyr)
library(dbplyr)
library(teradatasql)
library(tdplyr)
library(askpass)


td_remove_context()

mycon <- td_create_context(host="tdprd.td.teradata.com", uid='MK255125', pwd=askpass(), 
                            dType = "native", logmech = "LDAP")

data_event_log <- tbl(mycon, in_schema('fis_med_claims_dev','data_event_log_perm'))

head(data_event_log)


get_event_log_object(HEADER_join_DETAIL = data_event_log, digit_count = "all")

