library(dplyr)
library(bupaR)
library(lubridate)
library(processcheckR)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("process_mining_research.R")

STARTING_DIAGNOSIS <- "S33.5XX"
file_name <- "C:\\Users\\tk250052\\Documents\\OneDrive - Teradata\\Eminence IC Work\\WC Medical Claims\\github\\InsuranceClaimsExplorer\\research\\data_set.txt"

event_log_df <- create_event_log_df(file_name)
event_log_df <- substitute_diagnoses(event_log_df, STARTING_DIAGNOSIS)
event_log_object <- create_event_log_object(event_log_df)

##################ANALYSIS#######################################
##EVENT LOG DATA - STARTS WITH
event_log_object %>%
  filter_rules(activity_id = starts("start"))

##PROCESS MAP - NOT COLLAPSED
event_log_object %>%
  filter_rules(activity_id = starts("start")) %>%
  process_map()

##PROCESS MAP - COLLAPSED
event_log_object %>%
  filter_rules(activity_id = starts("start")) %>%
  act_collapse(start = c("start"), 
               same_chapter = c("same_chapter"),
               different_chapter = c("different_chapter"),
               unspecified = c("unspecified"),
               method = "consecutive") %>%
  process_map()

##TRACES - NOT COLLAPSED
event_log_object %>%
  filter_rules(activity_id = starts("start")) %>%
  traces()

##TRACES - COLLAPSED
event_log_object %>%
  filter_rules(activity_id = starts("start")) %>%
  act_collapse(start = c("start"), 
               same_chapter = c("same_chapter"),
               different_chapter = c("different_chapter"),
               unspecified = c("unspecified"),
               method = "consecutive") %>%
  traces()

##TRACE EXPLORER - NOT COLLAPSED
event_log_object %>%
  filter_rules(activity_id = starts("start")) %>%
  trace_explorer(coverage = 1)

##TRACE EXPLORER - COLLAPSED
event_log_object_collapsed <- event_log_object %>%
  filter_rules(activity_id = starts("start")) %>%
  act_collapse(start = c("start"), 
               same_chapter = c("same_chapter"),
               different_chapter = c("different_chapter"),
               method = "consecutive")
unspecified_row_count <- event_log_object %>%
  filter_rules(activity_id = starts("start")) %>%
  filter(activity_id == "unspecified") %>%
  as.data.frame() %>%
  nrow()
if (unspecified_row_count > 0){
  event_log_object_collapsed <- event_log_object_collapsed %>%
    act_collapse(unspecified = c("unspecified"),
                 method = "consecutive")
}
event_log_object_collapsed %>%
  trace_explorer(coverage = 1)