# data_grouped <- HEADER_join_DETAIL %>%
#   dplyr::group_by(Claim.Administrator.Claim.Number, !!sym(activity_column),
#                   Service.Line.From.Date) %>%
#   dplyr::summarize(dummy = 1) %>%
#   show_query()
