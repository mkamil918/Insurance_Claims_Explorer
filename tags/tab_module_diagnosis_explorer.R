# Front page module

# UI function
tab_diagnosis_explorer_ui <- function(id) {
  
  # Main UI
  fluidPage(
    titlePanel("Diagnosis Explorer"),
    dataTableOutput("diagnosis_exploration_table")
  )
  
}

# Server function
tab_diagnosis_explorer_server <- function(input, output, session, vals) {
  
  output$diagnosis_exploration_table <- renderDataTable({
    #first, get top $$ claim numbers
    top_dollar_claim_numbers_df <- vals$claims_data %>%
      dplyr::group_by(Claim.Administrator.Claim.Number) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      filter(!is.na(total_paid)) %>%
      mutate(rank = rank(-total_paid)) %>%
      filter(rank <= 5000) %>%
      as.data.frame()
    top_dollar_claim_numbers_vector <- top_dollar_claim_numbers_df$Claim.Administrator.Claim.Number
    
    display_tbl <- vals$claims_data %>%
      filter(Claim.Administrator.Claim.Number %in% top_dollar_claim_numbers_vector) %>%
      dplyr::group_by(Claim.Administrator.Claim.Number, Body.Part) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      filter(Body.Part != "Not specified") %>%
      dplyr::arrange((!!as.name("Claim.Administrator.Claim.Number")))
    
    display_df <- display_tbl %>%
      as.data.frame()
    display_df_spread <- display_df %>% 
      spread(Body.Part, total_paid)
    total_body_parts <- ncol(display_df_spread) - 1
    display_df_spread$body_part_count <- (total_body_parts - rowSums(is.na(display_df_spread %>% select(-Claim.Administrator.Claim.Number))))
    display_df_spread %>% 
      arrange(desc(body_part_count))
    
  }, options = list(scrollX = TRUE))
  
}
