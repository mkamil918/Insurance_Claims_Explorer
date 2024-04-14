# Front page module

# UI function
tab_exploration_ui <- function(id) {
  
  # Main UI
  fluidPage(
    titlePanel("Exploration"),
    rpivotTableOutput("pivot")
  )
  
}

# Server function
tab_exploration_server <- function(input, output, session, vals) {
  
  output$pivot <- renderRpivotTable({
    rpivotTable(data = as.data.frame(head(vals$claims_data, n = 5000)), 
                rows = c("Claim.Administrator.Claim.Number", "Date.of.Bill", "Bill.ID"),
                aggregatorName = "Sum", vals = "Total.Amount.Paid.Per.Line", sorter = "Total.Amount.Paid.Per.Line"
    )
  })
  
}
