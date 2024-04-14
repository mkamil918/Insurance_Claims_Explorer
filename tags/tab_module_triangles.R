# Front page module

# UI function
tab_triangles_ui <- function(id) {
  
  # Main UI
  fluidPage(
    titlePanel("Actuarial Triangle Maker"),
    fluidRow(
      column(7,
        htmlOutput("exception_triangle_title"),
        tableOutput("exception_triangle"),
        htmlOutput("residual_triangle_title"),
        tableOutput("residual_triangle")
      ),
      column(5,
        actionButton("adjust_claims_data", "Update / Load Triangles"),
        dataTableOutput("claim_listing"),
        textOutput("selected_claim_count"),
        tableOutput("selected_claim_numbers"),
        tableOutput("selected_claim_dollars")
      )
    ),
    fluidRow(
      column(6, 
        tableOutput("triangle"),
        tableOutput("ata_factors")
      )
    )
  )
  
}

# Server function
tab_triangles_server <- function(input, output, session, vals) {

  output$exception_triangle_title <- renderUI({
    HTML("<h3>All Claims With T14.90X (\"Unspecified Injury\")</h3>")
  })
  
  output$exception_triangle <- renderTable({
    read.table(paste(getwd(), "/data/", "triangles_exception.txt", sep = ""), header = TRUE) %>%
      mutate(M12 = number(M12, big.mark = ",")) %>%
      mutate(M12 = ifelse(is.na(M12), M12, paste("$", M12, sep = ""))) %>%
      mutate(M24 = number(M24, big.mark = ",")) %>%
      mutate(M24 = ifelse(is.na(M24), M24, paste("$", M24, sep = ""))) %>%
      mutate(M36 = number(M36, big.mark = ",")) %>%
      mutate(M36 = ifelse(is.na(M36), M36, paste("$", M36, sep = ""))) %>%
      mutate(M48 = number(M48, big.mark = ",")) %>%
      mutate(M48 = ifelse(is.na(M48), M48, paste("$", M48, sep = ""))) %>%
      replace(is.na(.), "") %>%
      rbind(c("", "", "", "", "")) %>%
      rbind(c("ATA-2017", "2.646", "1.265", "1.039", "")) %>%
      rbind(c("ATA-2018", "1.778", "1.113", "", "")) %>%
      rbind(c("ATA-2019", "1.356", "", "", "")) %>%
      #rbind(c("", "", "", "", "")) %>%
      rbind(c("AVG-SMPL", "1.927", "1.189", "1.039", "")) %>%
      rbind(c("AVG-WTD", "1.739", "1.158", "1.039", ""))
  })
  
  output$residual_triangle_title <- renderUI({
    HTML("<h3>All Claims WithOut T14.90X (\"Unspecified Injury\")</h3>")
  })
  
  output$residual_triangle <- renderTable({
    read.table(paste(getwd(), "/data/", "triangles_residual.txt", sep = ""), header = TRUE) %>%
      mutate(M12 = number(M12, big.mark = ",")) %>%
      mutate(M12 = ifelse(is.na(M12), M12, paste("$", M12, sep = ""))) %>%
      mutate(M24 = number(M24, big.mark = ",")) %>%
      mutate(M24 = ifelse(is.na(M24), M24, paste("$", M24, sep = ""))) %>%
      mutate(M36 = number(M36, big.mark = ",")) %>%
      mutate(M36 = ifelse(is.na(M36), M36, paste("$", M36, sep = ""))) %>%
      mutate(M48 = number(M48, big.mark = ",")) %>%
      mutate(M48 = ifelse(is.na(M48), M48, paste("$", M48, sep = ""))) %>%
      replace(is.na(.), "") %>%
      rbind(c("", "", "", "", "")) %>%
      rbind(c("ATA-2017", "1.497", "1.087", "1.009", "")) %>%
      rbind(c("ATA-2018", "1.571", "1.037", "", "")) %>%
      rbind(c("ATA-2019", "1.272", "", "", "")) %>%
      #rbind(c("", "", "", "", "")) %>%
      rbind(c("AVG-SMPL", "1.447", "1.062", "1.009", "")) %>%
      rbind(c("AVG-WTD", "1.437", "1.062", "1.009", ""))
  })
  
  output$triangle <- renderTable({
    if (!is.null(vals$triangle_source_data)){
      cumulative_triangle <- get_triangle()
    }
  })
  
  output$ata_factors <- renderTable({
    if (!is.null(vals$triangle_source_data)){
      cumulative_triangle <- as.triangle(get_cumulative_df(), origin = "accident_year", dev = "maturity", value = "value")
    
      ata_factors <- ata(cumulative_triangle) %>%
        as.data.frame() %>%
        filter(!is.na(value)) %>%
        mutate(maturity = (maturity * 12)) %>%
        spread(maturity, value)
    }
  })

  get_triangle <- reactive({
    get_cumulative_df() %>%
      mutate(value = number(value, big.mark = ",")) %>%
      mutate(value = paste("$", value, sep = "")) %>%
      spread(maturity, value)
  })
  
  triangle_source_data <- observeEvent(input$adjust_claims_data, {
    if(length(input$claim_listing_rows_selected) > 0){
      selected_claim_numbers <- get_selected_claim_numbers()
      vals$triangle_source_data <- vals$claims_data %>%
        filter(!(Claim.Administrator.Claim.Number %in% local(selected_claim_numbers$claim_number)))
    }
    else{
      vals$triangle_source_data <- vals$claims_data
    }
  })

  get_cumulative_df <- reactive({
    incremental_df <- vals$triangle_source_data %>%
      mutate(accident_year = as.integer(Employee.Date.of.Injury / 100)) %>%
      mutate(bill_year = as.integer(Date.of.Bill / 10000)) %>%
      mutate(maturity = 12 * (bill_year - accident_year + 1)) %>%
      group_by(accident_year, maturity) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      as.data.frame() %>%
      arrange(accident_year, maturity)
    
    cumulative_df <- incr2cum(as.triangle(incremental_df, origin = "accident_year", dev = "maturity", value = "total_paid")) %>%
      as.data.frame() %>%
      filter(!is.na(value)) %>%
      mutate(value = round(value))
  })
 
  output$claim_listing <- renderDataTable({
    get_claims_listing()
  }, options = list(scrollX = TRUE))
    
  output$selected_claim_count <- renderText({
    paste("selected claim count: ", length(input$claim_listing_rows_selected), sep = "")
  })
  
  output$selected_claim_numbers <- renderTable({
    get_selected_claim_numbers()
  })
  
  output$selected_claim_dollars <- renderTable({
    if(length(input$claim_listing_rows_selected) > 0){
      selected_claim_numbers <- get_selected_claim_numbers()
      vals$claims_data %>%
        filter(Claim.Administrator.Claim.Number %in% local(selected_claim_numbers$claim_number)) %>%
        summarize(total_paid = sum(Total.Amount.Paid.Per.Line))
    }
  })
  
  get_claims_listing <- reactive({
    vals$claims_data %>%
      mutate(accident_year = as.integer(Employee.Date.of.Injury / 100)) %>%
      group_by(Claim.Administrator.Claim.Number, accident_year) %>%
      summarize(total_paid = sum(Total.Amount.Paid.Per.Line)) %>%
      mutate(total_paid_paid_rank = rank(-total_paid)) %>%
      filter(total_paid_paid_rank <= 100) %>%
      rename("Claim #" = Claim.Administrator.Claim.Number, rank = total_paid_paid_rank) %>%
      arrange(desc(total_paid)) %>%
      as.data.frame()
  })
  
  get_selected_claim_numbers <- reactive({
    selected <- input$claim_listing_rows_selected
    claim_numbers_df <- get_claims_listing()[selected, "Claim #"]  %>%
      as.data.frame()
    colnames(claim_numbers_df) <- c("claim_number")
    claim_numbers_df
  })
   
}