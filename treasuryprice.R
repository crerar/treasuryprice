############### Bond specifications ################
isin_list = c("US912810SQ22","US912810SN90")
cpn_list = c(1.125, 1.250)

payment_dates_1=c("2024-02-15","2024-08-15","2025-02-15","2025-08-15","2026-02-15","2026-08-15","2027-02-15","2027-08-15","2028-02-15",
                  "2028-08-15","2029-02-15","2029-08-15","2030-02-15","2030-08-15","2031-02-15","2031-08-15","2032-02-15","2032-08-15",
                  "2033-02-15","2033-08-15","2034-02-15","2034-08-15","2035-02-15","2035-08-15","2036-02-15","2036-08-15","2037-02-15",
                  "2037-08-15","2038-02-15","2038-08-15","2039-02-15","2039-08-15","2040-02-15","2040-08-15")

payment_dates_2=c("2023-11-15","2024-05-15","2024-11-15","2025-05-15","2025-11-15","2026-05-15","2026-11-15","2027-05-15","2027-11-15",
                  "2028-05-15","2028-11-15","2029-05-15","2029-11-15","2030-05-15","2030-11-15","2031-05-15","2031-11-15","2032-05-15",
                  "2032-11-15","2033-05-15","2033-11-15","2034-05-15","2034-11-15","2035-05-15","2035-11-15","2036-05-15","2036-11-15",
                  "2037-05-15","2037-11-15","2038-05-15","2038-11-15","2039-05-15","2039-11-15","2040-05-15","2040-11-15","2041-05-15",
                  "2041-11-15","2042-05-15","2042-11-15","2043-05-15","2043-11-15","2044-05-15","2044-11-15","2045-05-15","2045-11-15",
                  "2046-05-15","2046-11-15","2047-05-15","2047-11-15","2048-05-15","2048-11-15","2049-05-15","2049-11-15","2050-05-15")

payment_dates_list = list(payment_dates_1,payment_dates_2)

############### Calculating function ################
TreasuryPrice = function(yield,isin,settle_date,principal){
  
  pick = which(isin_list == isin)
  
  cpn_schedule_raw = as.Date(payment_dates_list[[pick]])
  cpn_schedule = cpn_schedule_raw[which(cpn_schedule_raw > settle_date)]
  previous_cpn_date = tail(cpn_schedule_raw[which(cpn_schedule_raw <= settle_date)],n = 1)
  
  cpn_amount = rep(cpn_list[pick]/100/2*principal,length(cpn_schedule))
  
  first_remaining_days = cpn_schedule[1]-settle_date
  first_act = cpn_schedule[1]-previous_cpn_date
  act_act_first = as.numeric(first_remaining_days)/as.numeric(first_act)
  act_act = c(0:(length(cpn_schedule)-1))+act_act_first
  
  DF = 1/(1+yield/100/2)^act_act
  PV_cpn = cpn_amount*DF
  PV_principal = principal*DF[length(DF)]
  accrued_interest = cpn_amount[1]*as.numeric(settle_date - previous_cpn_date)/as.numeric(cpn_schedule[1] - previous_cpn_date)
  total = sum(PV_cpn) + PV_principal #+ accrued_interest
  
  print(total)
  
}

############### shiny app ################
library(shiny)

ui= shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("U.S.Treasury Price Calculator"),
  
  sidebarPanel(
    width = 3,
    span(tags$i(h6("Select the options")), style="color:#045a8d"),
    
    # Input variables
    selectInput(inputId = "selected_isin",
                label = "ISIN Code",
                choices = isin_list,
                selected = isin_list[1],
                multiple = F),
    
    numericInput(inputId = "selected_yield",
                 label = "Yield(%)",
                 value = 4.78,
                 step = 0.01),
    
    dateInput(inputId = "selected_settle_date",
              label = "Settlement Date",
              value = Sys.Date(),
              min = "2024-01-01", max = "2034-05-01",
              startview = "year"),
    
    numericInput(inputId = "selected_principal",
                 label = "Principal Amount in USD",
                 value = 1000000),
    
    numericInput(inputId = "selected_usdkrw",
                 label = "USD/KRW",
                 value = 1350,
                 step = 1),
    
  ),
  
  # Output
  mainPanel(
    br(),
    p(strong("Yield vs Price"), style="color:#045a8d"),
    tableOutput(outputId = "output_treasuryprice")
  )
  
))


server=shinyServer(function(input, output) {
  
  output$output_treasuryprice <- renderTable({
    
    span = c(-10:10)/100
    Price_in_USD = NULL
    for(i in 1:length(span)){
      Price_in_USD[i] = TreasuryPrice(input$selected_yield + span[i],
                                      input$selected_isin,
                                      input$selected_settle_date,
                                      input$selected_principal)
    }
    
    Yield = input$selected_yield+span
    Price_in_KRW = format(round(Price_in_USD*input$selected_usdkrw,0), big.mark = ",", scientific = FALSE)
    price_change_in_USD = format(round(c(0,diff(Price_in_USD)),0), big.mark = ",", scientific = FALSE)
    price_change_in_KRW = format(round(c(0,diff(Price_in_USD*input$selected_usdkrw)),0), big.mark = ",", scientific = FALSE)
    
    data.frame(Yield, Price_in_USD,price_change_in_USD,Price_in_KRW,price_change_in_KRW)
    
  })
  
})


shinyApp(ui=ui,server=server)
