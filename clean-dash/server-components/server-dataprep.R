# Sidebar Panel Data Filters ----
# Select vessel
output$sidebar_vessel_input <- renderUI({
  selectInput(inputId = "vessel",
              label = "Select Vessel",
              choices = c(sort(unique(evtr$vessel))))
})

# Subset data to vessel
evtr_full <- reactive({
  req(input$vessel)
  evtr %>% filter(vessel == input$vessel)
  })

em_full <- reactive({
  req(input$vessel)
  em %>% filter(vessel == input$vessel)
  })

simm_full <- reactive({
  req(input$vessel)
  simm %>% filter(vessel == input$vessel)
  })

# Select date range
output$sidebar_date_input <- renderUI({
  datesub <- c(unique(evtr_full()$usedate), unique(em_full()$start.date), unique(simm_full()$date.sold))
  dateRangeInput(inputId = "date", label = "Date Range",
                 min = min(datesub, na.rm = TRUE),
                 max = max(datesub, na.rm = TRUE),
                 start = max(datesub, na.rm = TRUE) - 90,
                 end = max(datesub, na.rm = TRUE))
})

# Subset data to date range
evtr_ <- reactive(evtr_full() %>% filter(between(usedate, input$date[1], input$date[2])) %>% ungroup())
em_ <- reactive(em_full() %>% filter(between(start.date, input$date[1], input$date[2])) %>% ungroup())
simm_ <- reactive(simm_full() %>% filter(between(date.sold, input$date[1], input$date[2])) %>% ungroup())
enviro_data_ <- reactive(enviro_data %>% filter(between(usedate, input$date[1], input$date[2])))