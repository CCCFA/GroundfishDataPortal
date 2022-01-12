# Title ----
output$dealer_compare_title <- renderText(paste0("<h4><big><b>Compare Landings Data to eVTR Data</b></big> (Vessel ", input$vessel, " )</h4>"))

# Input controls and data processing ----
output$compare_port_input <- renderUI({
  selectInput(inputId = "compare_port",
              label = "Port of Sale",
              choices = c("All Ports",
                          unique(c(evtr_()$port.land, simm_()$port.land))),
              selected = "All Ports",
              multiple = TRUE)
})

output$compare_species_input <- renderUI({
  selectInput(inputId = "compare_species",
              label = "Species",
              choices = c("All Species",
                          sort(unique(c(evtr_()$species, simm_()$species)))),
              selected = "All Species",
              multiple = TRUE)
})

output$compare_match_input <- renderUI({
  radioButtons(inputId = "compare_match", label = "Match on",
               choices = c("eVTR", "eVTR and Port"),
               selected = "eVTR",
               inline  = TRUE)
})

compare_data <- reactive({
  req(input$date)
  req(input$compare_species)
  
  df1 <- evtr_() %>%
    select(eVTR = vtr,
           `eVTR Date` = usedate, `eVTR Port` = port.land,
           Species = species, kept) %>%
    group_by(eVTR, Species, `eVTR Date`, `eVTR Port`) %>%
    summarise(`eVTR Weight` = sum(kept, na.rm = TRUE))
  
  df2 <- simm_() %>%
    select(eVTR = vtr,
           `Dealer Date` = date.sold, `Dealer Port` = port.land,
           Species = species, landed.weight) %>%
    group_by(eVTR, Species, `Dealer Date`, `Dealer Port`) %>%
    summarise(`Dealer Weight` = sum(landed.weight, na.rm = TRUE))
  
  df <- df1 %>%
    left_join(df2)
  
  if(input$compare_species != "All Species") {
    df <- df %>% filter(Species %in% input$compare_species)
  }
  
  if(input$compare_port != "All Ports") {
    df <- df %>% filter(`eVTR Port` %in% input$compare_port |
                          `Dealer Port` %in% input$compare_port)
  }
  
  df
})

# Figure ----
compare_plot_prep <- reactive({
  req(input$compare_match)
  
  compare_data() %>%
    {if(input$compare_match == "eVTR and Port") filter(., `eVTR Port` == `Dealer Port`) else .} %>%
    group_by(eVTR) %>%
    summarise(`Weight Sold (Landed)` = sum(`Dealer Weight`, na.rm = TRUE),
              `Weight Reported (eVTR)` = sum(`eVTR Weight`, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_longer(cols = c(`Weight Sold (Landed)`, `Weight Reported (eVTR)`), names_to = "Metric", values_to = "Reported") %>%
    ggplot() +
    geom_bar(aes(x = as.factor(eVTR), y = Reported, fill = Metric),
             position = "dodge", stat = "identity", col = "black") +
    fillScaleMetric +
    coord_flip() +
    xlab("eVTR") +
    ylab("Weight") +
    theme_bw(16) +
    theme(legend.position = "bottom")
})

output$compare_plot <- renderPlot({
  req(input$date)
  if(nrow(compare_plot_prep()$data) > 0) {
    compare_plot_prep()  
  } else {
    ggplot() + 
      annotate("text", x = 4, y = 25, size = 4,
               label = "No matches have been found between the eVTR data and the dealer data") + 
      theme_void()
  }
  })

# Table ----
output$compare_table_res_input <- renderUI({
  radioButtons(inputId = "compare_table_res",
               label = "Table Detail",
               choices = c("Overall", "By Port", "By Species", "By Port and Species"),
               selected = "Overall",
               inline = TRUE)
})

compare_table_prep <- reactive({
  req(input$compare_table_res)
  
  grouping_vars <- if(input$compare_table_res == "By Species") {
    c("eVTR", "Species")
  } else { if(input$compare_table_res == "By Port") {
    c("eVTR", "eVTR Port", "Dealer Port")
  } else { if(input$compare_table_res == "By Port and Species") {
    c("eVTR", "Species", "eVTR Port", "Dealer Port")
  } else {c("eVTR")
  }
  }
  }
  
  compare_data() %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(`eVTR Weight` = sum(`eVTR Weight`, na.rm = TRUE),
              `Dealer Weight` = sum(`Dealer Weight`, na.rm = TRUE)) %>%
    arrange(desc(eVTR))
})

output$compare_table <- renderDataTable({
  compare_table_prep()},
rownames = FALSE,
options = list(paging = FALSE,
               dom = "ft",
               ordering = FALSE,
               initComplete = JS(
                 "function(settings, json) {",
                 "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                 "}"))
)
 
# Downloads ----
output$export_compare_data <- downloadHandler(
  filename = function() {
    paste("compare_data.csv", sep = "")
  },
  content = function(file) {
    write.csv(compare_data(),
              file, row.names = FALSE)
  }
)

output$export_compare_plot <- downloadHandler(
  filename = function() {
    paste("compare_figure.png")
  },
  content = function(file) {
    saveplot <- compare_plot_prep()
    ggsave(file, plot = saveplot,
           width = 8.5, height= 11, units = "in",
           dpi = 300)
  })

output$export_compare_table <- downloadHandler(
  filename = function() {
    paste("compare_table.csv", sep = "")
  },
  content = function(file) {
    write.csv(compare_table_prep()  %>%
                mutate(eVTR = paste0("eVTR", eVTR)),
              file, row.names = FALSE)
  }
)