# Title ----
output$em_catchreview_title <- renderText(paste0("<h4><big><b>Electronic Monitoring Feedback: Catch Reviews</b></big> (Vessel ", input$vessel, " )</h4>"))

# Input controls and data processing ----
output$catchreview_species_input <- renderUI({
  selectInput(inputId = "catchreview_species",
              label = "Species",
              choices = c("All Species", sort(unique(em_()$species))),
              selected = "All Species",
              multiple = TRUE)
  })

output$catchreview_detail_input <- renderUI({
  selectInput(inputId = "catchreview_detail",
               label = "Count Type",
               choices = c("Individual Fish Reviewed", "Reviews"))
})

output$catchreview_plottype_input <- renderUI({
  selectInput(inputId = "catchreview_plottype",
              label = "Event View",
              choices = c("Date", "Species"))
})

catchreview_data <- reactive({
  req(input$date)
  req(input$catchreview_species)
  req(input$catchreview_plottype)
  req(input$catchreview_detail)
  
  grouping_vars <- if(input$catchreview_plottype == "Date") {
    c("evtr", "species", "start.date")
  } else {c("evtr", "species")}

  df <- em_() %>%
    filter(category == c("Catch Review")) %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(count = ifelse(input$catchreview_detail == "Individual Fish Reviewed",
                             sum(quantity, na.rm = TRUE),
                             n())) %>%
    ungroup()
  
  if(!"All Species" %in% input$catchreview_species) {
    df <- df %>% filter(species %in% input$catchreview_species)
  }
  
  df
})

output$catchreview_evtr_input <- renderUI({
  selectInput(inputId = "catchreview_evtr", label = "",
              choices = unique(catchreview_data()$evtr),
              multiple = TRUE)
})

# Figure ----
catchreview_plot_prep <- reactive({
  req(input$catchreview_plottype)

  grouping_vars <- if(input$catchreview_plottype == "Date") {
    c("species", "start.date")
  } else {c("species")}
  
  use_x <- ifelse(input$catchreview_plottype == "Date", "start.date", "count")
  use_y <- ifelse(input$catchreview_plottype == "Date", "count", "species")
  use_x_lab <- ifelse(input$catchreview_plottype == "Date", "Date", "Count")
  use_y_lab <- ifelse(input$catchreview_plottype == "Date", "Count", "Species")
  
  baseplot <- catchreview_data() %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%
    ggplot() +
    geom_bar(aes(x = .data[[use_x]], y = .data[[use_y]]),
             stat = "identity", fill = "white", col = "black")
  
  plotme <- catchreview_data() %>%
    group_by(evtr, across(all_of(grouping_vars))) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%
    {if(length(input$catchreview_evtr) > 0) filter(., evtr %in% input$catchreview_evtr) else .}
  
  baseplot +
    geom_bar(aes(x = .data[[use_x]], y = .data[[use_y]], fill = species),
             stat = "identity", col = "black",
             data = plotme %>%
               group_by(across(all_of(grouping_vars))) %>%
               summarise(count = sum(count, na.rm = TRUE))) +
    fillScaleSpecies +
    xlab(use_x_lab) +
    ylab(use_y_lab) +
    theme_bw(16) +
    theme(legend.position = "bottom")
})

output$catchreview_plot <- renderPlot({
  if(nrow(catchreview_plot_prep()$data) > 0 ) {
    catchreview_plot_prep()} else {
      ggplot() + 
        annotate("text", x = 4, y = 25, size = 4,
                 label = "No data available") + 
        theme_void()        
    }
})

# Information on Click
catchreview_click_info_prep <- reactive({
  if(length(input$catchreview_click$x) > 0) {
    
    if(input$catchreview_plottype == "Date") {
      date.print <- as.Date(input$catchreview_click$x, origin = "1970-01-01") + 1
      evtr.print <- catchreview_data() %>%
        filter(start.date == ymd(date.print))
      paste("<b>Date</b>:", date.print, "<br>",
            "<b>eVTR</b>:", paste(unique(evtr.print$evtr), collapse = ", "))
    } else {
      species.print <- catchreview_plot_prep()$data$species[round(input$catchreview_click$y, 0)]
      evtr.print <- catchreview_data() %>%
        filter(species == species.print)
      paste("<b>Species</b>:", species.print, "<br>",
            "<b>eVTR</b>:", paste(unique(evtr.print$evtr), collapse = ", ")) 
    }
  } else {
    "Click on the bar plot to learn more."
  }
})

output$catchreview_click_info <- renderText({catchreview_click_info_prep()})

# Table ----
catchreview_table_prep <- reactive({
  df <- em_() %>%
    filter(category == c("Catch Review")) %>%
    select(species, quantity, start.date, start.timestamp, end.timestamp, vessel, evtr, comments) %>%
    group_by(evtr, species) %>%
    summarise(`Catch Reviews` = n(),
              `Total Individuals` = sum(quantity, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(em_() %>%
                filter(category == c("Catch Review")) %>%
                group_by(evtr) %>%
                summarise(mindate = min(start.date),
                          maxdate = max(start.date)) %>%
                mutate(`Date Range` = case_when(
                  mindate == maxdate ~ as.character(mindate),
                  mindate != maxdate ~ paste(mindate, str_sub(maxdate, start = -5), sep = " to ")))) %>%
    select(eVTR = evtr, `Date Range`, Species = species,
           `Catch Reviews`, `Total Individuals`) %>%
    {if(!"All Species" %in% input$catchreview_species) filter(., Species %in% input$catchreview_species) else .}

  df
})

output$catchreview_table <- renderDataTable({catchreview_table_prep()},
                                       rownames = FALSE,
                                       options = list(paging = FALSE,
                                                      dom = "ft",
                                                      ordering = FALSE,
                                                      initComplete = JS(
                                                        "function(settings, json) {",
                                                        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                        "}")))


# Downloads ----
output$export_catchreview_data <- downloadHandler(
  filename = function() {
    paste("catchreview_data.csv", sep = "")
  },
  content = function(file) {
    write.csv(em_() %>%
                filter(category == c("Catch Review")) %>%
                mutate(eVTR = paste0("eVTR", evtr)) %>%
                select(eVTR, `Start Date` = start.date,
                       Vessel = vessel,
                       `Start Timestamp` = start.timestamp,
                       `End Timestamp` = end.timestamp,
                       Species = species,
                       Quantity = quantity,
                       Details = comments),
              file, row.names = FALSE)
  }
)

output$export_catchreview_table <- downloadHandler(
  filename = function() {
    paste("catchreview_table.csv", sep = "")
  },
  content = function(file) {
    write.csv(catchreview_table_prep() %>%
                mutate(eVTR = paste0("eVTR", eVTR)),
              file, row.names = FALSE)
  }
)

output$export_catchreview_plot <- downloadHandler(
  filename = function() {
    paste("catchreview_figure.png")
  },
  content = function(file) {
    saveplot <- catchreview_plot_prep()
    ggsave(file, plot = saveplot,
           width = 8.5, height= 11, units = "in",
           dpi = 300)
  }
)