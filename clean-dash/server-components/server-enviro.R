# Title ----
output$evtr_enviro_title <- renderText(paste0("<h4><big><b>eVTR vs Environmental Data</b></big> (Vessel ", input$vessel, " )</h4>"))

# Input controls and data processing ----
output$enviro_category_input <- renderUI({
  selectInput(inputId = "enviro_category",
              label = "Category",
              choices = c("Temperature" = "temperature",
                          "Conductivity" = "conductivity",
                          "Salinity" = "salinity"))
})

output$enviro_type_input <- renderUI({
  radioButtons(inputId = "enviro_type",
               label = "Measurement",
               choices = c("Minimum" = "min",
                           "Average" = "mean",
                           "Maximum" = "max"),
               selected = "mean",
               inline = TRUE)
})

output$enviro_depth_input <- renderUI({
  radioButtons(inputId = "enviro_depth",
               label = "Depth (m)",
               choices = sort(unique(enviro_data_()$depth)),
               selected = 1,
               inline = TRUE)
})

# Station Map ----
station_map_prep <- reactive({
  req(input$enviro_depth)
  map <- leaflet(enviro_data_() %>%
                   filter(depth == input$enviro_depth) %>%
                   group_by(station) %>%
                   slice(1) %>%
                   mutate(hover = paste0(name, " (", station, ")"))) %>%
    addTiles() %>%
    addCircleMarkers(lat = ~latitude, lng = ~longitude,
                     layerId = ~station,
                     label = ~hover)
  if(length(input$station_map_marker_click$id) == 1) {
    map <- map %>%
      addCircleMarkers(lat = ~latitude, lng = ~longitude,
                       label = ~hover,
                       labelOptions = labelOptions(noHide = TRUE),
                       color = "black",
                       data = enviro_data_() %>%
                         filter(depth == input$enviro_depth,
                                station == input$station_map_marker_click$id) %>%
                         group_by(station) %>%
                         slice(1) %>%
                         mutate(hover = paste0(name, " (", station, ")")))
  }
  map
})

output$station_map <- renderLeaflet(station_map_prep() %>%
                                      addResetMapButton())

# Information on Click ----
observeEvent(input$station_map_click, {
  click <- input$station_map_click
  text <- paste("Lat ", click$lat, "Lon ", click$lng)
})

station_details <- reactive({
  req(input$station_map_marker_click)
  enviro_data_() %>%
    filter(station == input$station_map_marker_click$id) %>%
    select(station, name, link) %>%
    distinct()
})

output$select_station_message <- renderUI({
  if(length(input$station_map_marker_click$id) == 1) {
    HTML(paste0('<big><b>Station: </b><a href = "', station_details()$link,
                '", target="_blank">',
                input$station_map_marker_click$id, '</a></big><br>',
                "<big><b>Location Name: </b>", station_details()$name, "</big>"))
  } else {
    HTML("")
  }
})

output$warning_message <- renderUI({
  if(length(input$station_map_marker_click$id) == 1) {
    HTML("")
  } else {
    HTML("<big><i>Before continuing with the options below, please select a station by clicking one of the blue points on the map above.</i></big>")
  }
})

# Tab-Specific Data Filtering & Processing 1 ----
station_data <- reactive({
  req(input$enviro_depth)

  enviro_data_() %>%
    filter(depth == input$enviro_depth) %>%
    mutate(yval = .data[[paste(input$enviro_category, input$enviro_type, sep = "_")]]) %>%
    filter(!yval %in% c(Inf, -Inf))
})

# Figure 1 ----
enviro_plot_prep <- reactive({
  req(input$enviro_depth)
  
  myplot <- ggplot(station_data()) +
    geom_line(aes(x = usedate, y = yval, col = station)) +
    theme_bw(16) +
    colScaleStation +
    ylab(str_to_title(input$enviro_category)) +
    xlab("Date") +
    theme(legend.position = "bottom")
  
  if(length(input$station_map_marker_click$id) == 1) {
    myplot <- myplot + geom_line(aes(x = usedate, y = yval, col = station),
                                 size = 3,
                                 data = station_data() %>%
                                   filter(station == input$station_map_marker_click$id))
  }
  myplot
})

output$enviro_plot <- renderPlot(enviro_plot_prep())

# Tab-Specific Data Filtering & Processing 2 ----
output$enviro_catchtype_input <- renderUI({
  selectInput(inputId = "enviro_catchtype",
              label = "Catch Type",
            choices = c("All" = "all",
                        "Kept" = "kept",
                        "Discarded" = "discarded"))
})

output$enviro_species_input <- renderUI({
  selectInput(inputId = "enviro_species", label = "Species",
              choices = c("All Species", unique(evtr_()$species)),
              selected = "All Species",
              multiple = TRUE)
})

output$enviro_resolution_input <- renderUI({
  radioButtons(inputId = "enviro_resolution",
               label = "Aggregate Data By",
               choices = c("Day", "Week", "Month"),
               selected = "Day", inline = TRUE)
})

evtr_enviro_data <- reactive({
  req(input$date)
  req(input$enviro_species)
  req(input$enviro_resolution)
  
  grouping_vars <- if(input$enviro_resolution == "Day") {
    c("usedate", "species") } else {
      if(input$enviro_resolution == "Week") {
        c("week", "species") } else {
          c("month", "species")}}

  df <- evtr_() %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(kept = sum(kept, na.rm = TRUE),
              discarded = sum(discarded, na.rm = TRUE),
              total = case_when(
                !is.na(kept) & !is.na(discarded) ~ kept + discarded,
                !is.na(kept) & is.na(discarded) ~ kept,
                is.na(kept) & !is.na(discarded) ~ discarded
              )) %>%
    ungroup()
  
  if(input$enviro_resolution == "Day") {
    df <- df %>%
      left_join(station_data() %>% filter(station == input$station_map_marker_click$id)) %>%
      mutate(display_usedate = usedate)
  }
  
  if(input$enviro_resolution == "Week") {
    df <- df %>%
      left_join(station_data() %>%
                  filter(station == input$station_map_marker_click$id) %>%
                  group_by(station, week) %>%
                  summarise(yval = mean(yval, na.rm = TRUE))) %>%
      mutate(display_usedate = paste("Week:", week))
  }
  
  if(input$enviro_resolution == "Month") {
    df <- df %>%
      left_join(station_data() %>%
                  filter(station == input$station_map_marker_click$id) %>%
                  group_by(station, month) %>%
                  summarise(yval = mean(yval, na.rm = TRUE))) %>%
      mutate(display_usedate = paste("Month:", month))
  }
  
  df %>%
    {if(!"All Species" %in% input$enviro_species) filter(., species %in% input$enviro_species) else .}
})

# Figure 2 ----
enviro_catch_plot_prep <- reactive({
  usecol <- ifelse(input$enviro_catchtype %in% c("kept", "discarded"),
                   input$enviro_catchtype,
                   "total")
  
  ggplot(evtr_enviro_data()) +
    geom_point(aes_string(x = "yval", y = usecol, col = "species"),
               size = 3, alpha = 0.7) +
    geom_line(aes_string(x = "yval", y = usecol, col = "species"),
              size = 0.5, alpha = 0.7) +
    theme_bw(16) +
    theme(legend.position = "bottom") +
    ylab(paste(str_to_title(input$enviro_catchtype), "(Weight (lbs))")) +
    xlab(str_to_title(paste0(input$enviro_category, " (", input$enviro_type, ")"))) +
    colScaleSpecies
})

output$enviro_catch_plot <- renderPlot({
  if(length(input$station_map_marker_click$id) == 1) {
    enviro_catch_plot_prep()} else {
      ggplot() + 
        annotate("text", x = 4, y = 25, size = 4,
                 label = "Please select a station from the map above") + 
        theme_void()        
    } 
})

# Table ----
catch_enviro_table_prep <- reactive({
  returntable <- evtr_enviro_data() %>%
    mutate(yval = round(yval, 2)) %>%
    select(`Date Fished` = display_usedate,
           Station = station,
           `Environmental Metric` = yval,
           Species = species, 
           Kept = kept, Discarded = discarded, Total = total)
  
  returntable
})

output$enviro_table <- renderDataTable({
  if(length(input$station_map_marker_click$id) == 1) {
    catch_enviro_table_prep() %>%
      bind_rows(tibble(`Date Fished` = NA,
                       Station = "Mean",
                       `Metric` = round(mean(catch_enviro_table_prep()$`Environmental Metric`, na.rm = TRUE), 2),
                       Species = "Total",
                       Kept = sum(catch_enviro_table_prep()$Kept, na.rm = TRUE),
                       Discarded = sum(catch_enviro_table_prep()$Discarded, na.rm = TRUE),
                       Total = sum(catch_enviro_table_prep()$Total, na.rm = TRUE)))} else {
                         tibble(`Please select a station from the map above` = NA)        
                       }
},
rownames = FALSE,
options = list(paging = FALSE,
               dom = "ft",
               ordering = FALSE,
               initComplete = JS(
                 "function(settings, json) {",
                 "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                 "}")))

# Downloads ----
output$export_enviro_data <- downloadHandler(
  filename = function() {
    paste("enviro_data.csv")
  },
  content = function(file) {
    write.csv(enviro_data_() %>% rename(date = usedate),
              file, row.names = FALSE)
  }
)

output$export_enviro_plot <- downloadHandler(
  filename = function() {
    paste("enviro_figure.png")
  },
  content = function(file) {
    saveplot <- enviro_plot_prep()
    ggsave(file, plot = saveplot,
           width = 8.5, height= 11, units = "in",
           dpi = 300)
  })

# Catch related downloads
# Data
output$catch_enviro_data_available <- renderUI({
  if(length(input$station_map_marker_click$id) == 1) {
    downloadButton("export_catch_enviro_data", "Export Catch Data")}
})

output$export_catch_enviro_data <- downloadHandler(
  filename = function() {
    paste("catch_enviro_data.csv")
  },
  content = function(file) {
    write.csv(evtr_enviro_data() %>%
                select(starts_with("date"),
                       everything(),
                       -yval, -display_usedate),
              file, row.names = FALSE)
  }
)

# Plot
output$catch_enviro_plot_available <- renderUI({
  if(length(input$station_map_marker_click$id) == 1) {
    downloadButton("export_catch_enviro_plot", "Export Catch Plot")}
})

output$export_catch_enviro_plot <- downloadHandler(
  filename = function() {
    paste("catch_enviro_figure.png")
  },
  content = function(file) {
    saveplot <- enviro_catch_plot_prep()
    ggsave(file, plot = saveplot,
           width = 8.5, height= 11, units = "in",
           dpi = 300)
  })

# Table
output$catch_enviro_table_available <- renderUI({
  if(length(input$station_map_marker_click$id) == 1) {
    downloadButton("export_catch_enviro_table", "Export Catch Table")}
})

output$export_catch_enviro_table <- downloadHandler(
  filename = function() {
    paste("catch_enviro_table.csv", sep = "")
  },
  content = function(file) {
    write.csv(catch_enviro_table_prep(),
              file, row.names = FALSE)
  }
)

