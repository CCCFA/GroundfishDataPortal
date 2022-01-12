# Title ----
output$evtr_effort_title <- renderText(paste0("<h4><big><b>eVTR Effort Data</b></big> (Vessel ", input$vessel, " )</h4>"))

# Input controls and data processing ----
output$effort_gear_input <- renderUI({
  selectInput(inputId = "effort_gear", label = "Gear",
              choices = unique(evtr_() %>%
                                 filter(gear != "Other") %>%
                                 pull(gear) %>%
                                 unique()))
})

effort_data <- reactive({
  req(input$date)
  req(input$effort_gear)
  
  df <- evtr_() %>%
    filter(gear == input$effort_gear)
  if(input$effort_gear %in% c("Jig", "Trawl", "Gillnet")) {
    df <- df %>%
      select(vtr, gear, usedate, lat, lon, gearqty,
             gearsize, haulstarttime, haulendtime) %>%
      distinct() %>%
      mutate(haultime = as.numeric(difftime(ymd_hms(haulendtime), ymd_hms(haulstarttime)))) %>%
      select(-haulstarttime, -haulendtime) 
  } else {
    if(input$effort_gear %in% c("Lobster Pot", "Longline")) {
      df <- df %>%
        select(vtr, gear, usedate, lat, lon, gearqty) %>%
        distinct()
    } 
  }
  df
})

output$effort_evtr_input <- renderUI({
  selectInput(inputId = "effort_evtr",
              label = "",
              choices = unique(effort_data()$vtr),
              multiple = TRUE)
})

# Figure ----
effort_plot_prep <- reactive({
  req(input$effort_gear)
  
  if(input$effort_gear %in% c("Jig", "Trawl", "Gillnet")) {
    baseplot <- effort_data() %>%
      group_by(usedate) %>%
      summarise(haultime = sum(haultime, na.rm = TRUE)) %>%
      ggplot() +
      geom_bar(aes(x = usedate, y = haultime),
               width = 1,
               fill = "white", stat = "identity", col = "black") +
      ylab("Hours")
  } else {
    if(input$effort_gear %in% c("Lobster Pot", "Longline")) {
      baseplot <- effort_data() %>%
        group_by(usedate) %>%
        summarise(gearqty = sum(gearqty, na.rm = TRUE)) %>%
        ggplot() +
        geom_bar(aes(x = usedate, y = gearqty),
                 width = 1,
                 fill = "white", stat = "identity", col = "black") +
        ylab("Quantity")
    }
  }
  
  plotme <- effort_data() %>%
    {if(length(input$effort_evtr) > 0) filter(., vtr %in% input$effort_evtr) else .}

  if(input$effort_gear %in% c("Jig", "Trawl", "Gillnet")) {
    myplot <- baseplot +
      geom_bar(aes(x = usedate, y = haultime, fill = gear),
               width = 1,
               stat = "identity", col = "black",
               data = plotme %>%
                 group_by(usedate, gear) %>%
                 summarise(haultime = sum(haultime, na.rm = TRUE))) +
      ylab("Hours")
  }
  if(input$effort_gear %in% c("Lobster Pot", "Longline")) {
    myplot <- baseplot +
      geom_bar(aes(x = usedate, y = gearqty, fill = gear),
               width = 1,
               stat = "identity", col = "black",
               data = plotme %>%
                 group_by(usedate, gear) %>%
                 summarise(gearqty = sum(gearqty, na.rm = TRUE))) +
      ylab("Quantity")
  }
  
  myplot +
    theme_bw(16) +
    xlab("Date") +
    scale_x_date(limits = c(input$date[1]-1, input$date[2]+1)) +
    fillScaleGearType +
    theme(legend.position = "none")
})

output$effort_plot <- renderPlot({
  req(input$date)
  if(nrow(effort_plot_prep()$data) > 0) {
    effort_plot_prep()  
  } else {
    ggplot() + 
      annotate("text", x = 4, y = 25, size = 4,
               label = "No data available") + 
      theme_void()
  }
})

# Information on Click
effort_click_info_prep <- reactive({
  if(length(input$effort_click$x) > 0) {
    date.print <- as.Date(input$effort_click$x, origin = "1970-01-01") + 1
    evtr.print <- effort_data() %>%
      filter(usedate == ymd(date.print))
    paste("<b>Date</b>:", date.print, "<br>",
          "<b>eVTR</b>:", paste(unique(evtr.print$vtr), collapse = ", "))
  } else {
    "Click on the bar plot to learn more."
  }
})

output$effort_click_info <- renderText({effort_click_info_prep()})

# Map ----
evtr_effort_map_prep <- reactive({
  
  mapme_heat <- effort_data() %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    group_by(lat, lon) %>%
    {if(input$effort_gear %in% c("Jig", "Trawl", "Gillnet"))
      summarise(., use_value = sum(haultime, na.rm = TRUE)
                else .} %>%
    {if(input$effort_gear %in% c("Lobster Pot", "Longline"))
      summarise(., use_value = sum(gearqty, na.rm = TRUE)
                else .}
  
  mapme_points <- effort_data() %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    group_by(vtr, gear, lat, lon) %>%
    {if(input$effort_gear %in% c("Jig", "Trawl", "Gillnet"))
      summarise(., use_value = sum(haultime, na.rm = TRUE)
                else .} %>%
    {if(input$effort_gear %in% c("Lobster Pot", "Longline"))
      summarise(., use_value = sum(gearqty, na.rm = TRUE)
                else .} %>%
    filter(use_value > 0) %>%
    mutate(pop = paste0("<b>eVTR:</b>", vtr,
                        "<br><b>Lat:</b> ", round(lat, 2),
                        "<b>  Lon:</b> ", round(lon, 2),
                        "<br><b>",
                        ifelse(input$effort_gear %in% c("Jig", "Trawl", "Gillnet"),
                               "Hours",
                               "Quantity"),
                        ":</b> ", use_value))
    
 if(nrow(mapme_heat) > 0) {
    mymap <- leaflet(data = mapme_heat) %>%
      addTiles() %>%
      addHeatmap(blur = 35,
                 gradient = "Spectral",
                 lat = ~lat, lng = ~lon) %>%
      addCircleMarkers(radius = 1, color = "black",
                       lat = ~lat, lng = ~lon,
                       popup = ~pop,
                       data = mapme_points)
    if(length(input$effort_evtr) > 0) {
      mymap <- mymap %>%
        addCircleMarkers(radius = 1, color = "gold",
                         lat = ~lat, lng = ~lon,
                         popup = ~pop,
                         data = mapme_points %>%
                           filter(vtr %in% input$effort_evtr))
    }
  } else {
    mymap <- leaflet() %>%
      addControl("No data available", position = "topright")
  }
  mymap
})

output$evtr_effort_map <- renderLeaflet(evtr_effort_map_prep() %>%
                                          addResetMapButton() %>%
                                          fitBounds(-72, 40, -67, 43))

# Table ----
effort_table_prep <- reactive({
  return_table <- evtr_() %>%
    filter(gear != "Other") %>%
    select(vtr, usedate, gear, gearqty, gearsize, haulstarttime, haulendtime) %>%
    distinct() %>%
    mutate(haultime = as.numeric(difftime(ymd_hms(haulendtime), ymd_hms(haulstarttime))),
           usevalue = case_when(
             gear %in% c("Lobster Pot", "Longline") ~ as.double(gearqty),
             gear %in% c("Trawl", "Jig", "Gillnet") ~ as.double(haultime)
           )) %>%
    group_by(vtr, gear, usedate) %>%
    summarise(usevalue = sum(usevalue, na.rm = TRUE)) %>%
    select(eVTR = vtr, gear,`Date Fished` = usedate, usevalue) %>%
    pivot_wider(names_from = gear, values_from = usevalue)
  
  mycols <- c("Gillnet", "Lobster Pot",
              "Longline", "Jig", "Trawl")
  
  return_table[mycols[!(mycols %in% colnames(return_table))]] <- NA
  return_table <- return_table %>%
    select(eVTR, `Date Fished`, Gillnet, `Lobster Pot`, Longline, Jig, Trawl)
  
  if(length(input$effort_evtr) > 0) {
    return_table <- return_table %>%
      filter(eVTR %in% input$effort_evtr)
  }
  
  return_table
})

output$effort_table <- renderDataTable({effort_table_prep() %>%
    mutate(eVTR = as.character(eVTR)) %>%
    bind_rows(tibble(eVTR = "Total",
                     Gillnet = sum(effort_table_prep()$Gillnet, na.rm = TRUE),
                     `Lobster Pot` = sum(effort_table_prep()$`Lobster Pot`, na.rm = TRUE),
                     Longline = sum(effort_table_prep()$Longline, na.rm = TRUE),
                     Jig = sum(effort_table_prep()$Jig, na.rm = TRUE),
                     Trawl = sum(effort_table_prep()$Trawl, na.rm = TRUE)))},
    rownames = FALSE,
    options = list(paging = FALSE,
                   dom = "ft",
                   ordering = FALSE,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                     "}")))

# Downloads ----
output$export_effort_data <- downloadHandler(
  filename = function() {
    paste("effort_data.csv")
  },
  content = function(file) {
    write.csv(effort_data() %>%
                mutate(eVTR = paste0("eVTR", vtr)) %>%
                left_join(evtr %>% select(vtr, date.sail, date.land)) %>%
                rename("date.general" = "usedate") %>%
                distinct() %>%
                select(eVTR, starts_with("date"), everything(), -vtr),
              file, row.names = FALSE)
  }
)

output$export_effort_plot <- downloadHandler(
  filename = function() {
    paste("effort_figure.png")
  },
  content = function(file) {
    saveplot <- effort_plot_prep()
    ggsave(file, plot = saveplot,
           width = 8.5, height= 11, units = "in",
           dpi = 300)
  }
)

output$export_effort_table <- downloadHandler(
  filename = function() {
    paste("effort_table.csv", sep = "")
  },
  content = function(file) {
    write.csv(effort_table_prep() %>%
                mutate(eVTR = paste0("eVTR", eVTR)), file, row.names = FALSE)
  }
)