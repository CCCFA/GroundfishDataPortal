# Title ----
output$evtr_catch_title <- renderText({paste0("<h4><big><b>eVTR Catch Data</big></b> (Vessel ", input$vessel, " )</h4>")})

# Input controls and data processing ----
output$catch_catchtype_input <- renderUI({
  selectInput(inputId = "catch_catchtype",
              label = "Catch Type",
              choices = c("All", "Kept", "Discarded"))
})

output$catch_gear_input <- renderUI({
  selectInput(inputId = "catch_gear", label = "Gear",
              choices = c("All Gear", unique(evtr_()$gear)),
              selected = "All Gear", multiple = TRUE)
})

output$catch_species_input <- renderUI({
  selectInput(inputId = "catch_species", label = "Species",
              choices = c("All Species", unique(evtr_()$species)),
              selected = "All Species",
              multiple = TRUE)
})

output$catch_fill_input <- renderUI({
  selectInput(inputId = "catch_fill",
              label = "Fill Based On",
              choices = c("Catch Type", "Gear Type", "Species"))
})

output$catch_table_res_input <- renderUI({
  radioButtons(inputId = "catch_table_res",
               label = "Table Detail",
               choices = c("Overall", "By Species", "By Gear"),
               selected = "Overall",
               inline = TRUE)
})

catch_data <- reactive({
  req(input$catch_species)
  req(input$date)
  req(input$catch_gear)
  req(input$catch_catchtype)
  
  df <- evtr_() %>%
    group_by(usedate, gear, species, lat, lon, vtr) %>%
    summarise(kept = sum(kept, na.rm = TRUE),
              discarded = sum(discarded, na.rm = TRUE),
              total = case_when(
                !is.na(kept) & !is.na(discarded) ~ kept + discarded,
                !is.na(kept) & is.na(discarded) ~ kept,
                is.na(kept) & !is.na(discarded) ~ discarded
              )) %>%
    ungroup()
  
  if(!"All Species" %in% input$catch_species) {
    df <- df %>% filter(species %in% input$catch_species)
  }
  
  if(!"All Gear" %in% input$catch_gear) {
    df <- df %>% filter(gear %in% input$catch_gear)
  }
  
  if(input$catch_catchtype == "All") {df <- df %>% mutate(use_col = total)} else {
    if(input$catch_catchtype == "Kept") {df <- df %>% mutate(use_col = kept)} else {
      df <- df %>% mutate(use_col = discarded)
    }
  }
  
  df
})

output$catch_evtr_input <- renderUI({
  selectInput(inputId = "catch_evtr", label = "",
              choices = unique(catch_data()$vtr),
              multiple = TRUE)
})

# Figure ----
catch_plot_prep <- reactive({
  req(input$catch_fill)
  req(input$catch_species)
  req(input$catch_gear)
  req(input$catch_catchtype)
  
  baseplot <- catch_data() %>%
    group_by(usedate) %>%
    summarise(use_col = sum(use_col, na.rm = TRUE)) %>%
    ggplot() +
    geom_bar(aes(x = usedate, y = use_col),
             width = 1,
             stat = "identity", fill = "white", col = "black")
  
  grouping_vars <- if(input$catch_fill == "Gear Type") {
    c("usedate", "gear")
  } else {
    if(input$catch_fill == "Species") {
      c("usedate", "species")
    } else {
      c("usedate")
    }
  }
  
  plotme <- catch_data() %>%
    {if(length(input$catch_evtr) > 0) filter(., vtr %in% input$catch_evtr) else .} %>%
    group_by(across(all_of(grouping_vars)))
    
  if(input$catch_fill == "Gear Type") {
    myplot <- baseplot +
      geom_bar(aes(x = usedate, y = use_col, fill = gear),
               width = 1,
               data = plotme %>%
                 summarise(use_col = sum(use_col, na.rm = TRUE)),
               stat = "identity", col = "black") +
      fillScaleGearType
  }
  
  if(input$catch_fill == "Species") {
    myplot <- baseplot +
      geom_bar(aes(x = usedate, y = use_col, fill = species),
               width = 1,
               data = plotme %>%
                 summarise(use_col = sum(use_col, na.rm = TRUE)),
               stat = "identity", col = "black") +
      fillScaleSpecies
  }
  
  if(input$catch_fill == "Catch Type") {
    plotme <- plotme %>%
      summarise(Kept = sum(kept, na.rm = TRUE),
                Discarded = sum(discarded, na.rm = TRUE)) %>%
      pivot_longer(cols = c(Kept, Discarded), names_to = "catchtype", values_to = "All") %>%
      {if(input$catch_catchtype == "Kept") filter(., catchtype == "Kept") else . } %>%
      {if(input$catch_catchtype == "Discarded") filter(., catchtype == "Discarded") else . }

    myplot <- baseplot +
      geom_bar(aes(x = usedate, y = All, fill = catchtype),
               width = 1,
               data = plotme,
               stat = "identity", col = "black") +
      fillScaleCatchType
  }
  
  myplot +
    ylab(paste(input$catch_catchtype, "(Weight (lbs))")) +
    theme_bw(16) +
    xlab("Date") +
    scale_x_date(limits = c(input$date[1]-1, input$date[2]+1)) +
    theme(legend.position = "bottom")
})

output$catch_plot <- renderPlot({
  if(nrow(catch_data()) > 0 ) {
    catch_plot_prep()} else {
      ggplot() + 
        annotate("text", x = 4, y = 25, size = 4,
                 label = "No data available") + 
        theme_void()        
    }
})

# Information on Click
catch_click_info_prep <- reactive({
  if(length(input$catch_click$x) > 0) {
    date.print <- as.Date(input$catch_click$x, origin = "1970-01-01") + 1
    evtr.print <- catch_data() %>%
      filter(usedate == ymd(date.print))
    paste("<b>Date</b>:", date.print, "<br>",
          "<b>eVTR</b>:", paste(unique(evtr.print$vtr), collapse = ", "))
  } else {
    "Click on the bar plot to learn more."
  }
})

output$catch_click_info <- renderText({catch_click_info_prep()})

# Map ----
catch_map_prep <- reactive({
  mapme_heat <- catch_data() %>%
    filter(!is.na(use_col), !is.na(lat), !is.na(lon)) %>%
    group_by(lat, lon) %>%
    summarise(use_col = sum(use_col, na.rm = TRUE)) %>%
    ungroup() %>%
    uncount(use_col)
  
  mapme_points <- catch_data() %>%
    filter(!is.na(use_col), !is.na(lat), !is.na(lon)) %>%
    group_by(vtr, lat, lon, species) %>%
    summarise(use_col = sum(use_col, na.rm = TRUE)) %>%
    filter(use_col > 0) %>%
    mutate(pop = paste0("<b>eVTR:</b> ", vtr, "<br><b>Lat:</b> ", round(lat, 2), "<b>  Lon:</b> ", round(lon, 2)))
  
  
  if(length(input$catch_species) < 11 & !"All Species" %in% input$catch_species) {
    mapme_points <- mapme_points %>%
      select(vtr, lat, lon, species, use_col) %>%
      complete(vtr, lat, lon, species = input$catch_species, fill = list(use_col = 0)) %>%
      mutate(pop.pre = paste0(species, ": ", use_col, collapse = "<br>"),
             pop = paste0("<b>eVTR:</b> ", vtr, "<br><b>Lat:</b> ", round(lat, 2), "<b>  Lon:</b> ", round(lon, 2), "<br>", pop.pre))
  }
  
  if(nrow(mapme_heat) > 0) {
    mymap <- leaflet(data = mapme_heat) %>%
      addTiles() %>%
      addHeatmap(blur = 35,
                 gradient = "Spectral", lat = ~lat, lng = ~lon) %>%
      addCircleMarkers(radius = 1, color = "black", lat = ~lat, lng = ~lon,
                       popup = ~pop,
                       data = mapme_points)
    
    if(length(input$catch_evtr) > 0) {
      mymap <- mymap %>%
        addCircleMarkers(radius = 1, color = "gold", lat = ~lat, lng = ~lon,
                         popup = ~pop,
                         data = mapme_points %>%
                           filter(vtr %in% input$catch_evtr))
    }
    
    
  } else {
    mymap <- leaflet() %>%
      addControl("No data available", position = "topright")
  }
  mymap
})

output$catch_map <- renderLeaflet({catch_map_prep() %>%
    addResetMapButton() %>%
    fitBounds(-72, 40, -67, 43)})

# Table ----
evtr_catch_table_prep <- reactive({
  req(input$catch_table_res)
  
  grouping_vars <- if(input$catch_table_res == "Overall") {
    c("vtr", "usedate")
  } else {
    if(input$catch_table_res == "By Species") {
      c("vtr", "usedate", "species")
    } else {
      if(input$catch_table_res == "By Gear") {
        c("vtr", "usedate", "gear")
      }
    }
  }
  
catch_data() %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(`Total Kept` = sum(kept, na.rm = TRUE),
              `Total Discarded` = sum(discarded, na.rm = TRUE),
              Total = sum(total, na.rm = TRUE)) %>%
    filter(Total > 0) %>%
    {if(length(input$catch_evtr) > 0) filter(., vtr %in% input$catch_evtr) else .} %>%
    rename("eVTR" = "vtr", "Date Fished" = usedate) %>%
    {if("gear" %in% names(.)) rename(., "Gear" = "gear") else .} %>%
    {if("species" %in% names(.)) rename(., "Species" = "species") else .}
})

output$evtr_catch_table <- renderDataTable({
  evtr_catch_table_prep() %>%
    mutate(`Date Fished` = as.character(`Date Fished`)) %>%
    bind_rows(tibble(eVTR = NA,
                     `Date Fished` = "Total",
                     `Total Kept` = sum(evtr_catch_table_prep()$`Total Kept`, na.rm = TRUE),
                     `Total Discarded` = sum(evtr_catch_table_prep()$`Total Discarded`, na.rm = TRUE),
                     Total = sum(evtr_catch_table_prep()$Total, na.rm = TRUE)))},
  rownames = FALSE,
  options = list(paging = FALSE,
                 dom = "ft",
                 ordering = FALSE,
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                   "}")))

# Downloads ----
output$export_catch_data <- downloadHandler(
  filename = function() {
    paste("catch_data.csv", sep = "")
  },
  content = function(file) {
    write.csv(catch_data() %>%
                mutate(eVTR = paste0("eVTR", vtr)) %>%
                rename("date.general" = "usedate") %>%
                distinct() %>%
                select(eVTR, starts_with("date"), everything(), -use_col, -vtr),
              file, row.names = FALSE)
  }
)

output$export_catch_plot <- downloadHandler(
  filename = function() {
    paste("evtr_catch_figure.png")
  },
  content = function(file) {
    saveplot <- catch_plot_prep()
    ggsave(file, plot = saveplot,
           width = 8.5, height = 11, units = "in",
           dpi = 300)
  }
)

output$export_catch_table <- downloadHandler(
  filename = function() {
    paste("evtr_catch_table.csv", sep = "")
  },
  content = function(file) {
    write.csv(evtr_catch_table_prep() %>%
                mutate(eVTR = paste0("eVTR", eVTR)), file, row.names = FALSE)
  }
)
