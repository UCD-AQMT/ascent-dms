
smpsUI <- function(id) {
  
  ns <- NS(id)
  
  df <- tbl(con, I("smps.sample_analysis")) |>
    select(-concentration_json, -raw_concentration_json) |>
    filter(1 == 0) |>
    collect()
  options <- colnames(df)
  
  tagList(
    fluidRow(
      infoBoxOutput(ns("reporting"), width = 3),
      infoBoxOutput(ns("detector"), width = 3),
      infoBoxOutput(ns("classifier"), width = 3),
      infoBoxOutput(ns("sheath_flow"), width = 3),
      infoBoxOutput(ns("flows"), width = 3),
      infoBoxOutput(ns("sheath_rh"), width = 3),
      infoBoxOutput(ns("sheath_pressure"), width = 3),
      infoBoxOutput(ns("sheath_temp"), width = 3)
    ),
    fluidRow(
      column(2, selectInput(ns("plot1_y"), "Parameter", choices = options,
                            selected = "total_concentration")),
      column(10, plotlyOutput(ns("plot1"), height = 200))
    ),
    fluidRow(
      column(2, selectInput(ns("plot2_y"), "Parameter", choices = options,
                            selected = "geo_mean")),
      column(10, plotlyOutput(ns("plot2"), height = 200))
    ),
    fluidRow(
      column(10, offset = 2, plotlyOutput(ns("scan"), height = 300))
    )
  )
  
}

smpsServer <- function(id, site) {
  moduleServer(id, function(input, output, session) {
    
    get_last <- reactive({
      
      get_range() |>
        arrange(desc(sample_start)) |>
        slice(1)
      
    })
    
    get_range <- reactive({
      
      invalidateLater(1000 * 60 * 3) # every three minutes
      
      tbl(con, I("smps.sample_analysis")) |>
        inner_join(select(tbl_sites, site_number, site_code), by = "site_number") |>
        filter(site_code == !!site()) |>
        select(-concentration_json, -raw_concentration_json) |>
        arrange(desc(sample_start)) |>
        head(200) |>
        collect()

    })
    
    get_scan <- reactive({
      
      invalidateLater(1000 * 60 * 3) # every three minutes
      df <- tbl(con, I("smps.sample_analysis")) |>
        inner_join(select(tbl_sites, site_number, site_code), by = "site_number") |>
        filter(site_code == !!site()) |>
        select(-raw_concentration_json) |>
        arrange(desc(sample_start)) |>
        head(1) |>
        collect()

      scan <- jsonlite::stream_in(textConnection(df$concentration_json), 
                                          verbose = FALSE) |>
        tidyr::pivot_longer(everything(), names_to = "midpoint", values_to = "value") |>
        mutate(midpoint = as.numeric(midpoint))

    })
    
    output$reporting <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))

      tdiff <- difftime(Sys.time(), s$sample_start, units = "mins")
      
      if (tdiff > 60 * 24) {
        txt <- "Offline"
        hours <- tdiff  / 60
        if (hours > 48) {
          sub <- glue::glue("Last data {round(hours / 24)} days ago")
        } else {
          sub <- glue::glue("Last data {round(hours)} hours ago")          
        }
        infoBox("Reporting", txt, subtitle = sub, color = "red", icon = icon("exclamation"),
                width = 3)
      } else if (tdiff > 60) {
        txt <- "Lagging"
        sub <- glue::glue("Last data {round(tdiff)} minutes ago")
        infoBox("Reporting", txt, subtitle = sub, color = "yellow", icon = icon("clock"),
                width = 3)
      } else {
        txt <- "Online"
        sub <- glue::glue("Last data {round(tdiff)} minutes ago")
        infoBox("Reporting", txt, subtitle = sub, color = "blue", icon = icon("check"),
                width = 3)
        
      }

    })
    
    output$detector <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$detector_status == "Normal Scan") {
        infoBox("Detector Status", s$detector_status, color = "blue", icon = icon("check"),
                width = 3)
      } else {
        infoBox("Detector Status", "Error", color = "red", icon = icon("exclamation"),
                subtitle = s$detector_status, width = 3)
      }
      
    })
    
    output$classifier <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$classifier_errors == "Normal Scan") {
        infoBox("Classifier Status", s$classifier_errors, color = "blue", icon = icon("check"),
                width = 3)
      } else {
        infoBox("Classifier Status", "Error", color = "red", icon = icon("exclamation"),
                subtitle = s$classifier_errors, width = 3)
      }
      
    })
    
    output$sheath_flow <- renderInfoBox({

      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$sheath_flow != 4.8) {
        infoBox("Sheath Flow", paste(s$sheath_flow, "L/min"), color = "yellow", 
                icon = icon("question"), 
                subtitle = "Sheath flow not expected value (4.8 L/min)", width = 3)
      } else {
        infoBox("Sheath Flow", paste(s$sheath_flow, "L/min"), color = "blue", 
                icon = icon("check"), width = 3)
      }
      
    })
    
    output$flows <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$detector_inlet_flow != 0.6 | s$impactor_flow != 0.6) {
        value <- glue::glue("Detector inlet: {s$detector_inlet_flow} L/min\n",
                            "Impactor: {s$impactor_flow} L/min")
        infoBox("Detector Inlet & Impactor Flow", value,
                subtitle = "One or more flows not expected value (0.6 L/min)",
                color = "yellow", icon = icon("question"))
      } else {
        infoBox("Detector Inlet & Impactor Flow", "0.6 L/min",
                color = "blue", icon = icon("check"))
      }
      
    })
    
    output$sheath_rh <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$sheath_relative_humidity > 20) {
        infoBox("Sheath RH", paste(s$sheath_relative_humidity, "%"),
                subtitle = "Sheath RH high",
                color = "yellow", icon = icon("question"))
      } else {
        infoBox("Sheath RH", paste(s$sheath_relative_humidity, "%"),
                color = "blue", icon = icon("check"))
      }
    })
    
    output$sheath_pressure <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      infoBox("Sheath Pressure", paste(s$sheath_pressure, "kPa"),
                color = "blue", icon = icon("info"))
      
    })
    
    output$sheath_temp <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      infoBox("Sheath Temperature", paste0(s$sheath_temp, " ", "\U00b0", "C"),
              color = "blue", icon = icon("info"))
      
    })
    
    ts_plot <- function(param, df) {
      
      ggplot(df, aes(x = sample_start, y = .data[[param]])) + geom_line() +
        scale_x_datetime(labels = scales::label_date()) +
        theme(axis.title.x = element_blank())
      
    }
    
    output$plot1 <- renderPlotly({
      
      df <- get_range()
      g <- ts_plot(input$plot1_y, df)
      ggplotly(g, dynamicTicks = TRUE)
      
    })
    
    output$plot2 <- renderPlotly({
      
      df <- get_range()
      g <- ts_plot(input$plot2_y, df)
      ggplotly(g, dynamicTicks = TRUE)
      
    })
    
    output$scan <- renderPlotly({
      
      df <- get_scan()
      
      g <- ggplot(df, aes(x = midpoint, y = value)) +
        geom_bar(stat = "identity") +
        scale_x_log10(breaks = scales::breaks_log(n = 10),
                      limits = c(10, 1000)) +
        labs(x = "mobility diameter (nm)",
             y = paste("Number", ctscm3(), sep = "\n"),
             title = "Last scan")
      ggplotly(g)
      
    })
    
  })
}