
smpsUI <- function(id) {
  
  ns <- NS(id)
  
  df <- tbl(con, I("smps.sample_analysis")) |>
    select(-concentration_json, -raw_concentration_json) |>
    filter(1 == 0) |>
    collect() |>
    select(sample_start, where(is.numeric))
  options <- colnames(df)
  
  page_fillable(
    gap = "8px",
    layout_column_wrap(
      width = 1/4,
      min_height = "200px",
      gap = "6px",
      uiOutput(ns("reporting")),
      uiOutput(ns("detector")),
      uiOutput(ns("classifier")),
      uiOutput(ns("sheath_flow")),
      uiOutput(ns("flows")),
      uiOutput(ns("sheath_rh")),
      uiOutput(ns("sheath_pressure")),
      uiOutput(ns("sheath_temp")),
    ),
    layout_column_wrap(
      width = 1/6,
      min_height = "60px",
      checkboxInput(ns("by_date"), "Specify Date Range", value = FALSE),
      conditionalPanel(
        condition = "input.by_date == 1",
        dateRangeInput(ns("dates"), "Date Range", min = "2023-01-01",
                       max = Sys.Date() + 1, start = Sys.Date() - 1,
                       end = Sys.Date() + 1),
        ns = ns
      ) 
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        plotlyOutput(ns("scan")),
        full_screen = TRUE
      ),
      layout_column_wrap(
        width = 1,
        card(
          selectInput(ns("plot1_y"), label = NULL, choices = options, multiple = TRUE,
                      selected = "total_concentration"),
          plotlyOutput(ns("plot1")),
          selectInput(ns("plot2_y"), label = NULL, choices = options, multiple = TRUE,
                      selected = "geo_mean"),
          plotlyOutput(ns("plot2")),
          full_screen = TRUE
        )
      )
      
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
      
      if (!input$by_date) {
        df <- tbl(con, I("smps.sample_analysis")) |>
          inner_join(select(tbl_sites, site_number, site_code), by = "site_number") |>
          filter(site_code == !!site()) |>
          select(-concentration_json, -raw_concentration_json) |>
          arrange(desc(sample_start)) |>
          head(200) |>
          collect()
      } else {
        interval <- input$dates[2] - input$dates[1]
        validate(need(interval >= 0, "waiting for valid date range"))
        validate(need(interval < 60, "Please limit date interval for SMPS to 60 days"))
        df <- tbl(con, I("smps.sample_analysis")) |>
          inner_join(select(tbl_sites, site_number, site_code), by = "site_number") |>
          filter(site_code == !!site(),
                 sample_start >= !!input$dates[1],
                 sample_start <= !!input$dates[2]) |>
          select(-concentration_json, -raw_concentration_json) |>
          collect()
      }
      
      validate(need(nrow(df) > 0, "No data in time range"))
      
      df


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
      
      scan <- as_tibble(yyjsonr::read_json_str(df$concentration_json)) |>
        tidyr::pivot_longer(everything(), names_to = "midpoint", values_to = "value") |>
        mutate(midpoint = as.numeric(midpoint))

    })
    
    output$reporting <- renderUI({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))

      tdiff <- difftime(Sys.time(), s$sample_start, units = "mins")
      
      if (tdiff > 60 * 24) {
        hours <- tdiff  / 60
        if (hours > 48) {
          sub <- paste("Last data", round(hours / 24), "days ago")
        } else {
          sub <- paste("Last data", round(hours), "hours ago")          
        }
        value_box(title = sub, value = sub, theme = "danger", showcase = bs_icon("exclamation"))
      } else if (tdiff > 60) {
        sub <- paste("Last data", round(tdiff), "minutes ago")
        value_box("Lagging", value = sub, theme = "warning", showcase = bs_icon("question"))
      } else {
        sub <- paste("Last data", round(tdiff), "minutes ago")
        value_box("Online", value = sub, theme = "primary", showcase = bs_icon("check"))
      }
    })
    
    output$detector <- renderUI({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$detector_status == "Normal Scan") {
        value_box("Detector Status", value = s$detector_status, theme = "primary",
                  showcase = bs_icon("check"))
      } else {
        value_box("Detector Status Error", value = s$detector_status, theme = "danger",
                  showcase = bs_icon("exclamation"))
      }
      
    })
    
    output$classifier <- renderUI({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$classifier_errors == "Normal Scan") {
        value_box("Classifier Status", value = s$classifier_errors, theme = "primary",
                  showcase = bs_icon("check"))
      } else {
        value_box("Classifier Status Error", value = s$classifier_errors, theme = "danger",
                  showcase = bs_icon("exclamation"))
      }
      
    })
    
    output$sheath_flow <- renderUI({

      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$sheath_flow != 4.8) {
        value_box("Sheath flow not expected value (4.8 L/min)", 
                  value = paste(s$sheath_flow, "L/min"), theme = "warning",
                  showcase = bs_icon("question"))
      } else {
        value_box("Sheath Flow", value = paste(s$sheath_flow, "L/min"), theme = "primary",
                  showcase = bs_icon("check"))
      }
      
    })
    
    output$flows <- renderUI({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      val <- paste("Detector inlet:", s$detector_inlet_flow, "L/min\n",
                   "Impactor:", s$impactor_flow, "L/min")
      if (s$detector_inlet_flow != 0.6 | s$impactor_flow != 0.6) {
        value_box("One or more flows not expected value (0.6 L/min)",
                  value = val, theme = "warning", showcase = bs_icon("question"))
      } else {
        value_box("Flows", value = val, theme = "primary", showcase = bs_icon("check"))
      }
    })
    
    output$sheath_rh <- renderUI({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$sheath_relative_humidity > 20) {
        value_box("Sheath RH High", value = paste(s$sheath_relative_humidity, "%"),
                  theme = "warning", showcase = bs_icon("question"))
      } else {
        value_box("Sheath RH", value = paste(s$sheath_relative_humidity, "%"),
                  theme = "primary", showcase = bs_icon("check"))
      }
    })
    
    output$sheath_pressure <- renderUI({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      value_box("Sheath Pressure", value = paste(s$sheath_pressure, "kPa"),
                theme = "primary", showcase = bs_icon("info"))
    })
    
    output$sheath_temp <- renderUI({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      value_box("Sheath Temperature", value = paste0(s$sheath_temp, " ", "\U00b0", "C"),
                theme = "primary", showcase = bs_icon("info"))
    })
    
    ts_plot <- function(df) {
      
      ggplot(df, aes(x = sample_start, y = value, color = param)) + geom_line() +
        scale_x_datetime(labels = scales::label_date()) +
        theme(axis.title.x = element_blank())
      
    }
    
    output$plot1 <- renderPlotly({
      
      df <- get_range() |>
      select(sample_start, any_of(input$plot1_y)) |>
        tidyr::pivot_longer(any_of(input$plot1_y), names_to = "param", values_to = "value")
      g <- ts_plot(df)
      ggplotly(g, dynamicTicks = TRUE)
      
    })
    
    output$plot2 <- renderPlotly({
      
      df <- get_range() |>
        select(sample_start, any_of(input$plot2_y)) |>
        tidyr::pivot_longer(any_of(input$plot2_y), names_to = "param", values_to = "value")
      g <- ts_plot(df)
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