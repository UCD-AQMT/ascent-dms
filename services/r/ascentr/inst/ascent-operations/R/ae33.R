
ae33UI <- function(id) {
  
  ns <- NS(id)

  page_fillable(
    layout_column_wrap(
      width = 1/6,
      min_height = "200px",
      gap = "6px",
      uiOutput(ns("reporting")),
      uiOutput(ns("operation")),
      uiOutput(ns("flow")),
      uiOutput(ns("optical")),
      uiOutput(ns("chamber")),
      uiOutput(ns("tape")),
      uiOutput(ns("setup")),
      uiOutput(ns("tests")),
      uiOutput(ns("external_device")),
      uiOutput(ns("clean_air")),
      uiOutput(ns("cf_card")),
      uiOutput(ns("database")),
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
      gap = "8px",
      card(
        withSpinner(plotlyOutput(ns("ebc")), fill = TRUE),
        full_screen = TRUE
      ),
      card(
        plotlyOutput(ns("attenuation")),
        full_screen = TRUE
      )
    ),
    layout_column_wrap(
      width = 1/2,
      gap = "8px",
      card(
        selectInput(ns("plot_y"), label = "Label", choices = ae33_fields, multiple = TRUE,
                    selected = "tpcnt"),
        plotlyOutput(ns("ts_plot")),
        full_screen = TRUE
      ),
      card(
        plotlyOutput(ns("compensation")),
        full_screen = TRUE
      )
    )
  )
}

ae33Server <- function(id, site) {
  moduleServer(id, function(input, output, session) {
    
    
    get_last <- reactive({
      
      invalidateLater(1000 * 60 * 3) # every three minutes
      
      # Using this sort |> limit approach is much fast than last()!
      flux_query <- glue::glue('from(bucket: "measurements") |> ', 
                               'range(start: 0, stop: -0s) |> ',
                               'filter(fn: (r) => r._measurement == "ae33_{site()}_raw" and ',
                               'r._field == "STinst") |> ',
                               'sort(columns: ["_time"], desc: true) |> ',
                               'limit(n:1)'
                               )
      ae33_client$query(flux_query)[[1]]

    })
    
    get_status <- reactive({
      
      df <- get_last()
      flags <- parse_ae33_flags(df$`_value`)
      
    })
    
    get_recent <- reactive({
      
      invalidateLater(1000 * 60 * 3) # every three minutes
      
      if (!input$by_date) {
        # last 12 hours
        flux_query <- glue::glue('from(bucket: "measurements") |> ',
                                 'range(start: -12h) |> ',
                                 'filter(fn: (r) => r._measurement == "ae33_{site()}_raw") |> ',
                                 'drop(columns: ["_start", "_stop"])'
        )
      } else {
        interval <- input$dates[2] - input$dates[1]
        validate(need(interval >= 0, "waiting for valid date range"))
        validate(need(interval < 15, "Please limit date interval for AE33 to 14 days"))
        flux_query <- glue::glue('from(bucket: "measurements") |> ',
                                 'range(start: {input$dates[1]}T00:00:00Z,',
                                 'stop: {input$dates[2]}T23:59:59Z) |> ',
                                 'filter(fn: (r) => r._measurement == "ae33_{site()}_raw") |> ',
                                 'drop(columns: ["_start", "_stop"])'
        )
      }
      
      
      df_list <- ae33_client$query(flux_query)
      validate(need(!is.null(df_list), "No data within time period"))
      
      clean_fields <- function(df) {
        param <- df$`_field`[1]
        colnames(df) <- c("time", param, "f", "m", "t")
        select(df, 1:2)
      }
      
      df <- purrr::map(df_list, clean_fields) |>
        purrr::reduce(\(x, y) inner_join(x, y, by = "time"))
      
      
    })
    
    make_valuebox <- function(title, vec) {
      th <- switch(vec[1],
                   "OK" = "primary",
                   "info" = "info",
                   "warning" = "warning",
                   "error" = "danger")
      ic <- switch(vec[1],
                   "OK" = "check",
                   "info" = "info",
                   "warning" = "question",
                   "error" = "exclamation")
      # Do it with no showcase icon to save space
      value_box(title = title, value = vec[2], theme = th, max_height = "130px")
    }
    
    output$reporting <- renderUI({
      
      df <- get_last()
      tdiff <- difftime(Sys.time(), df$time, units = "mins")
      
      if (tdiff > (60 * 24)) {
        txt <- "Offline"
        sub <- glue::glue("Last data {round(tdiff / (60*24))} days ago")
        theme <- "danger"
        icon <- "exclamation"
      } else if (tdiff > 60) {
        txt <- "Lagging"
        sub <- glue::glue("Last data {round(tdiff)} minutes ago")
        theme <- "warning"
        icon <- "clock"
      } else {
        txt <- "Online"
        sub <- glue::glue("Last data {round(tdiff)} minutes ago")
        theme <- "primary"
        icon <- "check"
      }
      value_box(title = txt, value = sub, theme = theme, max_height = "130px")
      
    })
    
    output$operation <- renderUI({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_valuebox(title = "Operation Status", st$operation)
    })
    
    output$flow <- renderUI({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_valuebox(title = "Flow Status", st$flow)
    })
    
    output$optical <- renderUI({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_valuebox(title = "Optical Source", st$optical)
    })
    output$chamber <- renderUI({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_valuebox(title = "Chamber", st$chamber)
    })
    output$tape <- renderUI({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_valuebox(title = "Filter Tape", st$tape)
    })
    output$setup <- renderUI({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_valuebox(title = "Setup File", st$setup)
    })
    output$tests <- renderUI({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_valuebox(title = "Tests/Procedures", st$tests)
    })
    output$external_device <- renderUI({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_valuebox(title = "External Device", st$external_device)
    })
    output$clean_air <- renderUI({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_valuebox(title = "Auto Clean Air Test", st$clean_air)
    })
    output$cf_card <- renderUI({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_valuebox(title = "CF Card", st$cf_card)
    })
    output$database <- renderUI({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_valuebox(title = "Internal Database", st$database)
    })
    
    # Shared margins so the plots x-axes (almost) line up
    plot_margins <- list(
      l = 80,
      r = 80,
      t = 0,
      b = 0,
      pad = 0
    )
    
    output$ebc <- renderPlotly({
    
      df <- get_recent() 
      validate(need(nrow(df > 0), "No data in time period"))
      
      df <- df |>
        select(time, EBC_1:EBC_7) |>
        mutate(time = as.POSIXct(time, tz = "UTC")) |>
        tidyr::pivot_longer(EBC_1:EBC_7, names_to = "channel", values_to = "value")
      
      g <- ggplot(df, aes(x = time, y = value, color = channel)) +
        geom_line() +
        scale_x_datetime(labels = scales::label_date()) +
        labs(y = paste("Concentration", ugm3())) +
        theme(axis.title.x = element_blank())
      
      ggplotly(g, dynamicTicks = TRUE) |>
        layout(margin = plot_margins,
               legend = list(tracegroupgap = 0))
      
    })
    
    output$attenuation <- renderPlotly({
      
      df <- get_recent() 
      validate(need(nrow(df > 0), "No data in time period"))
  
      df <- df |>
        select(time, att1_1:att1_7) |>
        mutate(time = as.POSIXct(time, tz = "UTC")) |>
        tidyr::pivot_longer(att1_1:att1_7, names_to = "channel", values_to = "value")
      
      g <- ggplot(df, aes(x = time, y = value, color = channel)) +
        geom_line() +
        scale_x_datetime(labels = scales::label_date()) +
        labs(y = "Attenuation") +
        theme(axis.title.x = element_blank())
      ggplotly(g, dynamicTicks = TRUE) |>
        layout(margin = plot_margins,
               legend = list(tracegroupgap = 0))
      
    })
    
    output$ts_plot <- renderPlotly({
      
      df <- get_recent()
      validate(need(nrow(df > 0), "No data in time period"))

      df <- df |>
        select(time, any_of(input$plot_y)) |>
        mutate(time = as.POSIXct(time, tz = "UTC")) |>
        mutate(across(bit64::is.integer64, bit64::as.integer.integer64)) |>
        tidyr::pivot_longer(any_of(input$plot_y), names_to = "param", values_to = "value")

      g <- ggplot(df, aes(x = time, y = value, color = param)) + geom_line() +
          scale_x_datetime(labels = scales::label_date()) +
          theme(axis.title.x = element_blank())
      ggplotly(g, dynamicTicks = TRUE)  

    })    
    
    # output$flow_plot <- renderPlotly({
    #   df <- get_recent() 
    #   validate(need(nrow(df > 0), "No data in time period"))
    #   
    #   df <- df |>
    #     select(time, flow1, flow2, flowC) |>
    #     mutate(time = as.POSIXct(time, tz = "UTC")) |>
    #     tidyr::pivot_longer(flow1:flowC, names_to = "flow", values_to = "value")
    #   
    #   g <- ggplot(df, aes(x = time, y = value, color = flow)) +
    #     geom_line() +
    #     scale_x_datetime(labels = scales::label_date()) +
    #     labs(y = "L/min") +
    #     theme(axis.title.x = element_blank())
    #   ggplotly(g, dynamicTicks = TRUE) |>
    #     layout(margin = plot_margins,
    #            legend = list(tracegroupgap = 0))
    #   
    #   
    # })
    
    output$compensation <- renderPlotly({
      df <- get_recent() 
      validate(need(nrow(df > 0), "No data in time period"))
      
      df <- df |>
        select(time, k_1:k_7) |>
        mutate(time = as.POSIXct(time, tz = "UTC")) |>
        tidyr::pivot_longer(k_1:k_7, names_to = "channel", values_to = "value")
      
      g <- ggplot(df, aes(x = time, y = value, color = channel)) +
        geom_line() +
        scale_x_datetime(labels = scales::label_date()) +
        labs(y = "Compensation") +
        theme(axis.title.x = element_blank())
      ggplotly(g, dynamicTicks = TRUE) |>
        layout(margin = plot_margins,
               legend = list(tracegroupgap = 0))
      
    })
    
  })
}