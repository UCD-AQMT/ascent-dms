
ae33UI <- function(id) {
  
  ns <- NS(id)
  
  options <- "test"
  
  tagList(
    fluidRow(
      infoBoxOutput(ns("reporting"), width = 3),
      infoBoxOutput(ns("operation"), width = 3),
      infoBoxOutput(ns("flow"), width = 3),
      infoBoxOutput(ns("optical"), width = 3),
      infoBoxOutput(ns("chamber"), width = 3),
      infoBoxOutput(ns("tape"), width = 3),
      infoBoxOutput(ns("setup"), width = 3),
      infoBoxOutput(ns("tests"), width = 3),
      infoBoxOutput(ns("external_device"), width = 3),
      infoBoxOutput(ns("clean_air"), width = 3),
      infoBoxOutput(ns("cf_card"), width = 3),
      infoBoxOutput(ns("database"), width = 3)
    ),
    fluidRow(
      column(6, plotlyOutput(ns("ebc"), height = 250)),
      column(6, plotlyOutput(ns("attenuation"), height = 250))
    ),
    fluidRow(
      column(6, plotlyOutput(ns("flow_plot"), height = 250)),
      column(6, plotlyOutput(ns("compensation"), height = 250))
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
      
      # last 12 hours
      flux_query <- glue::glue('from(bucket: "measurements") |> ',
                               'range(start: -12h) |> ',
                               'filter(fn: (r) => r._measurement == "ae33_{site()}_raw") |> ',
                               'drop(columns: ["_start", "_stop"])'
                               )

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
    
    make_infoBox <- function(title, vec) {
      col <- switch(vec[1],
                      "OK" = "blue",
                      "info" = "blue",
                      "warning" = "yellow",
                      "error" = "red")
      ic <- switch(vec[1],
                     "OK" = "check",
                     "info" = "info",
                     "warning" = "question",
                     "error" = "exclamation")
      infoBox(title, vec[2], color = col, icon = icon(ic))
      
    }
    
    output$reporting <- renderInfoBox({
      
      df <- get_last()
      tdiff <- difftime(Sys.time(), df$time, units = "mins")
      
      if (tdiff > (60 * 24)) {
        txt <- "Offline"
        sub <- glue::glue("Last data {round(tdiff / (60*24))} days ago")
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
    
    output$operation <- renderInfoBox({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_infoBox(title = "Operation Status", st$operation)
    })
    
    output$flow <- renderInfoBox({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_infoBox(title = "Flow Status", st$flow)
    })
    
    output$optical <- renderInfoBox({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_infoBox(title = "Optical Source", st$optical)
    })
    output$chamber <- renderInfoBox({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_infoBox(title = "Chamber", st$chamber)
    })
    output$tape <- renderInfoBox({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_infoBox(title = "Filter Tape", st$tape)
    })
    output$setup <- renderInfoBox({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_infoBox(title = "Setup File", st$setup)
    })
    output$tests <- renderInfoBox({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_infoBox(title = "Tests/Procedures", st$tests)
    })
    output$external_device <- renderInfoBox({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_infoBox(title = "External Device", st$external_device)
    })
    output$clean_air <- renderInfoBox({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_infoBox(title = "Auto Clean Air Test", st$clean_air)
    })
    output$cf_card <- renderInfoBox({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_infoBox(title = "CF Card", st$cf_card)
    })
    output$database <- renderInfoBox({
      st <- get_status()
      validate(need(length(st) > 0, "No data for site"))
      make_infoBox(title = "Internal Database", st$database)
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
    
    output$flow_plot <- renderPlotly({
      df <- get_recent() 
      validate(need(nrow(df > 0), "No data in time period"))
      
      df <- df |>
        select(time, flow1, flow2, flowC) |>
        mutate(time = as.POSIXct(time, tz = "UTC")) |>
        tidyr::pivot_longer(flow1:flowC, names_to = "flow", values_to = "value")
      
      g <- ggplot(df, aes(x = time, y = value, color = flow)) +
        geom_line() +
        scale_x_datetime(labels = scales::label_date()) +
        labs(y = "L/min") +
        theme(axis.title.x = element_blank())
      ggplotly(g, dynamicTicks = TRUE) |>
        layout(margin = plot_margins,
               legend = list(tracegroupgap = 0))
      
      
    })
    
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