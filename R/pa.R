
paUI <- function(id) {
  
  ns <- NS(id)
  
  df <- tbl(con, I("purpleair.sample_analysis")) |>
    filter(1 == 0) |>
    select(-last_seen) |>
    collect()
  options <- colnames(df)

  tagList(
    fluidRow(
      infoBoxOutput(ns("reporting"), width = 3),
      infoBoxOutput(ns("channel_state"), width = 3),
      infoBoxOutput(ns("channel_flags"), width = 3),
      infoBoxOutput(ns("confidence"), width = 3)
    ),
    fluidRow(
      column(2, selectInput(ns("plot1_y"), "Parameter", choices = options,
                            multiple = TRUE, selected = c("pm1_0_atm", "pm2_5_atm", "pm10_0_atm"))),
      column(10, plotlyOutput(ns("plot1"), height = 300))
    ),
    fluidRow(
      column(2, selectInput(ns("plot2_y"), "Parameter", choices = options,
                            multiple = TRUE, selected = c("pm2_5_atm_a", "pm2_5_atm_b", "pm2_5_atm"))),
      column(10, plotlyOutput(ns("plot2"), height = 300))
    )
  )
  
  
}

paServer <- function(id, site) {
  moduleServer(id, function(input, output, session) {
    
    get_last <- reactive({
      
      get_range() |>
        arrange(desc(last_seen)) |>
        slice(1)
      
    })
    
    
    get_range <- reactive({
      
      invalidateLater(1000 * 60 * 3) # every three minutes
      
      tbl(con, I("purpleair.sample_analysis")) |>
        inner_join(tbl(con, I("purpleair.sensors")), by = "sensor_index") |>
        inner_join(select(tbl_sites, site_number, site_code), by = "site_number") |>
        filter(site_code == !!site()) |>
        arrange(desc(last_seen)) |>
        head(200) |>
        collect()
      
    })
    
    
    output$reporting <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      tdiff <- difftime(Sys.time(), s$last_seen, units = "mins")
      
      if (tdiff > (24 * 60)) {
        txt <- "Offline"
        sub <- glue::glue("Last data {round(tdiff / (60 * 24))} days ago")
        infoBox("Reporting", txt, subtitle = sub, color = "red", icon = icon("exclamation"),
                width = 3)
      } else if (tdiff > 60) {
        txt <- "Lagging"
        sub <- glue::glue("Last data {round(tdiff / 60)} hours ago")
        infoBox("Reporting", txt, subtitle = sub, color = "yellow", icon = icon("clock"),
                width = 3)
      } else {
        txt <- "Online"
        sub <- glue::glue("Last data {round(tdiff)} minutes ago")
        infoBox("Reporting", txt, subtitle = sub, color = "blue", icon = icon("check"),
                width = 3)
        
      }
      
    })
    
    output$channel_state <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      # These are defined on the PurpleAir API docs page. Channel state is from 0-3. Add 1
      # here to match R array indexing for the switch function.
      state <- switch(s$channel_state + 1,
                      list(text = "NO PM", subtitle = "No PM sensors were detected",
                                   icon = "exclamation", color = "red"),
                      list(text = "PM-A", subtitle = "PM sensor on channel A only",
                                   icon = "question", color = "yellow"),
                      list(text = "PM-B", subtitle = "PM sensor on channel B only",
                                   icon = "question", color = "yellow"),
                      list(text = "PM-A+PM-B", subtitle = "PM sensor on both channels A and B",
                                   icon = "check", color = "blue")
      )
      infoBox("Channel State", state$text, subtitle = state$subtitle, color = state$color,
              icon = icon(state$icon))

    })

    output$channel_flags <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      flags <- switch(s$channel_flags + 1,
                      list(text = "Normal", subtitle = "No PM sensors downgraded",
                           icon = "check", color = "blue"),
                      list(text = "A-Downgraded", subtitle = "PM sensor A downgraded",
                           icon = "question", color = "yellow"),
                      list(text = "B-Downgraded", subtitle = "PM sensor B downgraded",
                           icon = "question", color = "yellow"),
                      list(text = "A+B-Downgraded", subtitle = "Both PM sensors downgraded",
                           icon = "exclamation", color = "red")
                      )
      infoBox("Channel Flags", flags$text, subtitle = flags$subtitle, color = flags$color,
              icon = icon(flags$icon))
      
      
    })    
    
    output$confidence <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      infoBox("Confidence", s$confidence, color = "blue", icon = icon("info"))
      
    })
    
    output$plot1 <- renderPlotly({
      
      df <- get_range() |>
        select(last_seen, any_of(input$plot1_y)) |>
        tidyr::pivot_longer(any_of(input$plot1_y), names_to = "param", values_to = "value")
      g <- ggplot(df, aes(x = last_seen, y = value, color = param)) + geom_line() +
        scale_x_datetime(labels = scales::label_date()) +
        theme(axis.title.x = element_blank())
      ggplotly(g, dynamicTicks = TRUE)
      
    })
    
    output$plot2 <- renderPlotly({
      
      df <- get_range() |>
        select(last_seen, any_of(input$plot2_y)) |>
        tidyr::pivot_longer(any_of(input$plot2_y), names_to = "param", values_to = "value")
      g <- ggplot(df, aes(x = last_seen, y = value, color = param)) + geom_line() +
        scale_x_datetime(labels = scales::label_date()) +
        theme(axis.title.x = element_blank())
      ggplotly(g, dynamicTicks = TRUE)
      
    })
    
  })
}