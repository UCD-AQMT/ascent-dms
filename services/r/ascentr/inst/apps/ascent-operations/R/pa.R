
paUI <- function(id) {
  
  ns <- NS(id)
  
  df <- tbl(con, I("purpleair.sample_analysis")) |>
    filter(1 == 0) |>
    select(-last_seen) |>
    collect()
  options <- colnames(df)

  page_fillable(
    layout_column_wrap(
      width = "200px",
      fill = FALSE,
      uiOutput(ns("reporting")),
      uiOutput(ns("channel_state")),
      uiOutput(ns("channel_flags")),
      uiOutput(ns("confidence"))
    ),
    layout_column_wrap(
      width = 1/6,
      checkboxInput(ns("by_date"), "Specify Date Range", value = FALSE),
      conditionalPanel(
        condition = "input.by_date == 1",
        dateRangeInput(ns("dates"), "Date Range", min = "2023-01-01",
                       max = Sys.Date() + 1, start = Sys.Date() - 1,
                       end = Sys.Date() + 1),
        ns = ns
      )
    ),
    card(
      layout_sidebar(
        sidebar = sidebar(selectInput(ns("plot1_y"), "Parameter", choices = options,
                                      multiple = TRUE, 
                                      selected = c("pm1_0_atm", "pm2_5_atm", "pm10_0_atm"))),
        withSpinner(plotlyOutput(ns("plot1")), fill = TRUE)
      ),
      full_screen = TRUE
    ),
    card(
      layout_sidebar(
        sidebar = sidebar(selectInput(ns("plot2_y"), "Parameter", choices = options,
                                      multiple = TRUE, 
                                      selected = c("pm2_5_atm_a", "pm2_5_atm_b", "pm2_5_atm"))),
        plotlyOutput(ns("plot2"))
      ),
      full_screen = TRUE
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
      
      if (!input$by_date) {
        df <- tbl(con, I("purpleair.sample_analysis")) |>
          inner_join(tbl(con, I("purpleair.sensors")), by = "sensor_index") |>
          inner_join(select(tbl_sites, site_number, site_code), by = "site_number") |>
          filter(site_code == !!site()) |>
          arrange(desc(last_seen)) |>
          head(200) |>
          collect()
      } else {
        interval <- input$dates[2] - input$dates[1]
        validate(need(interval >= 0, "waiting for valid date range"))
        validate(need(interval < 90, "Please limit date interval for PurpleAir to 90 days"))
        df <- tbl(con, I("purpleair.sample_analysis")) |>
          inner_join(tbl(con, I("purpleair.sensors")), by = "sensor_index") |>
          inner_join(select(tbl_sites, site_number, site_code), by = "site_number") |>
          filter(site_code == !!site(),
                 last_seen >= !!input$dates[1],
                 last_seen <= !!input$dates[2]) |>
          collect()
      }

      validate(need(nrow(df) > 0, "No data in time range"))
      df
      
    })
    
    output$reporting <- renderUI({
        s <- get_last()
        validate(need(nrow(s) > 0, "No data for site"))

        tdiff <- difftime(Sys.time(), s$last_seen, units = "mins")

        if (tdiff > (24 * 60)) {
          txt <- "Offline"
          sub <- glue::glue("Last data {round(tdiff / (60 * 24))} days ago")
          icon <- bs_icon("exclamation")
          theme <- "danger"
        } else if (tdiff > 60) {
          txt <- "Lagging"
          sub <- glue::glue("Last data {round(tdiff / 60)} hours ago")
          icon <- bs_icon("clock")
          theme <- "warning"
        } else {
          txt <- "Online"
          sub <- glue::glue("Last data {round(tdiff)} minutes ago")
          icon <- bs_icon("check")
          theme <- "primary"
        }
        value_box(title = sub, value = txt, showcase = icon, theme = theme)
      
    })
    
    output$channel_state <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      # These are defined on the PurpleAir API docs page. Channel state is from 0-3. Add 1
      # here to match R array indexing for the switch function.
      state <- switch(s$channel_state + 1,
                      list(text = "NO PM", subtitle = "No PM sensors were detected",
                           icon = "exclamation", theme = "danger"),
                      list(text = "PM-A", subtitle = "PM sensor on channel A only",
                           icon = "question", theme = "warning"),
                      list(text = "PM-B", subtitle = "PM sensor on channel B only",
                           icon = "question", theme = "warning"),
                      list(text = "PM-A+PM-B", subtitle = "PM sensor on both channels",
                           icon = "check", theme = "primary")
      )
      value_box(title = state$subtitle, value = state$text, showcase = bs_icon(state$icon),
                theme = state$theme)
    })

    output$channel_flags <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      flags <- switch(s$channel_flags + 1,
                      list(text = "Normal", subtitle = "No PM sensors downgraded",
                           icon = "check", theme = "primary"),
                      list(text = "A-Downgraded", subtitle = "PM sensor A downgraded",
                           icon = "question", theme = "warning"),
                      list(text = "B-Downgraded", subtitle = "PM sensor B downgraded",
                           icon = "question", theme = "warning"),
                      list(text = "A+B-Downgraded", subtitle = "Both PM sensors downgraded",
                           icon = "exclamation", theme = "danger")
      )
      value_box(title = flags$subtitle, value = flags$text, showcase = bs_icon(flags$icon),
                theme = flags$theme)
    })
    
    output$confidence <- renderUI({
      
      l <- get_last()
      validate(need(nrow(l) > 0, "No data for site"))
      
      theme <- case_when(
        l$confidence >= 95 ~ "primary",
        l$confidence < 55 ~ "danger",
        .default = "warning"
      )
      value_box(value = l$confidence, title = "Confidence", 
                showcase = bs_icon("info"), theme = theme)
      
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