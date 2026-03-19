
xactUI <- function(id) {
  
  ns <- NS(id)
  
  df <- tbl(con, I("xact.sample_analysis")) |>
    filter(1 == 0) |>
    collect()
  options <- colnames(df)
  
  elems <- tbl(con, I("xact.element_params")) |>
    arrange(atomic_number) |>
    pull(element)
  
  page_fillable(
    gap = "8px",
    layout_column_wrap(
      width = 1/6,
      min_height = "200px",
      gap = "6px",
      uiOutput(ns("reporting")),
      uiOutput(ns("status")),
      uiOutput(ns("ambient")),
      uiOutput(ns("sample")),
      uiOutput(ns("enclosure")),
      uiOutput(ns("tube")),
      uiOutput(ns("flows")),
      uiOutput(ns("volume")),
      uiOutput(ns("filament")),
      uiOutput(ns("tape")),
      uiOutput(ns("sdd")),
      uiOutput(ns("dpp"))
    ),
    layout_column_wrap(
      width = 1/6,
      checkboxInput(ns("by_date"), "Specify Date Range", value = FALSE),
      conditionalPanel(
        condition = "input.by_date == 1",
        dateRangeInput(ns("dates"), "Date Range", min = "2023-01-01",
                       max = Sys.Date() + 1, start = Sys.Date() - 7,
                       end = Sys.Date() + 1),
        ns = ns
      )
    ),
    layout_column_wrap(
      width = 1/2,
      gap = "8px",
      card(
        layout_sidebar(
          sidebar = sidebar(selectInput(ns("upscale_element"), "Upscale check", 
                                        choices = c("Nb", "Cr", "Cd", "Pb"),
                                        selected = "Nb")),
          plotlyOutput(ns("upscale"), height = "250px"),
          padding = c(0,5,0,5)
        ),
        full_screen = TRUE,
        class = "border-0"
      ),
      card(
        layout_sidebar(
          sidebar = sidebar(selectInput(ns("plot2_y"), "Parameter", choices = options,
                                        selected = "tape")),
          plotlyOutput(ns("plot2"), height = "250px"),
          padding = c(0,5,0,5)
        ),
        full_screen = TRUE,
        class = "border-0"
      )
    ),
    layout_column_wrap(
      width = 1/2,
      gap = "8px",
      card(
        layout_sidebar(
          sidebar = sidebar(selectInput(ns("element"), "Element", choices = elems,
                                        selected = "S")),
          withSpinner(plotlyOutput(ns("ts")), fill = TRUE),
          height = "250px",
          padding = c(0,5,0,5)
        ),
        full_screen = TRUE,
        class = "border-0"
      ),
      card(
        layout_sidebar(
          sidebar = sidebar(selectInput(ns("element_list"), "Elements", choices = elems,
                                        multiple = TRUE, selected = c("S","K"))),
          plotlyOutput(ns("stacked"), height = "400px"),
          padding = c(0,5,0,5)
        ),
        full_screen = TRUE,
        class = "border-0"
      )
    )
  )
  

}

xactServer <- function(id, site) {
  moduleServer(id, function(input, output, session) {
    
    get_last <- reactive({
      
      get_range() |>
        arrange(desc(sample_datetime)) |>
        slice(1)
      
    })
    
    get_range <- reactive({
      
      invalidateLater(1000 * 60 * 3) # every three minutes
      
      if (!input$by_date) {
       df <- tbl(con, I("xact.sample_analysis")) |>
         inner_join(select(tbl_sites, site_number, site_code), by = "site_number") |>
         filter(site_code == !!site()) |>
         arrange(desc(sample_datetime)) |>
         head(200) |>
         collect()
      } else {
        interval <- input$dates[2] - input$dates[1]
        validate(need(interval >= 0, "waiting for valid date range"))
        validate(need(interval < 180, "Please limit date interval for Xact to 180 days"))
        df <- tbl(con, I("xact.sample_analysis")) |>
          inner_join(select(tbl_sites, site_number, site_code), by = "site_number") |>
          filter(site_code == !!site(),
                 sample_datetime >= !!input$dates[1],
                 sample_datetime <= !!input$dates[2]) |>
          collect()
      }
      
      validate(need(nrow(df) > 0, "No data in time range"))
      
      df
      
    })
    
    get_elements <- reactive({
      
      # Use date range from get_range() to determine query range
      range <- get_range()
      first <- min(range$sample_datetime)
      last <- max(range$sample_datetime)
      
      df <- tbl(con, I("xact.raw_measurements")) |>
        select(-id, -site_record_id, -site_number) |>
        inner_join(tbl(con, I("xact.sample_analysis")),
                   by = c("sample_analysis_id"="id")) |>
        inner_join(select(tbl_sites, site_number, site_code), by = "site_number") |>
        filter(site_code == !!site(),
               sample_type == 1,
               sample_datetime >= first,
               sample_datetime <= last) |>
        arrange(desc(sample_datetime)) |>
        collect()
      
    })
    
    get_qc_samples <- reactive({
      
      # Use date range from get_range() to determine query range
      range <- get_range()
      first <- min(range$sample_datetime)
      last <- max(range$sample_datetime)
      
      df <- tbl(con, I("xact.raw_measurements")) |>
        select(-id, -site_record_id, -site_number) |>
        inner_join(tbl(con, I("xact.sample_analysis")),
                   by = c("sample_analysis_id"="id")) |>
        inner_join(select(tbl_sites, site_number, site_code), by = "site_number") |>
        filter(site_code == !!site(),
               sample_type == 2,
               sample_datetime >= first,
               sample_datetime <= last) |>
        arrange(desc(sample_datetime)) |>
        collect()

    })
    
    make_valuebox <- function(title, value, theme) {
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
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      tdiff <- difftime(Sys.time(), s$sample_datetime, units = "hours")
      
      if (tdiff > 48) {
        txt <- "Offline"
        sub <- glue::glue("Last data {round(tdiff / 24)} days ago")
        th <- "danger"
      } else if (tdiff > 12) {
        txt <- "Lagging"
        sub <- glue::glue("Last data {round(tdiff)} hours ago")
        th <- "warning"
      } else {
        txt <- "Online"
        tdiff <- difftime(Sys.time(), s$sample_datetime, units = "mins")
        sub <- glue::glue("Last data {round(tdiff)} minutes ago")
        th <- "primary"
      }
      value_box(title = txt, value = sub, theme = th, max_height = "130px")
      
    })
    
    output$status <- renderUI({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      status <- case_match(s$alarm,
                           0 ~ "OK",
                           100 ~ "Xray V",
                           101 ~ "Xray I",
                           102 ~ "Tube Temp",
                           103 ~ "Enclosure Temp",
                           104 ~ "Tape",
                           105 ~ "Pump",
                           106 ~ "Filter Wheel",
                           107 ~ "Dynamic Rod",
                           108 ~ "Nozzle",
                           109 ~ "Ecal",
                           110 ~ "Software",
                           111 ~ "Flow",
                           200 ~ "Upscale Cr",
                           201 ~ "Upscale Pb",
                           202 ~ "Upscale Cd",
                           203 ~ "Upscale Nb",
                           204 ~ "Upscale EC4",
                           205 ~ "Nb Above Max",
                           206 ~ "Tape Low",
                           211 ~ "Low Flow",
                           213 ~ "Processing Error",
                           .default = "Unknown Error")
      if (s$alarm == 0) {
        th <- "primary"
      } else if (s$alarm >= 200) {
        th <- "warning"
      } else {
        th <- "danger"
      }
      value_box("Status", value = status, theme = th, max_height = 130)
      
    })
    
    output$ambient <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      value_box("Ambient Temp", value = paste0(s$at, " ", "\U00b0", "C"), theme = "info", 
                max_height = "130px")
    })
    
    output$enclosure <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$enclosure > 35) {
        th <- "danger"
      } else if (s$enclosure > 30) {
        th <- "warning"
      } else {
        th <- "primary"
      }
      value_box("Enclosure Temp", value = paste0(s$enclosure, " ", "\U00b0", "C"),
                theme = th, max_height = "130px")
    })    
    
    output$sample <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      value_box("Sample Temperature", value = paste0(s$sample, " ", "\U00b0", "C"),
              theme = "info", max_height = "130px")
    })
    
    output$tube <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      # Tube should be <= enclosure + 13 degrees
      if (s$tube > s$enclosure + 13) {
        title <- paste0("Tube temp > Enclosure temp + 13 ", "\U00b0", "C")
        th <- "danger"
      } else if (s$tube > 40) {
        title <- "Tube temp high"
        th <- "warning"
      } else {
        title <- "Tube temp"
        th <- "primary"
      }
      value_box(title = title, value = paste0(s$tube, " ", "\U00b0", "C"), theme = th,
                max_height = "130px")
    }) 
    
    output$flows <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      value_box("Flow Act/25/Std", value = paste0(s$flow_act, "/", s$flow_25, "/", s$flow_std, "  L/min"),
              theme = "info", max_height = "130px")
    })
    
    output$volume <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      value_box("Volume", value = paste(s$volume, "L"), theme = "info", max_height = "130px")
    })
    
    output$filament <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$filament > 3.2) {
        title <- "Filament voltage high"
        th <- "warning"
      } else {
        title <- "Filament"
        th <- "primary"
      }
      value_box(title = title, value = paste0(s$filament, "V"), theme = th, 
                max_height = "130px")
    })
    
    output$tape <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      value_box("Tape Pressure", value = paste(s$tape, "mmHg"), theme = "info", 
                max_height = "130px")
      
    })
    
    output$sdd <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      value_box("SDD", value = paste0(s$sdd, " ", "\U00b0", "C"), theme = "info", 
                max_height = "130px")
    })
    
    output$dpp <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      value_box("DPP", value = paste0(s$dpp, " ", "\U00b0", "C"), theme = "info", 
                max_height = "130px")
    })
    
    ts_plot <- function(param, df) {
      
      ggplot(df, aes(x = sample_datetime, y = .data[[param]])) + geom_line() +
        scale_x_datetime(labels = scales::label_date()) +
        theme(axis.title.x = element_blank())
      
    }
    
    
    output$upscale <- renderPlotly({
      
      if (input$upscale_element == "Nb") {
        df <- get_elements() |>
          filter(element == input$upscale_element)
      } else {
        df <- get_qc_samples() |>
          filter(element == input$upscale_element)
      }

      # get the settings (temporarily read from csv)
      param <- paste0("upscale_", tolower(input$upscale_element))
      
      upscale_settings <- xact_settings |>
        filter(name == param) |>
        rename(upscale=value)
      
      df <- df |>
        left_join(upscale_settings,
                  join_by(site_number, between(sample_datetime, start_date, end_date))) |>
        mutate(upscale_min = upscale * 0.85,
               upscale_max = upscale * 1.15)
      
      
      g <- ggplot(df, aes(x = sample_datetime, y = value)) + 
        geom_hline(aes(yintercept = upscale_min), linetype = "dashed") +
        geom_hline(aes(yintercept = upscale_max), linetype = "dashed") +
        geom_line() +
        geom_point() +
        scale_x_datetime(labels = scales::label_date()) +
        labs(y = ngm3()) +
        theme(axis.title.x = element_blank())
      ggplotly(g, dynamicTicks = TRUE)
      
      
    })
    
    output$plot2 <- renderPlotly({
      
      df <- get_range()
      g <- ts_plot(input$plot2_y, df)
      ggplotly(g, dynamicTicks = TRUE)
      
    })
    
    
    output$ts <- renderPlotly({
      
      df <- get_elements() |>
        filter(element == input$element)
      
      g <- ggplot(df, aes(x = sample_datetime, y = value)) +
        geom_line() +
        geom_point() +
        geom_errorbar(aes(ymin = value - uncertainty,
                          ymax = value + uncertainty)) +
        scale_x_datetime(labels = scales::label_date()) +
        labs(y = paste("Concentration", ngm3())) +
        theme(axis.title.x = element_blank())
      
      ggplotly(g, dynamicTicks = TRUE)
      
    })
    
    output$stacked <- renderPlotly({
      
      df <- get_elements() |>
        filter(element %in% input$element_list)
      
      g <- ggplot(df, aes(x = sample_datetime, y = value, fill = element)) +
        geom_bar(stat = "identity") +
        scale_x_datetime(labels = scales::label_date()) +
        labs(y = paste("Concentration", ngm3())) +
        theme(axis.title.x = element_blank())
      
      ggplotly(g, dynamicTicks = TRUE)
      
    })
    
  })
}