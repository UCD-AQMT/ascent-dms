
xactUI <- function(id) {
  
  ns <- NS(id)
  
  df <- tbl(con, I("xact.sample_analysis")) |>
    filter(1 == 0) |>
    collect()
  options <- colnames(df)
  
  elems <- tbl(con, I("xact.element_params")) |>
    arrange(atomic_number) |>
    pull(element)
  
  tagList(
    fluidRow(
      infoBoxOutput(ns("reporting"), width = 3),
      infoBoxOutput(ns("status"), width = 3),
      infoBoxOutput(ns("ambient"), width = 3),
      infoBoxOutput(ns("sample"), width = 3),
      infoBoxOutput(ns("enclosure"), width = 3),

      infoBoxOutput(ns("tube"), width = 3),
      infoBoxOutput(ns("flows"), width = 3),
      infoBoxOutput(ns("volume"), width = 3),
      infoBoxOutput(ns("filament"), width = 3),
      infoBoxOutput(ns("tape"), width = 3),
      infoBoxOutput(ns("sdd"), width = 3),
      infoBoxOutput(ns("dpp"), width = 3)
    ),
    fluidRow(
      column(2, selectInput(ns("plot1_y"), "Parameter", choices = options,
                            selected = "tube")),
      column(10, plotlyOutput(ns("plot1"), height = 150))
    ),
    fluidRow(
      column(2, selectInput(ns("plot2_y"), "Parameter", choices = options,
                            selected = "tape")),
      column(10, plotlyOutput(ns("plot2"), height = 150))
    ),
    fluidRow(
      column(2, selectInput(ns("element"), "Elements", choices = elems,
             selected = "S")),
      column(10, plotlyOutput(ns("ts")))
    ),
    fluidRow(
      column(2, selectInput(ns("element_list"), "Elements", choices = elems,
                            multiple = TRUE, selected = c("S","K"))),
      column(10, plotlyOutput(ns("stacked")))
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
      
      tbl(con, I("xact.sample_analysis")) |>
        inner_join(select(tbl_sites, site_number, site_code), by = "site_number") |>
        filter(site_code == !!site()) |>
        arrange(desc(sample_datetime)) |>
        head(200) |>
        collect()
      
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
    
    
    output$reporting <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      tdiff <- difftime(Sys.time(), s$sample_datetime, units = "hours")
      
      if (tdiff > 48) {
        txt <- "Offline"
        sub <- glue::glue("Last data {round(tdiff / 24)} days ago")
        infoBox("Reporting", txt, subtitle = sub, color = "red", icon = icon("exclamation"),
                width = 3)
      } else if (tdiff > 12) {
        txt <- "Lagging"
        sub <- glue::glue("Last data {round(tdiff)} hours ago")
        infoBox("Reporting", txt, subtitle = sub, color = "yellow", icon = icon("clock"),
                width = 3)
      } else {
        txt <- "Online"
        tdiff <- difftime(Sys.time(), s$sample_datetime, units = "mins")
        sub <- glue::glue("Last data {round(tdiff)} minutes ago")
        infoBox("Reporting", txt, subtitle = sub, color = "blue", icon = icon("check"),
                width = 3)
        
      }
      
    })
    
    output$status <- renderInfoBox({
      
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
        infoBox("Status", status, color = "blue", icon = icon("check"),
                width = 3)
      } else if (s$alarm >= 200) {
        infoBox("Status", status, color = "yellow", icon = icon("question"),
                width = 3)
      } else {
        infoBox("Status", status, color = "red", icon = icon("exclamation"),
                width = 3)
      }
      
    })
    
    output$ambient <- renderInfoBox({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      infoBox("Ambient Temperature", paste0(s$at, " ", "\U00b0", "C"),
              color = "blue", icon = icon("info"))
    })
    
    output$enclosure <- renderInfoBox({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$enclosure > 35) {
        infoBox("Enclosure Temperature", paste0(s$enclosure, " ", "\U00b0", "C"),
                subtitle = "Enclosure temperature very high", color = "red",
                icon = icon("exclamation"))
      } else if (s$enclosure > 30) {
        infoBox("Enclosure Temperature", paste0(s$enclosure, " ", "\U00b0", "C"),
                subtitle = "Enclosure temperature high", color = "yellow",
                icon = icon("question"))
      } else {
        infoBox("Enclosure Temperature", paste0(s$enclosure, " ", "\U00b0", "C"),
                color = "blue",
                icon = icon("check"))
      }
    })    
    
    output$sample <- renderInfoBox({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      infoBox("Sample Temperature", paste0(s$sample, " ", "\U00b0", "C"),
              color = "blue", icon = icon("info"))
    })
    
    output$tube <- renderInfoBox({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      # Tube should be <= enclosure + 13 degrees
      if (s$tube > s$enclosure + 13) {
        infoBox("Tube Temperature", paste0(s$tube, " ", "\U00b0", "C"),
                subtitle = paste0("Tube temp > Enclosure temp + 13 ", "\U00b0", "C"),
                color = "red", icon = icon("exclamation"))
      } else if (s$tube > 40) {
        infoBox("Tube Temperature", paste0(s$tube, " ", "\U00b0", "C"),
                subtitle = "Tube temperature high", color = "yellow",
                icon = icon("question"))
      } else {
        infoBox("Tube Temperature", paste0(s$tube, " ", "\U00b0", "C"),
                color = "blue",
                icon = icon("check"))
      }
    }) 
    
    output$flows <- renderInfoBox({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      infoBox("Flow Act/25/Std", paste0(s$flow_act, "/", s$flow_25, "/", s$flow_std, "  L/min"),
              color = "blue", icon = icon("info"))
    })
    
    output$volume <- renderInfoBox({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      infoBox("Volume", paste(s$volume, "L"),
              color = "blue", icon = icon("info"))
    })
    
    output$filament <- renderInfoBox({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$filament > 3.2) {
        infoBox("Filament", paste(s$filament, "V"),
                subtitle = "Filament voltage high", color = "yellow",
                icon = icon("question"))
      } else {
        infoBox("Filament", paste0(s$filament, "V"),
                color = "blue",
                icon = icon("check"))
      }
    })
    
    output$tape <- renderInfoBox({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      infoBox("Tape Pressure", paste(s$tape, "mmHg"),
              color = "blue", icon = icon("info"))
    })
    
    output$sdd <- renderInfoBox({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      infoBox("SDD", paste0(s$sdd, " ", "\U00b0", "C"),
              color = "blue", icon = icon("info"))
    })
    output$dpp <- renderInfoBox({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      infoBox("DPP", paste0(s$dpp, " ", "\U00b0", "C"),
              color = "blue", icon = icon("info"))
    })
    
    ts_plot <- function(param, df) {
      
      ggplot(df, aes(x = sample_datetime, y = .data[[param]])) + geom_line() +
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