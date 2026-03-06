
siteUI <- function(id) {
  
  ns <- NS(id)
  today <- Sys.Date() + 1
  
  page_fillable(
    layout_column_wrap(
      width = 1/6,
      selectInput(ns("site"), "Site", choices = site_names$site_name),
      dateInput(ns("date"), "Date", min = minimum_date, value = Sys.Date()),
      actionButton(ns("prev"), "< Prev"),
      actionButton(ns("nextdt"), "Next >"),
      p("All data displayed in local time")
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("ACSM"),
        plotOutput(ns("acsm"))
        ),
      card(
        card_header("Xact"),
        plotOutput(ns("xact"))
        ),
      card(
        card_header("Aethalometer"),
        plotOutput(ns("ae33"))
        ),
      card(
        card_header("SMPS"),
        plotOutput(ns("smps"))
        )
    )
  )
  
}

siteServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Date navigation
    observeEvent(input$prev, {
      dt <- input$date - lubridate::days(1)
      updateDateInput(session, "date", value = dt)
    })
    
    observeEvent(input$nextdt, {
      dt <- input$date + lubridate::days(1)
      updateDateInput(session, "date", value = dt)
    })

    utc_dates <- reactive({
      
      min_time_local <- lubridate::force_tz(input$date, tzone = local_tz())
      min_time_utc <- lubridate::with_tz(min_time_local, tzone = "UTC")
      max_time_utc <- min_time_utc + lubridate::days(1)
      
      c(min_time_utc, max_time_utc)
      
    })
    
    local_tz <- reactive({
      tz <- tbl_sites |>
        filter(site_name == input$site) |>
        pull(timezone)
    })
    
    
    output$acsm <- renderPlot({
      
      dts <- utc_dates()
    
      df <- tbl(con, I("acsm.sample_analysis")) |>
        inner_join(select(tbl_sites, site_number, site_code, site_name), by = "site_number") |>
        inner_join(tbl(con, I("acsm.mass_loadings")), by = c("id"="sample_analysis_id")) |>
        filter(site_name == !!input$site,
               start_date >= !!dts[1],
               start_date <= !!dts[2]) |>
        collect()
    
      validate(need(nrow(df) > 0, "No data for time period"))
      
      pdf <- df |>
        select(start_date, chl:so4) |>
        tidyr::pivot_longer(chl:so4, names_to = "param", values_to = "value")
      
      
      g <- ggplot(pdf) +
        geom_bar(aes(x = start_date, y = value, fill = param), stat = "identity") +
        scale_fill_manual(values = acsm_colors) +
        scale_x_datetime(labels = scales::label_date_short(tz = local_tz()),
                         date_breaks = "2 hours") +
        labs(y = expression("ACSM Species"~(mu*g/m^3))) +
        theme(axis.title.x = element_blank())
      g 
      
    })
    
    output$xact <- renderPlot({
      
      dts <- utc_dates()
      
      df <- tbl(con, I("xact.raw_measurements")) |>
        select(-id, -site_record_id, -site_number) |>
        inner_join(select(tbl(con, I("xact.sample_analysis")),
                          sample_analysis_id=id, sample_datetime, site_number, sample_type),
                   by = "sample_analysis_id") |>
        inner_join(select(tbl_sites, site_number, site_code, site_name), by = "site_number") |>
        filter(site_name == !!input$site,
               sample_type == 1,
               sample_datetime >= !!dts[1],
               sample_datetime <= !!dts[2]) |>
        arrange(desc(sample_datetime)) |>
        collect()
      
      validate(need(nrow(df) > 0, "No data for time period"))
      
      # limit to the 8 most abundant elements in the data
      elems <- df |>
        filter(element != "Nb") |>
        summarise(value = sum(value, na.rm = TRUE),
                  .by = element) |>
        arrange(desc(value)) |>
        slice(1:8) |>
        pull(element)
      
      df <- df |>
        filter(element %in% elems)
      
      ggplot(df, aes(x = sample_datetime, y = value, fill = element)) +
        geom_bar(stat = "identity") +
        scale_x_datetime(labels = scales::label_date_short(tz = local_tz()),
                         date_breaks = "2 hours") +
        labs(y = expression("Most abundant elements"~(ng/m^3))) +
#        theme_minimal(base_size = 14) +
        theme(axis.title.x = element_blank())
    
    })
    
    output$ae33 <- renderPlot({
      
      dts <- utc_dates()
      
      # get 15 minute averages
      site <- tbl_sites |>
        filter(site_name == input$site) |>
        pull(site_code)

      flux_query <- glue::glue('from(bucket: "measurements") |> ',
                               'range(start: {strftime(dts[1], format = "%FT%TZ")},',
                               'stop: {strftime(dts[2], format = "%FT%TZ")}) |> ',
                               'filter(fn: (r) => r._measurement == "ae33_{site}_raw" ',
                               'and (r._field == "EBC_1" or r._field == "EBC_2" or r._field == "EBC_3" ',
                               'or r._field == "EBC_4" or r._field == "EBC_5" or r._field == "EBC_6")) |> ',
                               'drop(columns: ["_start", "_stop"]) |> ',
                               'aggregateWindow(every: 15m, fn: mean)',)
            
      df_list <- ae33_con$query(flux_query)
      validate(need(!is.null(df_list), "No data in time period"))
      
      df <- df_list |>
        purrr::list_rbind() |>
        select(time, parameter=`_field`, value=`_value`) |>
        filter(!is.na(value)) |>
        left_join(ae33_channels, by = "parameter")
      
      validate(need(nrow(df > 0), "No data in time period"))
      
      g <- ggplot(df, aes(x = time, y = value, color = factor(wavelength))) +
        geom_line() +
        scale_x_datetime(labels = scales::label_date_short(), date_breaks = "2 hours") +
        labs(y = expression("BC by wavelength "~(mu*g/m^3)),
             color = "wavelength") +
        #theme_minimal(base_size = 14) +
        theme(axis.title.x = element_blank())
      
      g
      
    })
    
    output$smps <- renderPlot({
      
      dts <- utc_dates()
      
      site <- tbl_sites |>
        filter(site_name == input$site) |>
        pull(site_code)
      
      
      df <- tbl(con, I("smps.sample_analysis")) |>
        inner_join(select(tbl_sites, site_code, site_number), 
                   by = "site_number") |>
        filter(sample_start >= !!dts[1],
               sample_start <= !!dts[2],
               site_code == site) |>
        select(site_code, datetime=sample_start, concentration_json) |>
        collect()
        
      shiny::validate(need(nrow(df) > 0, "No data for time period."))
        
      dNdlogDp <- data.matrix(jsonlite::stream_in(textConnection(df$concentration_json), 
                                                  verbose = FALSE))

      df <- df |>
        select(datetime) |>
        bind_cols(as.data.frame(dNdlogDp)) |>
        tidyr::pivot_longer(!datetime, names_to = "midpoint", values_to = "value") |>
        mutate(midpoint = as.numeric(midpoint)) |>
        filter(value > 0)

      g <- ggplot(df, aes(x = datetime, y = midpoint, fill = value, color = value)) + 
        geom_tile() +
        scale_y_log10(limits = c(13, 600), expand = c(0, 0)) +
        scale_x_datetime(labels = scales::label_date_short(tz = local_tz()),
                         expand = c(0, 0), date_breaks = "2 hours") +
        guides(color = "none") +
        scale_fill_viridis_c(option = "H", limits = c(0, 50000), oob = scales::squish) +
        scale_color_viridis_c(option = "H", limits = c(0, 50000), oob = scales::squish) +
        labs(y = "mobility diameter (nm)",
             fill = "dN/dlogDp") +
        theme(legend.position = "top",
              legend.direction = "horizontal",
              legend.key.width = unit(30, "mm"),
              axis.title.x = element_blank()) 
      g
    })
    
    
  })
}