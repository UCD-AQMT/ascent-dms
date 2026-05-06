
networkUI <- function(id) {
  
  ns <- NS(id)
  
  page_fillable(
    layout_column_wrap(
      width = 1/6,
      selectInput(ns("parameter"), "Parameter", choices = grouped_parameters),
      dateInput(ns("date"), "Date", min = minimum_date, value = Sys.Date()),
      actionButton(ns("prev"), "< Prev"),
      actionButton(ns("nextdt"), "Next >"),
      p("Data are preliminary and unvalidated. All data displayed in local time.")
    ),
    layout_column_wrap(
      width = NULL,
      style = css(grid_template_columns = "3fr 5fr"),
      layout_column_wrap(
        width = 1,
        card(
          max_height = "400px",
          plotOutput(ns("map"))
        ),
        card(
          textOutput(ns("dummy"))
        )
      ),
      card(
        checkboxInput(ns("free_y"), "Independant y-scales", value = FALSE),
        plotOutput(ns("ts")),
        full_screen = TRUE
      )
    )
  )
  
}
networkServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$dummy <- renderText("What should go in this space?")
    
    
    # background map
    basemap <- png::readPNG("./data/basemap2.png")
    coords <- read.csv("./data/static_map_coords_800x450.csv")
    xdim <- 800
    ydim <- 450
 
    # Date navigation
    observeEvent(input$prev, {
      dt <- input$date - lubridate::days(1)
      updateDateInput(session, "date", value = dt)
    })
    
    observeEvent(input$nextdt, {
      dt <- input$date + lubridate::days(1)
      updateDateInput(session, "date", value = dt)
    })
    
    units <- reactive({
      
      p <- input$parameter
      if (p == "total_concentration") {
        u <- expression("#"/cm^2)
      } else if (p %in% c("mean", "median", "geo_mean")) {
        u <- "nm"
      } else if (p == "geo_std_dev") {
        u <- "unitless"
      } else if (nchar(p) <= 2) {
        # elemental symbol
        u <- expression(ng/m^3)
      } else if (substr(p, 1, 2) == "BC") {
        # Aethalometer
        u <- expression(mu*g/m^3)
      } else {
        # ACSM
        u <- expression(mu*g/m^3)
      }
    })
    
    plot_data <- reactive({
      
      # Get a wider range of data make sure we have all hours in local time
      min_date <- input$date - 1
      max_date <- input$date + 1
      
      # What instrument is this from
      instrument <- names(which(sapply(grouped_parameters, \(x) input$parameter %in% x)))
      
      if (instrument == "ACSM") {
        
        df <- tbl(con, I("acsm.sample_analysis")) |>
          select(sample_analysis_id=id, start_date, site_number) |>
          inner_join(select(tbl(con, I("acsm.mass_loadings")),
                            sample_analysis_id, value=input$parameter),
                     by = "sample_analysis_id") |>
          inner_join(select(tbl_sites, site_number, site_code, site_name, timezone),
                     by = "site_number") |>
          filter(start_date >= min_date,
                 start_date <= max_date) |>
          collect() |>
          rename(date_utc=start_date)
        
      }
      
      if (instrument == "Xact") {
        
        df <- tbl(con, I("xact.raw_measurements")) |>
          select(-id, -site_record_id, -site_number) |>
          inner_join(select(tbl(con, I("xact.sample_analysis")),
                            sample_analysis_id=id, sample_datetime, site_number, sample_type),
                     by = "sample_analysis_id") |>
          inner_join(select(tbl_sites, site_number, site_code, site_name, timezone),
                     by = "site_number") |>
          filter(sample_type == 1,
                 element == !!input$parameter,
                 sample_datetime >= min_date,
                 sample_datetime <= max_date) |>
          collect() |>
          rename(date_utc=sample_datetime)
        df
        
      }
      
      if (instrument == "SMPS") {
        
        df <- tbl(con, I("smps.sample_analysis")) |>
          inner_join(select(tbl_sites, site_code, site_number, site_name, timezone), 
                     by = "site_number") |>
          filter(sample_start >= min_date,
                 sample_start <= max_date) |>
          select(site_code, site_name, date_utc=sample_start, timezone, total_concentration, mean, 
                 median, geo_mean, geo_std_dev) |>
          collect()
        
        df <- df |>
          tidyr::pivot_longer(total_concentration:geo_std_dev, names_to = "param") |>
          filter(param == input$parameter)
        
      }
      
      if (instrument == "AE33") {
        
        flux_query <- glue::glue('from(bucket: "measurements") |> ',
                                 'range(start: {strftime(min_date, format = "%FT%TZ", tz = "UTC")},',
                                 'stop: {strftime(max_date, format = "%FT%TZ", tz = "UTC")}) |> ',
                                 'filter(fn: (r) => r._field == "{input$parameter}") |> ',
                                 'drop(columns: ["_start", "_stop"]) |> ',
                                 'aggregateWindow(every: 15m, fn: mean)')
        
        df_list <- ae33_con$query(flux_query)
        shiny::validate(need(!is.null(df_list), "No data in time period"))
       
        df <- df_list |>
          purrr::list_rbind() |>
          select(date_utc=time, influx_site=`_measurement`, value=`_value`) |>
          filter(!is.na(value)) |>
          inner_join(influx_sites, by = "influx_site") |>
          inner_join(select(tbl_sites, site_code, site_number, site_name, timezone), 
                     by = "site_code", copy = TRUE)
        
      }
      
      # Trim to local time
      df <- df |>
        mutate(local_time = lubridate::with_tz(date_utc, tzone = timezone)) |>
        filter(local_time >= input$date,
               local_time < max_date)
      
    })
    
    
    output$map <- renderPlot({
      
      df <- plot_data() |>
        summarise(value = mean(value, na.rm = TRUE),
                  .by = site_code)
      
      df <- df |>
        left_join(coords, by = c("site_code"="Site")) |>
        mutate(x1 = x / xdim,
               y1 = 1 - (y / ydim))
      
      ggplot(df, aes(x = x1, y = y1, color = value)) +
        background_image(basemap) +
        geom_point(size = 7) +
        scale_color_viridis_c() +
        scale_x_continuous(limits = c(0, 1), expand = 0) +
        scale_y_continuous(limits = c(0, 1), expand = 0) +
        labs(color = units(),
             title = paste0(input$parameter, " - 24-hr average")) +
        theme_void()

    })
    
    output$ts <- renderPlot({
      
      df <- plot_data() |>
        mutate(site_name = factor(site_name, levels = site_names$site_name))

      g <- ggplot(df, aes(x = local_time, y = value)) + 
        geom_line() +
        labs(y = units())
      
      if (input$free_y) {
        g <- g +
          facet_wrap(~site_name, ncol = 2, scales = "free_y", dir = "v")
      } else {
        g <- g +
          facet_wrap(~site_name, ncol = 2, dir = "v")
      }
      
      g

    })

  })
}

# From ggpubr - add a raster background to a ggplot
background_image <- function(raster.img) {
  annotation_raster(raster.img, interpolate = TRUE,
                    xmin = -Inf, xmax = Inf,
                    ymin = -Inf, ymax = Inf
  )
}