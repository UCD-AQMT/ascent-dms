
networkUI <- function(id) {
  
  ns <- NS(id)
  
  page_fillable(
    dateInput(ns("date"), "Date", min = minimum_date, value = Sys.Date()),
    selectInput(ns("parameter"), "Parameter", choices = c("chloride"="chl", "ammonia"="nh4",
                                                          "nitrate"="no3", "organics"="org",
                                                          "sulfate"="so4")),
    layout_column_wrap(
      width = 1/2,
      card(plotOutput(ns("map"))),
      card(plotOutput(ns("ts")))
    )
    
  )
  
}

networkServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # background map
    states_sf <- sf::st_read(system.file("/data/gis/cb_2018_us_state_5m.shp", 
                                         package="ascentr"))
    
    plot_data <- reactive({
      
      # Toy data for now - just org from ACSM
      
      min_time <- as.POSIXct(input$date)
      max_time <- as.POSIXct(input$date) + lubridate::days(1)

      df <- tbl(con, I("acsm.sample_analysis")) |>
        select(sample_analysis_id=id, start_date, site_number) |>
        inner_join(select(tbl(con, I("acsm.mass_loadings")),
                          sample_analysis_id, value=input$parameter),
                   by = "sample_analysis_id") |>
        inner_join(select(tbl(con, I("common.sites")),
                          site_number, site_code, latitude, longitude),
                   by = "site_number") |>
        filter(start_date >= min_time,
               start_date <= max_time) |>
        collect()
      df
      
    })
    
    
    output$map <- renderPlot({
      
      df <- plot_data()
      
      dfs <- sf::st_as_sf(df, coords = c("longitude", "latitude"),
                          crs = "WGS84") |>
        summarise(value = mean(value, na.rm = TRUE),
                  .by = site_code,
                  across(geometry, sf::st_union))
      
      
      p <- map_ascent(dfs, states = states_sf)
      p
      
    })
    
    output$ts <- renderPlot({
      
      df <- plot_data()
      
      ggplot(df, aes(x = start_date, y = value)) + 
        geom_line() +
        geom_point(size = 1) +
        facet_wrap(~site_code, ncol = 1) +
        theme_minimal(base_size = 14)

    })

  })
}