
siteUI <- function(id) {
  
  ns <- NS(id)
  today <- Sys.Date() + 1
  start_day <- today - 7
  
  elems <- tbl(con, I("xact.element_params")) |>
    arrange(atomic_number) |>
    pull(element)
  
  layout_sidebar(
    sidebar = sidebar(
      dateRangeInput(ns("dates"), "Dates", start = start_day, end = today,
                               min = "2023-01-01", max = today),
      selectInput(ns("mass"), "Aerosol Mass", multiple = TRUE,
                            choices = c("SMPS (loads slowly)"="smps", 
                                        "Purple Air"="purpleair",
                                        "ACSM + BC + Xact (minus S)"="acsm"),
                            selected = c("smps", "purpleair", "acsm")),
      selectInput(ns("elements"), "Elements", choices = elems, multiple = TRUE, 
                            selected = c("S", "Si", "Cl", "K", "Al", "Fe", "Ca")),
      selectInput(ns("fraction"), "Mass Fraction", 
                            choices = c("Total Mass", "Fraction of Mass"),
                            selected = "Total Mass"),
      input_task_button(ns("go"), "Plot")
      ),
    card(plotOutput(ns("plot"), height = 1000))
  )
}

siteServer <- function(id, site) {
  moduleServer(id, function(input, output, session) {

    # Data reactives -------
    
    date_range <- reactive({
      c(as.POSIXct(input$dates[1]), as.POSIXct(input$dates[2]))
    })
    
    purpleair_ts <- reactive({
      
      df <- tbl(con, I("purpleair.sample_analysis")) |>
        inner_join(tbl(con, I("purpleair.sensors")), by = "sensor_index") |>
        inner_join(select(tbl_sites, site_number, site_code),
                   by = "site_number") |>
        filter(site_code == !!site(),
               last_seen >= !!input$dates[1],
               last_seen <= !!input$dates[2]) |>
        select(last_seen, pm2_5_atm) |>
        mutate(time_hour = lubridate::floor_date(last_seen, "hour")) |>
        summarise(PA_PM25 = median(pm2_5_atm, na.rm = TRUE),
                  .by = time_hour) |>
        collect()
      df
    })
    
    xact_ts <- reactive({
      
      df <- tbl(con, I("xact.sample_analysis")) |>
        select(id, site_number, sample_datetime, sample_type) |>
        inner_join(select(tbl(con, I("xact.raw_measurements")),  
                          sample_analysis_id, element, value),
                   by = c("id"="sample_analysis_id")) |>
        inner_join(select(tbl_sites, site_number, site_code),
                   by = "site_number") |>
        filter(site_code == !!site(),
               sample_datetime >= !!input$dates[1],
               sample_datetime <= !!input$dates[2],
               sample_type == 1) |>
        select(sample_datetime, element, value) |>
        collect()
      df
      
    })
    
    smps_ts <- reactive({
      
      df <- tbl(con, I("smps.sample_analysis")) |>
        inner_join(select(tbl_sites, site_code, site_number), 
                   by = "site_number") |>
        filter(sample_start >= !!input$dates[1],
               sample_start <= !!input$dates[2],
               site_code == !!site()) |>
        select(site_code, datetime=sample_start, concentration_json) |>
        collect()
    
      if (nrow(df) == 0) {
        df <- df |>
          select(time_hour=datetime) |>
          tibble::add_column(SMPS = NA_real_)
        return(df)
      }
      
      smps_records <- jsonlite::stream_in(textConnection(df$concentration_json), 
                                          verbose = FALSE)
      midpoints <- as.numeric(names(smps_records))
      
      dM <- dMdlogDp(midpoints, smps_records) * dlogDp(midpoints)
      M <- rowSums(dM)
      
      df <- df |>
        select(datetime) |>
        mutate(value = M,
               time_hour = lubridate::floor_date(datetime, "hour")) |>
        summarise(SMPS = median(value, na.rm = TRUE),
                  .by = time_hour)
      return(df)
      
    })
    
    acsm_ts <- reactive({
      
      df <- tbl(con, I("acsm.sample_analysis")) |>
        inner_join(select(tbl_sites, site_number, site_code),
                   by = "site_number") |>
        inner_join(select(tbl(con, I("acsm.mass_loadings")), -id, -site_record_id), 
                   by = c("id"="sample_analysis_id")) |>
        filter(site_code == !!site(),
               start_date > !!input$dates[1],
               start_date < !!input$dates[2]) |>
        select(start_date, chl:so4) |>
        collect() |>
        tidyr::pivot_longer(chl:so4, names_to = "param", values_to = "value")
      
      df
      
    })
    
    ae33_ts <- reactive({

      flux_query <- glue::glue('from(bucket: "measurements") |> ', 
                               'range(start: {input$dates[1]}T00:00:00Z,',
                               'stop: {input$dates[2]}T23:59:59Z) |> ', 
                               'filter(fn: (r) => r._measurement == "ae33_{site()}_raw"',
                               'and r._field == "EBC_6") |> ',
                               'aggregateWindow(every: 1h, fn: median) |> ',
                               'drop(columns: ["_start", "_stop"])')
    
      bc6 <- ae33_client$query(flux_query)[[1]]

    })
    
    ## Plots -----
    pm <- reactive({
      
      if ("purpleair" %in% input$mass) {
        pa <- purpleair_ts()  
      } else {
        pa <- tibble(time_hour = as.POSIXct(NA),
                     PA_PM25 = numeric(0))
      }
      
      if ("acsm" %in% input$mass) {
        # For Xact, need total minus S
        xact <- xact_ts() |>
          filter(!element %in% c("S", "Nb")) |>
          summarise(Metals = sum(value, na.rm = TRUE) / 1000,
                    .by = sample_datetime) |>
          mutate(time_hour = lubridate::floor_date(sample_datetime, "hour")) |>
          summarise(Metals = median(Metals, na.rm = TRUE),
                    .by = time_hour)
        
        # If 4-hr data, fill in gaps
        if (site() %in% c("DeltaJunction", "Yellowstone", "LookRock",
                          "CheekaPeak", "JoshuaTree")) {
          x2 <- xact |>
            mutate(time_hour = time_hour + lubridate::hours(1))
          x3 <- xact |>
            mutate(time_hour = time_hour - lubridate::hours(2))
          x4 <- xact |>
            mutate(time_hour = time_hour - lubridate::hours(1))
          xact <- bind_rows(xact, x2, x3, x4)
        }

        ae33 <- ae33_ts()
        if (is.null(ae33)) {
          ae33 <- tibble(time_hour = as.POSIXct(NA),
                         BC = numeric(0))
        } else {
          ae33 <- select(ae33, time_hour=time, BC=`_value`)  
        }
        acsm <- acsm_ts() |>
          mutate(time_hour = lubridate::floor_date(start_date, "hour")) |>
          summarise(value = median(value, na.rm = TRUE),
                    .by = c(time_hour, param)) |>
          summarise(ACSM = sum(value, na.rm = TRUE),
                    .by = time_hour)
        
        acsm_all <- acsm |>
          full_join(xact, by = "time_hour") |>
          full_join(ae33, by = "time_hour") |>
          mutate(Comb = ACSM + Metals + BC) |>
          select(time_hour, Comb)
        
      } else {
        acsm_all <- tibble(time_hour = as.POSIXct(NA),
                           Comb = numeric(0))
      }

      if ("smps" %in% input$mass) {
        smps <- smps_ts()  
      } else {
        smps <- tibble(time_hour = as.POSIXct(NA),
                       SMPS = numeric(0))
      }

      ts_data <- pa |>
        full_join(smps, by = "time_hour") |>
        full_join(acsm_all, by = "time_hour") |>
        select(time_hour, Comb, SMPS, PA_PM25) |>
        tidyr::pivot_longer(Comb:PA_PM25, names_to = "name", values_to = "value") |>
        mutate(name = if_else(name == "Comb", "ACSM + BC +\nXact (minus S)",
                              if_else(name == "PA_PM25", "PurpleAir PM2.5",
                                      name)))
      
      title <- site_names |>
        filter(site_code == site()) |>
        pull(site_name)
      
      g <- ggplot(ts_data, aes(x = time_hour, y = value, color = name)) + 
        geom_line(linewidth = 1) +
        scale_x_datetime(labels = scales::label_date_short(),
                         limits = date_range()) +
        labs(y = expression(atop("Aerosol Mass", mu*g~m^-3)),
             title = title) +
        theme(axis.title.x = element_blank())

    })
    
    acsm <- reactive({
      
      acsm <- acsm_ts() |>
        mutate(time_hour = lubridate::floor_date(start_date, "hour")) |>
        summarise(value = median(value, na.rm = TRUE),
                  .by = c(time_hour, param))

      g <- ggplot(acsm, aes(x = time_hour, y = value, color = param)) + 
        geom_line(linewidth = 1) +
        geom_point(size = 1.8) +
        scale_color_manual(values = acsm_colors) +
        scale_x_datetime(labels = scales::label_date_short(),
                         limits = date_range()) +
        labs(y = expression(atop("ACSM Species", mu*g~m^-3))) +
        theme(axis.title.x = element_blank())
      
    })
    
    xact <- reactive({
      
      df <- xact_ts() |>
        filter(element %in% input$elements) |>
        mutate(time_hour = lubridate::floor_date(sample_datetime, "hour"),
               value = value / 1000) |>
        summarise(value = median(value, na.rm = TRUE),
                  .by = c(time_hour, element))

      g <- ggplot(df, aes(x = time_hour, y = value, color = element)) + 
        geom_line(linewidth = 1) +
        geom_point(size = 1.8) +
        scale_x_datetime(labels = scales::label_date_short(),
                         limits = date_range()) +
        labs(y = expression(atop("Xact Elements", mu*g~m^-3))) +
        theme(axis.title.x = element_blank())

    })
    
    aeth <- reactive({
      
      df <- ae33_ts()
      if (is.null(df)) {
        df <- tibble(time_hour = as.POSIXct(NA),
                     BC = numeric(0))
      } else {
        df <- df |>
          select(time_hour=time, BC=`_value`) 
      }
      
      g <- ggplot(df, aes(x = time_hour, y = BC)) + 
        geom_line(linewidth = 1) +
        geom_point(size = 1.8) +
        scale_x_datetime(labels = scales::label_date_short(),
                         limits = date_range()) +
        labs(y = expression(atop("Black Carbon", mu*g~m^-3))) +
        theme(axis.title.x = element_blank())

    })
    
    mass_fraction <- reactive({
      acsm <- acsm_ts() |>
        mutate(time_hour = lubridate::floor_date(start_date, "hour")) |>
        summarise(value = median(value, na.rm = TRUE),
                  .by = c(time_hour, param))
      
      # For Xact, need total minus S
      xact <- xact_ts() |>
        filter(!element %in% c("S", "Nb")) |>
        summarise(Metals = sum(value, na.rm = TRUE) / 1000,
                  .by = sample_datetime) |>
        mutate(time_hour = lubridate::floor_date(sample_datetime, "hour")) |>
        summarise(Metals = median(Metals, na.rm = TRUE),
                  .by = time_hour)

      # # If 4-hr data, fill in gaps
      if (site() %in% rural_sites) {
        x2 <- xact |>
          mutate(time_hour = time_hour + lubridate::hours(1))
        x3 <- xact |>
          mutate(time_hour = time_hour - lubridate::hours(2))
        x4 <- xact |>
          mutate(time_hour = time_hour - lubridate::hours(1))
        xact <- bind_rows(xact, x2, x3, x4)
      }
      xact <- xact |>
        mutate(param = "xact") |>
        rename(value=Metals)
      
      if (nrow(xact) == 0 | nrow(acsm) == 0) {
        return(NULL)
      }
      
      # We want BC too (I think)
      ae33 <- ae33_ts()
      if (is.null(ae33)) {
        ae33 <- tibble(time_hour = as.POSIXct(NA),
                       BC = numeric(0))
      } else {
        ae33 <- ae33 |>
          mutate(param = "BC") |>
          select(time_hour=time, param, value=`_value`)
      }

      if (input$fraction == "Fraction of Mass") {
        # Set negatives to 0 to make a reasonable mass fraction plot
        df <- bind_rows(acsm, xact, ae33) |>
          mutate(value = if_else(value < 0, 0, value)) |>
          tidyr::pivot_wider(names_from = param, values_from = value) |>
          mutate(Total = chl + nh4 + no3 + org + so4 + xact + BC,
                 chl = chl / Total * 100,
                 nh4 = nh4 / Total * 100,
                 no3 = no3 / Total * 100,
                 org = org / Total * 100,
                 so4 = so4 / Total * 100,
                 xact = xact / Total * 100,
                 BC = BC / Total * 100) |>
          select(-Total) |>
          rename(`Xact (minus S)`=xact) |>
          tidyr::pivot_longer(chl:BC, names_to = "param", values_to = "value") |>
          arrange(time_hour)
        
        g <- ggplot(df, aes(x = time_hour, y = value, fill = param)) + 
          geom_area() +
          scale_fill_manual(values = acsm_colors) +
          scale_x_datetime(labels = scales::label_date_short(),
                           limits = date_range()) +
          labs(y = "Aerosol Mass\nFraction (%)") +
          theme(axis.title.x = element_blank())
      } else {
        
        df <- bind_rows(acsm, xact, ae33) |>
          mutate(param = if_else(param == "xact", "Xact (minus S)", param)) |>
          arrange(time_hour)

        g <- ggplot(df, aes(x = time_hour, y = value, fill = param)) + 
          geom_bar(stat = "identity") +
          scale_fill_manual(values = acsm_colors) +
          scale_x_datetime(labels = scales::label_date_short(),
                           limits = date_range()) +
          labs(y = expression(atop("Aerosol Mass", mu*g~m^-3))) +
          theme(axis.title.x = element_blank())
      }
      

    })
    
    output$plot <- renderPlot({
     
      validate(need(input$dates[1] <= input$dates[2], "End date must not be before start date"))
      p1 <- pm()
      p2 <- acsm()
      p3 <- xact()
      p4 <- aeth()
      p5 <- mass_fraction()
      
      # Compose the plot using the patchwork package
      p1 / p2 / p3 / p4 / p5 +
        plot_layout(axis_titles = "collect")
      
      
    }) |>
      bindEvent(input$go)
    
    
    ### Some SMPS calculations -----
    dlogDp <- function(midpoints) {
      # Calculate the lower and upper bound for each size bin
      avg_diff <- mean(diff(log10(midpoints)))
      
      # The value of the midpoint 1 before
      previous_mid <- c(NA, midpoints)[1:length(midpoints)]
      
      # Create the bounds (one larger than the midpoints)
      bounds <- 10^(0.5 * (log10(midpoints) + log10(previous_mid)))
      bounds <- c(bounds, NA)
      
      # First and last boundary are based on the average difference
      bounds[1] <- 10^(log10(midpoints[1]) - 0.5 * avg_diff)
      bounds[length(bounds)] <- 10^(log10(midpoints[length(midpoints)]) + 0.5 * avg_diff)
      
      D_low <- bounds[1:length(bounds)-1]
      D_high <- bounds[2:length(bounds)]
      dlogDp <- log10(D_high) - log10(D_low)
    }
    
    dMdlogDp <- function(midpoints, smps_records) {
      dNdlogDp <- data.matrix(smps_records)
      # calculate mass distribution and total mass of scan. Must assume a particle density.
      density <- 1.4  # g/cm3
      mass_convert <- (density / 1e9) * (pi / 6) * midpoints^3
      dMdlogDp <- t(apply(dNdlogDp, MARGIN = 1, function(x) x * mass_convert))    #ug/m3
    }
    

  })
}