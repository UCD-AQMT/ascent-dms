
acsmUI <- function(id) {
  
  ns <- NS(id)
  
  # ACSM tables
  sa <- tbl(con, I("acsm.sample_analysis"))
  tps <- tbl(con, I("acsm.tps"))
  mls <- tbl(con, I("acsm.mass_loadings"))
  calib <- tbl(con, I("acsm.diag_calib"))
  ds <- tbl(con, I("acsm.dryer_stats"))
  
  # Get the tablenames
  df <- sa |>
    inner_join(select(tbl_sites, site_number, site_code), by = "site_number") |>
    inner_join(select(tps, -id, -site_record_id),
               by = c("id"="sample_analysis_id")) |>
    inner_join(select(mls, -id, -site_record_id), by = c("id"="sample_analysis_id")) |>
    inner_join(select(calib, -id, -site_record_id), 
               by = c("id"="sample_analysis_id")) |>
    filter(1==0) |>
    collect()
  options <- colnames(df)
  
  df <- ds |>
    filter(1==0) |>
    collect()
  opts_dryer <- colnames(df)
  
  page_fillable(
    gap = "8px",
    layout_column_wrap(
      width = 1/7,
      min_height = "200px",
      gap = "6px",
      uiOutput(ns("status")),
      uiOutput(ns("ce")),
      uiOutput(ns("pressure")),
      uiOutput(ns("airbeam_total")),
      uiOutput(ns("airbeam_ref")),
      uiOutput(ns("heater_temp")),
      uiOutput(ns("heater_current")),
      uiOutput(ns("filament")),
      uiOutput(ns("detector_voltage")),
      uiOutput(ns("turbo_speed")),
      uiOutput(ns("turbo_power")),
      uiOutput(ns("fore_pc")),
      uiOutput(ns("rh")),
      uiOutput(ns("rh_out"))
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        plotlyOutput(ns("fractions")),
        full_screen = TRUE
      ),
      layout_column_wrap(
        width = 1,
        card(
          selectInput(ns("plot1_y"), label = NULL, choices = options, multiple = TRUE,
                      selected = c("ab_total", "abref")),
          plotlyOutput(ns("plot1")),
          selectInput(ns("plot2_y"), label = NULL, choices = opts_dryer, multiple = TRUE,
                      selected = c("rh_dry", "rh_in")),
          plotlyOutput(ns("plot2")),
          full_screen = TRUE
        )
      )

    )
    
  )
}

acsmServer <- function(id, site) {
  moduleServer(id, function(input, output, session) {
    
    # ACSM tables
    sa <- tbl(con, I("acsm.sample_analysis"))
    tps <- tbl(con, I("acsm.tps"))
    mls <- tbl(con, I("acsm.mass_loadings"))
    calib <- tbl(con, I("acsm.diag_calib"))
    ds <- tbl(con, I("acsm.dryer_stats"))
    
    get_last <- reactive({

      get_range() |>
        arrange(desc(start_date)) |>
        slice(1)
            
    })
    
    get_range <- reactive({
    
      invalidateLater(1000 * 60 * 3) # every three minutes
      
      last <- Sys.time()
      first <- last - 60 * 60 * 24 # 24 hrs
      df <- sa |>
        inner_join(select(tbl_sites, site_number, site_code), by = "site_number") |>
        inner_join(select(tps, -id, -site_record_id),
                   by = c("id"="sample_analysis_id")) |>
        inner_join(select(mls, -id, -site_record_id), by = c("id"="sample_analysis_id")) |>
        inner_join(select(calib, -id, -site_record_id), 
                   by = c("id"="sample_analysis_id")) |>
        filter(site_code == !!site(),
               start_date > first) |>
        collect()
      validate(need(nrow(df) > 0, "No data for site"))
      
      df
      
    })
    
    #DryerStats
    get_ds <- reactive({
      
      # want the time range to match the data from the other tables
      r <- get_range()
      min_dt <- min(r$start_date)
      max_dt <- max(r$start_date)
      df <- ds |>
        inner_join(select(tbl_sites, site_number, site_code), by = "site_number") |>
        filter(site_code == !!site(),
               datetime >= min_dt,
               datetime <= max_dt) |>
        collect()
      
      
    })
    
    get_last_ds <- reactive({
      
      get_ds() |>
        arrange(desc(datetime)) |>
        slice(1)
      
    })
    
    
    ts_plot <- function(df) {
      
      ggplot(df, aes(x = start_date, y = value, color = param)) + geom_line() +
        scale_x_datetime(labels = scales::label_date()) +
        theme(axis.title.x = element_blank())
      
    }
    
    output$plot1 <- renderPlotly({
      
      df <- get_range() |>
        select(start_date, any_of(input$plot1_y)) |>
        tidyr::pivot_longer(any_of(input$plot1_y), names_to = "param", values_to = "value")
      g <- ts_plot(df)
      ggplotly(g, dynamicTicks = TRUE)
      
    })
    
    output$plot2 <- renderPlotly({
      
      df <- get_ds() |>
        select(start_date=datetime, any_of(input$plot2_y)) |>
        tidyr::pivot_longer(any_of(input$plot2_y), names_to = "param", values_to = "value")
      
      g <- ts_plot(df)
      ggplotly(g, dynamicTicks = TRUE)
      
    })
    

    output$fractions <- renderPlotly({
      
      df <- get_range()
      
      pdf <- df |>
        select(start_date, stop_date, chl:so4) |>
        tidyr::pivot_longer(chl:so4, names_to = "param", values_to = "value")
      g <- ggplot(pdf) +
        geom_bar(aes(x = start_date, y = value, fill = param), stat = "identity") +
        scale_fill_manual(values = acsm_colors) +
        scale_x_datetime(labels = scales::label_date_short()) +
        labs(y = paste("ACSM Species", ugm3(), sep = " "))
      ggplotly(g, dynamicTicks = TRUE)
      
    })
    
    output$status <- renderUI({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))

      if (s$interlock != 0) {
        val <- "Error"
        th <- "danger"
        title <- "Interlock Error"
      } else if (s$status != 0) {
        val <- "Error"
        th <- "danger"
        title <- "Status Error"
      } else {
        tdiff <- Sys.time() - s$stop_date
        if (tdiff > (60 * 24)) {
          title <- paste("Last data", round(tdiff / (60 * 24)), "days ago")
          val <- "Offline"
          th <- "danger"
        } else if (tdiff > 60) {
          title <- paste("Last data", round(tdiff), "minutes ago")
          val <- "Lagging"
          th <- "warning"
        } else {
          title <- paste("Last data", round(tdiff), "minutes ago")
          val <- "Online"
          th <- "primary"
        }
      }
      value_box(title = title, theme = th, value = val)
    })
    
    output$ce <- renderUI({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$ce == 0.5) {
        value_box("Collection Efficiency", value = s$ce, theme = "primary")
      } else {
        value_box("Collection Efficiency Mismatch", value = s$ce, theme = "warning")
      }
    })
    
    output$pressure <- renderUI({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$press_inlet < 4 | s$press_inlet > 4.8) {
        value_box("Inlet pressure not within 4.0-4.8 mbar", 
                  value = paste(round(s$press_inlet, 2), "mbar"), theme = "danger")
      } else {
        value_box("Inlet Pressure", value = paste(round(s$press_inlet, 2), "mbar"),
                theme = "primary") 
      }
    })
    
    output$airbeam_total <- renderUI({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      abtot <- s$ab_total
      if (abtot < 1e5 | abtot > 5e5) {
        value_box("Airbeam not within 1e5 - 5e5 ions/s", value = paste(abtot, "ions/s"),
                theme = "warning")
      } else {
        value_box("Airbeam", value = paste(abtot, "ions/s"), theme = "primary")
      }
      
    })
    
    output$airbeam_ref <- renderUI({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      abratio <- s$ab_total / s$abref
      perc_off <- (abratio - 1) * 100
      val <- paste0(round(perc_off), "%")
      
      if (abratio < 0.7 | abratio > 1.3) {
        title <- "Airbeam not within 30% of reference - AB correction potentially invalid"
        th <- "danger"
      } else if (abratio < 0.8 | abratio > 1.2) {
        title <- "Airbeam not within 20% of reference - Check single ion signal"
        th <- "warning"
      } else {
        title <- "Airbeam to Reference"
        th <- "primary"
      }
      value_box(title = title, value = val, theme = th)
    })
    
    output$heater_temp <- renderUI({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$heater_t < 575 | s$heater_t > 625) {
        value_box("Heater temp not within 575-625 \u00B0C", 
                  value = paste(s$heater_t, "\u00B0C"), theme = "danger")
      } else {
        value_box("Heater Temp", value = paste(s$heater_t, "\u00B0C"), theme = "primary")
      }
      
    })
    
    output$heater_current <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$heater_i < 1.1 | s$heater_i > 1.3) {
        value_box("Heater current not within 1.1-1.3 A", value = paste(s$heater_i, "A"),
                  theme = "warning")
      } else {
        value_box("Heater Current", , value = paste(s$heater_i, "A"), theme = "primary")
      }
    })
    
    output$filament <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$filament_emm <= 0) {
        value_box("URGENT - Filament emission not a positive value. Switch filament and confirm that new filament emission is at setpoint (typically 1.0, 0.7, or 0.4 mA)",
                  value = paste(s$filament_emm, "mA"), theme = "danger", 
                  showcase = bs_icon("exclamation"))
      } else {
        value_box("Filament Emission", value = paste(s$filament_emm, "mA"), 
                  theme = "primary")
      }
      
    })
    
    output$detector_voltage <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$detector >= 3900) {
        value_box("WARNING: Detector voltage > 3900 V. Purchase new detector and prepare for replacement.",
                  value = paste(s$detector, "V"), theme = "danger")
      } else {
        value_box("Detector Voltage", value = paste(s$detector, "V"), theme = "primary")
      }
      
    })
    
    output$turbo_speed <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))

      if (s$turbo_speed < 999) {
        value_box("Turbo speed low. Consider checking for leaks.",
                  value = paste(s$turbo_speed, "Hz"), theme = "warning")
      } else {
        value_box("Turbo speed", value = paste(s$turbo_speed, "Hz"), theme = "primary")
      }
    })
    
    output$turbo_power <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))

      if (s$turbo_power > 145) {
        value_box("Urgent: Turbo power above 145 W. Check for leaks.",
                  value = paste(s$turbo_power, "W"), theme = "danger",
                  showcase = bs_icon("exclamation"))
      } else {
        value_box("Turbo power", value = paste(s$turbo_power, "W"), theme = "primary")
      }
      
    })
    
    output$fore_pc <- renderUI({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$fore_pc != 100) {
        value_box("URGENT: Fore percentage not 100%", value = paste(s$fore_pc, "%"),
                  theme = "danger", showcase = bs_icon("exclamation"))
      } else {
        value_box("Fore Percentage",value = paste(s$fore_pc, "%"),
                  theme = "primary")
      }
    })
    
    output$rh <- renderUI({
      s <- get_last_ds()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$rh_dry > s$rh_in) {
        value_box("RH out > RH in", value = paste("RH out:", s$rh_dry, "RH in:", s$rh_in),
                  theme = "danger")
      } else {
        value_box("RH Difference", value = paste("RH out:", s$rh_dry, "RH in:", s$rh_in),
                  theme = "primary")
      }
    })
    
    output$rh_out <- renderUI({
      s <- get_last_ds()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$rh_dry > 40) {
        value_box("RH out > 40%", value = paste(s$rh_dry, "%"), theme = "danger")
      } else {
        value_box("RH Out", value = paste(s$rh_dry, "%"), theme = "primary")
      }
    })
  })
}