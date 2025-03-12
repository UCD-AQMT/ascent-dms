
acsmUI <- function(id) {
  
  ns <- NS(id)
  
  # ACSM tables
  sa <- tbl(con, I("acsm.sample_analysis"))
  sites <- tbl(con, I("common.sites"))
  tps <- tbl(con, I("acsm.tps"))
  mls <- tbl(con, I("acsm.mass_loadings"))
  calib <- tbl(con, I("acsm.diag_calib"))
  
  # Get the tablenames
  df <- sa |>
    inner_join(select(sites, site_number, site_code), by = "site_number") |>
    inner_join(select(tps, -id, -site_record_id),
               by = c("id"="sample_analysis_id")) |>
    inner_join(select(mls, -id, -site_record_id), by = c("id"="sample_analysis_id")) |>
    inner_join(select(calib, -id, -site_record_id), 
               by = c("id"="sample_analysis_id")) |>
    filter(1==0) |>
    collect()
  options <- colnames(df)
  
  tagList(
    fluidRow(
      infoBoxOutput(ns("status"), width = 3),
      infoBoxOutput(ns("ce"), width = 3),
      infoBoxOutput(ns("pressure"), width = 3),
      infoBoxOutput(ns("airbeam_total"), width = 3),
      infoBoxOutput(ns("airbeam_ref"), width = 3),
      infoBoxOutput(ns("heater_temp"), width = 3),
      infoBoxOutput(ns("heater_current"), width = 3),
      infoBoxOutput(ns("filament"), width = 3),
      infoBoxOutput(ns("detector_voltage"), width = 3),
      infoBoxOutput(ns("turbo_speed"), width = 3),
      infoBoxOutput(ns("turbo_power"), width = 3),
      infoBoxOutput(ns("fore_pc"), width = 3)
    ),
    fluidRow(
      column(2, selectInput(ns("plot1_y"), "Parameter", choices = options,
             selected = "ab_total")),
      column(10, plotOutput(ns("plot1"), height = 150))
    ),
    fluidRow(
      column(2, selectInput(ns("plot2_y"), "Parameter", choices = options,
                            selected = "detector")),
      column(10, plotOutput(ns("plot2"), height = 150))
    ),
    fluidRow(
      column(2, selectInput(ns("plot3_y"), "Parameter", choices = options,
                            selected = "flow_ccs")),
      column(10, plotOutput(ns("plot3"), height = 150))
    ),
    fluidRow(
      column(2, selectInput(ns("plot4_y"), "Parameter", choices = options,
                            selected = "turbo_power")),
      column(10, plotOutput(ns("plot4"), height = 150))
    ),
    fluidRow(
      column(10, offset = 2, plotOutput(ns("fractions")))
    )
    
  )

}

acsmServer <- function(id, site) {
  moduleServer(id, function(input, output, session) {
    
    # ACSM tables
    sa <- tbl(con, I("acsm.sample_analysis"))
    sites <- tbl(con, I("common.sites"))
    tps <- tbl(con, I("acsm.tps"))
    mls <- tbl(con, I("acsm.mass_loadings"))
    calib <- tbl(con, I("acsm.diag_calib"))
    
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
        inner_join(select(sites, site_number, site_code), by = "site_number") |>
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
    
    ts_plot <- function(param, df) {
      
      ggplot(df, aes(x = start_date, y = .data[[param]])) + geom_line() +
        scale_x_datetime(labels = scales::label_date_short()) +
        #theme_minimal() +
        theme(axis.title.x = element_blank())
      
    }
    
    output$plot1 <- renderPlot({
      
      df <- get_range()
      ts_plot(input$plot1_y, df)
      
    })
    
    output$plot2 <- renderPlot({
      
      df <- get_range()
      ts_plot(input$plot2_y, df)
      
    })
    
    output$plot3 <- renderPlot({
      
      df <- get_range()
      ts_plot(input$plot3_y, df)
      
    })
    
    output$plot4 <- renderPlot({
      
      df <- get_range()
      ts_plot(input$plot4_y, df)
      
    })
    
    output$fractions <- renderPlot({
      
      df <- get_range()
      
      pdf <- df |>
        select(start_date, stop_date, chl:so4) |>
        tidyr::pivot_longer(chl:so4, names_to = "param", values_to = "value")
      ggplot(pdf) +
        geom_bar(aes(x = start_date, y = value, fill = param), stat = "identity") +
        scale_fill_manual(values = acsm_colors) +
        scale_x_datetime(labels = scales::label_date_short()) +
        labs(y = expression(atop("ACSM Species", mu*g~m^-3)))
      
    })
    
    output$status <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))

      if (s$status == 0 & s$interlock == 0) {
        tdiff <- Sys.time() - s$stop_date
        if (tdiff > 60) {
          txt <- "Lagging"
          sub <- glue::glue("Last data {round(tdiff)} minutes ago")
          infoBox("Status", txt, subtitle = sub, color = "yellow", icon = icon("clock"),
                  width = 3)
        } else {
          txt <- "OK"
          sub <- glue::glue("Last data {round(tdiff)} minutes ago")
          infoBox("Status", txt, subtitle = sub, color = "blue", icon = icon("check"),
                  width = 3)
        }

      } else {
        if (s$interlock != 0) {
          txt <- "Interlock Error"
        } else {
          txt <- "Status Error"
        }
        infoBox("Status", txt, color = "red", icon = icon("exclamation"),
                width = 3)
      }

    })
    
    output$ce <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$ce == 0.5) {
        infoBox("Collection Efficiency", s$ce, color = "blue", icon = icon("check"),
                width = 3)
      } else {
        infoBox("Collection Efficiency", s$ce, subtitle = "collection efficiency mismatch",
                color = "yellow", icon = icon("question"),
                width = 3)
      }
      
    })
    
    output$pressure <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$press_inlet < 4 | s$press_inlet > 4.8) {
        infoBox("Inlet Pressure", paste(round(s$press_inlet, 2), "mbar"), 
                subtitle = "Inlet pressure not within 4.0-4.8 mbar",
                color = "red", icon = icon("x"),
                width = 3)
      } else {
        infoBox("Inlet Pressure", paste(round(s$press_inlet, 2), "mbar"),
                color = "blue", icon = icon("check"),
                width = 3) 
      }

    })
    
    output$airbeam_total <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      abtot <- s$ab_total
      if (abtot < 1e5 | abtot > 5e5) {
        infoBox("Airbeam", paste(abtot, "ions/s"),
                subtitle = "Airbeam not within 1e5 - 5e5 ions/s",
                color = "yellow", icon = icon("question"),
                width = 3)
      } else {
        infoBox("Airbeam", paste(abtot, "ions/s"),
                color = "blue", icon = icon("check"),
                width = 3)
      }
      
    })
    
    output$airbeam_ref <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      abratio <- s$ab_total / s$abref
      perc_off <- (abratio - 1) * 100
      
      if (abratio < 0.7 | abratio > 1.3) {
        infoBox("Airbeam to Reference", paste0(round(perc_off), "%"),
                subtitle = "Airbeam not within 30% of reference - AB correction potentially invalid",
                color = "red", icon = icon("x"),
                width = 3)
      } else if (abratio < 0.8 | abratio > 1.2) {
        infoBox("Airbeam to Reference", paste0(round(perc_off), "%"),
                subtitle = "Airbeam not within 20% of reference - Check single ion signal",
                color = "yellow", icon = icon("question"),
                width = 3)
      } else {
        infoBox("Airbeam to Reference", paste0(round(perc_off), "%"),
                color = "blue", icon = icon("check"),
                width = 3)
      }
      
    })
    
    output$heater_temp <- renderInfoBox({
      
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$heater_t < 575 | s$heater_t > 625) {
        infoBox("Heater Temp", paste(s$heater_t, "\u00B0C"),
                subtitle = "Heater temp not within 575-625 \u00B0C",
                color = "red", icon = icon("x"),
                width = 3)
      } else {
        infoBox("Heater Temp", paste(s$heater_t, "\u00B0C"),
                color = "blue", icon = icon("check"),
                width = 3)
        
      }
      
    })
    
    output$heater_current <- renderInfoBox({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$heater_i < 1.1 | s$heater_i > 1.3) {
        infoBox("Heater Current", paste(s$heater_i, "A"),
                subtitle = "Heater current not within 1.1-1.3 A",
                color = "yellow", icon = icon("question"),
                width = 3)
      } else {
        infoBox("Heater Current", paste(s$heater_i, "A"),
                color = "blue", icon = icon("check"),
                width = 3)
      }
    })
    
    output$filament <- renderInfoBox({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$filament_emm <= 0) {
        infoBox("Filament Emission", paste(s$filament_emm, "mA"),
                subtitle = "URGENT - Filament emission not a positive value. Switch filament and confirm that new filament emission is at setpoint (typically 1.0, 0.7, or 0.4 mA",
                color = "red", icon = icon("exclamation"),
                width = 3)
      } else {
        infoBox("Filament Emission", paste(s$filament_emm, "mA"),
                color = "blue", icon = icon("check"),
                width = 3)
      }
      
    })
    
    output$detector_voltage <- renderInfoBox({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$detector >= 3900) {
        infoBox("Detector Voltage", paste(s$detector, "V"),
                subtitle = "WARNING: Detector voltage > 3900 V. Purchase new detector and prepare for replacement.",
                color = "red", icon = icon("exclamation"),
                width = 3)
      } else {
        infoBox("Detector Voltage", paste(s$detector, "V"),
                color = "blue", icon = icon("check"),
                width = 3)
      }
      
    })
    
    output$turbo_speed <- renderInfoBox({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))

      if (s$turbo_speed < 999) {
        infoBox("Turbo speed", paste(s$turbo_speed, "Hz"),
                subtitle = "Turbo speed low. Consider checking for leaks.",
                color = "yellow", icon = icon("question"),
                width = 3)
      } else {
        infoBox("Turbo speed", paste(s$turbo_speed, "Hz"),
                color = "blue", icon = icon("check"),
                width = 3)
      }
    })
    
    output$turbo_power <- renderInfoBox({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))

      if (s$turbo_power > 145) {
        infoBox("Turbo power", paste(s$turbo_power, "W"),
                subtitle = "Urgent: Turbo power above 145 W. Consider checking for leaks.",
                color = "red", icon = icon("exclamation"),
                width = 3)
      } else {
        infoBox("Turbo power", paste(s$turbo_power, "W"),
                color = "blue", icon = icon("check",
                                            width = 3))
      }
      
    })
    
    output$fore_pc <- renderInfoBox({
      s <- get_last()
      validate(need(nrow(s) > 0, "No data for site"))
      
      if (s$fore_pc != 100) {
        infoBox("Fore Percentage", paste(s$fore_pc, "%"),
                subtitle = "URGENT: Fore percentage not 100%",
                color = "red", icon = icon("exclamation"),
                width = 3)
      } else {
        infoBox("Fore Percentage", paste(s$fore_pc, "%"),
                color = "blue", icon = icon("check"),
                width = 3)
      }
    })
    
  })
}