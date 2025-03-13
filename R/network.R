
networkUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    box("Network Status", width = 10,
        DT::dataTableOutput(ns("tbl")) |>
          withSpinner()
        )
  )
  

}

networkServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    xact_status <- reactive({
      
      df <- tbl(con, I("xact.sample_analysis")) |>
        select(site_number, sample_datetime, sample_type) |>
        inner_join(select(tbl(con, I("common.sites")), site_number, site_code),
                   by = "site_number") |>
        summarise(Last = max(sample_datetime, na.rm = TRUE),
                  .by = site_code) |>
        collect() |>
        mutate(Lag = difftime(Sys.time(), Last, units = "hours"),
               HoursAllowed = if_else(site_code %in% rural_sites, 12, 4),
               Status = if_else(Lag < HoursAllowed, "online",
                                if_else(Lag < 24, "lagging", "offline")),
               Instrument = "Xact") |>
        select(site_code, Instrument, Lag, Status)
      
    })
    
    smps_status <- reactive({
      
      df <- tbl(con, I("smps.sample_analysis")) |>
        inner_join(select(tbl(con, I("common.sites")), site_code, site_number), 
                   by = "site_number") |>
        select(site_code, sample_start) |>
        summarise(Last = max(sample_start, na.rm = TRUE),
                  .by = site_code) |>
        collect() |>
        mutate(Lag = difftime(Sys.time(), Last, units = "hours"),
               HoursAllowed = 1,
               Status = if_else(Lag < HoursAllowed, "online",
                                if_else(Lag < 24, "lagging", "offline")),
               Instrument = "SMPS") |>
        select(site_code, Instrument, Lag, Status)
      
      
    })
    
    ae33_status <- reactive({
      
      # look over the last 2 days only
      today <- Sys.Date()
      first_day <- today - 1
      
      flux_query <- glue::glue('from(bucket: "measurements") |> ', 
                               'range(start: {format(first_day, "%Y-%m-%d")}T00:00:00Z,',
                               'stop: {format(today, "%Y-%m-%d")}T23:59:59Z) |> ', 
                               'filter(fn: (r) => r._field == "EBC_6") |> ',
                               'group(columns: ["_measurement"]) |> ',
                               'last() |> ',
                               'drop(columns: ["_start", "_stop"])')
      
      # influx returns each site as a separate table in a list
      df <- ae33_client$query(flux_query) |>
        purrr::list_rbind() |>
        mutate(site_code = substr(`_measurement`, 6, nchar(`_measurement`) - 4),
               Lag = difftime(Sys.time(), time, units = "hours"),
               HoursAllowed = 1,
               Status = if_else(Lag < HoursAllowed, "online",
                                if_else(Lag < 24, "lagging", "offline")))
      
      # attach any missing sites
      all_sites <- tibble(site_code = site_list)
      df <- df |>
        right_join(all_sites, by = "site_code") |>
        mutate(Status = if_else(is.na(Status), "offline", Status),
               Instrument = "AE33") |>
        select(site_code, Instrument, Lag, Status)
      
    })
    
    acsm_status <- reactive({
      
      df <- tbl(con, I("acsm.sample_analysis")) |>
        inner_join(select(tbl(con, I("common.sites")), site_code, site_number), 
                   by = "site_number") |>
        select(site_code, start_date) |>
        summarise(Last = max(start_date, na.rm = TRUE),
                  .by = site_code) |>
        collect() |>
        mutate(Lag = difftime(Sys.time(), Last, units = "hours"),
               HoursAllowed = 1,
               Status = if_else(Lag < HoursAllowed, "online",
                                if_else(Lag < 24, "lagging", "offline")),
               Instrument = "ACSM") |>
        select(site_code, Instrument, Lag, Status)
      
    })
    
    pa_status <- reactive({
      
      df <- tbl(con, I("purpleair.sample_analysis")) |>
        inner_join(tbl(con, I("purpleair.sensors")), by = "sensor_index") |>
        inner_join(select(tbl(con, I("common.sites")), site_code, site_number), 
                   by = "site_number") |>
        select(site_code, last_seen) |>
        summarise(Last = max(last_seen, na.rm = TRUE),
                  .by = site_code) |>
        collect() |>
        mutate(Lag = difftime(Sys.time(), Last, units = "hours"),
               HoursAllowed = 1,
               Status = if_else(Lag < HoursAllowed, "online",
                                if_else(Lag < 24, "lagging", "offline")),
               Instrument = "PurpleAir") |>
        select(site_code, Instrument, Lag, Status)
      
    })
    
    status_df <- reactive({
      
      invalidateLater(1000 * 60 * 3) # every three minutes
      
      xact <- xact_status()
      acsm <- acsm_status()
      pa <- pa_status()
      ae33 <- ae33_status()
      smps <- smps_status()

      bind_rows(xact, acsm, pa, ae33, smps) |>
        select(-Lag) |>
        tidyr::pivot_wider(names_from = Instrument, values_from = Status) |>
        select(Site=site_code, ACSM, AE33, SMPS, Xact, PurpleAir) |>
        tidyr::complete(fill = list(Xact="offline", ACSM="offline", PurpleAir="offline",
                                    AE33="offline", SMPS="offline")) |>
        arrange(Site)
      
    })
    
    
    output$tbl <- DT::renderDataTable({
      
      df <- status_df()
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t",
                                   ordering = FALSE,
                                   pageLength = 15),
                    selection = list(mode = "single", target = "cell")) |>
        formatStyle(2:6, 
                    backgroundColor = styleEqual(
                      c("online", "lagging", "offline"),
                      c("dodgerblue", "yellow", "red")
                    ))
      
    })
    
    # Return the value of the click for use outside of this module (controlling tabs)
    return(reactive(input$tbl_cell_clicked))
    

  })
}