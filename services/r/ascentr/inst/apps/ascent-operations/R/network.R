
networkUI <- function(id) {
  
  ns <- NS(id)
  
  page_fillable(
    DT::dataTableOutput(ns("tbl")),
    p("click on a cell for details"),
    p("Offline: data over 24 hrs old"),
    p("Lagging: data over 1 hr old (non-Xact); over 4 hrs old (Xact urban); over 12 hrs old (Xact rural)")
  )
  
}

# Latest sample time per site for the four Postgres instruments, in a single
# round trip. Only the last 7 days are searched so the time indexes can be used;
# sites with no recent data are filled in as offline downstream. ACSM also needs
# an upper bound to remove future dates caused by the end-of-year bug.
pg_status_sql <- '
SELECT s.site_code, v.instrument AS "Instrument", v.last AS "Last"
FROM (
  SELECT site_number, \'Xact\' AS instrument, max(sample_datetime) AS last
    FROM xact.sample_analysis
    WHERE sample_datetime > $1::timestamptz
    GROUP BY site_number
  UNION ALL
  SELECT site_number, \'SMPS\', max(sample_start)
    FROM smps.sample_analysis
    WHERE sample_start > $1::timestamptz
    GROUP BY site_number
  UNION ALL
  SELECT site_number, \'ACSM\', max(start_date)
    FROM acsm.sample_analysis
    WHERE start_date < $2::timestamptz AND start_date > $1::timestamptz
    GROUP BY site_number
  UNION ALL
  SELECT sen.site_number, \'PurpleAir\', max(p.last_seen)
    FROM purpleair.sample_analysis p
    JOIN purpleair.sensors sen USING (sensor_index)
    WHERE p.last_seen > $1::timestamptz
    GROUP BY sen.site_number
) v
JOIN common.sites s USING (site_number)'

networkServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    pg_status <- reactive({
      reload_heartbeat()
      
      now <- Sys.time()
      cutoff <- now - 7 * 24 * 3600
      
      DBI::dbGetQuery(con, pg_status_sql, params = list(cutoff, now)) |>
        as_tibble() |>
        mutate(Lag = difftime(Sys.time(), Last, units = "hours"),
               HoursAllowed = if_else(Instrument == "Xact" & site_code %in% rural_sites, 12,
                                      if_else(Instrument == "Xact", 4, 1)),
               Status = if_else(Lag < HoursAllowed, "online",
                                if_else(Lag < 24, "lagging", "offline"))) |>
        select(site_code, Instrument, Lag, Status)
      
    })
    
    ae33_status <- reactive({
      reload_heartbeat()
      flux_query <- glue::glue('from(bucket: "measurements") |> ', 
                               'range(start: -7d) |> ',
                               'filter(fn: (r) => r._field == "STinst") |> ',
                               'sort(columns: ["_time"], desc: true) |> ',
                               'limit(n:1) |>',
                               'drop(columns: ["_start", "_stop"])'
      )
      
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
    
    reload_heartbeat <- reactiveVal(0)
    
    status_df <- reactive({
      
      invalidateLater(1000 * 60 * 10) # every ten minutes
      isolate(reload_heartbeat(reload_heartbeat() + 1))
      
      pg <- pg_status()
      ae33 <- ae33_status()

      bind_rows(pg, ae33) |>
        select(-Lag) |>
        tidyr::pivot_wider(names_from = Instrument, values_from = Status) |>
        inner_join(site_names, by = "site_code") |>
        arrange(site_number) |>
        select(Site=site_name, ACSM, AE33, SMPS, Xact, PurpleAir) |>
        tidyr::complete(fill = list(Xact="offline", ACSM="offline", PurpleAir="offline",
                                    AE33="offline", SMPS="offline"))
      
    })
    
    
    output$tbl <- DT::renderDataTable({
      
      df <- status_df()
      DT::datatable(df, rownames = FALSE,
                    style = "bootstrap",
                    options = list(dom = "t",
                                   ordering = FALSE,
                                   pageLength = 15,
                                   autoWidth = TRUE,
                                   columnDefs = list(list(width = "30%", targets = 0))),
                    selection = list(mode = "single", target = "cell")) |>
        formatStyle(2:6,
                    backgroundColor = styleEqual(
                      c("online", "lagging", "offline"),
                      c("#007bc2", "#f9b928", "#c10000")
                    ))
      
    })
    
    # Return the value of the click for use outside of this module (controlling tabs)
    return(reactive(input$tbl_cell_clicked))
    

  })
}
