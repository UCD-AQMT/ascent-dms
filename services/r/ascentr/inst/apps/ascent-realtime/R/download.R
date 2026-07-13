
downloadUI <- function(id) {
  
  ns <- NS(id)
  today <- Sys.Date() + 1
  start_day <- today - 7
  
  elems <- tbl(con, I("xact.element_params")) |>
    arrange(atomic_number) |>
    pull(element)
  
  sites_menu <- site_names$site_code
  names(sites_menu) <- site_names$site_name
  
  # Time range, instrument, (site), level, metadata
  layout_sidebar(
    sidebar = sidebar(
      selectInput(ns("site"), "Site", choices = sites_menu),
      dateRangeInput(ns("dates"), "Dates", start = start_day, end = today,
                     min = minimum_date, max = today),
      selectInput(ns("instrument"), "Instrument", choices = c("Xact", "SMPS", "AE33", "ACSM")),
      checkboxInput(ns("metadata"), "Include metadata file?", value = TRUE),
      textOutput(ns("expected")),
      checkboxInput(ns("agree"), "Accept data policy?", value = FALSE),
      # dynamic export button that kicks off file export
      uiOutput(ns("mybutton")),
      # hidden button for file download
      downloadButton(ns("download"), "Download", 
                     style = "position: absolute; left: -9999px; top: -9999px;"),
      width = "300px"
    ),
    htmlOutput(ns("download_note")),
    card(verbatimTextOutput(ns("meta_text"))
    )
  )
}

downloadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Maximum allowed records for download depend on instrument
    ae33_max <- 15000
    acsm_max <- 10000
    xact_max <- 2000
    smps_max <- 5000
    
    output$download_note <- renderUI({
      
      url <- a("Geoscience Data Exchange", 
               href = "https://gdex.ucar.edu/gsearch/dataset-search/?q=ASCENT",
               target = "_blank")
      p("This page is for downloading small portions of the most recent preliminary data. It may take a few minutes to prepare your data. For fast access to the full archive, please visit the NCAR", url, "(GDEX).")
      
    })
      
    # Metadata text to display on the main panel
    output$meta_text <- renderPrint({
      
      m <- basic_metadata(input$site, input$instrument, input$dates[1], 
                          input$dates[2], level = "1", con)
      m
      
    })
    
    # Dynamic UI ---------
    
    # Don't allow download unless number of expected records is >0 and < max and agreement
    # is signed
    allow_download <- reactive({
      
      allowed_samples <- switch(input$instrument,
                                "AE33" = ae33_max,
                                "ACSM" = acsm_max,
                                "Xact" = xact_max,
                                "SMPS" = smps_max)
      
      isTruthy(input$agree) && 
        expected_samples() > 0 &&
        expected_samples() < allowed_samples

    })
    
    output$mybutton <- renderUI({
      
      ns <- session$ns
      if (allow_download()) {
        bslib::input_task_button(ns("export"), "Export", icon = icon("file-export"))
      } else {
        actionButton(ns("dummybutton"), "Export", icon = icon("file-export"))
      }

    })
    

    observeEvent(input$dummybutton, {
      showModal(modalDialog(
        title = "Message",
        if (!isTruthy(input$agree)) {
          "Please read and accept the data policy to download data"
        } else if (expected_samples() == 0) {
          "No data for this instrument/date"
        } else {
          allowed <- switch(input$instrument,
                            "AE33" = ae33_max,
                            "ACSM" = acsm_max,
                            "Xact" = xact_max,
                            "SMPS" = smps_max)
          glue::glue("Please limit {input$instrument} downloads to {allowed} samples by reducing the time window.")
        }
        
      ))
    })
    
    ### Estimate the number of records to return - keep button disabled if zero or too high
    expected_samples <- reactiveVal(0)
    
    output$expected <- renderText({
      
      expected <- availability(input$site, input$dates[1], input$dates[2], input$instrument)
      expected_samples(expected)
      glue::glue("Approx. sample count: {expected}")
      
    })
    
    #### Extended Task handler
    export_task <- ExtendedTask$new(function(file) {

      # Need to capture values for reactives to pass to futures
      instrument <- isolate(input$instrument)
      site <- isolate(input$site)
      start <- isolate(input$dates[1])
      end <- isolate(input$dates[2])
      metadata <- isolate(input$metadata)      
      fname <- isolate(filename_noext())

      l1b_fn <- switch(instrument,
                       "Xact" = ascentr::xact_l1b,
                       "SMPS" = ascentr::smps_l1b_df,
                       "ACSM" = ascentr::acsm_l1b,
                       "AE33" = ascentr::ae33_l1b)
      meta_fn <- switch(instrument,
                        "Xact" = ascentr::xact_metadata,
                        "SMPS" = ascentr::smps_metadata,
                        "ACSM" = ascentr::acsm_metadata,
                        "AE33" = ascentr::ae33_metadata)

      promises::future_promise({
        
        con <- get_db_connection("dataconnection")
        
        if (instrument == "AE33") {
          influx_con <- get_flux_client("dataconnection")
          results <- l1b_fn(site, start, end, influx_con)
        } else {
          results <- l1b_fn(site, start, end, con)  
        }
        
        
        if (metadata) {
          if (instrument == "ACSM") {
            meta <- meta_fn(site, start, end, con, metadata_fields = results$mdf, level = "1b")
            results <- results$df
          } else {
            meta <- meta_fn(site, start, end, con, level = "1b")
          }
          f <- ascentr:::export_zip_shiny(results, meta, fname = fname, temp_file = file)
        } else {
          f <- ascentr:::export_csv(results, file)
        }
        DBI::dbDisconnect(con)
        f

      }, seed = NULL)
      
    }) |> bslib::bind_task_button("export")
    
    # Set up a reusable file for this session's download data.
    download_content_path <- tempfile("download_content")
    
    # Once the Export button is pressed, invoke a long running task to process
    observeEvent(input$export, export_task$invoke(download_content_path))
    
    # Show download button only when file is ready.
    observe({
      if (export_task$status() == "success") {
        showNotification("Your download is ready.")
        shinyjs::click("download")
      }
    })
    
    
    
    # Download handler ------
    
    # Provide the filename without extension, since we won't know the structure
    filename_noext <- reactive({
      glue::glue("ASCENT_{input$site}_{input$instrument}_",
                 "{input$dates[1]}_{input$dates[2]}_level-1")
    })

    # Need to fix this
    content_type <- reactive({
      if (input$metadata) {
        "application/zip"
      } else {
        "text/csv"
      }
    })
    
    output$download <- downloadHandler(
      filename = function() {
        
        base_filename <- filename_noext()
        if (input$metadata) {
          f <- paste0(base_filename, ".zip")
        } else {
          f <- paste0(base_filename, ".csv")  
        }
        
        return(f)
        
      },
      content = function(filename) {
        
        # Get the temporary file managed by Shiny and returned in export_task$result() and
        # give it our desired name
        file.rename(export_task$result(), filename)
        
      },
      contentType = content_type()
    )
    
  })
}

# A function for estimating the number of samples in a request
availability <- function(site, date_start, date_end, instrument) {
 
  if (instrument == "Xact") {
    
    last_date <- date_end + 1
    
    expected <- tbl(con, I("xact.sample_analysis")) |>
      select(site_number, sample_datetime, sample_type) |>
      inner_join(select(tbl_sites, site_number, site_code), by = "site_number") |>
      filter(sample_datetime >= date_start,
             sample_datetime <= last_date,
             site_code == site) |>
      summarise(Samples = n()) |>
      pull(Samples)
    
  }
  
  if (instrument == "SMPS") {
    
    last_date <- date_end + 1
    
    expected <- tbl(con, I("smps.sample_analysis")) |>
      select(site_number, sample_start) |>
      inner_join(select(tbl_sites, site_code, site_number), by = "site_number") |>
      filter(sample_start >= date_start,
             sample_start <= last_date,
             site_code == site) |>
      summarise(Samples = n()) |>
      pull(Samples)
      
  }
  
  if (instrument == "AE33") {

    flux_query <- glue::glue('from(bucket: "measurements") |> ', 
                             'range(start: {date_start}T00:00:00Z,',
                             'stop: {date_end}T23:59:59Z) |> ', 
                             'filter(fn: (r) => r._field == "EBC_1") |>',
                             'aggregateWindow(every: 1d, fn: count, timeSrc: "_start") |>',
                             'drop(columns: ["_start", "_stop"])')
    ret <- ae33_con$query(flux_query)
    if (is.null(ret)) {
      expected <- 0
    } else {
      expected <- ret |>
        purrr::list_rbind() |>
        mutate(site_code = stringr::str_sub(`_measurement`, start = 6, end = -5),
               Instrument = "AE33",
               Samples = as.numeric(`_value`)) |>
        filter(site_code == site) |>
        summarise(Samples = sum(Samples)) |>
        pull(Samples)
    }
  }
  
  if (instrument == "ACSM") {
    
    last_day <- date_end + 1
    expected <- tbl(con, I("acsm.sample_analysis")) |>
      select(site_number, start_date) |>
      inner_join(select(tbl_sites, site_code, site_number), by = "site_number") |>
      filter(start_date >= date_start,
             start_date < last_day,
             site_code == site) |>
      summarise(Samples = n()) |>
      pull(Samples)

  }
 
  return(expected)
  
}

