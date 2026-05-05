
downloadUI <- function(id) {
  
  ns <- NS(id)
  today <- Sys.Date() + 1
  start_day <- today - 7
  
  elems <- tbl(con, I("xact.element_params")) |>
    arrange(atomic_number) |>
    pull(element)
  
  # Time range, instrument, (site), level, metadata
  layout_sidebar(
    sidebar = sidebar(
      selectInput(ns("site"), "Site", choices = site_list),
      dateRangeInput(ns("dates"), "Dates", start = start_day, end = today,
                     min = minimum_date, max = today),
      selectInput(ns("instrument"), "Instrument", choices = c("Xact", "SMPS", "AE33", "ACSM")),
      selectInput(ns("level"), "Data level", choices = "1"),
      checkboxInput(ns("metadata"), "Include metadata file?", value = TRUE),
      checkboxInput(ns("agree"), "Accept data policy?", value = FALSE),
      textOutput(ns("expected")),
      uiOutput(ns("mybutton")),
      width = "300px"
    ),
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
        downloadButton(ns("download"), "Download", icon = icon("download"))
      } else {
        actionButton(ns("dummybutton"), "Download", icon = icon("download"))
      }

    })
    
    observeEvent(input$instrument, {
      
      levels <- switch(input$instrument,
                       "Xact" = c("1"),
                       "SMPS" = c("1"),
                       "AE33" = c("1"))
      updateSelectInput(session, "level", choices = levels)
      
    })
    
    # TODO: Make this more general and add conditions for too much data that depend on instrument
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
    
    # Data reactives -------
    
    export_data <- reactive({
      if (input$instrument == "SMPS") {
        d <- smps_data_reactive()
      }
      if (input$instrument == "Xact") {
        d <- xact_data()
      }
      if (input$instrument == "AE33") {
        d <- ae33_data()
      }
      if (input$instrument == "ACSM") {
        d <- acsm_data()
      }
    })
    
    
    ## Xact -----
    xact_data <- reactive({
      
      ds <- switch(input$level,
                   "1" = xact_l1b_reactive(),
                   "1a" = xact_l1a())
      
    })
    
    # Temporary situation. Once auto-qc is finalized, there will be no l1a and l1b, just l1  
    xact_l1a <- reactive({
      
      results <- xact_l1a_df(input$site, input$dates[1], input$dates[2], con)
      
      if (input$metadata) {
        meta <- xact_metadata(input$site, input$dates[1], input$dates[2], level = "1a",
                              con = con, metadata_fields = results$mdf)
        export_zip_shiny(results$df, meta, fname = filename_noext(), temp_file = temp_file())
      } else {
        export_csv(results$df, temp_file())
      }
    })
    
    xact_l1b_reactive <- reactive({
      
      results <- xact_l1b(input$site, input$dates[1], input$dates[2], con)

      if (input$metadata) {
        meta <- xact_metadata(input$site, input$dates[1], input$dates[2],
                                 level = "1b", con = con)
        export_zip_shiny(results, meta, fname = filename_noext(), temp_file = temp_file())
      } else {
        export_csv(results, temp_file())
      }
      
    })
    
    ## SMPS ------
    smps_data_reactive <- reactive({
      
      ds <- switch(input$level,
                   "0" = smps_l0(),
                   "1" = smps_l1())
      
    })
    
    # Build the AIM csv file from the database output
    ## Not currently working
    smps_l0 <- reactive({
      
      datasets <- smps_datasets(input$site, input$dates[1], input$dates[2], con)
      params <- datasets |>
        # Find the time range that overlaps between the metadata and the requested dates
        mutate(dataset_end = if_else(is.na(dataset_end), Sys.time(), dataset_end),
               start_time = pmax(as.POSIXct(input$dates[1], tz = "UTC"), dataset_start),
               end_time = pmin(as.POSIXct(input$dates[2], tz = "UTC"), dataset_end)) |>
        select(ds_value=value, start_time, end_time)
      
      # Iterate over the datasets
      aim_file_data <- function(ds_value, start_time, end_time, site, cols, con, n) {
        # get data and metadata
        meta <- smps_settings(site, start_time, end_time, con)
        df <- smps_data(site, start_time, end_time, con)
        # build aim file
        shiny::incProgress(1/n, message = paste("Constructing", n, 
                                                "AIM files for download.", 
                                                "Thank you for your patience."))
        file <- build_aim_file(meta, df, ds_value, cols)
        
      }
      cols <- smps_columns(con)
      withProgress(message = "Constructing AIM files", value = 0, {
        n <- nrow(params)
        dat <- purrr::pmap(params, aim_file_data, site = input$site, cols = cols, con = con, n = n)
        filenames <- file.path(tempdir(), paste0(filename_noext(), "_", 
                                                 seq(1, length(dat)), ".csv"))
        purrr::walk2(dat, filenames, \(x, y) readr::write_lines(x, y, na = ""))
      })
      zip::zip(
        zipfile = temp_file(),
        files = filenames,
        mode = "cherry-pick"
      )
      
    })
    

    smps_l1 <- reactive({
      df <- smps_l1b_df(input$site, input$dates[1], input$dates[2], con)
      if (input$metadata) {
        meta <- smps_metadata(input$site, input$dates[1], input$dates[2], level = "1b", con)  
        export_zip_shiny(df, meta, fname = filename_noext(), temp_file = temp_file())
      } else {
        export_csv(df, temp_file())
      }
    })
    
    ## AE33 --------
    ae33_data <- reactive({
      
      ds <- switch(input$level,
                   "0" = ae33_l0_reactive(),
                   "1" = ae33_l1_reactive())
      
    })
    

    ae33_l1_reactive <- reactive({
      
      results <- ae33_l1b(input$site, input$dates[1], input$dates[2], ae33_con)
      
      if (input$metadata) {
        
        metadata <- ae33_metadata(input$site, input$dates[1], input$dates[2],
                                     level = "1b", con)
        export_zip_shiny(results, metadata, fname = filename_noext(), temp_file = temp_file())
      } else {
        export_csv(results, temp_file())
      }

    })
    
    #### ACSM -----
    
    acsm_data <- reactive({
      ds <- acsm_reactive()
    })
    
    acsm_reactive <- reactive({
      
      results <- acsm_l1a(input$site, input$dates[1], input$dates[2], con)
      
      if (input$metadata) {
        metadata <- acsm_metadata(input$site, input$dates[1], input$dates[2], con,
                                  metadata_fields = results$mdf, level = "1a")
        export_zip_shiny(results$df, metadata, fname = filename_noext(), temp_file = temp_file())
      } else {
        export_csv(results$df, temp_file())
      }

    })
    
    # Need to fix this
    content_type <- reactive({
      if (input$metadata) {
        "application/zip"
      } else {
        "text/csv"
      }
    })
    
    ### Estimate the number of records to return - keep button disabled if zero or too high
    expected_samples <- reactiveVal(0)
    
    output$expected <- renderText({
      
      expected <- availability(input$site, input$dates[1], input$dates[2], input$instrument)
      expected_samples(expected)
      glue::glue("Approx. sample count: {expected}")
      
    })
    
    # Download handler ------
    filename_noext <- reactiveVal()
    temp_file <- reactiveVal()
    
    output$download <- downloadHandler(
      filename = function() {
        # Provide the filename without extension, since we won't know the structure
        base_filename <- glue::glue("ASCENT_{input$site}_{input$instrument}_",
                                    "{input$dates[1]}_{input$dates[2]}_level-{input$level}")
        if (input$metadata) {
          f <- paste0(base_filename, ".zip")
        } else {
          if (input$instrument == "SMPS" & input$level == "0") {
            f <- paste0(base_filename, ".zip")
          } else {
            f <- paste0(base_filename, ".csv")  
          }
        }
        # Make accessible to reactives
        filename_noext(base_filename)
        return(f)
        
      },
      content = function(filename) {
        
        # Get the temporary file managed by Shiny so we can access it outside this func
        temp_file(filename)
        export_data()
        
      },
      contentType = content_type()
    )
    
    output$meta_text <- renderPrint({

      m <- basic_metadata(input$site, input$instrument, input$dates[1], 
                          input$dates[2], input$level, con)
      m
    })
    
  })
}

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

