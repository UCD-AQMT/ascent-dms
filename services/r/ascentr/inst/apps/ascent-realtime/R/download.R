
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
      uiOutput(ns("mybutton")),
      width = "300px"
    ),
    card(verbatimTextOutput(ns("meta_text"))
    )
  )
}

downloadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Dynamic UI ---------
    
    output$mybutton <- renderUI({
      
      ns <- session$ns
      if (isTruthy(input$agree)) {
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
    
    
    observeEvent(input$dummybutton, {
      showModal(modalDialog(
        title = "Message",
        "Please accept the data policy to download data"
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
        meta <- xact_l1_metadata(input$site, input$dates[1], input$dates[2], con = con,
                                           metadata_fields = results$mdf)
        temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_dir)
        txt_file <- file.path(temp_dir, paste0(filename_noext(), ".txt"))
        writeLines(meta, txt_file)
        csv_file <- file.path(temp_dir, paste0(filename_noext(), ".csv"))
        readr::write_csv(results$df, csv_file, na = "")
        zip::zip(
          zipfile = temp_file(),
          files = c(txt_file, csv_file),
          mode = "cherry-pick"
        )
      } else {
        readr::write_csv(results$df, temp_file(), na = "")
      }
    })
    
    xact_l1b_reactive <- reactive({
      
      results <- xact_l1b(input$site, input$dates[1], input$dates[2], con)
    
      if (input$metadata) {
        meta <- xact_l1_metadata(input$site, input$dates[1], input$dates[2],
                                 level = "1b", con = con)
        temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_dir)
        txt_file <- file.path(temp_dir, paste0(filename_noext(), ".txt"))
        writeLines(meta, txt_file)
        csv_file <- file.path(temp_dir, paste0(filename_noext(), ".csv"))
        readr::write_csv(results, csv_file, na = "")
        zip::zip(
          zipfile = temp_file(),
          files = c(txt_file, csv_file),
          mode = "cherry-pick"
        )
      } else {
        readr::write_csv(results, temp_file(), na = "")
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
    
    # Deprecated
    # smps_l1a <- reactive({
    #   df <- smps_l1a_df(input$site, input$dates[1], input$dates[2], con)
    #   if (input$metadata) {
    #     meta <- smps_l1_metadata(input$site, input$dates[1], input$dates[2], level = "1a", con)  
    #     temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
    #     dir.create(temp_dir)
    #     txt_file <- file.path(temp_dir, paste0(filename_noext(), ".txt"))
    #     writeLines(meta, txt_file)
    #     csv_file <- file.path(temp_dir, paste0(filename_noext(), ".csv"))
    #     readr::write_csv(df, csv_file, na = "")
    #     zip::zip(
    #       zipfile = temp_file(), 
    #       files = c(txt_file, csv_file),
    #       mode = "cherry-pick"
    #     )
    #   } else {
    #     readr::write_csv(df, temp_file(), na = "")
    #   }
    # })
    
    smps_l1 <- reactive({
      df <- smps_l1b_df(input$site, input$dates[1], input$dates[2], con)
      if (input$metadata) {
        meta <- smps_l1_metadata(input$site, input$dates[1], input$dates[2], level = "1b", con)  
        temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_dir)
        txt_file <- file.path(temp_dir, paste0(filename_noext(), ".txt"))
        writeLines(meta, txt_file)
        csv_file <- file.path(temp_dir, paste0(filename_noext(), ".csv"))
        readr::write_csv(df, csv_file, na = "")
        zip::zip(
          zipfile = temp_file(), 
          files = c(txt_file, csv_file),
          mode = "cherry-pick"
        )
      } else {
        readr::write_csv(df, temp_file(), na = "")
      }
    })
    
    ## AE33 --------
    ae33_data <- reactive({
      
      ds <- switch(input$level,
                   "0" = ae33_l0_reactive(),
                   "1" = ae33_l1_reactive())
      
    })
    
    ae33_l0_reactive <- reactive({
      
      #TODO
      
    })
    
    # Deprecated
    # ae33_l1a_reactive <- reactive({
    #   
    #   results <- ae33_l1a(input$site, input$dates[1], input$dates[2], ae33_con)
    #   
    #   if (input$metadata) {
    #     
    #     metadata <- ae33_l1_metadata(input$site, input$dates[1], input$dates[2], 
    #                                  level = "1a", con)
    #     temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
    #     dir.create(temp_dir)
    #     txt_file <- file.path(temp_dir, paste0(filename_noext(), ".txt"))
    #     writeLines(metadata, txt_file)
    #     csv_file <- file.path(temp_dir, paste0(filename_noext(), ".csv"))
    #     readr::write_csv(results, csv_file, na = "")
    #     zip::zip(
    #       zipfile = temp_file(), 
    #       files = c(txt_file, csv_file),
    #       mode = "cherry-pick"
    #     )
    #   } else {
    #     readr::write_csv(results, temp_file(), na = "")  
    #   }
    #   
    #   
    # })
    
    ae33_l1_reactive <- reactive({
      
      results <- ae33_l1b(input$site, input$dates[1], input$dates[2], ae33_con)
      
      if (input$metadata) {
        
        metadata <- ae33_l1_metadata(input$site, input$dates[1], input$dates[2], 
                                     level = "1b", con)
        temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_dir)
        txt_file <- file.path(temp_dir, paste0(filename_noext(), ".txt"))
        writeLines(metadata, txt_file)
        csv_file <- file.path(temp_dir, paste0(filename_noext(), ".csv"))
        readr::write_csv(results, csv_file, na = "")
        zip::zip(
          zipfile = temp_file(), 
          files = c(txt_file, csv_file),
          mode = "cherry-pick"
        )
      } else {
        readr::write_csv(results, temp_file(), na = "")  
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

