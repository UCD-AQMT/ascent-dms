
# Some functions for exporting delivery files, ensuring proper formatting

#' Title
#'
#' @param site 
#' @param month_date 
#' @param instrument 
#' @param folder 
#' @param con 
#' @param influx_con 
#'
#' @returns
#' @export
#'
#' @examples
monthly_l1b <- function(site, month_date, instrument, folder, con, influx_con = NULL) {
  
  end_date <- lubridate::rollforward(as.Date(month_date))
  
  fname <- paste("ASCENT", instrument, site, strftime(month_date, format = "%Y%m%d"),
                 strftime(end_date, format = "%Y%m%d"), "L1b", sep = "_")
  
  if (instrument == "AE33") {
    if (is.null(influx_con)) {
      stop("influx_con must be provided to process AE33")
    }
    df <- ae33_l1b(site, month_date, end_date, influx_con)
    if (is.null(df)) {
      warning("No AE33 data for ", site, " ", month_date, " - ", end_date)
      return(NULL)
    }
    meta <- ae33_metadata(site, month_date, end_date, level = "1b", con = con)
  } else if (instrument == "SMPS") {
    df <- smps_l1b_df(site, month_date, end_date, con)
    if (is.null(df)) {
      warning("No SMPS data for ", site, " ", month_date, " - ", end_date)
      return(NULL)
    }
    meta <- smps_metadata(site, month_date, end_date, level = "1b", con = con)
  } else if (instrument == "Xact") {
    df <- xact_l1b(site, month_date, end_date, con)
    if (is.null(df)) {
      warning("No Xact data for ", site, " ", month_date, " - ", end_date)
      return(NULL)
    }
    meta <- xact_metadata(site, month_date, end_date, level = "1b", con = con)
  } else if (instrument == "ACSM") {
    result <- acsm_l1b(site, month_date, end_date, con)
    df <- result$df
    mdf <- result$mdf
    if (is.null(df)) {
      warning("No Xact data for ", site, " ", month_date, " - ", end_date)
      return(NULL)
    }
    meta <- acsm_metadata(site, month_date, end_date, level = "1b", con = con, metadata_fields = mdf)
  } else {
    stop("Cannot export instrument named ", instrument)
  }
  
  
  export_zip(df, meta, folder, fname)
  
}


export_csv <- function(df, outfile) {
  
  # Convert all POSIX to formatted strings
  df <- format_dates(df)
  write.csv(df, outfile, na = "", row.names = FALSE)
  
}

export_zip <- function(df, meta, folder, fname) {

  temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
  dir.create(temp_dir)
  txt_file <- file.path(temp_dir, paste0(fname, ".txt"))
  writeLines(meta, txt_file)
  csv_file <- file.path(temp_dir, paste0(fname, ".csv"))
  # Convert all POSIX to formatted strings
  df <- format_dates(df)
  write.csv(df, csv_file, na = "", row.names = FALSE)
  zip_file <- file.path(folder, paste0(fname, ".zip"))
  zip::zip(zipfile = zip_file, files = c(txt_file, csv_file), mode = "cherry-pick")
   
}

export_zip_shiny <- function(df, meta, fname, temp_file) {
  
  temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
  dir.create(temp_dir)
  txt_file <- file.path(temp_dir, paste0(fname, ".txt"))
  writeLines(meta, txt_file)
  csv_file <- file.path(temp_dir, paste0(fname, ".csv"))
  # Convert all POSIX to formatted strings
  df <- format_dates(df)
  write.csv(df, csv_file, na = "", row.names = FALSE)
  zip::zip(
    zipfile = temp_file,
    files = c(txt_file, csv_file),
    mode = "cherry-pick"
  )
  
}

format_dates <- function(df) {
  mutate(df, across(where(lubridate::is.POSIXt), 
                    ~strftime(.x, format = "%F %T", tz = "UTC")))
}