
# Some functions for exporting delivery files, ensuring proper formatting

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

format_dates <- function(df) {
  mutate(df, across(where(lubridate::is.POSIXt), 
                    ~strftime(.x, format = "%F %T", tz = "UTC")))
}