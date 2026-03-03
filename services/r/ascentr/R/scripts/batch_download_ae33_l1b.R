
# Batch download L1b AE33 data for 2024 and earlier. Just
# csv without metadata. Do one month with metadata as a sample (April 2025, when most
# sites were running)

library(dplyr)
library(ascentr)
con <- ascentr::get_db_connection("dataconnection")
influx <- ascentr::get_flux_client("dataconnection")


site <- tbl(con, I("common.sites")) |>
  filter(site_code != "Test") |>
  pull(site_code)


folder <- "C:/Users/sraffuse/OneDrive - University of California, Davis/Documents/ASCENT/DataExport/L1b/AE33/"

# zip file with metadata (as an example)
with_metadata <- function(start_date, site, con, influx_con, folder) {

  end_date <- lubridate::rollforward(start_date)

  fname <- paste("ASCENT_AE33", site, strftime(start_date, format = "%Y%m%d"),
                 strftime(end_date, format = "%Y%m%d"), "L1b", sep = "_")

  df <- ae33_l1b(site, start_date, end_date, influx_con)
  if (is.null(df)) {
    warning("No AE33 data for ", site, " ", start_date, " - ", end_date)
    return(NULL)
  }
  meta <- ae33_l1_metadata(site, start_date, end_date, level = "1b", con)
  temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
  dir.create(temp_dir)
  txt_file <- file.path(temp_dir, paste0(fname, ".txt"))
  writeLines(meta, txt_file)
  csv_file <- file.path(temp_dir, paste0(fname, ".csv"))
  write.csv(df, csv_file, na = "", row.names = FALSE)
  zip_file <- file.path(folder, paste0(fname, ".zip"))
  zip::zip(zipfile = zip_file, files = c(txt_file, csv_file), mode = "cherry-pick")

}

start_date <- as.Date("2025-04-01")
runs <- tidyr::crossing(start_date, site)
purrr::walk2(runs$start_date, runs$site, \(x, y) with_metadata(x, y, con, influx, folder),
             .progress = TRUE)


# And Feb 2023-Dec 2024
without_metadata <- function(start_date, site, influx_con, folder) {

  end_date <- lubridate::rollforward(start_date)

  fname <- paste("ASCENT_AE33", site, strftime(start_date, format = "%Y%m%d"),
                 strftime(end_date, format = "%Y%m%d"), "L1b", sep = "_")

  df <- ae33_l1b(site, start_date, end_date, influx_con)
  if (is.null(df)) {
    warning("No AE33 data for ", site, " ", start_date, " - ", end_date)
    return(NULL)
  }
  csv_file <- file.path(folder, paste0(fname, ".csv"))
  write.csv(df, csv_file, na = "", row.names = FALSE)
}

start_date <- seq.Date(from = as.Date("2023-02-01"), to = as.Date("2024-12-01"),
                       by = "1 month")

runs <- tidyr::crossing(start_date, site)

purrr::walk2(runs$start_date, runs$site, \(x, y) without_metadata(x, y, influx, folder),
             .progress = TRUE)
