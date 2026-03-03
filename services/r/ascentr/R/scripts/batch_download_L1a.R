
# Batch download L1a data from ASCENT database for all instruments by site, month,
# starting with 2024 when most of the sites were in the right place

library(dplyr)
library(ascentr)
con <- ascentr::get_db_connection("dataconnection")

site <- tbl(con, I("common.sites")) |>
  filter(site_code != "Test") |>
  pull(site_code)

start_date <- seq.Date(from = as.Date("2024-01-01"), to = as.Date("2025-08-01"),
                        by = "1 month")

runs <- tidyr::crossing(start_date, site)

folder <- "C:/Users/sraffuse/OneDrive - University of California, Davis/Documents/ASCENT/DataExport/"

# Xact
xact_monthly <- function(start_date, site, con, folder) {

  end_date <- lubridate::rollforward(start_date)

  fname <- paste("ASCENT_Xact", site, strftime(start_date, format = "%Y%m%d"),
                  strftime(end_date, format = "%Y%m%d"), "L1a", sep = "_")

  results <- xact_l1a_df(site, start_date, end_date, con)
  if (nrow(results$df) == 0) {
    warning("No Xact data for ", site, " ", start_date, " - ", end_date)
    return(NULL)
  }
  meta <- xact_l1a_metadata(site, start_date, end_date, con, results$mdf)
  temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
  dir.create(temp_dir)
  txt_file <- file.path(temp_dir, paste0(fname, ".txt"))
  writeLines(meta, txt_file)
  csv_file <- file.path(temp_dir, paste0(fname, ".csv"))
  write.csv(results$df, csv_file, na = "", row.names = FALSE)
  zip_file <- file.path(folder, paste0(fname, ".zip"))
  zip::zip(zipfile = zip_file, files = c(txt_file, csv_file), mode = "cherry-pick")

}

purrr::walk2(runs$start_date, runs$site, \(x, y) xact_monthly(x, y, con, folder),
             .progress = TRUE)


# smps
smps_monthly <- function(start_date, site, con, folder) {

  end_date <- lubridate::rollforward(start_date)

  fname <- paste("ASCENT_SMPS", site, strftime(start_date, format = "%Y%m%d"),
                 strftime(end_date, format = "%Y%m%d"), "L1a", sep = "_")

  df <- smps_l1a_df(site, start_date, end_date, con)
  if (is.null(df)) {
    warning("No SMPS data for ", site, " ", start_date, " - ", end_date)
    return(NULL)
  }
  meta <- smps_l1a_metadata(site, start_date, end_date, con)
  temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
  dir.create(temp_dir)
  txt_file <- file.path(temp_dir, paste0(fname, ".txt"))
  writeLines(meta, txt_file)
  csv_file <- file.path(temp_dir, paste0(fname, ".csv"))
  write.csv(df, csv_file, na = "", row.names = FALSE)
  zip_file <- file.path(folder, paste0(fname, ".zip"))
  zip::zip(zipfile = zip_file, files = c(txt_file, csv_file), mode = "cherry-pick")

}

purrr::walk2(runs$start_date, runs$site, \(x, y) smps_monthly(x, y, con ,folder),
            .progress = TRUE)


# ae33
ae33_con <- ascentr::get_flux_client("dataconnection")

ae33_monthly <- function(start_date, site, con, ae33_con, folder) {

  end_date <- lubridate::rollforward(start_date)

  fname <- paste("ASCENT_AE33", site, strftime(start_date, format = "%Y%m%d"),
                 strftime(end_date, format = "%Y%m%d"), "L1a", sep = "_")
  df <- ae33_l1a(site, start_date, end_date, ae33_con)
  if (is.null(df)) {
    warning("No AE33 data for ", site, " ", start_date, " - ", end_date)
    return(NULL)
  }
  meta <- ae33_l1a_metadata(site, start_date, end_date, con)
  temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
  dir.create(temp_dir)
  txt_file <- file.path(temp_dir, paste0(fname, ".txt"))
  writeLines(meta, txt_file)
  csv_file <- file.path(temp_dir, paste0(fname, ".csv"))
  write.csv(df, csv_file, na = "", row.names = FALSE)
  zip_file <- file.path(folder, paste0(fname, ".zip"))
  zip::zip(zipfile = zip_file, files = c(txt_file, csv_file), mode = "cherry-pick")

}

purrr::walk2(runs$start_date, runs$site, \(x, y) ae33_monthly(x, y, con, ae33_con, folder),
             .progress = TRUE)

acsm_monthly <- function(start_date, site, con, folder) {

  end_date <- lubridate::rollforward(start_date)

  fname <- paste("ASCENT_ACSM", site, strftime(start_date, format = "%Y%m%d"),
                 strftime(end_date, format = "%Y%m%d"), "L1a", sep = "_")

  results <- acsm_l1a(site, start_date, end_date, con)
  if (nrow(results$df) == 0) {
    warning("No ACSM data for ", site, " ", start_date, " - ", end_date)
    return(NULL)
  }
  meta <- acsm_l1a_metadata(site, start_date, end_date, con, results$mdf)
  temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
  dir.create(temp_dir)
  txt_file <- file.path(temp_dir, paste0(fname, ".txt"))
  writeLines(meta, txt_file)
  csv_file <- file.path(temp_dir, paste0(fname, ".csv"))
  write.csv(results$df, csv_file, na = "", row.names = FALSE)
  zip_file <- file.path(folder, paste0(fname, ".zip"))
  zip::zip(zipfile = zip_file, files = c(txt_file, csv_file), mode = "cherry-pick")

}


purrr::walk2(runs$start_date, runs$site, \(x, y) acsm_monthly(x, y, con, folder),
             .progress = TRUE)

