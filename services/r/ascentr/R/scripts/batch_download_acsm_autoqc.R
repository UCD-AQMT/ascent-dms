
# Batch download the ACSM auto QC files for all time through 2024 one file per year per site.

library(dplyr)
#library(ascentr)
con <- get_db_connection("dataconnection")

site <- tbl(con, I("common.sites")) |>
  filter(site_code != "Test") |>
  pull(site_code)


folder <- "C:/Users/sraffuse/OneDrive - University of California, Davis/Documents/ASCENT/DataExport/AutoQC/"

autoqc <- function(site, start_dt) {

  yr <- substr(start_dt, 1, 4)
  end_dt <- paste0(yr, "-12-31")

  df <- acsm_autoqc(site, start_dt, end_dt, con)

  # if (nrow(df) == 0) {
  #   return(NULL)
  # }

  filename <- paste0("ACSM_AutoQC_", site, "_", strftime(start_dt, format = "%Y%m%d"),
                    "_", strftime(end_dt, format = "%Y%m%d"), ".csv")
  filename <- fs::path_join(c(folder, filename))
  write.csv(df, filename, na = "", row.names = FALSE)

}


autoqc("LosAngeles", "2025-01-01")

start_dt <- seq.Date(from = as.Date("2022-07-01"), to = as.Date("2024-12-01"),
                                   by = "1 month")
runs <- tidyr::crossing(site, start_dt)

purrr::pwalk(runs, autoqc, .progress = TRUE)
