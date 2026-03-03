
#' Title
#'
#' @param site_num
#' @param start_dt
#' @param end_dt
#' @param con
#'
#' @returns
#' @export
#' @import dplyr
#'
#' @examples
smps_settings <- function(site, start_dt, end_dt, con) {

  smps_meta <- tbl(con, I("smps.instrument_settings"))
  sites <- tbl(con, I("common.sites"))

  # smps.instruments_settings data is a bit messy. The end_dates are not reliable and
  # there are some duplicate entries.
  settings <- smps_meta |>
    inner_join(select(sites, site_number, site_code), by = "site_number") |>
    filter(start_date <= end_dt,
           site_code == site,
           name != "Dataset Name") |>
    select(name, value, start_date) |>
    distinct() |>
    collect()

  settings <- settings |>
    summarise(start_date = min(start_date),
              .by = c(name, value)) |>
    arrange(name, start_date)


  settings


}


#' Title
#'
#' @param site
#' @param start_dt
#' @param end_dt
#' @param con
#'
#' @returns
#' @export
#'
#' @examples
smps_data <- function(site, start_dt, end_dt, con) {

  smps_sa <- tbl(con, I("smps.sample_analysis"))
  sites <- tbl(con, I("common.sites"))

  # make sure to get the hours from the last day
  end_date <- as.Date(end_dt) + 1

  df <- smps_sa |>
    inner_join(select(sites, site_code, site_number),
               by = "site_number") |>
    filter(sample_start >= start_dt,
           sample_start < end_date,
           site_code == site) |>
    arrange(sample_start) |>
    collect()

}

#' Title
#'
#' @param site
#' @param start_dt
#' @param end_dt
#' @param con
#'
#' @returns
#' @export
#'
#' @examples
smps_datasets <- function(site, start_dt, end_dt, con) {

  smps_settings <- tbl(con, I("smps.instrument_settings"))
  sites <- tbl(con, I("common.sites"))

  # Get the dataset names from the instrument settings table for the site/time.
  datasets <- smps_settings |>
    inner_join(select(sites, site_number, site_code), by = "site_number") |>
    filter(start_date <= end_dt,
           (end_date >= start_dt | is.na(end_date)),
           site_code == site,
           name == "Dataset Name") |>
    select(value, dataset_start=start_date, dataset_end=end_date,
           ds_site_number=site_number) |>
    collect()

}

# Get the column names to remap from the database
#' Title
#'
#' @param con
#'
#' @returns
#' @export
#'
#' @examples
smps_columns <- function(con) {

  tbl(con, I("common.column_mappings")) |>
    filter(table_schema == "smps",
           table_name == "sample_analysis",
           !db_column_name %in% c("concentration_json", "raw_concentration_json")) |>
    select(file_column_name, db_column_name, column_type) |>
    collect()
}


#' Integrate number/volume/mass distribution
#'
#' @param dWdlogDp
#' @param dlogDp
#'
#' @returns
#' @export
#'
#' @examples
calc_W <- function(dWdlogDp, dlogDp) {
  dW <- dWdlogDp * dlogDp
  W <- rowSums(dW)
}


#' Title
#'
#' @param midpoints
#'
#' @returns
#' @export
#'
#' @examples
calc_dlogDp <- function(midpoints) {
  # Calculate the lower and upper bound for each size bin
  avg_diff <- mean(diff(log10(midpoints)))

  # The value of the midpoint 1 before
  previous_mid <- c(NA, midpoints)[1:length(midpoints)]

  # Create the bounds (one larger than the midpoints)
  bounds <- 10^(0.5 * (log10(midpoints) + log10(previous_mid)))
  bounds <- c(bounds, NA)

  # First and last boundary are based on the average difference
  bounds[1] <- 10^(log10(midpoints[1]) - 0.5 * avg_diff)
  bounds[length(bounds)] <- 10^(log10(midpoints[length(midpoints)]) + 0.5 * avg_diff)

  D_low <- bounds[1:length(bounds)-1]
  D_high <- bounds[2:length(bounds)]
  dlogDp <- log10(D_high) - log10(D_low)

}

calc_dVdlogDp <- function(dNdlogDp, midpoints) {
  # Calculate the volume distribution and total volume of scan
  # Need to make sure our dimensions are correct
  vol_convert <- (pi / 6) * (midpoints / 1000)^3
  dVdlogDp <-  t(apply(dNdlogDp, MARGIN = 1, function(x) x * vol_convert)) #um3/cm3
}

calc_dMdlogDp <- function(dNdlogDp, midpoints, density = 1.4) {
  # calculate mass distribution and total mass of scan. Must assume a particle density. (g/cm3)
  mass_convert <- (density / 1e9) * (pi / 6) * midpoints^3
  dMdlogDp <- t(apply(dNdlogDp, MARGIN = 1, function(x) x * mass_convert))    #ug/m3
}

