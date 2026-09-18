
#' Retrieve SMPS instrument settings for a site and time range
#'
#' Retrieves distinct instrument setting name/value pairs (excluding
#' `"Dataset Name"`) that were in effect on or before `end_dt`, with the
#' earliest date each name/value combination took effect.
#'
#' @param site ASCENT site code
#' @param start_dt Start date/datetime of the requested range (used only to
#'   bound the returned range together with `end_dt`)
#' @param end_dt End date/datetime (inclusive) of the requested range
#' @param con A database connection, as returned by [get_db_connection()]
#'
#' @returns A data frame with columns `name`, `value`, and `start_date`
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


#' Retrieve raw SMPS sample analysis data for a site and time range
#'
#' @param site ASCENT site code
#' @param start_dt Start date/datetime (inclusive) of the requested range
#' @param end_dt End date (inclusive) of the requested range
#' @param con A database connection, as returned by [get_db_connection()]
#'
#' @returns A data frame of SMPS sample analysis records
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

#' Retrieve SMPS dataset names in effect during a time range
#'
#' Looks up the `"Dataset Name"` instrument setting records that overlap the
#' requested time range for a site.
#'
#' @param site ASCENT site code
#' @param start_dt Start date/datetime (inclusive) of the requested range
#' @param end_dt End date (inclusive) of the requested range
#' @param con A database connection, as returned by [get_db_connection()]
#'
#' @returns A data frame with columns `value` (the dataset name),
#'   `dataset_start`, `dataset_end`, and `ds_site_number`
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

#' Get SMPS column name mappings from the database
#'
#' Retrieves the mapping between the SMPS instrument file column names and
#' the corresponding database column names, excluding the JSON blob
#' columns.
#'
#' @param con A database connection, as returned by [get_db_connection()]
#'
#' @returns A data frame with columns `file_column_name`, `db_column_name`,
#'   and `column_type`
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


#' Build SMPS metadata text
#'
#' Assembles the text metadata file that accompanies an SMPS data export,
#' including basic site/instrument metadata, field descriptions appropriate
#' to the requested data level, and the instrument settings in effect
#' during the requested time range.
#'
#' @param site ASCENT site code
#' @param start_dt Start date/datetime (inclusive) of the requested range
#' @param end_dt End date (inclusive) of the requested range
#' @param level Data level: one of `"1a"`, `"1b"`, `"2"`, or `"2N"`
#' @param con A database connection, as returned by [get_db_connection()]
#'
#' @returns A single string containing the formatted metadata text
#' @export
#'
#' @examples
smps_metadata <- function(site, start_dt, end_dt, level = "1a", con) {
  
  # basic metadata
  basic <- basic_metadata(site, "SMPS", start_dt, end_dt, level = level, con = con)
  
  # field definitions
  template <- switch(level,
                     "1a" = "smps_l1a_field_descriptions.txt",
                     "1b" = "smps_l1b_field_descriptions.txt",
                     "2" = "smps_l2_field_descriptions.txt",
                     "2N" = "smps_l2N_field_descriptions.txt")
  fields_path <- system.file(template, package="ascentr")
  fields <- paste(readLines(fields_path), collapse = "\n")
  
  # metadata from instrument settings
  settings <- smps_settings(site, start_dt, end_dt, con)
  
  # Only include metadata relevant to the date range
  # Need only the last record prior to the date range and all records within the range
  before <- settings |>
    filter(start_date < start_dt) |>
    arrange(desc(start_date)) |>
    slice(1, .by = name)
  during <- settings |>
    filter(start_date >= start_dt,
           start_date < end_dt)
  settings <- bind_rows(before, during) |>
    arrange(name, start_date) |>
    mutate(line = paste0(name, ": ", value, "    ", start_date))
  setting_desc <- paste(settings$line, collapse = "\n")
  
  te <- paste("Data are reported without consideration of transmission efficiency (TE)",
              "in the sampling inlet. An estimate of TE has been calculated and is",
              "available upon request from the site PI.")
  
  glue::glue("{basic}\n",
             "\n",
             "Data Processing Details\n",
             "{te}\n",
             "\n",
             "Field Descriptions\n",
             "{fields}\n",
             "\n",
             "Instrument metadata\n",
             "(Parameter: value    effective datetime (UTC))\n",
             "{setting_desc}")
  
}

#' Integrate number/volume/mass distribution
#'
#' Integrates a distribution (e.g., dN/dlogDp, dV/dlogDp, or dM/dlogDp) over
#' size bins to get the corresponding total (e.g., N, V, or M).
#'
#' @param dWdlogDp A matrix or data frame of distribution values, with one
#'   row per scan and one column per size bin
#' @param dlogDp Numeric vector of log-diameter bin widths, as returned by
#'   [calc_dlogDp()], with one value per column of `dWdlogDp`
#'
#' @returns A numeric vector of integrated totals, one per row of
#'   `dWdlogDp`
#' @export
#'
#' @examples
calc_W <- function(dWdlogDp, dlogDp) {
  dW <- dWdlogDp * dlogDp
  W <- rowSums(dW)
}


#' Calculate log-diameter bin widths for SMPS size bins
#'
#' Given the midpoint diameters of a set of SMPS size bins, calculates the
#' lower and upper bin boundaries (as the geometric mean of adjacent
#' midpoints, with the first and last boundaries extrapolated using the
#' average log-spacing) and returns the resulting log10 bin widths.
#'
#' @param midpoints Numeric vector of size bin midpoint diameters (nm), in
#'   increasing order
#'
#' @returns A numeric vector of `log10(D_high) - log10(D_low)` bin widths,
#'   the same length as `midpoints`
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

# Need to convert to lists to convince yyjsonr that these are not arrays   
write_atomic_json <- function(x) {
  if (typeof(x) != "list") {
    return(NA)
  }
  y <- as.list(x)
  # Remove any NA that could come from different scan range
  y <- y[which(!is.na(y))]
  yyjsonr::write_json_str(y, auto_unbox = TRUE, digits = 4)
}
