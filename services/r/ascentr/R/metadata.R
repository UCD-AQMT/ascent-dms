# tools for producing text metadata


## TODO: Grant number to credit should depend on the time range of data
# The basic metadata without instrument-specific field definitions
#' Title
#'
#' @param site
#' @param instrument
#' @param start_dt
#' @param end_dt
#' @param level
#' @param con
#'
#' @returns
#' @export
#'
#' @examples
basic_metadata <- function(site, instrument, start_dt, end_dt, level, con) {

  # tbl_sites <- tbl(con, I("common.sites"))
  # site_name <- tbl_sites |>
  #   filter(site_code == site) |>
  #   pull(site_name)

  site_info <- tbl(con, I("common.sites")) |>
    filter(site_code == site) |>
    collect()

  instrument_description <- switch(instrument,
                                   "Xact" = "Cooper Xact 625 Ambient Metals Monitor",
                                   "SMPS" = "TSI Scanning Mobility Particle Sizer (SMPS) Model 3938",
                                   "AE33" = "Magee Aethalometer Model AE33",
                                   "ACSM" = "Aerodyne Time-of-Flight Aerosol Chemical Speciation Monitor (TOF-ACSM)")

  level <- switch(level,
                  "0" = "Level 0. Raw instrument data",
                  "1" = "Level 1. Preliminary, unvalidated data in native time resolution as acquired in real time with automated quality control checks applied.",
                  "1a" = "Level 1a. Preliminary, unvalidated data in native time resolution as acquired in real time.",
                  "1b" = "Level 1b. Preliminary, unvalidated data in native time resolution as acquired in real time with automated quality control checks applied.",
                  "2" = "Level 2. Delivered, validated data in hourly resolution and standard atmospheric conditions.")

  policy_path <- system.file("data_policy.txt", package="ascentr")
  data_policy <- paste(readLines(policy_path), collapse = "\n")

  site_pi <- paste0(site_info$site_contact, " (", site_info$contact_email, ")")
  contacts <- glue::glue("ASCENT PI: Nga Lee (Sally) Ng (ng@caltech.edu)\n",
                   "Site PI: {site_pi}")

  metadata_text <- glue::glue("ASCENT Site: {site_info$site_name}\n",
                        "Instrument: {instrument_description}\n",
                        "Data Level: {level}\n",
                        "Time Period: {start_dt} - {end_dt}\n",
                        "\n",
                        "ASCENT Data Policy\n",
                        "{data_policy}\n",
                        "\n",
                        "Contacts\n",
                        "{contacts}\n",
                        "\n",
                        "Site Information\n",
                        "ASCENT Site Code: {site_info$site_code}\n",
                        "ASCENT Site Number: {site_info$site_number}\n",
                        "Coordinates: ({site_info$longitude}, {site_info$latitude})\n",
                        "Elevation: {site_info$elevation} (m above mean sea level)\n",
                        "Time Zone: {site_info$timezone} (UTC {site_info$gmt_offset})\n",
                        "\n")

}

# Fields that appear on output for all instruments
common_fields <- function(level) {

  if (level == "1a") {
    df <- tibble(param = c("site_number", "site_code", "sample_datetime_UTC",
                           "sample_analysis_id",  "site_record_id"),
                 unit = c(NA, NA, "UTC", NA, NA),
                 description = c("ASCENT site number",
                                "ASCENT site code",
                                "Datetime of sample start",
                                "An id that uniquely identifies this measurement in the respective table of the ASCENT database",
                                "An id that identifies this measurement in the respective table of the local site database")
    )
  }

  if (level == "1b") {
    df <- tibble(param = c("site_number", "site_code", "sample_datetime_UTC",
                           "sample_analysis_id",  "site_record_id", "qc_outcome",
                           "flag", "comment"),
                 unit = c(NA, NA, "UTC", NA, NA, NA, NA, NA),
                 description = c("ASCENT site number",
                                 "ASCENT site code",
                                 "Datetime of sample start",
                                 "An id that uniquely identifies this measurement in the respective table of the ASCENT database",
                                 "An id that identifies this measurement in the respective table of the local site database",
                                 "A quality control outcome 1-Good, 2-Not evaluated/unknown, 3-Questionable/suspect, 4-Bad/Invalid, 9-Missing",
                                 "One or more quality control flags",
                                 "Comment describing flagging details")
    )
  }

  df

}
