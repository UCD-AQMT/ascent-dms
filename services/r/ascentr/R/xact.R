
#' Xact l1a table structure - internal
#'
#' @param con
#'
#' @returns
#'
#' @examples
xact_l1a <- function(con) {
  xact_sa <- tbl(con, I("xact.sample_analysis"))
  xact_raw <- tbl(con, I("xact.raw_measurements"))
  sites <- tbl(con, I("common.sites"))

  xact_sa |>
    inner_join(select(xact_raw, sample_analysis_id, element, value, uncertainty),
               by = c("id"="sample_analysis_id")) |>
    inner_join(sites, by = "site_number") |>
    select(site_number, site_code,
           sample_datetime_UTC=sample_datetime, sample_type, alarm, element,
           concentration=value, uncertainty, pump_start_time_UTC=pump_start_time,
           sample_time, at, sample, bp, tape, flow_25, flow_act, flow_std, volume,
           tube, enclosure, filament, sdd, dpp, rh, wind, wind_dir,
           sample_analysis_id=id, site_record_id)

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
xact_l1a_df <- function(site, start_dt, end_dt, con) {

  xact_sa <- tbl(con, I("xact.sample_analysis"))
  xact_raw <- tbl(con, I("xact.raw_measurements"))
  sites <- tbl(con, I("common.sites"))
  params <- tbl(con, I("xact.env_params")) |>
    collect()
  settings <- tbl(con, I("xact.instrument_settings"))

  # make sure to get the hours from the last day
  end_date <- as.Date(end_dt) + 1

  df <- xact_l1a(con) |>
    filter(site_code == site,
           sample_datetime_UTC >= start_dt,
           sample_datetime_UTC < end_date) |>
    arrange(sample_datetime_UTC) |>
    collect() |>
    mutate(sample_type = if_else(sample_type == 1, "sample", "QC"),
           .after = sample_datetime_UTC)

  # Eliminate duplicate records - these should be eliminated in the db, but this is a
  # stopgap/guardrail
  df <- df[!duplicated(df[, c("sample_datetime_UTC", "sample_type", "element")]),]

  # Recalculate uncertainties for those records prior to Xact software version 1.2.2.123
  versions <- settings |>
    inner_join(sites, by = "site_number") |>
    filter(site_code == site,
           name == "xc_ver",
           start_date < end_dt) |>
    select(start_date, version=value) |>
    summarise(start_date = min(start_date, na.rm = TRUE),
              .by = version) |>
    collect()

  min_version <- package_version("1.2.2.123")

  df2 <- df |>
    left_join(versions,
              by = join_by(closest(sample_datetime_UTC >= start_date))) |>
    mutate(version = package_version(version),
           updated = if_else(version >= min_version, TRUE, FALSE),
           uncertainty = if_else(updated == TRUE, uncertainty,
                                 concentration * sqrt((uncertainty / concentration)^2 + 0.003176)),
           uncertainty = if_else(is.nan(uncertainty), 0, uncertainty))

  df <- df2 |>
    select(site_number:site_record_id)

  # Make sure degrees and percents aren't converted to symbols
  units::units_options(auto_convert_names_to_symbols = FALSE)

  # Make df unit aware and convert to the desired units
  df_fields <- tibble(param = colnames(df)) |>
    left_join(params, by = "param")
  dfu <- purrr::map(colnames(df), \(x) attach_units(x, df, df_fields)) |>
    purrr::list_cbind()

  # Convert pressures to Pa (and integers)
# Add units to concentration and uncertainty
  odd_pressures <- df_fields |>
    filter(unit %in% c("mmHg", "kPa")) |>
    pull(param)
  dfu <- dfu |>
    # This works, but is a hack and a half. The first set_units converts from mmHg to Pa,
    # then we convert to int,but that drops the units, the second set_units restores them
    mutate(across(odd_pressures, ~units::set_units(.x, Pa))) |>
    mutate(across(odd_pressures, as.integer)) |>
    mutate(across(odd_pressures, ~units::set_units(.x, Pa))) |>
    mutate(across(c(concentration, uncertainty), ~units::set_units(.x, ng/m3)))

  # Convert concentration and uncertainty to ASCENT standard conditions (0 C, 101325 Pa)
  dfu <- dfu |>
    mutate(amb_temp_K = units::set_units(at, K),
           stp_factor = (units::set_units(101325, Pa)  / bp) * (amb_temp_K / units::set_units(273.15, K)),
           concentration_stp = concentration * stp_factor,
           uncertainty_stp = uncertainty * stp_factor) |>
    select(-amb_temp_K)

  # add to the list of fields
  stp_fields <- tibble(param = c("stp_factor", "concentration_stp", "uncertainty_stp"),
                       unit = c(NA, "ng/m3", "ng/m3"),
                       description = c(NA, NA, NA))
  df_fields <- bind_rows(df_fields, stp_fields)

  # Add units to field names
  unit_suffix <- purrr::map(dfu, get_unit_suffix)
  unit_parens <- purrr::map(dfu, get_unit_paren)
  new_colnames <- paste0(colnames(dfu), unit_suffix)
  colnames(dfu) <- new_colnames

  # Convert to reasonable significant digits
  dfu2 <- dfu |>
    mutate(across(where(is.numeric) & !c(sample_analysis_id, site_record_id),
                  \(x) signif(x, digits = 6)))

  # Create field descriptions for metadata
  common <- common_fields("1a") |>
    mutate(common_export_description = if_else(is.na(unit), description,
                                paste0(description, " (", unit, ")"))) |>
    select(param, common_export_description)
  xact_extra <- tibble(param = c("sample_type", "element", "concentration", "uncertainty",
                                 "stp_factor", "concentration_stp", "uncertainty_stp",
                                 "pump_start_time_UTC"),
                       common_export_description = c("'sample'= an ambient sample, 'QC'= a quality control measurement",
                                       "Element atomic symbol",
                                       "Element concentration at ambient conditions (ng/m3)",
                                       "Instrument concentration uncertainty calculated based on the most updated recommendation from manufacturer at ambient conditions (ng/m3)",
                                       "Multiplication conversion factor to standard temperature (0\u00B0C) and pressure (1 atm)",
                                       "Element concentration at standard temperature and pressure (ng/m3)",
                                       "Instrumental concentration uncertainty calculated based on the most updated recommendation from manufacturer at standard temperature and pressure (ng/m3)",
                                       "Pump start datetime (UTC)"))
  common <- bind_rows(common, xact_extra)

  output_fields <- tibble(param = colnames(dfu)) |>
    left_join(params, by = "param")

  metadata_fields <- df_fields |>
    left_join(common, by = "param") |>
    mutate(export_fieldname = new_colnames,
           export_description = if_else(is.na(description), common_export_description,
                                        paste0(description, unit_parens))) |>
    select(param, export_fieldname, export_description)


  return(list(df=dfu, mdf=metadata_fields))

}


#' Title
#'
#' @param site
#' @param start_dt
#' @param end_dt
#' @param con
#' @param metadata_fields
#'
#' @returns
#' @export
#'
#' @examples
xact_l1_metadata <- function(site, start_dt, end_dt, level = "1a", con,
                              metadata_fields = NULL) {

  # basic metadata
  basic <- basic_metadata(site, "Xact", start_dt, end_dt, level = level, con = con)

  # What software versions apply?
  versions <- tbl(con, I("xact.instrument_settings")) |>
    inner_join(select(tbl(con, I("common.sites")), site_number, site_code),
               by = "site_number") |>
    filter(site_code == site,
           name == "xc_ver") |>
    select(value, start_date, end_date) |>
    collect()

  # End dates are currently unreliable, use start dates, then limit to data range
  vers <- versions |>
    arrange(start_date) |>
    mutate(end_date = lead(start_date, default = Sys.time())) |>
    filter(start_date < end_dt & (end_date > start_date | is.na(end_date)),
           end_date > start_dt) |>
    mutate(vers_text = paste(value, start_date, sep = ", "))
  v_text <- paste(vers$vers_text, collapse = "\n")

  # If the metadata fields aren't provided, get them
  if (is.null(metadata_fields)) {
    metadata_fields <- xact_l1a_df(site, start_dt, end_dt, con)$mdf
  }

  m <- metadata_fields |>
    mutate(field_text = paste0(export_fieldname, ": ", export_description))
  field_descriptions <- paste(m$field_text, collapse = "\n")

  out <- glue::glue("{basic}\n",
             "Xact Software Versions\n",
             "Version, start date\n",
             "{v_text}\n",
             "\n",
             "Field Descriptions\n",
             "{field_descriptions}")

  if (level == "1b") {
    out <- glue::glue("{out}\n",
                      "qc_outcome: An overall quality control outcome for this record (1-Good, 2-Not evaluated/Unknown, 3-Questionable, 4-Bad/Invalid, 9-Missing)\n",
                      "flag: Data quality flag(s) as defined by EBAS (ebas.nilu.no)\n",
                      "comment: Comments that provide more detail about any flags applied to the record"
    )
  }

  out

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
xact_l1b <- function(site, start_dt, end_dt, con) {

  l1a <- xact_l1a_df(site, start_dt, end_dt, con)
  df <- l1a$df

  # Apply auto-qc

  # 1. Terminal statuses - these are not in our data because they do not make it to the
  # csv files (only modbus)

  f1 <- df |>
    filter(alarm >= 100,
           alarm < 200) |>
    select(sample_analysis_id, element, alarm) |>
    mutate(flag = "659",
           qc_outcome = 4,
           comment = case_match(alarm,
                                100 ~ "659-Critical Operation Error (malfunctioning Xray)",
                                101 ~ "659-Critical Operation Error (malfunctioning Xray)",
                                102 ~ "659-Critical Operation Error (Xray tube warmer than 45C)",
                                103 ~ "659-Critical Operation Error (enclosure warmer than 40C)",
                                104 ~ "659-Critical Operation Error (tape move failure)",
                                105 ~ "659-Critical Operation Error (pump control failure)",
                                106 ~ "659-Critical Operation Error (filter wheel failure)",
                                107 ~ "659-Critical Operation Error (dynamic rod failure)",
                                108 ~ "659-Critical Operation Error (nozzle move failure)",
                                109 ~ "659-Critical Operation Error (detector connection failure)",
                                110 ~ "659-Critical Operation Error (internal software failed)",
                                111 ~ "659-Critical Operation Error (Low flow and overloaded filter)",
                                .default = "659-Critical Operation Error"
                                )
           ) |>
    select(-alarm)


  # 2. software processing error
  f2 <- df |>
    filter(alarm == 213) |>
    select(sample_analysis_id, element) |>
    mutate(flag = "659",
           qc_outcome = 4,
           comment = "659-Critical Operation Error (internal software failed to process data)"
           )

  # 3. Non-sample (i.e., QA upscale)
  f3 <- df |>
    filter(sample_type != "sample") |>
    select(sample_analysis_id, element) |>
    mutate(flag = "683",
           qc_outcome = 9,
           comment = "683-Daily upscale or XRF energy checks")

  # 4. Upscale deviation
  f4 <- df |>
    filter(alarm >= 200,
           alarm <= 204) |>
    select(sample_analysis_id, element) |>
    mutate(flag = "660",
           qc_outcome = 3,
           comment = "660-Upscale value deviates from set point by more than 15%")

  # 5. Nb above max
  f5 <- df |>
    filter(alarm == 205) |>
    select(sample_analysis_id, element) |>
    mutate(flag = "659",
           qc_outcome = 4,
           comment = "659-Nb exceeds max limit of 1000")

  # 6. Low flow
  flow_threshold <- units::set_units(15.7, L/min)
  f6 <- df |>
    filter(flow_act_L_min < flow_threshold) |>
    select(sample_analysis_id, element) |>
    mutate(flag = "664",
           qc_outcome = 4,
           comment = "664-Average flow rate lower than 15.7 L/min")

  # 7. Tape pressure high
  pressure_ratio <- units::set_units(0.95, 1)
  f7 <- df |>
    filter(tape_Pa / bp_Pa > pressure_ratio) |>
    select(sample_analysis_id, element) |>
    mutate(flag = "641",
           qc_outcome = 4,
           comment = "641-Tape pressure too high (filter tape not installed properly)")

  # 8. Low flow alert
  f8 <- df |>
    filter(alarm == 211) |>
    select(sample_analysis_id, element) |>
    mutate(flag = "660",
           qc_outcome = 3,
           comment = "660-Flow too low during sampling period")


  # 9. High uncertainty (don't apply to QC samples)
  uncertainty_threshold <- units::set_units(3)
  f9 <- df |>
    filter((concentration_ng_m3 / uncertainty_ng_m3) < uncertainty_threshold,
           sample_type == "sample") |>
    select(sample_analysis_id, element) |>
    mutate(flag = "453",
           qc_outcome = 1,
           comment = "453-Measured value less than 3 times the estimated uncertainty")

  # Combine all flags
  flags <- bind_rows(f1, f2, f3, f4, f5, f6, f7, f8, f9) |>
    summarise(qc_outcome = max(qc_outcome),
              flag = paste(sort(unique(flag)), collapse = ":"),
              comment = paste(comment, collapse = "  :  "),
              .by = c(sample_analysis_id, element))


  df <- df |>
    left_join(flags, by = c("sample_analysis_id", "element")) |>
    mutate(qc_outcome = if_else(is.na(qc_outcome), 1, qc_outcome))

}

xact_l2_from_files <- function(l1b_file, manual_qc_file) {
  
  # Xact specific flags
  available_flags <- tibble(manual_flag = c("111", "686", "459", "659"),
                            manual_qc_outcome = c(1, 9, 4, 4))
  
  available_flags <- bind_rows(available_flags, common_manual_flags) |>
    distinct()
  
  l1b <- readr::read_csv(l1b_file)
  qc <- readr::read_csv(manual_qc_file)
  
  qc <- qc |>
    mutate(flag = as.character(flag),
           sample_datetime_UTC_end = if_else(is.na(sample_datetime_UTC_end),
                                             sample_datetime_UTC_start,
                                             sample_datetime_UTC_end)) |>
    rename(manual_flag=flag, manual_comment=comment)
  
  df <- l1b |>
    left_join(qc, by = join_by(between(sample_datetime_UTC,
                                       sample_datetime_UTC_start,
                                       sample_datetime_UTC_end))) |>
    left_join(available_flags, by = "manual_flag") |>
    mutate(flag = as.character(flag))

  df
  
}

