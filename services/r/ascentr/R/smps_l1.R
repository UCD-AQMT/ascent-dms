# Produce SMPS L1a (and L1b?) files

# csv and metadata
# calculate number, volume, and mass concentration
# convert to stp

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
smps_l1a_df <- function(site, start_dt, end_dt, con) {

  df <- smps_data(site, start_dt, end_dt, con)

  if (nrow(df) == 0) {
    warning(paste("No data for", site))
    return(NULL)
  }

  read_and_trim <- function(js, min_scan, max_scan) {
    if (is.na(js)) {
      return(NA)
    }
    x <- as_tibble(yyjsonr::read_json_str(js)) |>
      tidyr::pivot_longer(everything(), names_to = "size", values_to = "value") |>
      mutate(size = as.numeric(size)) |>
      filter(size >= min_scan, size <= max_scan) |>
      tidyr::pivot_wider(names_from = size, values_from = value)
  }

  scan_inputs <- df |>
    select(js=concentration_json, min_scan=lower_size, max_scan=upper_size)
  smps_records <- purrr::pmap(scan_inputs, read_and_trim,
                              .progress = "Reading and trimming SMPS scans")
  conc_json <- purrr::map(smps_records, yyjsonr::write_json_str)

  raw_inputs <- df |>
    select(js=raw_concentration_json, min_scan=lower_size, max_scan=upper_size)
  raw_check <- raw_inputs |>
    filter(!is.na(js))
  if (nrow(raw_check) == 0) {
    conc_json_raw <- NA
  } else {
    smps_raw <- purrr::pmap(raw_inputs, read_and_trim,
                            .progress = "Reading and trimming SMPS raw scans")
    conc_json_raw <- purrr::map(smps_raw, yyjsonr::write_json_str)
  }
  
  
  
  
  # smps_records <- purrr::map(df$concentration_json,
  #                            \(x) as_tibble(yyjsonr::read_json_str(x)))

  # Make unit aware
  # Make sure degrees and percents aren't converted to symbols
  units::units_options(auto_convert_names_to_symbols = FALSE)

  df <- df |>
    mutate(across(c("detector_inlet_flow", "detector_counting_flow", "impactor_flow", "sheath_flow"),
                  ~units::set_units(.x, L/min))) |>
    mutate(across(c("lower_size", "upper_size", "impactor_d50", "mean", "median", "geo_mean", "mode"),
                  ~units::set_units(.x, nm))) |>
    mutate(across(c("dma_column_transit_time", "dma_v_ramping_up", "dma_v_ramping_down",
                    "dma_exit_to_optical_detector", "dma_at_low_voltage",
                    "dma_at_high_voltage", "adjustment"),
                  ~units::set_units(.x, s))) |>
    mutate(across(c("sheath_temp", "aerosol_temperature"),
                  ~units::set_units(.x, degC))) |>
    mutate(across(c("sheath_pressure"),
                  ~units::set_units(.x, kPa))) |>
    mutate(across(c("sheath_relative_humidity", "aerosol_humidity"),
                  ~units::set_units(.x, percent))) |>
    mutate(across(c("mean_free_path"),
                  ~units::set_units(.x, m))) |>
    mutate(across(c("gas_viscosity"),
                  ~units::set_units(.x, Pa*s))) |>
    mutate(across(c("low_voltage", "high_voltage"),
                  ~units::set_units(.x, V))) |>
    mutate(across(c("aerosol_density"),
                  ~units::set_units(.x, g/cm3))) |>
    mutate(across(c("total_concentration"),
                  ~units::set_units(.x, 1/cm3)))

  # Convert to standard units (Pa)
  df <- df |>
    mutate(sheath_pressure = units::set_units(sheath_pressure, Pa))

  # This processing must be done per-scan, in case the bins change
  process_scan_volume <- function(record) {

    midpoints <- as.numeric(names(record))
    dNdlogDp <- data.matrix(record)

    dlogDp <- calc_dlogDp(midpoints)
    dVdlogDp <- calc_dVdlogDp(dNdlogDp, midpoints)
    V <- calc_W(dVdlogDp, dlogDp)

  }

  V <- purrr::map_dbl(smps_records, process_scan_volume)
  V <- units::set_units(V, um3/cm3)

  std_pressure <- units::set_units(101.325, kPa)
  std_temp <- units::set_units(273.15, K)
  
  df <- df |>
    mutate(volume_concentration = V,
           sheath_temp_K = units::set_units(sheath_temp, K),
           stp_factor = (std_pressure / sheath_pressure) * (sheath_temp_K / std_temp),
           total_concentration_stp = total_concentration * stp_factor,
           volume_concentration_stp = volume_concentration * stp_factor,
           volume_concentration_stp = units::set_units(volume_concentration_stp, um3/cm3)) |>
    # Here we cut out aerosol_humidity and aerosol_temperature because our setup doesn't have it
    select(site_number, site_code, sample_datetime_utc=sample_start,
           scan_number, test_name:total_concentration, volume_concentration,
           stp_factor, total_concentration_stp, volume_concentration_stp,
           sample_analysis_id=id, site_record_id)

  # Add the trimmed json back
  df$concentration_json <- unlist(conc_json)
  df$raw_concentration_json <- unlist(conc_json_raw)
  df <- df |>
    mutate(raw_concentration_json = if_else(raw_concentration_json == "[null]",
                                            NA, raw_concentration_json)) |>
    relocate(sample_analysis_id, site_record_id, .after = raw_concentration_json)
  
  # Add units to field names
  unit_suffix <- purrr::map(df, get_unit_suffix)
  unit_parens <- purrr::map(df, get_unit_paren)
  new_colnames <- paste0(colnames(df), unit_suffix)
  colnames(df) <- new_colnames

  # reduce sigfigs
  df <- df |>
    mutate(across(where(is.numeric) & !c(sample_analysis_id, site_record_id),
                  \(x) signif(x, digits = 6)))

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
smps_l1b_df <- function(site, start_dt, end_dt, con) {

  df <- smps_l1a_df(site, start_dt, end_dt, con)

  if (is.null(df)) {
    return(NULL)
  }
  #### Apply auto-qc

  # 1. Detector Status error
  f1 <- df |>
    filter(detector_status != "Normal Scan") |>
    select(sample_analysis_id, detector_status) |>
    mutate(flag = "659",
           qc_outcome = 4,
           comment = paste("659-Detector status error:", detector_status)) |>
    select(-detector_status)

  # 2. Classifier error
  f2 <- df |>
    filter(classifier_errors != "Normal Scan") |>
    select(sample_analysis_id, classifier_errors) |>
    mutate(flag = "659",
           qc_outcome = 4,
           comment = paste("659-Classifier error:", classifier_errors)) |>
    select(-classifier_errors)

  # 3. Sheath RH too high - informational
  rh_threshold <- units::set_units(40, percent)
  f3 <- df |>
    filter(sheath_relative_humidity_percent > rh_threshold) |>
    select(sample_analysis_id) |>
    mutate(flag = "640",
           qc_outcome = 1,
           comment = "640-Sheath RH > 40%")

  # 4. Sheath flow not set properly
  sheath_flow_set <- units::set_units(4.8, L/min)
  f4 <- df |>
    filter(sheath_flow_L_min != sheath_flow_set) |>
    select(sample_analysis_id) |>
    mutate(flag = "660",
           qc_outcome = 1,
           comment = "660-Sheath flow rate not consistent with SOP requirements")

  # 5. Inlet flow not set properly
  inlet_flow_set <- units::set_units(0.6, L/min)
  f5 <- df |>
    filter(detector_inlet_flow_L_min != inlet_flow_set) |>
    select(sample_analysis_id) |>
    mutate(flag = "660",
           qc_outcome = 1,
           comment = "660-Detector inlet flow rate not consistent with SOP requirements")

  # 6. Impactor flow not set properly
  impactor_flow_set <- units::set_units(0.6, L/min)
  f6 <- df |>
    filter(detector_inlet_flow_L_min != inlet_flow_set) |>
    select(sample_analysis_id) |>
    mutate(flag = "660",
           qc_outcome = 1,
           comment = "660-Impactor flow rate not consistent with SOP requirements")

  # 7. Very low concentration (experimental)
  conc_threshold <- units::set_units(10, 1/cm3)
  f7 <- df |>
    filter(total_concentration_1_cm3 < conc_threshold) |>
    select(sample_analysis_id) |>
    mutate(flag = "457A",
           qc_outcome = 3,
           comment = "457A-Extremely low number concentration")

  # Combine all flags
  # f2 and f3 currently inactive
  flags <- bind_rows(f1, f2, f3, f4, f5, f6, f7) |>
    summarise(qc_outcome = max(qc_outcome),
              flag = paste(sort(unique(flag)), collapse = ":"),
              comment = paste(comment, collapse = "  :  "),
              .by = sample_analysis_id)

  df <- df |>
    left_join(flags, by = "sample_analysis_id") |>
    mutate(qc_outcome = if_else(is.na(qc_outcome), 1, qc_outcome))


}


#' Title
#'
#' @param site
#' @param start_dt
#' @param end_dt
#' @param con
#' @param version
#'
#' @returns
#' @export
#'
#' @examples
smps_l1_metadata <- function(site, start_dt, end_dt, level = "1a", con) {

  # basic metadata
  basic <- basic_metadata(site, "SMPS", start_dt, end_dt, level = level, con = con)

  # field definitions
  template <- switch(level,
                     "1a" = "smps_l1a_field_descriptions.txt",
                     "1b" = "smps_l1b_field_descriptions.txt")
  fields_path <- system.file(template, package="ascentr")
  fields <- paste(readLines(fields_path), collapse = "\n")

  # metadata from instrument settings
  settings <- smps_settings(site, start_dt, end_dt, con) |>
    mutate(line = paste0(name, ": ", value, "    ", start_date))
  setting_desc <- paste(settings$line, collapse = "\n")

  glue::glue("{basic}\n",
             "\n",
             "Field Descriptions\n",
             "{fields}\n",
             "\n",
             "Instrument metadata\n",
             "(Parameter: value    effective datetime (UTC))\n",
             "{setting_desc}")

}
