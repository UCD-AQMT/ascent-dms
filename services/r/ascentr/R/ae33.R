# starting with a simple wide format that looks like the influx output - will edit with
# feedback
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
ae33_l1a <- function(site, start_dt, end_dt, con) {

  df <- ae33_raw(site, start_dt, end_dt, con)

  if (is.null(df)) {
    warning("No AE33 data for ", site, " ", start_dt, " - ", end_dt)
    return(NULL)
  }

  # Assign units (based on what comes from the ae33_data_logger)

  # Make sure degrees and percents aren't converted to symbols
  units::units_options(auto_convert_names_to_symbols = FALSE)

  df <- df |>
    mutate(across(starts_with("EBC"), ~units::set_units(.x, μg/m3))) |>
    mutate(across(c("T_LED", "Tcntrl", "Tsupply"), ~units::set_units(.x, degC))) |>
    mutate(across(starts_with("flow"), ~units::set_units(.x, L/min))) |>
    mutate(reftemp = units::set_units(reftemp, K),
           BB = units::set_units(BB, percent),
           refpress = units::set_units(refpress, hPa))

  # Need to convert some units to ASCENT standard (ng/m3, Pa, C, LPM)
  df <- df |>
    mutate(across(starts_with("EBC"), ~units::set_units(.x, ng/m3))) |>
    mutate(refpress = units::set_units(refpress, Pa),
           reftemp = units::set_units(reftemp, degC))

  # Attach site info - hardcoded here until ae33 goes to postgres, which will require a
  # complete rewrite anyway
  sites <- tibble(site_code = c("LookRock", "Test", "DeltaJunction", "CheekaPeak",
                                "LosAngeles", "Rubidoux", "JoshuaTree", "Yellowstone",
                                "LaCasa", "Houston", "Lawrenceville", "QueensCollege",
                                "SouthDeKalb"),
                  site_number = c(12, 99, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
  site <- filter(sites, site_code == site)

  # Also add the fields that we will have (sample_analysis_id, site_record_id)
  df <- df |>
    mutate(site_number = site$site_number,
           site_code = site$site_code,
           sample_analysis_id = NA,
           site_record_id = NA)

  # arrange and name all fields -  Ignore unsupported EBAS flags
  df <- df |>
    select(site_number, site_code, sample_datetime_UTC=time, status=STinst,
           cont_status=STcnt, detect_status=STdet, led_status=STled, valve_status=STvalv,
           cont_temp=Tcntrl, supply_temp=Tsupply, pressure=refpress, temperature=reftemp,
           flow1, flow2, flowc=flowC, tape_adv_count=tpcnt,
           ref_1, ref_2, ref_3, ref_4, ref_5, ref_6, ref_7,
           sens1_1, sens1_2, sens1_3, sens1_4, sens1_5, sens1_6, sens1_7,
           sens2_1, sens2_2, sens2_3, sens2_4, sens2_5, sens2_6, sens2_7,
           bc_1_STP=EBC_1, bc_2_STP=EBC_2, bc_3_STP=EBC_3, bc_4_STP=EBC_4, bc_5_STP=EBC_5,
           bc_6_STP=EBC_6, bc_7_STP=EBC_7, bc1_1_STP=EBC1_1, bc1_2_STP=EBC1_2,
           bc1_3_STP=EBC1_3, bc1_4_STP=EBC1_4, bc1_5_STP=EBC1_5, bc1_6_STP=EBC1_6,
           bc1_7_STP=EBC1_7, bc2_1_STP=EBC2_1, bc2_2_STP=EBC2_2, bc2_3_STP=EBC2_3,
           bc2_4_STP=EBC2_4, bc2_5_STP=EBC2_5, bc2_6_STP=EBC2_6, bc2_7_STP=EBC2_7,
           k_1, k_2, k_3, k_4, k_5, k_6, k_7,
           bb=BB,
           att1_1, att1_2, att1_3, att1_4, att1_5, att1_6, att1_7,
           att2_1, att2_2, att2_3, att2_4, att2_5, att2_6, att2_7,
           #c001=C001, e110=E110, e559=E559, e640=E640, e980=E980, e999=E999,
           sample_analysis_id, site_record_id
           )

  # Add units to field names
  unit_suffix <- purrr::map(df, get_unit_suffix)
  unit_parens <- purrr::map(df, get_unit_paren)
  new_colnames <- paste0(colnames(df), unit_suffix)
  colnames(df) <- new_colnames

  # Convert to reasonable significant digits
  df <- df |>
    mutate(across(where(is.numeric) & !c(sample_analysis_id, site_record_id),
                  \(x) signif(x, digits = 6)))

  df


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
ae33_l1_metadata <- function(site, start_dt, end_dt, level = "1a", con) {

  # basic metadata
  basic <- basic_metadata(site, "AE33", start_dt, end_dt, level = level, con = con)

  channels <- tibble(channel = 1:7,
                     wavelength = c(370, 470, 520, 590, 660, 880, 950),
                     MAC = c(18.47, 14.54, 13.14, 11.58, 10.35, 7.77, 7.19)) |>
    tidyr::unite("line", channel, wavelength, MAC, sep = ", ")
  channels <- paste(channels$line, collapse = "\n")

  # field definitions
  template <- switch(level,
                     "1a" = "ae33_l1a_field_descriptions.txt",
                     "1b" = "ae33_l1b_field_descriptions.txt")
  fields_path <- system.file(template, package="ascentr")
  fields <- paste(readLines(fields_path), collapse = "\n")

  glue::glue("{basic}\n",
             "Channel #, Measurement wavelength (nm), Mass absorption cross-section (m2/g):\n",
             "{channels}\n",
             "\n",
             "Field Descriptions\n",
             "{fields}")

}

# Level 0 for AE33 is the dat format used by the Magee CAAT software
ae33_l0 <- function(site, start_dt, end_dt, con) {

  df <- ae33_raw(site, start_dt, end_dt, con)

  # split date and time into two fields, plus another field for timebase (always 60?)
  df <- df |>
    mutate(Date = format(time, "%Y/%m/%d"),
           Time = format(time, "%T"),
           Timebase = 60)

  # Valve status seems to be a five digit record in the dat output
  df <- df |>
    mutate(STvalv = formatC(STvalv, width = 5, format = "d", flag = "0"))

  # Convert int64 to int
  classes <- sapply(df, class)
  df <- df |>
    mutate(across(which(classes == "integer64"), as.integer))

  # ACTRIS applied unit conversions to get to SI units (I guess). Need to
  # convert them back for the CAAT software expectations.
  # All EBC values ug -> ng
  # pressure hPa -> Pa
  # temperature K -> C
  # flows L -> ml
  df <- df |>
    mutate(across(starts_with("EBC"), ~ .x * 1000)) |>
    mutate(refpress = refpress * 100,
           reftemp = reftemp - 273.15) |>
    mutate(across(starts_with("flow"), ~ .x * 1000))

  # Need to arrange exactly as file
  df <- df |>
    select(Date, Time, Timebase,
           ref_1, sens1_1, sens2_1,
           ref_2, sens1_2, sens2_2,
           ref_3, sens1_3, sens2_3,
           ref_4, sens1_4, sens2_4,
           ref_5, sens1_5, sens2_5,
           ref_6, sens1_6, sens2_6,
           ref_7, sens1_7, sens2_7,
           flow1, flow2, flowC, refpress, reftemp,
           BB, Tcntrl, Tsupply,
           STinst, STcnt, STdet, STled, STvalv, T_LED,
           EBC1_1, EBC2_1, EBC_1,
           EBC1_2, EBC2_2, EBC_2,
           EBC1_3, EBC2_3, EBC_3,
           EBC1_4, EBC2_4, EBC_4,
           EBC1_5, EBC2_5, EBC_5,
           EBC1_6, EBC2_6, EBC_6,
           EBC1_7, EBC2_7, EBC_7,
           k_1, k_2, k_3, k_4, k_5, k_6, k_7,
           tpcnt)

  # Make space delimited lines
  lines <- purrr::map(1:nrow(df), \(x) paste(df[x, ], collapse = " "))

  sn <- ae33_serial_number(site)

  metadata <- glue::glue("AETHALOMETER\n",
                         "Serial number = {sn}\n",
                         "Application version = 1.7.0.0\n",
                         "Number of channels = 7\n\n")

  header <- "Date(yyyy/MM/dd); Time(hh:mm:ss); Timebase; RefCh1; Sen1Ch1; Sen2Ch1; RefCh2; Sen1Ch2; Sen2Ch2; RefCh3; Sen1Ch3; Sen2Ch3; RefCh4; Sen1Ch4; Sen2Ch4; RefCh5; Sen1Ch5; Sen2Ch5; RefCh6; Sen1Ch6; Sen2Ch6; RefCh7; Sen1Ch7; Sen2Ch7; Flow1; Flow2; FlowC; Pressure(Pa); Temperature(°C); BB(%); ContTemp; SupplyTemp; Status; ContStatus; DetectStatus; LedStatus; ValveStatus; LedTemp; BC11; BC12; BC1; BC21; BC22; BC2; BC31; BC32; BC3; BC41; BC42; BC4; BC51; BC52; BC5; BC61; BC62; BC6; BC71; BC72; BC7; K1; K2; K3; K4; K5; K6; K7; TapeAdvCount;\n\n"

  c(metadata, header, lines)


}


ae33_raw <- function(site, start_dt, end_dt, client) {

  flux_query <- glue::glue('from(bucket: "measurements") |> ',
                           'range(start: {start_dt}T00:00:00Z,',
                           'stop: {end_dt}T23:59:59Z) |> ',
                           'filter(fn: (r) => r._measurement == "ae33_{site}_raw") |>',
                           'drop(columns: ["_start", "_stop"])')
  df <- client$query(flux_query)

  if (is.null(df)) {
    warning("No AE33 data for ", site, " ", start_dt, " - ", end_dt)
    return(NULL)
  }

  # Create a wide format (because _value field may be of different data type)
  process_frame <- function(df) {
    param <- df$`_field`[1]
    df <- df |>
      select(time, `_value`)
    colnames(df)[2] <- param
    df
  }
  reformed <- purrr::map(df, process_frame) |>
    purrr::reduce(full_join, by = "time")

}

# Until we get into postgres
ae33_serial_number <- function(site) {
  tibble::tibble(
    site_code = c("DeltaJunction", "CheekaPeak", "LosAngeles", "Rubidoux",
                  "JoshuaTree", "Yellowstone", "LaCasa", "Houston",
                  "Lawrenceville", "QueensCollege", "SouthDeKalb", "LookRock"),
    serial_number = c("AE33-S10-01278", "AE33-S10-01288", "AE33-S10-01281", "AE33-S10-01276",
                      "AE33-S10-01275", "AE33-S10-01284", "AE33-S10-01283", "AE33-S10-01277",
                      "AE33-S10-01286", "AE33-S10-01287", "AE33-S10-01282", "AE33-S10-01322")
  ) |>
    filter(site_code == site) |>
    pull(serial_number)
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
ae33_l1b <- function(site, start_dt, end_dt, con) {

  df <- ae33_l1a(site, start_dt, end_dt, con)

  if (is.null(df)) {
    return(NULL)
  }
  
  
  # Make sure data are arranged
  df <- arrange(df, sample_datetime_UTC)
  
  # Originally, AE33's were set to the incorrect C value. Each site changed at a different
  # time. Change is stored in this data file
  c_change_path <- system.file("ae33_c_value_change_date.csv", package="ascentr")
  c_change <- read.csv(c_change_path, stringsAsFactors = FALSE) |>
    mutate(c_value_change_date = as.POSIXct(c_value_change_date, tz = "UTC")) |>
    filter(site_code == site) |>
    pull(c_value_change_date)
  
  correction_factor <- 1.57 / 1.39
  
  # If change happened after start_dt, at least some must be updated
  if (c_change > start_dt) {
    # If change is after end_dt, all of it needs to be updated, otherwise partial
    if (c_change > end_dt) {
      print("Data prior to C value update - applying correction")
      df <- df |>
        mutate(across(starts_with("bc"), ~.x * correction_factor))
    } else {
      print("Partial data prior to C value update - applying correction to earlier data")
      df_pre <- df |>
        filter(sample_datetime_UTC < c_change)
      df_post <- setdiff(df, df_pre)
      df_pre <- df_pre |>
        mutate(across(starts_with("bc"), ~.x * correction_factor))
      df <- bind_rows(df_pre, df_post)
    }
  }

  # Auto-qc


  # parse the statuses into flags.
  statuses <- df |>
    select(sample_datetime_UTC, status)

  status_flags <- purrr::map2(statuses$sample_datetime_UTC, statuses$status,
                              ae33_status_to_flags) |>
    purrr::list_rbind()

  df <- df |>
    left_join(status_flags, by = "sample_datetime_UTC")

}

# Convert the AE33 STinst field from decimal to bits and return the statuses. These are
# described in the AE33 user's manual ver 1.59, page 53
ae33_status_to_flags <- function(dt, status) {

  flag <- character()
  qc_outcome <- numeric()
  comment <- character()

  # Have to convert to int first because influxdb returns an int64 which is not properly
  # converted by intToBits
  bits <- as.integer(intToBits(as.integer(status)))

  # The mapping to specific ebas flags, outcomes and comments may not be final here

  # Operation
  chunk <- paste0(bits[1:2], collapse = "")
  if (chunk == "11") {
    flag <- c(flag, "659")
    qc_outcome <- c(qc_outcome, 4)
    comment <- c(comment, "659-Instrument stopped")
  }
  if (chunk == "01") {
    flag <- c(flag, "999")
    qc_outcome <- c(qc_outcome, 9)
    comment <- c(comment, "999-Tape advance (fast cal., warm-up)")
  }
  if (chunk == "10") {
    flag <- c(flag, "999")
    qc_outcome <- c(qc_outcome, 9)
    comment <- c(comment, "999-First measurement - obtaining ATN0")
  }

  # Flow
  chunk <- paste0(bits[3:4], collapse = "")
  if (chunk != "00") {
    flag <- c(flag, "660")
    qc_outcome <- c(qc_outcome, 3)
    comment <- c(comment, "660-Verify operation and make final determination of data quality. Flow calibration may be needed.")
  }

  # Optical
  chunk <- paste0(bits[5:6], collapse = "")
  if (chunk == "11") {
    flag <- c(flag, "659")
    qc_outcome <- c(qc_outcome, 4)
    comment <- c(comment, "659-Calibration error, all channels")
  } else if (chunk %in% c("01", "10")) {
    flag <- c(flag, "660")
    qc_outcome <- c(qc_outcome, 3)
    comment <- c(comment, "660-Verify LED performance")
  }

  # Chamber
  chunk <- as.character(bits[7])
  if (chunk == "1") {
    flag <- c(flag, "659")
    qc_outcome <- c(qc_outcome, 4)
    comment <- c(comment, "659-Chamber error")
  }

  # Tape
  chunk <- paste0(bits[8:9], collapse = "")
  if (chunk == "11") {
    flag <- c(flag, "659")
    qc_outcome <- c(qc_outcome, 4)
    comment <- c(comment, "659-Instrument stopped - no tape")
  }

  # Setup
  chunk <- as.character(bits[10])
  if (chunk == "1") {
    flag <- c(flag, "659")
    qc_outcome <- c(qc_outcome, 4)
    comment <- c(comment, "659-Setup error")
  }

  # Tests
  chunk <- paste0(bits[11:13], collapse = "")
  if (chunk == "010") {
    flag <- c(flag, "686")
    qc_outcome <- c(qc_outcome, 9)
    comment <- c(comment, "686-Clean air test")
  } else if (chunk != "000") {
    flag <- c(flag, "999")
    qc_outcome <- c(qc_outcome, 9)
    comment <- c(comment, case_match(chunk,
                                     "001" ~ "999-Stability test",
                                     "011" ~ "999-Change tape procedure",
                                     "100" ~ "999-Optical test",
                                     "110" ~ "999-Leakage test",
                                     .default = "999-Unknown test"
    ))
  }

  # External device
  chunk <- as.character(bits[14])
  if (chunk == "1") {
    flag <- c(flag, "660")
    qc_outcome <- c(qc_outcome, 3)
    comment <- c(comment, "660-Connection error")
  }

  # Auto clean air test
  chunk <- as.character(bits[15])
  if (chunk == "1") {
    flag <- c(flag, "660")
    qc_outcome <- c(qc_outcome, 3)
    comment <- c(comment, "660-Failed clean air test")
  }

  # CF card failure
  chunk <- as.character(bits[16])
  if (chunk == "1") {
    flag <- c(flag, "660")
    qc_outcome <- c(qc_outcome, 3)
    comment <- c(comment, "660-Problem while saving or retrieving files to/from CF card")
  }

  # Database
  chunk <- as.character(bits[17])
  # no test needed

  # Combine all statuses
  if (length(qc_outcome) == 0) {
    df <- tibble(sample_datetime_UTC = dt,
                 flag = NA_character_,
                 qc_outcome = 1,
                 comment = NA_character_)
  } else {
    df <- tibble(sample_datetime_UTC = dt,
                 flag = paste(sort(unique(flag)), collapse = ":"),
                 qc_outcome = max(qc_outcome),
                 comment = paste(comment, collapse = "  :  "))
  }

  df
}

ae33_l2_from_files <- function(l1b_file, manual_qc_file) {

  # AE33 specific list
  available_flags <- tibble(manual_flag = c("111", "686", "683", "659", "644A", "659"),
                            manual_qc_outcome = c(1, 9, 9, 4, 3, 4))

  available_flags <- bind_rows(available_flags, common_manual_flags) |>
    distinct()

  # Increasing guess_max here to make sure that some tape advances are caught
  l1b <- readr::read_csv(l1b_file, guess_max = 50000)
  qc <- readr::read_csv(manual_qc_file, guess_max = 50000)

  qc <- qc |>
    mutate(flag = as.character(flag)) |>
    rename(manual_flag=flag, manual_comment=comment)

  df <- l1b |>
    left_join(qc, by = join_by(between(sample_datetime_UTC,
                                       sample_datetime_UTC_start,
                                       sample_datetime_UTC_end))) |>
    left_join(available_flags, by = "manual_flag") |>
    mutate(flag = as.character(flag))

  # Coalesce flags and comments and calculate the base hour
  df <- df |>
    coalesce_flags() |>
    mutate(sample_hour_UTC = lubridate::floor_date(sample_datetime_UTC, "1 hour"),
           .after = site_code)

  # AE33 sampling is every 1 minute - 60 samples per hour
  # Require 30 samples for a valid hourly measurement
  samples_required <- 30
  hourly_counts <- df |>
    mutate(valid = if_else(qc_outcome < 4, "valid", "invalid")) |>
    summarise(count = n(),
              .by = c(sample_hour_UTC, valid)) |>
    tidyr::pivot_wider(names_from = valid, values_from = count)
  
  valid_hours <- hourly_counts |>
    filter(valid >= samples_required)
  invalid_hours <- setdiff(hourly_counts, valid_hours)

  # Process hourly results for valid samples
  data_hourly_valid <- df |>
    filter(sample_hour_UTC %in% valid_hours$sample_hour_UTC) |> # only valid hours
    filter(qc_outcome < 4) |> # within those hours only process valid observations
    summarise(across(starts_with("bc"), ~mean(.x, na.rm = TRUE)),
              across(starts_with("k_"), ~mean(.x, na.rm = TRUE)),
              across(starts_with("att"), ~mean(.x, na.rm = TRUE)),
              .by = sample_hour_UTC)

  
  # To properly calculate BB% at hourly resolution, we use the 1-hr BC values, along with
  # MAC values to derive hourly absorption. Then use Zotter et al., 2017 Eq 13 to find
  # BC_ff/BC_tot. 
  # Assumptions:
  # MAC_ff == MAC_bb
  # Absorption Angstrom Exponent (alpha) ff = 1, bb = 2
  mac_470 <- ae33_MAC |>
    filter(wavelength == 470) |>
    pull(MAC)
  
  mac_950 <- ae33_MAC |>
    filter(wavelength == 950) |>
    pull(MAC)
  
  alpha_ff <- 1
  alpha_bb <- 2
  
  data_hourly_valid <- data_hourly_valid |>
    mutate(abs_470 = bc_2_STP_ng_m3 * mac_470,
           abs_950 = bc_7_STP_ng_m3 * mac_950,
           upper_term = 1 - (abs_950 / abs_470) * (470 / 950)^-alpha_ff,
           lower_term = 1 - (abs_950 / abs_470) * (470 / 950)^-alpha_bb,
           ff_fraction = 1 / (1 - upper_term / lower_term),
           bb_percent = (1 - ff_fraction) * 100,
           bb_percent = if_else(bb_percent > 100, 100,
                                if_else(bb_percent < 0, 0, bb_percent))) |>
    select(-abs_470, -abs_950, -upper_term, -lower_term, -ff_fraction)
  
  # rejoin with flags and other info
  flags_hourly_valid <- df |>
    filter(sample_hour_UTC %in% valid_hours$sample_hour_UTC) |>
    filter(qc_outcome < 4) |>
    select(site_number, site_code, sample_hour_UTC, qc_outcome, flag, comment) |>
    summarise(site_number = first(site_number),
              site_code = first(site_code),
              qc_outcome = max(qc_outcome),
              flag = recompose_flags(flag),
              comment =recompose_flags(comment),
              .by = sample_hour_UTC)
  
  df_valid <- flags_hourly_valid |>
    left_join(select(valid_hours, sample_hour_UTC, sample_count=valid), 
              by = "sample_hour_UTC") |>
    left_join(data_hourly_valid, by = "sample_hour_UTC")

  # Get the flags and associated data for the invalid time periods, which will be filled
  # with nulls
  # Some may be invalid because not enough samples but no bad qc_outcome. If so, downgrade
  flags_hourly_invalid <- df |>
    filter(sample_hour_UTC %in% invalid_hours$sample_hour_UTC) |>
    select(site_number, site_code, sample_hour_UTC, qc_outcome, flag, comment) |>
    summarise(site_number = first(site_number),
              site_code = first(site_code),
              qc_outcome = max(qc_outcome),
              flag = recompose_flags(flag),
              comment = recompose_flags(comment),
              .by = sample_hour_UTC) |>
    left_join(select(invalid_hours, sample_hour_UTC, sample_count=valid))
  
  if (nrow(flags_hourly_invalid) > 0) {
    flags_hourly_invalid <- flags_hourly_invalid |>
      mutate(flag = if_else(qc_outcome < 4, "391", flag),
             comment = if_else(qc_outcome < 4, "391-Data completeness less than 50%", comment),
             qc_outcome = if_else(qc_outcome < 4, 9, qc_outcome))
  }

  if (nrow(flags_hourly_invalid) > 0) {
    result <- bind_rows(df_valid, flags_hourly_invalid) |>
      arrange(sample_hour_UTC) |>
      rename(sample_datetime_UTC=sample_hour_UTC)  
  } else {
    result <- df_valid |>
      arrange(sample_hour_UTC) |>
      rename(sample_datetime_UTC=sample_hour_UTC) 
  }
  
  # Rearrange to final field order
  result <- result |>
    select(sample_datetime_UTC, site_number, site_code, sample_count:bb_percent,
           qc_outcome, flag, comment) |>
    mutate()

  return(result)
  
}

# Mass Absorption Cross-sections from Magee manual (ver 1.59, pg 22)
ae33_MAC <- tibble(channel = 1:7,
                   wavelength = c(370, 470, 520, 590, 660, 880, 950),
                   MAC = c(18.47, 14.54, 13.14, 11.58, 10.35, 7.77, 7.19))

