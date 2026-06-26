
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
acsm_l1a_df <- function(site, start_dt, end_dt, con) {

  acsm_sa <- tbl(con, I("acsm.sample_analysis"))
  diag_calib <- tbl(con, I("acsm.diag_calib"))
  mass_loadings <- tbl(con, I("acsm.mass_loadings"))
  tps <- tbl(con, I("acsm.tps"))
  sites <- tbl(con, I("common.sites"))

  df <- acsm_sa |>
    inner_join(select(sites, site_number, site_code),
               by = "site_number") |>
    inner_join(select(mass_loadings, -id, -site_record_id),
               by = c("id"="sample_analysis_id")) |>
    inner_join(select(diag_calib, -id, -site_record_id),
               by = c("id"="sample_analysis_id")) |>
    inner_join(select(tps, -id, -site_record_id),
               by = c("id"="sample_analysis_id")) |>
    filter(site_code == site,
           start_date >= start_dt,
           stop_date <= end_dt) |>
    arrange(start_date) |>
    collect() |>
    rename(sample_analysis_id=id)

  # Get dryer stats separately

}

dryerstats_df <- function(site, start_dt, end_dt, con) {

  dryer_stats <- tbl(con, I("acsm.dryer_stats"))
  sites <- tbl(con, I("common.sites"))

  df <- dryer_stats |>
    inner_join(select(sites, site_number, site_code),
               by = "site_number") |>
    filter(site_code == site,
           datetime >= start_dt,
           datetime < end_dt) |>
    arrange(datetime) |>
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
acsm_l1a <- function(site, start_dt, end_dt, con) {

  df <- acsm_l1a_df(site, start_dt, end_dt, con) |>
    rename(sample_datetime_UTC=start_date,
           sample_datetime_end_UTC=stop_date) |>
    select(-year, -start_doy, -stop_doy)

  params <- tbl(con, I("acsm.params")) |>
    collect()

  # Need to add some custom units if they don't already exist
  try_units <- purrr::possibly(units::set_units)
  unit_check <- try_units(1, ions)
  if (is.null(unit_check)) {
    units::install_unit("ions", def = "unitless")
  }

  # Make sure degrees and percents aren't converted to symbols
  units::units_options(auto_convert_names_to_symbols = FALSE)

  # Make df unit aware and convert to the desired units
  df_fields <- tibble(param = colnames(df)) |>
    left_join(params, by = "param")
  dfu <- purrr::map(colnames(df), \(x) attach_units(x, df, df_fields)) |>
    purrr::list_cbind()

  # Pressures to Pa
  dfu <- dfu |>
    mutate(across(c(press_ioniser, press_inlet), ~units::set_units(.x, Pa)))

  # Add units to field names
  unit_suffix <- purrr::map(dfu, get_unit_suffix)
  unit_parens <- purrr::map(dfu, get_unit_paren)
  new_colnames <- paste0(colnames(dfu), unit_suffix)

  # A few of these already have units in the name, which is awkward. Fix these here.
  replace_me <- which(new_colnames %in% c("flow_ccs_cm3_s", "ie_ionspg_ions_pg",
                                          "fore_pc_percent", "heater_v_V",
                                          "filament_v_V"))
  new_colnames <- replace(new_colnames, replace_me, c("flow_cm3_s", "ie_ions_pg",
                                                      "fore_percent", "heater_V",
                                                      "filament_V"))
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
  acsm_extra <- tibble(param = c("sample_datetime_end_UTC"),
                       common_export_description = c("End time of sample (UTC)"))
  common <- bind_rows(common, acsm_extra)

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
acsm_metadata <- function(site, start_dt, end_dt, con, metadata_fields = NULL, level) {

  if (!level %in% c("1a", "1b", "2")) {
    stop("level must be one of '1a', '1b', or '2'")
  }

  # basic metadata
  basic <- basic_metadata(site, "ACSM", start_dt, end_dt, level = level, con = con)

  # If the metadata fields aren't provided, get them
  if (level == "2") {
    # field definitions
    template <- "acsm_l2_field_descriptions.txt"
    fields_path <- system.file(template, package="ascentr")
    field_descriptions <- paste(readLines(fields_path), collapse = "\n")
  } else {
    if (is.null(metadata_fields)) {
      if (level == "1a") {
        metadata_fields <- acsm_l1a(site, start_dt, end_dt, con)$mdf
        m <- metadata_fields |>
          mutate(field_text = paste0(export_fieldname, ": ", export_description))
        field_descriptions <- paste(m$field_text, collapse = "\n")
      } else if (level == "1b") {
        metadata_fields <- acsm_l1b(site, start_dt, end_dt, con)$mdf
        m <- metadata_fields |>
          mutate(field_text = paste0(export_fieldname, ": ", export_description))
        field_descriptions <- paste(m$field_text, collapse = "\n")
      } 
    } else {
      m <- metadata_fields |>
        mutate(field_text = paste0(export_fieldname, ": ", export_description))
      field_descriptions <- paste(m$field_text, collapse = "\n")
    }
  }
  

  if (level == "2") {
    # Need statement on processing. Not sure how we'll do this in the future
    statements_path <- system.file("ACSM IE_CDCE statements.csv",
                                   package = "ascentr")
    statements <- read.csv(statements_path)
    site_num <- tbl(con, I("common.sites")) |>
      filter(site_code == site) |>
      pull(site_number)
    statements <- statements |>
      filter(site_number == site_num)
  
    ie <- statements$Revised.Statement.for.IE..RIE
    ce <- statements$Revised.Statement.for.CE
  
    stp <- paste("Data converted to ASCENT STP (0 C and 101325 Pa) assuming hydrostatic pressure (scale height of 7.4 km) and sampling temperature of 25 C.\n",
                 "STP conversion factor for site = ", round(acsm_stp(site, con), 3))
    
    precision <- paste("Reported precisions for each species are the propagated",
                       "uncertainties of Poisson's counting statistics of the",
                       "corresponding open and closed spectra, as described in",
                       "Ng et al. (AST, 2011) and Ulbrich et al. (ACP, 2009).")
    
    out <- glue::glue("{basic}\n",
                        "\n",
                        "Data Processing Details\n",
                        "{clean_paste(c(stp, precision, ie, ce), collapse = '\n')}",
                        "\n\n",
                        "Field Descriptions\n",
                        "{field_descriptions}",
                        )
    
    
  } else {
    out <- glue::glue("{basic}\n",
               "\n",
               "Field Descriptions\n",
               "{field_descriptions}")
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
acsm_l1b <- function(site, start_dt, end_dt, con) {
  
  l1a <- acsm_l1a(site, start_dt, end_dt, con)
  df <- l1a$df
  
  autoqc <- acsm_autoqc(df)
  
  res <- df |>
    left_join(autoqc, by = c("sample_datetime_end_UTC"="stop_date")) |>
    mutate(qc_outcome = if_else(is.na(qc_outcome), 1, qc_outcome))
  
  l1b_fields <- tibble(param = c("qc_outcome", "flag", "comment"),
                       export_fieldname = c("qc_outcome", "flag", "comment"),
                       export_description = c("A quality control outcome 1-Good, 2-Not evaluated/unknown, 3-Questionable/suspect, 4-Bad/Invalid, 9-Missing",
                                              "Data quality flag(s) as defined by EBAS (ebas.nilu.no)",
                                              "Comment describing flagging details"))
  
  mdf <- l1a$mdf |>
    bind_rows(l1b_fields)
  
  list(df=res, mdf=mdf)
  
}

#' Apply auto-qc checks to ACSM L1a data and return a file for external merging with ACSM
#' HDF data in Igor. This process requires adjusting the time stamps to match.
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
acsm_autoqc_file <- function(site, start_dt, end_dt, con) {
  
  l1a <- acsm_l1a(site, start_dt, end_dt, con)
  df <- l1a$df
  
  # timestamp in the text file we ingest is of by 600 s because it is the start/stop time
  # of the final sample in the full 10 minute measurement so need to correct to match with
  # data we are getting from the hdf
  df <- df |>
    mutate(sample_datetime_UTC = sample_datetime_UTC - 600,
           sample_datetime_end_UTC = sample_datetime_end_UTC - 600)
  
  res <- acsm_autoqc(df)
  
  # Igor time format is seconds since 1904-01-01 (!?)
  res <- res |>
    mutate(igor_time = as.numeric(difftime(stop_date,
                                           as.Date("1904-01-01", tz = "UTC"),
                                           units = "secs")))
  
}

#' ACSM auto-qc checks
#'
#' @param df 
#'
#' @returns
#' @export
#'
#' @examples
acsm_autoqc <- function(df) {
  
  # 10. TPS readings bad (do not consider other TPS flags when this is triggered)
  f10 <- df |>
    filter(interlock == -9999 | heater_pwm == -9999) |>
    select(sample_analysis_id, sample_datetime_end_UTC) |>
    mutate(flag = "659",
           qc_outcome = 4,
           comment = "659-TPS failure")
  
  df_invalid <- f10 |>
    select(sample_analysis_id)
  
  # 1. Heater (vaporizer) temp high
  temp_high <- units::set_units(625, degC)
  f1 <- df |>
    anti_join(df_invalid, by = "sample_analysis_id") |>
    filter(heater_t_degC > temp_high) |>
    select(sample_analysis_id, sample_datetime_end_UTC) |>
    mutate(flag = "659",
           qc_outcome = 4,
           comment = "659-High heater temperature")
  
  # 2. Heater (vaporizer) temp low
  temp_low <- units::set_units(575, degC)
  f2 <- df |>
    anti_join(df_invalid, by = "sample_analysis_id") |>
    filter(heater_t_degC < temp_low) |>
    select(sample_analysis_id, sample_datetime_end_UTC) |>
    mutate(flag = "660",
           qc_outcome = 3,
           comment = "660-Low heater temperature")
  
  # 3. Heater current low
  current_low <- units::set_units(1, A)
  f3 <- df |>
    anti_join(df_invalid, by = "sample_analysis_id") |>
    filter(heater_i_A < current_low) |>
    select(sample_analysis_id, sample_datetime_end_UTC) |>
    mutate(flag = "659",
           qc_outcome = 4,
           comment = "659-Low heater current")
  
  # 4. Heater current high
  current_high <- units::set_units(1.2, A)
  f4 <- df |>
    anti_join(df_invalid, by = "sample_analysis_id") |>
    filter(heater_i_A > current_high) |>
    select(sample_analysis_id, sample_datetime_end_UTC) |>
    mutate(flag = "659",
           qc_outcome = 4,
           comment = "659-High heater current")
  
  # 5. Status error
  f5 <- df |>
    filter(status != 0) |>
    select(sample_analysis_id, sample_datetime_end_UTC) |>
    mutate(flag = "659",
           qc_outcome = 4,
           comment = "659-Critical operation error")
  
  # 6. Interlock error
  f6 <- df |>
    anti_join(df_invalid, by = "sample_analysis_id") |>
    filter(interlock != 0) |>
    select(sample_analysis_id, sample_datetime_end_UTC) |>
    mutate(flag = "659",
           qc_outcome = 4,
           comment = "659-Disruption in vacuum level")
  
  # 7. Low filament emission current
  filament_low <- units::set_units(0, A)
  f7 <- df |>
    anti_join(df_invalid, by = "sample_analysis_id") |>
    filter(filament_emm_A <= filament_low) |>
    select(sample_analysis_id, sample_datetime_end_UTC) |>
    mutate(flag = "659",
           qc_outcome = 4,
           comment = "659-Low filament emission current")
  
  
  res <- bind_rows(f1, f2, f3, f4, f6, f7, f10) |>
    summarise(qc_outcome = max(qc_outcome),
              comment = paste(comment, collapse = "  :  "),
              flag = paste(sort(unique(flag)), collapse = ":"),
              .by = sample_datetime_end_UTC) |>
    rename(stop_date=sample_datetime_end_UTC) |>
    arrange(stop_date)
  
}


#' Title
#'
#' @param site 
#' @param site_file 
#' @param con 
#'
#' @returns
#' @export
#'
#' @examples
acsm_l2_from_files <- function(site, site_files, con) {
  
  # ACMS specific list
  # 460A here is "interference in sulfate suspected from high organic signal, contact PI"
  # I'm making that a suspect outcome
  available_flags <- tibble(manual_flag = c("111", "686", "659", "664", "460A"),
                            manual_qc_outcome = c(1, 9, 4, 4, 3))
  
  available_flags <- bind_rows(available_flags, common_manual_flags) |>
    distinct() |>
    rename(flag=manual_flag, qc_outcome=manual_qc_outcome)
 
  # files produced by site using Igor code
  safe_read <- function(f) {
    df <- readr::read_csv(f, col_types = "nccddddddddddddddddddddddddddddddddd")
    prb <- vroom::problems(df)
    if (nrow(prb) > 0) {
      # Handle errors
      stop("Error reading file: ", f)
    }
    return(df)
  }
  
  site_df <- purrr::map(site_files, safe_read) |>
    purrr::list_rbind()
  
  # timestamp in database is off by 600 s because it is the start/stop time
  # it will not match with this
  
  # Igor time format is seconds since 1904-01-01 (!?)
  df <- site_df |>
    mutate(sample_datetime_UTC = as.POSIXct(timeW, origin = "1904-01-01"))
  
  # Get site number and elevation from db
  site_df <- tbl(con, I("common.sites")) |>
    filter(site_code == site) |>
    select(site_number, elevation) |>
    collect()
  
  site_number <- site_df$site_number
  elev <- site_df$elevation
  
  if (length(site_number) != 1) {
    stop(site, " is not a valid site_code")
  }
  
  # Add common fields
  df <- df |>
    mutate(site_code = site,
           site_number = site_number,
           flag = as.character(flag))
 
  ## Need to convert values to ASCENT STP (1 atm, 0C) assuming hydrostatic pressure from
  ## site altitude and trailer temp of 25C
  stp_fact <- acsm_stp(site, con)
  
  df <- df |>
    mutate(stp_factor = stp_fact,
           across(Org:OOA, ~.x * stp_fact))
  
  # The output includes composite flags, and some of them are overrides. For example,
  # 659:111 would mean a scan flagged as bad was changed to valid.
  df_flagged <- filter(df, !is.na(flag))
  df_unflagged <- setdiff(df, df_flagged)
  all_flags <- stringr::str_split(df_flagged$flag, pattern = ":")

  # Are there any unexpected flag values?
  flags <- unique(unlist(all_flags))
  bad_flags <- any(!flags %in% available_flags$flag)
  if (bad_flags) {
    bad <- flags[which(!flags %in% available_flags$flag)]
    msg <- paste(bad, collapse = ", ")
    stop("Unexpected flag value(s): ", msg)
  }
  
  # Flags are good, but need to take them apart - for each record with a flag, compute a
  # final qc_outcome and flag list. Generally, qc_outcome is max, but 111 overrides in
  # this case and removes all other flags.
  resolve_acsm_flags <- function(x) {
    if (length(x) == 1) {
      flag <- x
      qc_outcome <- available_flags$qc_outcome[available_flags$flag == x]
    } else {
      if (any(x == "111")) {
        flag <- "111"
        qc_outcome <- 1
      } else {
        qc_outcome <- max(available_flags$qc_outcome[available_flags$flag %in% x])
        flag <- paste0(sort(x), collapse = ":")
      }
    }
    data.frame(flag, qc_outcome)
  }
  
  flags_outcomes <- purrr::map(all_flags, resolve_acsm_flags) |>
    purrr::list_rbind()
  
  df_flagged <- df_flagged |>
    mutate(flag = flags_outcomes$flag,
           qc_outcome = flags_outcomes$qc_outcome)
  df_unflagged <- df_unflagged |>
    mutate(qc_outcome = 1)
  df <- bind_rows(df_flagged, df_unflagged) |>
    arrange(sample_datetime_UTC)
  
  # Now have the native resolution data in the necessary form and can average to hourly
  df <- df |>
    mutate(sample_hour_UTC = lubridate::floor_date(sample_datetime_UTC, "1 hour"))
  
  # ACSM sampling is every 10 minutes - 6 samples per hour
  # Require 3 samples for a valid hourly measurement
  samples_required <- 3
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
    summarise(across(c(Org, SO4, NH4, NO3, Chl), ~mean(.x, na.rm = TRUE)),
              across(starts_with("m"), ~mean(.x, na.rm = TRUE)),
              across(c(NO3_30, NO3_46, HOA, OOA), ~mean(.x, na.rm = TRUE)),
              across(ends_with("err"), ~sqrt(sum(.x^2))),
              .by = sample_hour_UTC)
  
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
  
  if (nrow(df_valid) == 0) {
    warning("No valid hours for ", site_file, "\nreturning null.")
    return(NULL)
  } 
  
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
    left_join(select(invalid_hours, sample_hour_UTC, sample_count=valid),
              by = "sample_hour_UTC")
  
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
  
  # As of June 2026, all fragment ions have to have RIE applied to them as a correction
  # This will possibly be done upstream in future igor processing
  org_rie <- 1.4
  no3_rie <- 1.05
  result <- result |>
    mutate(across(c(m29, m43, m44, m55, m57, m60, m69, m71, m73), ~ .x / org_rie),
           across(c(NO3_30, NO3_46), ~ .x / no3_rie))

  # Rearrange and rename for final export
  result <- result |>
    select(site_number, site_code, sample_datetime_UTC, sample_count, 
           organics_ug_STP_m3=Org, sulfate_STP_ug_m3=SO4, nitrate_STP_ug_m3=NO3,
           ammonium_STP_ug_m3=NH4, chloride_STP_ug_m3=Chl, 
           organics_precision_STP_ug_m3=Org_err, sulfate_precision_STP_ug_m3=SO4_err,
           nitrate_precision_STP_ug_m3=NO3_err, ammonium_precision_STP_ug_m3=NH4_err,
           chloride_precision_STP_ug_m3=Chl_err,
           org_mz29_STP_ug_m3=m29, org_mz43_STP_ug_m3=m44, org_mz44_STP_ug_m3=m44,
           org_mz55_STP_ug_m3=m55, org_mz57_STP_ug_m3=m57, org_mz60_STP_ug_m3=m60,
           org_mz69_STP_ug_m3=m69, org_mz71_STP_ug_m3=m71, org_mz73_STP_ug_m3=m73,
           no3_mz30_STP_ug_m3=NO3_30, no3_mz46_STP_ug_m3=NO3_46, hoa_STP_ug_m3=HOA,
           ooa_STP_ug_m3=OOA, qc_outcome, flag, comment)
  
}





## Need to convert values to ASCENT STP (1 atm, 0C) assuming hydrostatic pressure from
## site altitude and trailer temp of 25C
# This cannot currently be done further upstream because of the external reprocessing

# This is the hydrostatic pressure equation
# P = P0 exp(-Z/H)
# Z = altitude of site
# H = scale height = RT/Mg ~ 7.4 km
acsm_stp <- function(site, con) {
  
    # Assumed scale height of 7.4 km per Seinfeld & Pandis
    H <- 7400
    Z <- tbl(con, I("common.sites")) |>
      filter(site_code == site) |>
      pull(elevation)
    
    # Standard pressure in Pa
    P_STP <- 101325
    P_sample <- P_STP * exp(-Z/H)  

    # ASCENT STP is 0 degress C
    T_STP <- 298.15
    
    # We assume a trailer temperature of 25 degrees C
    T_sample <- 298.15 + 25
    
    stp_factor <- (P_STP * T_sample) / (P_sample * T_STP)
    
}


