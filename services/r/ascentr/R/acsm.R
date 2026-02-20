
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
  if (is.null(metadata_fields)) {
    if (level == "1a") {
      metadata_fields <- acsm_l1a(site, start_dt, end_dt, con)$mdf
    } else if (level == "1b") {
      metadata_fields <- acsm_l1b(site, start_dt, end_dt, con)$mdf
    } else if (level == "2") {
      l1b <- acsm_l1b(site, start_dt, end_dt, con)
      metadata_fields <- acsm_l2(l1b)$mdf
    }

  }

  m <- metadata_fields |>
    mutate(field_text = paste0(export_fieldname, ": ", export_description))
  field_descriptions <- paste(m$field_text, collapse = "\n")

  glue::glue("{basic}\n",
             "\n",
             "Field Descriptions\n",
             "{field_descriptions}")

}


acsm_autoqc <- function(site, start_dt, end_dt, con) {

  l1a <- acsm_l1a(site, start_dt, end_dt, con)
  df <- l1a$df

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

  # Igor time format is seconds since 1904-01-01 (!?)
  res <- res |>
    mutate(igor_time = as.numeric(difftime(stop_date,
                                           as.Date("1904-01-01", tz = "UTC"),
                                           units = "secs")))

}





#### Not ready yet
acsm_l1b <- function(site, start_dt, end_dt, con) {

  # load acsm data
  dfa <- acsm_l1a_df(site, start_dt, end_dt, con) |>
    rename(sample_datetime_UTC=start_date,
           sample_datetime_end_UTC=stop_date) |>
    select(-year, -start_doy, -stop_doy)

  # load dryerstats
  dfd <- dryerstats_df(site, start_dt, end_dt, con) |>
    select(-site_code, -site_number) |>
    rename(sample_analysis_id_ds=id, site_record_id_ds=site_record_id, datetime_ds=datetime)

  # join on time
  df_sample_analysis <- dfa |>
    select(sample_analysis_id, sample_datetime_UTC)

  # Collapse dryerstats data by sample_analysis_id - using median in case of outliers
  ds_med <- dfd |>
    left_join(df_sample_analysis, by = join_by(closest(datetime_ds > sample_datetime_UTC))) |>
    summarise(across(inlet_p:t_3, ~median(.x, na.rm = TRUE)),
              dryerstats_n = n(),
              .by = sample_analysis_id) |>
    filter(!is.na(sample_analysis_id))

  df <- dfa |>
    left_join(ds_med, by = "sample_analysis_id")

  # Apply flagging per https://docs.google.com/spreadsheets/d/1WMkRYh-2f-9awSBperRdCdSD4U0mv_AADarpIGQOMF4
  # These may be written to database

  # 1. If interlock or status are not 0
  f1 <- df |>
    filter(interlock != 0 | status != 0) |>
    select(sample_analysis_id) |>
    mutate(flag = 659,
           qc_outcome = 4,
           comment = "659-Status or interlock error")

  # 2. Dryer RH_out greater than Dryer RH_in
  # Inactive until dryer stats can be trusted
  # f2 <- df |>
  #   filter(rh_dry > rh_in) |>
  #   select(sample_analysis_id) |>
  #   mutate(flag = 699,
  #          qc_outcome = 4,
  #          comment = "Dryer RH out greater than dryer RH in")

  # # 3. RH dry not below 40% - flag 640
  # f3 <- df |>
  #   filter(rh_dry >= 40) |>
  #   select(sample_analysis_id) |>
  #   mutate(flag = 699,
  #          qc_outcome = 4,
  #          comment = "RH Dry not below 40%")

  # 4. Inlet pressure Not within 4.0-4.8 mbar

  ## TODO: These ranges should depend on orifice size, which is not the same at all sites
  f4 <- df |>
    filter(press_inlet < 4 | press_inlet > 4.8) |>
    select(sample_analysis_id) |>
    mutate(flag = 659,
           qc_outcome = 4,
           comment = "659-Inlet pressure not within specifications")

  # 5. AB not within 30% of reference - AB correction potentially invalid.
  f5 <- df |>
    filter(ab_total < (0.7 * abref) | ab_total > (1.3 * abref)) |>
    select(sample_analysis_id) |>
    mutate(flag = 659,
           qc_outcome = 4,
           comment = "659-Default airbeam not within 30% of reference. Correction potentially invalid")

  # 6. Heater Temp Not within 575-625 C
  f6 <- df |>
    filter(heater_t < 575 | heater_t > 625) |>
    select(sample_analysis_id) |>
    mutate(flag = 659,
           qc_outcome = 4,
           comment = "659-Heater temperature not within 575 - 625 C")

  # 7. Non-positive filament emission
  f7 <- df |>
    filter(filament_emm <= 0) |>
    select(sample_analysis_id) |>
    mutate(flag = 659,
           qc_outcome = 4,
           comment = "659-Filament out")

  # Combine all flags
  # f2 and f3 currently inactive
  flags <- bind_rows(f1, f4, f5, f6, f7) |>
    summarise(qc_outcome = max(qc_outcome),
              comment = paste(comment, collapse = "  :  "),
              flag = paste(sort(unique(flag)), collapse = ":"),
              .by = sample_analysis_id)

  df <- df |>
    left_join(flags, by = "sample_analysis_id") |>
    mutate(qc_outcome = if_else(is.na(qc_outcome), 1, qc_outcome))



  # Now prepare output

  # Need to add some custom units if they don't already exist
  try_units <- purrr::possibly(units::set_units)
  unit_check <- try_units(1, ions)
  if (is.null(unit_check)) {
    units::install_unit("ions", def = "unitless")
  }

  # Make sure degrees and percents aren't converted to symbols
  units::units_options(auto_convert_names_to_symbols = FALSE)

  params <- tbl(con, I("acsm.params")) |>
    collect()

  # Make df unit aware and convert to the desired units
  df_fields <- tibble(param = colnames(df)) |>
    left_join(params, by = "param")
  dfu <- purrr::map(colnames(df), \(x) attach_units(x, df, df_fields)) |>
    purrr::list_cbind()

  # Pressures to Pa
  dfu <- dfu |>
    mutate(across(c(press_ioniser, press_inlet, inlet_p, counter_p, p_drop),
                  ~units::set_units(.x, Pa)))

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
  common <- common_fields("1b") |>
    mutate(common_export_description = if_else(is.na(unit), description,
                                               paste0(description, " (", unit, ")"))) |>
    select(param, common_export_description)
  acsm_extra <- tibble(param = c("sample_datetime_end_UTC", "dryerstats_n"),
                       common_export_description = c("End time of sample (UTC)",
                                                     "Number of DryerStats records"))
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

# For now, takes l1b data and makes it hourly
acsm_l2 <- function(acsm_l1b_data) {

  df <- acsm_l1b_data$df

  # Average to hourly resolution
  df <- df |>
    mutate(sample_datetime_UTC = lubridate::floor_date(sample_datetime_UTC, unit = "hour")) |>
    summarise(site_record_id_start = min(site_record_id),
              site_record_id_end = max(site_record_id),
              samples = n(),
              status = max(status),
              site_number = first(site_number),
              site_code = first(site_code),
              across(chl_ug_m3:t_3_degC, median),
              dryerstats_n = sum(dryerstats_n),
              qc_outcome = max(qc_outcome),
              flag = clean_paste(sort(unique(flag)), collapse = ":"),
              comment = clean_paste(unique(comment), collapse = "  :  "),
              .by = sample_datetime_UTC)

  # Update metadata fields
  # Lose: sample_analysis_id, site_record_id, sample_datetime_end_UTC
  # Gain: site_record_id_start, site_record_id_end, samples
  mdf <- acsm_l1b_data$mdf |>
    filter(!param %in% c("sample_analysis_id", "site_record_id", "sample_datetime_end_UTC"))

  added_records <- tibble(param = c("site_record_id_start", "site_record_id_end", "samples"),
                        export_fieldname = c("site_record_id_start", "site_record_id_end", "samples"),
                        export_description = c("Id that identifies the first raw measurement in the respective table of the local site database",
                                               "Id that identifies the last raw measurement in the respective table of the local site database",
                                               "Number of measurements aggregated to create this record"))

  mdf <- bind_rows(added_records, mdf)
  return(list(df=df, mdf=mdf))

}


