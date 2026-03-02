# SMPS L2 data - hourly and validated for delivery
# Initial version uses L1b files as input. Later versions will be built from database.

smps_l2_from_files <- function(l1b_file, manual_qc_file) {

  # SMPS specific flags
  available_flags <- tibble(manual_flag = c("111", "686", "683", "458A", "659"),
                            manual_qc_outcome = c(1, 9, 9, 1, 4))

  available_flags <- bind_rows(available_flags, common_manual_flags) |>
    distinct()
  
  l1b <- readr::read_csv(l1b_file)
  qc <- readr::read_csv(manual_qc_file)
  
  
  qc <- qc |>
    mutate(flag = as.character(flag)) |>
    rename(manual_flag=flag, manual_comment=comment)

  df <- l1b |>
    left_join(qc, by = join_by(between(sample_datetime_utc,
                                       sample_datetime_UTC_start,
                                       sample_datetime_UTC_end))) |>
    left_join(available_flags, by = "manual_flag") |>
    mutate(flag = as.character(flag))

  # Coalesce flags and comments and calculate the base hour
  df <- df |>
    mutate(final_flag = if_else(is.na(manual_flag), flag,
                                if_else(is.na(flag), manual_flag,
                                        paste(manual_flag, flag, sep = ":"))),
           final_qc_outcome = if_else(is.na(manual_qc_outcome), qc_outcome,
                                      pmax(qc_outcome, manual_qc_outcome)),
           final_comment = if_else(is.na(manual_comment), comment,
                                   if_else(is.na(comment), manual_comment,
                                           paste(manual_comment, comment, sep = " : ")))) |>
    mutate(sample_hour_utc = lubridate::floor_date(sample_datetime_utc, "1 hour"),
           .after = site_code)
  
  df <- df |>
    select(!(qc_outcome:manual_qc_outcome)) |>
    rename(flag=final_flag, qc_outcome=final_qc_outcome, comment=final_comment)

  calc_mean_scan <- function(x) {
    purrr::map(x, \(x) as_tibble(yyjsonr::read_json_str(x))) |>
      purrr::list_rbind() |>
      summarise(across(everything(), mean))
  }

  calc_n_conc <- function(midpoints, vals) {
    dNdlogDp <- vals
    dlogDp <- calc_dlogDp(midpoints)
    dN <- dNdlogDp * dlogDp
    N <- sum(dN)
  }

  calc_v_conc <- function(midpoints, vals) {
    dNdlogDp <- vals
    dlogDp <- calc_dlogDp(midpoints)
    vol_convert <- (pi / 6) * (midpoints / 1000)^3
    dVdlogDp <-  dNdlogDp * vol_convert #um3/cm3
    dV <- dVdlogDp * dlogDp
    V <- sum(dV)

  }

  # These are from SO
  weighted.geomean <- function(x, w, ...) {
    exp(weighted.mean(log(x), w, ...))
  }

  weighted.median <- function(x, w) {
    w <- w[order(x)]
    x <- x[order(x)]

    prob <- cumsum(w)/sum(w)
    ps <- which(abs(prob - .5) == min(abs(prob - .5)))
    return(x[ps])
  }

  # From https://en.wikipedia.org/wiki/Geometric_standard_deviation
  calc_geosd <- function(x, n, geo_mean) {
    inner_term <- log(x / geo_mean)^2
    count <- sum(n)
    all_values <- inner_term * n
    exp(sqrt(sum(all_values) / count))
  }

  # SMPS sampling is every 2.5 minutes - 24 samples per hour
  # Require 12 samples for a valid hourly measurement
  samples_required <- 12
  hourly_counts <- df |>
    mutate(valid = if_else(qc_outcome < 4, "valid", "invalid")) |>
    summarise(count = n(),
              .by = c(sample_hour_utc, valid)) |>
    tidyr::pivot_wider(names_from = valid, values_from = count)
  
  valid_hours <- hourly_counts |>
    filter(valid >= samples_required)
  invalid_hours <- setdiff(hourly_counts, valid_hours)
  
  # Process scan statistics by hour for valid samples
  hour_scans <- df |>
    filter(sample_hour_utc %in% valid_hours$sample_hour_utc) |>
    group_by(sample_hour_utc) |>
    summarise(mean_scan = calc_mean_scan(concentration_json),
              mean_raw_scan = calc_mean_scan(raw_concentration_json))
  
  # Put columns in numerical order and replace NA w/ zero, then calculate stats
  hour_stats <- hour_scans |>
    select(-mean_raw_scan) |>
    tidyr::unnest(cols = mean_scan) |>
    tidyr::pivot_longer(-sample_hour_utc) |>
    mutate(name = as.numeric(name)) |>
    arrange(name) |>
    tidyr::replace_na(list(value = 0)) |>
    summarise(total_concentration_1_cm3 = calc_n_conc(name, value),
              volume_concentration_um3_cm3 = calc_v_conc(name, value),
              mean_nm = weighted.mean(name, value),
              geo_mean_nm = weighted.geomean(name, value),
              median_nm = weighted.median(name, value),
              mode_nm = name[which.max(value)],
              geo_std_dev = calc_geosd(name, value, geo_mean_nm),
              .by = sample_hour_utc)

  
  # Take all the flag/comment strings apart and put them back together with unique flags only
  recompose_flags <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return(NA)
    } else {
      y <- strsplit(x, ":", fixed = TRUE) |>
        purrr::list_c() |>
        unique() |>
        sort()
      if (length(y) > 1) {
        paste(y, collapse = ":")
      } else {
        return(y)
      }
    }
  }
  
  # rejoin with flags and other info
  flags_hourly_valid <- df |>
    filter(qc_outcome < 4) |>
    filter(sample_hour_utc %in% valid_hours$sample_hour_utc) |>
    select(site_number, site_code, sample_hour_utc, stp_factor, qc_outcome, flag, comment) |>
    summarise(site_number = first(site_number),
              site_code = first(site_code),
              stp_factor = mean(stp_factor, na.rm = TRUE),
              qc_outcome = max(qc_outcome),
              flag = recompose_flags(flag),
              comment =recompose_flags(comment),
              .by = sample_hour_utc)

  df_valid <- flags_hourly_valid |>
    left_join(select(valid_hours, sample_hour_utc, sample_count=valid), 
              by = "sample_hour_utc") |>
    left_join(hour_stats, by = "sample_hour_utc") |>
    mutate(number_concentration_stp_1_cm3 = total_concentration_1_cm3 * stp_factor,
           volume_concentration_stp_um3_cm3 = volume_concentration_um3_cm3 * stp_factor) |>
    left_join(hour_scans, by = "sample_hour_utc") 

  # Before reattaching json null any values outside of the sampling range
  df_valid <- df_valid |>
    rowwise() |>
    mutate(concentration_json = yyjsonr::write_json_str(mean_scan),
           raw_concentration_json = yyjsonr::write_json_str(mean_raw_scan),
           .keep = "unused")

  # Get the flags and associated data for the invalid time periods, which will be filled with nulls
  flags_hourly_invalid <- df |>
    filter(sample_hour_utc %in% invalid_hours$sample_hour_utc) |>
    select(site_number, site_code, sample_hour_utc, stp_factor, qc_outcome, flag, comment) |>
    summarise(site_number = first(site_number),
              site_code = first(site_code),
              stp_factor = mean(stp_factor, na.rm = TRUE),
              qc_outcome = max(qc_outcome),
              flag = recompose_flags(flag),
              comment = recompose_flags(comment),
              .by = sample_hour_utc)

  if (nrow(flags_hourly_invalid) > 0) {
    result <- bind_rows(df_valid, flags_hourly_invalid) |>
      arrange(sample_hour_utc) |>
      rename(sample_datetime_UTC=sample_hour_utc)  
  } else {
    result <- df_valid |>
      arrange(sample_hour_utc) |>
      rename(sample_datetime_UTC=sample_hour_utc) 
  }

  
  
  
}
