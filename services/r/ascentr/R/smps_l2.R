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

  #TODO: Remove invalids from manual and auto qc
browser()
  # Add enddates to qc
  qc <- qc |>
    mutate(sample_datetime_UTC_end = if_else(is.na(sample_datetime_UTC_end),
                                             sample_datetime_UTC_start,
                                             sample_datetime_UTC_end)) |>
    rename(manual_flag=flag, manual_comment=comment)

  df <- l1b |>
    left_join(qc, by = join_by(between(sample_datetime_utc, sample_datetime_UTC_start,
                                       sample_datetime_UTC_end)))

  # Process scan statistics by hour - will need to do this after removing invalids
  hour_scans <- df |>
    mutate(sample_hour_utc = lubridate::floor_date(sample_datetime_utc, "1 hour")) |>
    group_by(sample_hour_utc) |>
    summarise(mean_scan = calc_mean_scan(concentration_json))
browser()
  # Put columns in numerical order and replace NA w/ zero
  hour_stats <- hour_scans |>
    tidyr::unnest(cols = mean_scan) |>
    tidyr::pivot_longer(-sample_hour_utc) |>
    mutate(name = as.numeric(name)) |>
    arrange(name) |>
    tidyr::replace_na(list(value = 0)) |>
    summarise(number_conc = calc_n_conc(name, value),
              volume_conc = calc_v_conc(name, value),
              mean = weighted.mean(name, value),
              geo_mean = weighted.geomean(name, value),
              median = weighted.median(name, value),
              mode = name[which.max(value)],
              geo_sd = calc_geosd(name, value, geo_mean),
              .by = sample_hour_utc)


  ### Below here is old and probably wrong



  # Average to hourly
  hourly <- df |>
    mutate(sample_hour_utc = lubridate::floor_date(sample_datetime_utc, "1 hour")) |>
    group_by(sample_hour_utc) |>
    summarise(across(c(site_number, site_code), first),
              samples = n(),
              across(detector_inlet_flow_L_min:upper_size_nm, mean),
              across(median_nm:volume_concentration_stp_um3_cm3, mean),
              qc_outcome = max(qc_outcome),
              flag = paste(flag, sep = "-"),
              comment = paste(comment, sep = "-")
              ) |>
    ungroup()


}
