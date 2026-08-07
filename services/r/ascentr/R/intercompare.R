# Formulas for generating SMPS-based mass concentrations with composition dependent density

# Assumed particle densities (g/cm^3)
DENSITY <- tibble(
  parameter = c("org", "so4", "nh4", "no3", "chl", "EBC_6"),
  density = c(1.4, 1.75, 1.75, 1.75, 1.52, 1.8)
)

# Rural sites have slower Xact data
RURAL <- c("DeltaJunction", "Yellowstone", "LookRock",
                 "CheekaPeak", "JoshuaTree")

acsm_aggregated <- function(site, start_date, end_date, con,
                            resolution_minutes = 30,
                            min_fraction = 0.5,
                            parameters = NULL) {
  
  if (resolution_minutes >= 60) {
    resolution_hours <- resolution_minutes / 60
    floor_to <- paste(resolution_hours, "hour")
  } else {
    floor_to <- paste(resolution_minutes, "min")  
  }

  # make sure to get the hours from the last day
  end_date <- as.Date(end_date) + 1
  
  acsm <- tbl(con, I("acsm.mass_loadings")) |>
    select(sample_analysis_id, org, so4, nh4, no3, chl) |>
    inner_join(select(tbl(con, I("acsm.sample_analysis")),
                      sample_analysis_id=id, sample_datetime=start_date, site_number),
               by = "sample_analysis_id") |>
    inner_join(select(tbl(con, I("common.sites")), site_code, site_number),
               by = "site_number") |>
    filter(sample_datetime >= start_date,
           sample_datetime < end_date,
           site_code == site) |>
    arrange(sample_datetime) |>
    collect()
  
  acsm <- acsm |>
    select(sample_datetime, org, so4, nh4, no3, chl) |>
    tidyr::pivot_longer(org:chl, names_to = "parameter", values_to = "value")
  
  if (!is.null(parameters)) {
    acsm <- acsm |>
      filter(parameter %in% parameters)
  }
  
  # Aggregate and filter to time based on resolution_minutes with enough cases above min_fraction
  # ACSM expects 6 samples per hr
  acsm <- acsm |>
    mutate(sample_datetime = lubridate::floor_date(sample_datetime, unit = floor_to)) |>
    summarise(value = mean(value, na.rm = TRUE),
              count = n(),
              .by = c(sample_datetime, parameter)) |>
    filter(count >= 6 * resolution_minutes / 60) |>
    select(-count)
  
}

xact_aggregated <- function(site, start_date, end_date, con,
                            resolution_hours = 4,
                            min_fraction = 0.5,
                            choice_elements = c("Al", "Si", "Cl")) {
  
  floor_to <- paste(resolution_hours, "hour")
  
  # make sure to get the hours from the last day
  end_date <- as.Date(end_date) + 1
  
  # Do not include Nb (qc) or S (double counted with ACSM)
  xact <- tbl(con, I("xact.raw_measurements")) |>
    select(sample_analysis_id, element, value) |>
    inner_join(select(tbl(con, I("xact.sample_analysis")),
                      sample_analysis_id=id, sample_datetime, site_number, sample_type),
               by = "sample_analysis_id") |>
    inner_join(select(tbl(con, I("common.sites")), site_code, site_number),
               by = "site_number") |>
    filter(sample_datetime >= start_date,
           sample_datetime < end_date,
           site_code == site,
           sample_type == 1,
           !element %in% c("Nb", "S")) |>
    arrange(sample_datetime) |>
    select(sample_datetime, element, value) |>
    collect()
  
  # Segregate the choice elements and sum the rest
  choice <- xact |>
    filter(element %in% choice_elements)
  bulk <- setdiff(xact, choice) |>
    summarise(value = sum(value, na.rm = TRUE),
              .by = sample_datetime) |>
    mutate(element = "bulk")
  xact <- bind_rows(bulk, choice)
  
  # Aggregate and filter to time based on resolution_hours with enough cases above min_fraction
  # Xact expects 1 per hour for urban and one every 4 hours for rural
  if (site %in% RURAL) {
    expected <- 0.25
  } else {
    expected <- 1
  }

  xact <- xact |>
    mutate(sample_datetime = lubridate::floor_date(sample_datetime, unit = floor_to)) |>
    summarise(value = mean(value, na.rm = TRUE),
              count = n(),
              .by = c(sample_datetime, element)) |>
    filter(count >= expected * resolution_hours) |>
    select(-count) |>
    rename(parameter = element)
  
}

smps_volume_aggregated <- function(site, start_date, end_date, con,
                                   resolution_minutes = 30,
                                   min_fraction = 0.5) {
  
  if (resolution_minutes >= 60) {
    resolution_hours <- resolution_minutes / 60
    floor_to <- paste(resolution_hours, "hour")
  } else {
    floor_to <- paste(resolution_minutes, "min")  
  }
  
  # make sure to get the hours from the last day
  end_date <- as.Date(end_date) + 1
  
  # TODO: In the future, the API should calculate volume concentration so we don't have to
  # do it here
  smps <- tbl(con, I("smps.sample_analysis")) |>
    select(id, sample_start, total_concentration, site_number, concentration_json) |>
    inner_join(select(tbl(con, I("common.sites")), site_code, site_number),
               by = "site_number") |>
    filter(sample_start >= start_date,
           sample_start < end_date,
           site_code == site) |>
    arrange(sample_start) |>
    collect()
  
  # This processing must be done per-scan, in case the bins change
  process_scan_volume <- function(record) {
    
    lst <- yyjsonr::read_json_str(record)
    midpoints <- as.numeric(names(lst))
    dNdlogDp <- data.matrix(as_tibble(lst))
    
    dlogDp <- calc_dlogDp(midpoints)
    dVdlogDp <- calc_dVdlogDp(dNdlogDp, midpoints)
    V <- calc_W(dVdlogDp, dlogDp)
    
  }
  
  V <- purrr::map_dbl(smps$concentration_json, process_scan_volume, 
                      .progress = "Calculating scan volumes")
  
  smps <- smps |>
    mutate(smps_volume = V) |>
    select(smps_id=id, sample_datetime=sample_start, smps_volume)
  
  # Aggregate and filter to time based on resolution_minutes with enough cases above min_fraction
  # SMPS expects 24 samples per hr
  smps <- smps |>
    mutate(sample_datetime = lubridate::floor_date(sample_datetime, unit = floor_to)) |>
    summarise(smps_volume = mean(smps_volume, na.rm = TRUE),
              count = n(),
              .by = sample_datetime) |>
    filter(count >= 24 * resolution_minutes / 60) |>
    mutate(parameter = "smps_volume") |>
    select(sample_datetime, parameter, value = smps_volume)
  
}

# Calculate SMPS mass concentration using composition dependent particle density
smps_mass <- function(df_smps, df_rm) {

  df_rm <- df_rm |>
    filter(parameter %in% c("so4", "nh4", "no3", "chl", "org", "EBC_6")) 
  
  density <- df_rm |>
    left_join(DENSITY, by = "parameter") |>
    mutate(vol_conc = value / density)

  # volume concentration from acsm plus BC
  acsm_bc_volume <- density |>
    select(-value, -density) |>
    tidyr::pivot_wider(names_from = parameter, values_from = vol_conc) |>
    mutate(volume = so4 + nh4 + no3 + chl + org + EBC_6) |>
    select(sample_datetime, volume)
  
  # density from acsm plus BC
  acsm_bc_density <- df_rm |>
    tidyr::pivot_wider(names_from = parameter, values_from = value) |>
    left_join(acsm_bc_volume, by = "sample_datetime") |>
    mutate(density = (so4 + nh4 + no3 + chl + org + EBC_6) / volume) |>
    select(sample_datetime, density)
  
  # Finally, SMPS mass concentration
  mass <- df_smps |>
    filter(parameter == "smps_volume") |>
    left_join(acsm_bc_density, by = "sample_datetime") |>
    mutate(smps_mass = value * density) |>
    select(sample_datetime, smps_mass)
  
}

#' Grab data for reconstructed mass plots at a given resolution.
#' Title
#'
#' @param site 
#' @param start_date 
#' @param end_date 
#' @param con 
#' @param influx_con 
#' @param resolution_minutes 
#' @param require_full If true, only return complete cases (with all instruments)
#' @param include_xact If true, include Xact data (all elements but S, Nb, Al, and Si)
#'
#' @returns
#' @export
#'
#' @examples
reconstructed_mass <- function(site, start_date, end_date, con, influx_con, 
                               resolution_minutes = 30, min_fraction = 0.5,
                               require_full = FALSE,
                               include_xact = FALSE) {
  
  # make sure to get the hours from the last day
  end_date <- as.Date(end_date) + 1
  sites <- tbl(con, I("common.sites"))
  
  acsm <- acsm_aggregated(site, start_date, end_date, con,
                          resolution_minutes = resolution_minutes,
                          min_fraction = min_fraction)
  
  ae33 <- ae33_trace(site, start_date, end_date, parameter = "EBC_6", 
                     resolution = resolution_minutes, client = influx_con)
  
  if (include_xact) {
    xact <- xact_aggregated(site, start_date, end_date, con, 
                            resolution_hours = resolution_minutes / 60)
    
    df <- bind_rows(acsm, ae33, xact)
  } else {
    df <- bind_rows(acsm, ae33)
  }
  
  df
  
}





