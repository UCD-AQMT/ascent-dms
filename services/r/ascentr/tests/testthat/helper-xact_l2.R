
# Fixture helpers for xact_l2_from_files() tests
#
# Xact produces one row per element per sample period.
# Completeness: sample_time_frac = (volume_L / flow_act_L_min) / sample_time_min
# A sample is valid when sample_time_frac >= 0.45.

# Default elements used across tests (Nb excluded — filtered by function)
XACT_ELEMENTS <- c("S", "Si", "Fe")

# Build fixture rows for one sample (one row per element).
# Column order matches the real L1b CSV so that select(-at_degC:-wind_dir_degrees)
# operates on the correct range.
#
# Valid sample (frac >= 0.45):   volume_L = 59, flow_act_L_min = 1, sample_time_min = 60
# Invalid sample (frac < 0.45):  volume_L = 20, flow_act_L_min = 1, sample_time_min = 60
xact_scans <- function(base, elements = XACT_ELEMENTS,
                       qc_outcome     = 1L,
                       flag           = NA_character_,
                       comment        = NA_character_,
                       volume_L       = 59,
                       flow_act_L_min = 1.0,
                       sample_time_min = 60L) {
  n <- length(elements)
  tibble::tibble(
    site_number              = 1L,
    site_code                = "TestSite",
    sample_datetime_UTC      = base,
    sample_type              = "sample",
    alarm                    = 0L,
    element                  = elements,
    concentration_ng_m3      = 10.0,
    uncertainty_ng_m3        = 1.0,
    pump_start_time_UTC      = base,
    sample_time_min          = as.integer(sample_time_min),
    at_degC                  = 20.0,
    sample_degC              = 20.0,
    bp_Pa                    = 101325L,
    tape_Pa                  = 78000L,
    flow_25_L_min            = flow_act_L_min,
    flow_act_L_min           = flow_act_L_min,
    flow_std_L_min           = flow_act_L_min,
    volume_L                 = volume_L,
    tube_degC                = 30.0,
    enclosure_degC           = 25.0,
    filament_V               = 3.0,
    sdd_degC                 = -40.0,
    dpp_degC                 = 40.0,
    rh_percent               = 30.0,
    wind_m_s                 = 2.0,
    wind_dir_degrees         = 180.0,
    sample_analysis_id       = seq_len(n),
    site_record_id           = seq_len(n),
    stp_factor               = 1.1,
    concentration_stp_ng_m3  = 11.0,
    uncertainty_stp_ng_m3    = 1.1,
    qc_outcome               = as.double(rep_len(qc_outcome, n)),
    flag                     = as.character(rep_len(flag, n)),
    comment                  = as.character(rep_len(comment, n))
  )
}

# Write L1b / QC data frames to CSV and return paths.
write_xact_l1b <- function(path, df) {
  readr::write_csv(df, path, na = "")
  path
}

write_xact_qc <- function(path, df) {
  readr::write_csv(df, path, na = "")
  path
}

# Empty QC data frame (no manual flags applied).
xact_empty_qc <- function() {
  tibble::tibble(
    sample_datetime_UTC_start = as.POSIXct(character(), tz = "UTC"),
    sample_datetime_UTC_end   = as.POSIXct(character(), tz = "UTC"),
    flag                      = character(),
    comment                   = character()
  )
}

# One-row QC entry covering a time range.
xact_qc_entry <- function(start, end = NA, flag, comment = "") {
  tibble::tibble(
    sample_datetime_UTC_start = as.POSIXct(start, tz = "UTC"),
    sample_datetime_UTC_end   = if (is.na(end)) {
      as.POSIXct(NA, tz = "UTC")
    } else {
      as.POSIXct(end, tz = "UTC")
    },
    flag    = flag,
    comment = comment
  )
}
