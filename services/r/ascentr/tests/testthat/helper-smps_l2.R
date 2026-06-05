
# Fixture helpers for smps_l2_from_files() tests

# Simple 3-bin concentration JSON: midpoints 10, 100, 1000 nm
SMPS_TEST_JSON <- '{"10":100,"100":50,"1000":10}'

# Build a data frame of scan rows for one hour.
# `base`       : POSIXct start time
# `n`          : number of scans (2.5-min intervals)
# `qc_outcome` : scalar or vector recycled to length n
# `flag`       : scalar or vector recycled to length n
smps_scans <- function(base, n,
                       qc_outcome = 1L,
                       flag       = NA_character_,
                       comment    = NA_character_) {
  tibble::tibble(
    site_number         = 1L,
    site_code           = "TestSite",
    sample_datetime_utc = base + (seq_len(n) - 1L) * 150,
    stp_factor          = 1.1,
    qc_outcome          = as.double(rep_len(qc_outcome, n)),
    flag                = as.character(rep_len(flag, n)),
    comment             = as.character(rep_len(comment, n)),
    concentration_json  = SMPS_TEST_JSON
  )
}

# Write a L1b data frame to a CSV file and return the path.
write_smps_l1b <- function(path, df) {
  readr::write_csv(df, path, na = "")
  path
}

# Write a QC data frame to a CSV file and return the path.
write_smps_qc <- function(path, df) {
  readr::write_csv(df, path, na = "")
  path
}

# Return an empty QC data frame (no manual flags applied).
empty_qc <- function() {
  tibble::tibble(
    sample_datetime_UTC_start = as.POSIXct(character(), tz = "UTC"),
    sample_datetime_UTC_end   = as.POSIXct(character(), tz = "UTC"),
    flag                      = character(),
    comment                   = character()
  )
}

# Build a one-row QC entry covering a time range.
qc_entry <- function(start, end = NA, flag, comment = "") {
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
