
# Fixture helpers for ae33_l2_from_files() tests
#
# AE33 samples every minute; 60 samples per hour; 30 required for a valid hour.
# The function uses bc_2_STP_ng_m3 (470 nm) and bc_7_STP_ng_m3 (950 nm)
# to compute bb_percent via MAC constants from ae33_MAC.

AE33_BC2 <- 100   # bc_2_STP_ng_m3 fixture value (ng/m3)
AE33_BC7 <-  50   # bc_7_STP_ng_m3 fixture value (ng/m3)

# Build a data frame of AE33 scan rows for one hour.
# Scans are spaced 60 seconds apart starting at `base`.
# `qc_outcome`, `flag`, `comment` can be scalar or vector (recycled).
ae33_scans <- function(base, n,
                       qc_outcome = 1L,
                       flag       = NA_character_,
                       comment    = NA_character_) {
  tibble::tibble(
    site_number         = 1L,
    site_code           = "TestSite",
    sample_datetime_UTC = base + (seq_len(n) - 1L) * 60,
    bc_2_STP_ng_m3      = as.double(AE33_BC2),
    bc_7_STP_ng_m3      = as.double(AE33_BC7),
    qc_outcome          = as.double(rep_len(qc_outcome, n)),
    flag                = as.character(rep_len(flag, n)),
    comment             = as.character(rep_len(comment, n))
  )
}

# Write a L1b data frame to a CSV and return the path.
write_ae33_l1b <- function(path, df) {
  readr::write_csv(df, path, na = "")
  path
}

# Write a QC data frame to a CSV and return the path.
write_ae33_qc <- function(path, df) {
  readr::write_csv(df, path, na = "")
  path
}

# Empty QC data frame (no manual flags).
ae33_empty_qc <- function() {
  tibble::tibble(
    sample_datetime_UTC_start = as.POSIXct(character(), tz = "UTC"),
    sample_datetime_UTC_end   = as.POSIXct(character(), tz = "UTC"),
    flag                      = character(),
    comment                   = character()
  )
}

# Scrub volatile temp file paths from snapshot output so snapshots are stable
# across runs. Replaces the path in "No valid hours for <path>" warnings.
scrub_l1b_path <- function(x) {
  gsub("No valid hours for [^\n]+", "No valid hours for <l1b_file>", x)
}

# One-row QC entry covering a time range.
ae33_qc_entry <- function(start, end = NA, flag, comment = "") {
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
