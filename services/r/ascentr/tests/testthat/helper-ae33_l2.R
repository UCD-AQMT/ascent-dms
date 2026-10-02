
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
#
# ae33_l2_from_files()/ae33_l2_native_from_files() select and mutate a
# contiguous bc_1_STP_ng_m3:att2_7 range of columns (all wavelength-resolved
# BC channels, multiple-scattering correction factors, and attenuation
# channels), so every one of those columns must be present, with qc_outcome/
# flag/comment placed after them, to match the real L1b column layout.
ae33_scans <- function(base, n,
                       qc_outcome = 1L,
                       flag       = NA_character_,
                       comment    = NA_character_) {
  tibble::tibble(
    site_number         = 1L,
    site_code           = "TestSite",
    sample_datetime_UTC = base + (seq_len(n) - 1L) * 60,
    bc_1_STP_ng_m3      = 90,
    bc_2_STP_ng_m3      = as.double(AE33_BC2),
    bc_3_STP_ng_m3      = 85,
    bc_4_STP_ng_m3      = 75,
    bc_5_STP_ng_m3      = 65,
    bc_6_STP_ng_m3      = 55,
    bc_7_STP_ng_m3      = as.double(AE33_BC7),
    bc1_1_STP_ng_m3     = 90,
    bc1_2_STP_ng_m3     = 80,
    bc1_3_STP_ng_m3     = 70,
    bc1_4_STP_ng_m3     = 60,
    bc1_5_STP_ng_m3     = 50,
    bc1_6_STP_ng_m3     = 40,
    bc1_7_STP_ng_m3     = 30,
    bc2_1_STP_ng_m3     = 90,
    bc2_2_STP_ng_m3     = 80,
    bc2_3_STP_ng_m3     = 70,
    bc2_4_STP_ng_m3     = 60,
    bc2_5_STP_ng_m3     = 50,
    bc2_6_STP_ng_m3     = 40,
    bc2_7_STP_ng_m3     = 30,
    k_1                 = 1,
    k_2                 = 1,
    k_3                 = 1,
    k_4                 = 1,
    k_5                 = 1,
    k_6                 = 1,
    k_7                 = 1,
    att1_1              = 5,
    att1_2              = 5,
    att1_3              = 5,
    att1_4              = 5,
    att1_5              = 5,
    att1_6              = 5,
    att1_7              = 5,
    att2_1              = 5,
    att2_2              = 5,
    att2_3              = 5,
    att2_4              = 5,
    att2_5              = 5,
    att2_6              = 5,
    att2_7              = 5,
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
