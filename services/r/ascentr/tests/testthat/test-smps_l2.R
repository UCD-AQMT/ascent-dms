
base_time <- as.POSIXct("2023-01-01 01:00:00", tz = "UTC")

# Output structure ---------------------------------------------------------

test_that("output contains expected columns", {
  dir      <- withr::local_tempdir()
  l1b_path <- write_smps_l1b(file.path(dir, "l1b.csv"), smps_scans(base_time, 14))
  qc_path  <- write_smps_qc(file.path(dir, "qc.csv"), empty_qc())

  result <- smps_l2_from_files(l1b_path, qc_path)

  expected_cols <- c(
    "site_number", "site_code", "sample_datetime_UTC",
    "stp_factor", "qc_outcome", "flag", "comment",
    "sample_count",
    "total_concentration_1_cm3", "volume_concentration_um3_cm3",
    "mean_nm", "geo_mean_nm", "median_nm", "mode_nm", "geo_std_dev",
    "number_concentration_stp_1_cm3", "volume_concentration_stp_um3_cm3",
    "concentration_json"
  )
  expect_true(all(expected_cols %in% names(result)))
})

# Valid hour ---------------------------------------------------------------

test_that("valid hour (>=12 scans) returns one row with correct stats", {
  dir      <- withr::local_tempdir()
  l1b_path <- write_smps_l1b(file.path(dir, "l1b.csv"), smps_scans(base_time, 14))
  qc_path  <- write_smps_qc(file.path(dir, "qc.csv"), empty_qc())

  result <- smps_l2_from_files(l1b_path, qc_path)

  expect_equal(nrow(result), 1)
  expect_equal(result$sample_count, 14)
  expect_equal(result$qc_outcome, 1)
  expect_true(is.na(result$flag))

  # Stats derived from JSON {"10":100, "100":50, "1000":10} with dlogDp = c(1,1,1)
  expect_equal(result$total_concentration_1_cm3,       160,     tolerance = 0.01)
  expect_equal(result$number_concentration_stp_1_cm3,  176,     tolerance = 0.01)
  expect_equal(result$volume_concentration_um3_cm3,    5.262,   tolerance = 0.01)
  expect_equal(result$mean_nm,                         100,     tolerance = 0.01)
  expect_equal(result$geo_mean_nm,                     27.38,   tolerance = 0.01)
  expect_equal(result$median_nm,                       10,      tolerance = 0.01)
  expect_equal(result$mode_nm,                         10,      tolerance = 0.01)
  expect_equal(result$geo_std_dev,                     4.066,   tolerance = 0.01)
})

# Invalid hour -------------------------------------------------------------

test_that("hour with fewer than 12 valid scans returns flag 391 and qc_outcome 9", {
  dir      <- withr::local_tempdir()
  l1b_path <- write_smps_l1b(file.path(dir, "l1b.csv"), smps_scans(base_time, 28))
  qc_path  <- write_smps_qc(file.path(dir, "qc.csv"), empty_qc())

  result <- smps_l2_from_files(l1b_path, qc_path)

  expect_equal(nrow(result), 2)
  expect_equal(result$qc_outcome, c(1, 9))
  expect_equal(result$flag, c(NA_character_, "391"))
  expect_true(is.na(result$total_concentration_1_cm3[2]))
})

test_that("mix of valid and invalid hours returns one row per hour", {
  dir <- withr::local_tempdir()
  l1b_path <- write_smps_l1b(
    file.path(dir, "l1b.csv"),
    dplyr::bind_rows(
      smps_scans(base_time,          14),  # valid: 14 scans
      smps_scans(base_time + 3600,    5)   # invalid: 5 scans
    )
  )
  qc_path <- write_smps_qc(file.path(dir, "qc.csv"), empty_qc())

  result <- smps_l2_from_files(l1b_path, qc_path)

  expect_equal(nrow(result), 2)
  expect_equal(sort(result$qc_outcome), c(1, 9))
})

# start_datetime filter ----------------------------------------------------

test_that("start_datetime excludes scans before that time from hourly counts", {
  pre_start <- base_time - 3600  # one hour before base_time

  dir <- withr::local_tempdir()
  l1b_path <- write_smps_l1b(
    file.path(dir, "l1b.csv"),
    dplyr::bind_rows(
      smps_scans(pre_start,  14),  # hour before start
      smps_scans(base_time,  14)   # hour at start
    )
  )
  qc_path <- write_smps_qc(file.path(dir, "qc.csv"), empty_qc())

  result <- smps_l2_from_files(l1b_path, qc_path, start_datetime = base_time)

  expect_equal(nrow(result), 1)
  expect_equal(result$sample_datetime_UTC, base_time)
})

# Manual QC ----------------------------------------------------------------

test_that("manual QC flag is applied to scans within the specified time range", {
  # QC entry covers first 3 scans of the hour (t+0s, t+150s, t+300s)
  qc <- qc_entry(
    start   = base_time,
    end     = base_time + 300,
    flag    = "660",
    comment = "manual qc"
  )

  dir      <- withr::local_tempdir()
  l1b_path <- write_smps_l1b(file.path(dir, "l1b.csv"), smps_scans(base_time, 14))
  qc_path  <- write_smps_qc(file.path(dir, "qc.csv"), qc)

  result <- smps_l2_from_files(l1b_path, qc_path)

  expect_equal(nrow(result), 1)
  expect_equal(result$qc_outcome, 1)
  expect_equal(result$flag, "660")
})

test_that("manual QC with NA end time applies flag to the single matching scan", {
  qc <- qc_entry(start = base_time, end = NA, flag = "660", comment = "single scan")

  dir      <- withr::local_tempdir()
  l1b_path <- write_smps_l1b(file.path(dir, "l1b.csv"), smps_scans(base_time, 14))
  qc_path  <- write_smps_qc(file.path(dir, "qc.csv"), qc)

  result <- smps_l2_from_files(l1b_path, qc_path)

  expect_equal(result$flag, "660")
})

# Flag 111 override --------------------------------------------------------

test_that("manual flag 111 overrides auto bad-outcome scans to qc_outcome 1", {
  # First 2 scans are auto-bad (qc_outcome=4, flag=659); rest are good
  bad_scans  <- smps_scans(base_time, 2, qc_outcome = 4, flag = "659")
  good_scans <- smps_scans(base_time + 2 * 150, 12)

  # QC file overrides the two bad scans with flag 111
  qc <- qc_entry(
    start   = base_time,
    end     = base_time + 150,
    flag    = "111",
    comment = "override"
  )

  dir      <- withr::local_tempdir()
  l1b_path <- write_smps_l1b(file.path(dir, "l1b.csv"), dplyr::bind_rows(bad_scans, good_scans))
  qc_path  <- write_smps_qc(file.path(dir, "qc.csv"), qc)

  result <- smps_l2_from_files(l1b_path, qc_path)

  expect_equal(nrow(result), 1)
  expect_equal(result$qc_outcome, 1)
  expect_equal(result$flag, "111")
  expect_equal(result$sample_count, 14)
})

# All-invalid data ---------------------------------------------------------

test_that("all invalid scans emit a warning", {
  dir      <- withr::local_tempdir()
  l1b_path <- write_smps_l1b(
    file.path(dir, "l1b.csv"),
    smps_scans(base_time, 14, qc_outcome = 4, flag = "659")
  )
  qc_path <- write_smps_qc(file.path(dir, "qc.csv"), empty_qc())

  expect_snapshot(smps_l2_from_files(l1b_path, qc_path))
})
