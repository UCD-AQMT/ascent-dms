
ae33_base <- as.POSIXct("2023-01-01 01:00:00", tz = "UTC")

# Expected bb_percent for fixture bc_2=100, bc_7=50 with MAC constants
# (14.54 at 470 nm, 7.19 at 950 nm) — verified against ae33_MAC
AE33_BB_PERCENT <- 98.011

# Output structure ---------------------------------------------------------

test_that("output contains expected columns", {
  dir      <- withr::local_tempdir()
  l1b_path <- write_ae33_l1b(file.path(dir, "l1b.csv"), ae33_scans(ae33_base, 35))
  qc_path  <- write_ae33_qc(file.path(dir, "qc.csv"), ae33_empty_qc())

  result <- ae33_l2_from_files(l1b_path, qc_path)

  expected_cols <- c(
    "site_number", "site_code", "sample_datetime_UTC",
    "sample_count", "bc_2_STP_ng_m3", "bc_7_STP_ng_m3",
    "bb_percent", "qc_outcome", "flag", "comment"
  )
  expect_true(all(expected_cols %in% names(result)))
})

# Valid hour ---------------------------------------------------------------

test_that("valid hour (>=30 scans) returns one row with correct values", {
  dir      <- withr::local_tempdir()
  l1b_path <- write_ae33_l1b(file.path(dir, "l1b.csv"), ae33_scans(ae33_base, 35))
  qc_path  <- write_ae33_qc(file.path(dir, "qc.csv"), ae33_empty_qc())

  result <- ae33_l2_from_files(l1b_path, qc_path)

  expect_equal(nrow(result), 1)
  expect_equal(result$sample_count, 35)
  expect_equal(result$qc_outcome, 1)
  expect_true(is.na(result$flag))
  expect_equal(result$bc_2_STP_ng_m3, AE33_BC2, tolerance = 0.01)
  expect_equal(result$bc_7_STP_ng_m3, AE33_BC7, tolerance = 0.01)
  expect_equal(result$bb_percent, AE33_BB_PERCENT, tolerance = 0.01)
})

test_that("bb_percent is clamped to [0, 100]", {
  # Negative bc_7 drives bb_percent outside [0, 100]; function should clamp it
  dir <- withr::local_tempdir()
  scans <- ae33_scans(ae33_base, 35)
  scans$bc_7_STP_ng_m3 <- -500
  l1b_path <- write_ae33_l1b(file.path(dir, "l1b.csv"), scans)
  qc_path  <- write_ae33_qc(file.path(dir, "qc.csv"), ae33_empty_qc())

  result <- ae33_l2_from_files(l1b_path, qc_path)

  expect_gte(result$bb_percent, 0)
  expect_lte(result$bb_percent, 100)
})

# Invalid hour -------------------------------------------------------------

test_that("hour with fewer than 30 valid scans returns NULL with warning (no valid hours)", {
  dir      <- withr::local_tempdir()
  l1b_path <- write_ae33_l1b(file.path(dir, "l1b.csv"), ae33_scans(ae33_base, 15))
  qc_path  <- write_ae33_qc(file.path(dir, "qc.csv"), ae33_empty_qc())

  expect_snapshot(ae33_l2_from_files(l1b_path, qc_path), transform = scrub_l1b_path)
})

test_that("mix of valid and invalid hours returns one row per hour", {
  dir <- withr::local_tempdir()
  l1b_path <- write_ae33_l1b(
    file.path(dir, "l1b.csv"),
    dplyr::bind_rows(
      ae33_scans(ae33_base,         35),  # valid
      ae33_scans(ae33_base + 3600,  15)   # invalid
    )
  )
  qc_path <- write_ae33_qc(file.path(dir, "qc.csv"), ae33_empty_qc())

  result <- ae33_l2_from_files(l1b_path, qc_path)

  expect_equal(nrow(result), 2)
  expect_setequal(result$qc_outcome, c(1, 9))
})

# NULL return when no valid data --------------------------------------------

test_that("all invalid scans (qc_outcome=4) warn and return NULL", {
  dir      <- withr::local_tempdir()
  l1b_path <- write_ae33_l1b(
    file.path(dir, "l1b.csv"),
    ae33_scans(ae33_base, 35, qc_outcome = 4, flag = "659")
  )
  qc_path <- write_ae33_qc(file.path(dir, "qc.csv"), ae33_empty_qc())

  expect_snapshot(ae33_l2_from_files(l1b_path, qc_path), transform = scrub_l1b_path)
})

# start_datetime filter ----------------------------------------------------

test_that("start_datetime excludes scans before that time", {
  pre_start <- ae33_base - 3600

  dir <- withr::local_tempdir()
  l1b_path <- write_ae33_l1b(
    file.path(dir, "l1b.csv"),
    dplyr::bind_rows(
      ae33_scans(pre_start,  35),
      ae33_scans(ae33_base,  35)
    )
  )
  qc_path <- write_ae33_qc(file.path(dir, "qc.csv"), ae33_empty_qc())

  result <- ae33_l2_from_files(l1b_path, qc_path, start_datetime = ae33_base)

  expect_equal(nrow(result), 1)
  expect_equal(result$sample_datetime_UTC, ae33_base)
})

test_that("start_datetime mid-hour leaves too few scans and returns NULL", {
  # Skipping the first 20 scans leaves only 15 — below the 30-sample threshold.
  # With no valid hours, the function returns NULL (see NULL-return note above).
  mid_hour <- ae33_base + 20 * 60

  dir      <- withr::local_tempdir()
  l1b_path <- write_ae33_l1b(file.path(dir, "l1b.csv"), ae33_scans(ae33_base, 35))
  qc_path  <- write_ae33_qc(file.path(dir, "qc.csv"), ae33_empty_qc())

  expect_snapshot(
    ae33_l2_from_files(l1b_path, qc_path, start_datetime = mid_hour),
    transform = scrub_l1b_path
  )
})

# Manual QC ----------------------------------------------------------------

test_that("manual QC flag is applied to scans within the time range", {
  qc <- ae33_qc_entry(
    start   = ae33_base,
    end     = ae33_base + 5 * 60,
    flag    = "660",
    comment = "manual qc"
  )

  dir      <- withr::local_tempdir()
  l1b_path <- write_ae33_l1b(file.path(dir, "l1b.csv"), ae33_scans(ae33_base, 35))
  qc_path  <- write_ae33_qc(file.path(dir, "qc.csv"), qc)

  result <- ae33_l2_from_files(l1b_path, qc_path)

  expect_equal(nrow(result), 1)
  expect_equal(result$qc_outcome, 1)
  expect_equal(result$flag, "660")
})

test_that("manual QC with NA end applies flag only to the matching scan", {
  qc <- ae33_qc_entry(start = ae33_base, end = NA, flag = "660", comment = "single")

  dir      <- withr::local_tempdir()
  l1b_path <- write_ae33_l1b(file.path(dir, "l1b.csv"), ae33_scans(ae33_base, 35))
  qc_path  <- write_ae33_qc(file.path(dir, "qc.csv"), qc)

  result <- ae33_l2_from_files(l1b_path, qc_path)

  expect_equal(result$flag, "660")
})

# Flag 111 override --------------------------------------------------------

test_that("manual flag 111 overrides auto bad-outcome scans to qc_outcome 1", {
  # First 5 scans are auto-bad (qc_outcome=4); manual 111 rescues them.
  # All 35 scans are valid after the override.
  bad_scans  <- ae33_scans(ae33_base,          5, qc_outcome = 4, flag = "659")
  good_scans <- ae33_scans(ae33_base + 5 * 60, 30)

  qc <- ae33_qc_entry(
    start   = ae33_base,
    end     = ae33_base + 4 * 60,
    flag    = "111",
    comment = "override"
  )

  dir      <- withr::local_tempdir()
  l1b_path <- write_ae33_l1b(file.path(dir, "l1b.csv"), dplyr::bind_rows(bad_scans, good_scans))
  qc_path  <- write_ae33_qc(file.path(dir, "qc.csv"), qc)

  result <- ae33_l2_from_files(l1b_path, qc_path)

  expect_equal(nrow(result), 1)
  expect_equal(result$qc_outcome, 1)
  expect_equal(result$flag, "111")
  expect_equal(result$sample_count, 35)
})
