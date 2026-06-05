
xact_base <- as.POSIXct("2023-01-01 01:00:00", tz = "UTC")

# Output structure ---------------------------------------------------------

test_that("output contains expected columns", {
  dir      <- withr::local_tempdir()
  l1b_path <- write_xact_l1b(file.path(dir, "l1b.csv"), xact_scans(xact_base))
  qc_path  <- write_xact_qc(file.path(dir, "qc.csv"), xact_empty_qc())

  result <- xact_l2_from_files(l1b_path, qc_path)

  expected_cols <- c(
    "site_number", "site_code", "sample_datetime_UTC",
    "sample_type", "element",
    "concentration_ng_m3", "uncertainty_ng_m3",
    "sample_time_min", "sample_time_est_min",
    "stp_factor", "concentration_stp_ng_m3", "uncertainty_stp_ng_m3",
    "qc_outcome", "flag", "comment"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("dropped columns are absent from output", {
  dir      <- withr::local_tempdir()
  l1b_path <- write_xact_l1b(file.path(dir, "l1b.csv"), xact_scans(xact_base))
  qc_path  <- write_xact_qc(file.path(dir, "qc.csv"), xact_empty_qc())

  result <- xact_l2_from_files(l1b_path, qc_path)

  expect_false(any(c("alarm", "pump_start_time_UTC", "at_degC",
                      "volume_L", "wind_dir_degrees", "sample_time_frac") %in% names(result)))
})

# Valid sample -------------------------------------------------------------

test_that("complete sample (frac >= 0.45) returns one row per element with data", {
  # volume_L=59, flow=1 L/min, time=60 min -> frac = 59/60 = 0.983 -> valid
  dir      <- withr::local_tempdir()
  l1b_path <- write_xact_l1b(file.path(dir, "l1b.csv"), xact_scans(xact_base))
  qc_path  <- write_xact_qc(file.path(dir, "qc.csv"), xact_empty_qc())

  result <- xact_l2_from_files(l1b_path, qc_path)

  expect_equal(nrow(result), length(XACT_ELEMENTS))
  expect_equal(sort(result$element), sort(XACT_ELEMENTS))
  expect_equal(unique(result$qc_outcome), 1)
  expect_true(all(is.na(result$flag)))
  expect_false(any(is.na(result$concentration_ng_m3)))
})

test_that("sample_datetime_UTC is floored to the hour in output", {
  # Give the sample a timestamp 30 minutes into the hour
  mid_hour <- xact_base + 30 * 60

  dir      <- withr::local_tempdir()
  l1b_path <- write_xact_l1b(file.path(dir, "l1b.csv"), xact_scans(mid_hour))
  qc_path  <- write_xact_qc(file.path(dir, "qc.csv"), xact_empty_qc())

  result <- xact_l2_from_files(l1b_path, qc_path)

  expect_equal(unique(result$sample_datetime_UTC), xact_base)
})

# Incomplete sample --------------------------------------------------------

test_that("incomplete sample (frac < 0.45) gets flag 391, qc_outcome 9, NA concentrations", {
  # volume_L=20, flow=1 L/min, time=60 min -> frac = 20/60 = 0.333 -> invalid
  dir      <- withr::local_tempdir()
  l1b_path <- write_xact_l1b(
    file.path(dir, "l1b.csv"),
    xact_scans(xact_base, volume_L = 20)
  )
  qc_path <- write_xact_qc(file.path(dir, "qc.csv"), xact_empty_qc())

  result <- xact_l2_from_files(l1b_path, qc_path)

  expect_equal(nrow(result), length(XACT_ELEMENTS))
  expect_equal(unique(result$qc_outcome), 9)
  expect_equal(unique(result$flag), "391")
  expect_true(all(is.na(result$concentration_ng_m3)))
  expect_true(all(is.na(result$concentration_stp_ng_m3)))
  expect_true(all(is.na(result$uncertainty_ng_m3)))
  expect_true(all(is.na(result$uncertainty_stp_ng_m3)))
})

# Nb filtering -------------------------------------------------------------

test_that("rows with element == 'Nb' are excluded from output", {
  nb_row <- xact_scans(xact_base, elements = "Nb")
  other  <- xact_scans(xact_base, elements = XACT_ELEMENTS)

  dir      <- withr::local_tempdir()
  l1b_path <- write_xact_l1b(file.path(dir, "l1b.csv"), dplyr::bind_rows(nb_row, other))
  qc_path  <- write_xact_qc(file.path(dir, "qc.csv"), xact_empty_qc())

  result <- xact_l2_from_files(l1b_path, qc_path)

  expect_false("Nb" %in% result$element)
  expect_equal(nrow(result), length(XACT_ELEMENTS))
})

# start_datetime filter ----------------------------------------------------

test_that("start_datetime removes samples before that time", {
  dir <- withr::local_tempdir()
  l1b_path <- write_xact_l1b(
    file.path(dir, "l1b.csv"),
    dplyr::bind_rows(
      xact_scans(xact_base - 3600),  # one hour before — should be excluded
      xact_scans(xact_base)
    )
  )
  qc_path <- write_xact_qc(file.path(dir, "qc.csv"), xact_empty_qc())

  result <- xact_l2_from_files(l1b_path, qc_path, start_datetime = xact_base)

  expect_equal(nrow(result), length(XACT_ELEMENTS))
  expect_equal(unique(result$sample_datetime_UTC), xact_base)
})

# Manual QC ----------------------------------------------------------------

test_that("manual QC flag is applied to all elements within the time range", {
  qc <- xact_qc_entry(
    start   = xact_base,
    end     = xact_base,
    flag    = "660",
    comment = "manual qc"
  )

  dir      <- withr::local_tempdir()
  l1b_path <- write_xact_l1b(file.path(dir, "l1b.csv"), xact_scans(xact_base))
  qc_path  <- write_xact_qc(file.path(dir, "qc.csv"), qc)

  result <- xact_l2_from_files(l1b_path, qc_path)

  expect_equal(unique(result$qc_outcome), 1)
  expect_equal(unique(result$flag), "660")
})

# Composite flag resolution ------------------------------------------------

test_that("composite flag '659:111' resolves to override: qc_outcome 1, flag '111'", {
  # Sample is auto-bad (qc_outcome=4, flag=659); QC file has composite "659:111"
  # which the resolver interprets as an override — result should be valid.
  qc <- xact_qc_entry(
    start   = xact_base,
    end     = xact_base,
    flag    = "659:111",
    comment = "override bad scan"
  )

  dir      <- withr::local_tempdir()
  l1b_path <- write_xact_l1b(
    file.path(dir, "l1b.csv"),
    xact_scans(xact_base, qc_outcome = 4, flag = "659")
  )
  qc_path <- write_xact_qc(file.path(dir, "qc.csv"), qc)

  result <- xact_l2_from_files(l1b_path, qc_path)

  expect_equal(unique(result$qc_outcome), 1)
  expect_equal(unique(result$flag), "111")
})

test_that("composite flag without 111 takes max qc_outcome and sorts flag codes", {
  # "453:659" -> max(1, 4) = 4, flag = "453:659" (sorted)
  qc <- xact_qc_entry(
    start = xact_base,
    end   = xact_base,
    flag  = "453:659",
    comment = "combined flags"
  )

  dir      <- withr::local_tempdir()
  l1b_path <- write_xact_l1b(file.path(dir, "l1b.csv"), xact_scans(xact_base))
  qc_path  <- write_xact_qc(file.path(dir, "qc.csv"), qc)

  result <- xact_l2_from_files(l1b_path, qc_path)

  expect_equal(unique(result$qc_outcome), 4)   # pmax(auto=1, manual=4) = 4
  expect_equal(unique(result$flag), "453:659")
})

# Unknown flag stops -------------------------------------------------------

test_that("an unrecognised flag in the QC file stops with an informative error", {
  qc <- xact_qc_entry(start = xact_base, end = xact_base, flag = "ZZZ")

  dir      <- withr::local_tempdir()
  l1b_path <- write_xact_l1b(file.path(dir, "l1b.csv"), xact_scans(xact_base))
  qc_path  <- write_xact_qc(file.path(dir, "qc.csv"), qc)

  expect_snapshot(xact_l2_from_files(l1b_path, qc_path), error = TRUE)
})

# Row count integrity ------------------------------------------------------

test_that("output row count equals filtered input row count", {
  dir <- withr::local_tempdir()
  l1b_path <- write_xact_l1b(
    file.path(dir, "l1b.csv"),
    dplyr::bind_rows(
      xact_scans(xact_base),           # valid
      xact_scans(xact_base + 3600, volume_L = 20)  # invalid (incomplete)
    )
  )
  qc_path <- write_xact_qc(file.path(dir, "qc.csv"), xact_empty_qc())

  # If the row count check inside the function fails it stops() — so passing
  # means the integrity check passed
  result <- xact_l2_from_files(l1b_path, qc_path)

  expect_equal(nrow(result), length(XACT_ELEMENTS) * 2)
})
