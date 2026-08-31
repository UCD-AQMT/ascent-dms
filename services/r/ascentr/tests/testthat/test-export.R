
# export_smps_l2_monthly / export_smps_l2_native_monthly ------------------
#
# smps_metadata() requires a live database connection, so it is mocked out
# in these tests; we only need to verify that monthly splitting, file
# naming, and zip export behave correctly.

jan_time <- as.POSIXct("2023-01-01 01:00:00", tz = "UTC")
feb_time <- as.POSIXct("2023-02-01 01:00:00", tz = "UTC")

# Unzip a delivery zip and read back the CSV payload.
read_export_csv <- function(zip_path) {
  dir <- withr::local_tempdir()
  unzip(zip_path, exdir = dir)
  csv_file <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  readr::read_csv(csv_file, show_col_types = FALSE)
}

test_that("export_smps_l2_monthly writes one zip per month with data", {
  testthat::local_mocked_bindings(smps_metadata = function(...) "mock metadata")

  dir <- withr::local_tempdir()
  l1b_path <- write_smps_l1b(
    file.path(dir, "l1b.csv"),
    dplyr::bind_rows(
      smps_scans(jan_time, 14),  # valid hour in January
      smps_scans(feb_time, 14)   # valid hour in February
    )
  )
  qc_path <- write_smps_qc(file.path(dir, "qc.csv"), empty_qc())
  out_dir <- withr::local_tempdir()

  export_smps_l2_monthly(
    site           = "TestSite",
    start_date     = as.Date("2023-01-01"),
    end_date       = as.Date("2023-02-28"),
    l1b_file       = l1b_path,
    manual_qc_file = qc_path,
    out_folder     = out_dir,
    con            = NULL
  )

  zips <- list.files(out_dir, pattern = "\\.zip$", full.names = TRUE)
  expect_length(zips, 2)
  expect_true(any(grepl("ASCENT_SMPS_TestSite_2023-01-01_2023-01-31_L2\\.zip$", zips)))
  expect_true(any(grepl("ASCENT_SMPS_TestSite_2023-02-01_2023-02-28_L2\\.zip$", zips)))

  jan_zip <- zips[grepl("2023-01-01", zips)]
  result <- read_export_csv(jan_zip)

  # Data is hourly-aggregated: one row for the one valid hour of scans
  expect_equal(nrow(result), 1)
  expect_equal(result$sample_count, 14)
})

test_that("export_smps_l2_monthly skips months with no data", {
  testthat::local_mocked_bindings(smps_metadata = function(...) "mock metadata")

  dir <- withr::local_tempdir()
  l1b_path <- write_smps_l1b(file.path(dir, "l1b.csv"), smps_scans(jan_time, 14))
  qc_path  <- write_smps_qc(file.path(dir, "qc.csv"), empty_qc())
  out_dir  <- withr::local_tempdir()

  expect_message(
    export_smps_l2_monthly(
      site           = "TestSite",
      start_date     = as.Date("2023-01-01"),
      end_date       = as.Date("2023-02-28"),
      l1b_file       = l1b_path,
      manual_qc_file = qc_path,
      out_folder     = out_dir,
      con            = NULL
    ),
    "No data for this month"
  )

  zips <- list.files(out_dir, pattern = "\\.zip$")
  expect_length(zips, 1)
  expect_true(grepl("2023-01-01_2023-01-31_L2\\.zip$", zips))
})

test_that("export_smps_l2_native_monthly writes one zip per month with data", {
  testthat::local_mocked_bindings(smps_metadata = function(...) "mock metadata")

  dir <- withr::local_tempdir()
  l1b_path <- write_smps_l1b(
    file.path(dir, "l1b.csv"),
    dplyr::bind_rows(
      smps_scans(jan_time, 14),
      smps_scans(feb_time, 14)
    )
  )
  qc_path <- write_smps_qc(file.path(dir, "qc.csv"), empty_qc())
  out_dir <- withr::local_tempdir()

  export_smps_l2_native_monthly(
    site           = "TestSite",
    start_date     = as.Date("2023-01-01"),
    end_date       = as.Date("2023-02-28"),
    l1b_file       = l1b_path,
    manual_qc_file = qc_path,
    out_folder     = out_dir,
    con            = NULL
  )

  zips <- list.files(out_dir, pattern = "\\.zip$", full.names = TRUE)
  expect_length(zips, 2)
  expect_true(any(grepl("ASCENT_SMPS_TestSite_2023-01-01_2023-01-31_L2_native\\.zip$", zips)))
  expect_true(any(grepl("ASCENT_SMPS_TestSite_2023-02-01_2023-02-28_L2_native\\.zip$", zips)))

  jan_zip <- zips[grepl("2023-01-01", zips)]
  result <- read_export_csv(jan_zip)

  # Native output is un-aggregated: one row per scan
  expect_equal(nrow(result), 14)
  expect_true("concentration_json" %in% names(result))
})

test_that("export_smps_l2_native_monthly skips months with no data", {
  testthat::local_mocked_bindings(smps_metadata = function(...) "mock metadata")

  dir <- withr::local_tempdir()
  l1b_path <- write_smps_l1b(file.path(dir, "l1b.csv"), smps_scans(jan_time, 14))
  qc_path  <- write_smps_qc(file.path(dir, "qc.csv"), empty_qc())
  out_dir  <- withr::local_tempdir()

  expect_message(
    export_smps_l2_native_monthly(
      site           = "TestSite",
      start_date     = as.Date("2023-01-01"),
      end_date       = as.Date("2023-02-28"),
      l1b_file       = l1b_path,
      manual_qc_file = qc_path,
      out_folder     = out_dir,
      con            = NULL
    ),
    "No data for this month"
  )

  zips <- list.files(out_dir, pattern = "\\.zip$")
  expect_length(zips, 1)
  expect_true(grepl("2023-01-01_2023-01-31_L2_native\\.zip$", zips))
})
