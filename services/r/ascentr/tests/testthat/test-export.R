
# export_smps_l2_from_l1b / export_ae33_l2_from_l1b -----------------------
#
# Both functions unzip an L1b delivery zip, run the CSV inside it through
# the corresponding *_l2_from_files() (level "2") or *_l2_native_from_files()
# (level "2N") function for a single period, and write the delivery zip.
# smps_metadata() and ae33_metadata() require a live database connection,
# so they are mocked out here; we only need to verify that unzipping,
# level dispatch, file naming, and zip export behave correctly.

jan_time <- as.POSIXct("2023-01-01 01:00:00", tz = "UTC")

# Unzip a delivery zip and read back the CSV payload.
read_export_csv <- function(zip_path) {
  dir <- withr::local_tempdir()
  unzip(zip_path, exdir = dir)
  csv_file <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  readr::read_csv(csv_file, show_col_types = FALSE)
}

# Zip up a single L1b csv file, mimicking a Level 1b delivery zip, and
# return the path to the zip.
zip_l1b_csv <- function(csv_path, zip_path) {
  zip::zip(zipfile = zip_path, files = csv_path, mode = "cherry-pick")
  zip_path
}

test_that("export_smps_l2_from_l1b writes an L2 zip with hourly-aggregated data", {
  testthat::local_mocked_bindings(smps_metadata = function(...) "mock metadata")

  dir      <- withr::local_tempdir()
  csv_path <- write_smps_l1b(file.path(dir, "l1b.csv"), smps_scans(jan_time, 14))
  zip_path <- zip_l1b_csv(csv_path, file.path(dir, "l1b.zip"))
  qc_path  <- write_smps_qc(file.path(dir, "qc.csv"), empty_qc())
  out_dir  <- withr::local_tempdir()

  export_smps_l2_from_l1b(
    l1b_zip        = zip_path,
    start_date     = as.Date("2023-01-01"),
    end_date       = as.Date("2023-01-31"),
    site           = "TestSite",
    manual_qc_file = qc_path,
    level          = "2",
    out_folder     = out_dir,
    con            = NULL
  )

  zips <- list.files(out_dir, pattern = "\\.zip$", full.names = TRUE)
  expect_length(zips, 1)
  expect_true(grepl("ASCENT_SMPS_TestSite_2023-01-01_2023-01-31_L2\\.zip$", zips))

  result <- read_export_csv(zips)

  # Data is hourly-aggregated: one row for the one valid hour of scans
  expect_equal(nrow(result), 1)
  expect_equal(result$sample_count, 14)
})

test_that("export_smps_l2_from_l1b writes an L2N zip with native-resolution data", {
  testthat::local_mocked_bindings(smps_metadata = function(...) "mock metadata")

  dir      <- withr::local_tempdir()
  csv_path <- write_smps_l1b(file.path(dir, "l1b.csv"), smps_scans(jan_time, 14))
  zip_path <- zip_l1b_csv(csv_path, file.path(dir, "l1b.zip"))
  qc_path  <- write_smps_qc(file.path(dir, "qc.csv"), empty_qc())
  out_dir  <- withr::local_tempdir()

  export_smps_l2_from_l1b(
    l1b_zip        = zip_path,
    start_date     = as.Date("2023-01-01"),
    end_date       = as.Date("2023-01-31"),
    site           = "TestSite",
    manual_qc_file = qc_path,
    level          = "2N",
    out_folder     = out_dir,
    con            = NULL
  )

  zips <- list.files(out_dir, pattern = "\\.zip$", full.names = TRUE)
  expect_length(zips, 1)
  expect_true(grepl("ASCENT_SMPS_TestSite_2023-01-01_2023-01-31_L2_native\\.zip$", zips))

  result <- read_export_csv(zips)

  # Native output is un-aggregated: one row per scan
  expect_equal(nrow(result), 14)
  expect_true("concentration_json" %in% names(result))
})

test_that("export_smps_l2_from_l1b passes start_date through as the lower filter bound", {
  testthat::local_mocked_bindings(smps_metadata = function(...) "mock metadata")

  dec_time <- as.POSIXct("2022-12-31 01:00:00", tz = "UTC")

  dir <- withr::local_tempdir()
  csv_path <- write_smps_l1b(
    file.path(dir, "l1b.csv"),
    dplyr::bind_rows(
      smps_scans(dec_time, 14),  # before start_date: excluded
      smps_scans(jan_time, 14)   # at/after start_date: included
    )
  )
  zip_path <- zip_l1b_csv(csv_path, file.path(dir, "l1b.zip"))
  qc_path  <- write_smps_qc(file.path(dir, "qc.csv"), empty_qc())
  out_dir  <- withr::local_tempdir()

  export_smps_l2_from_l1b(
    l1b_zip        = zip_path,
    start_date     = as.Date("2023-01-01"),
    end_date       = as.Date("2023-01-31"),
    site           = "TestSite",
    manual_qc_file = qc_path,
    level          = "2",
    out_folder     = out_dir,
    con            = NULL
  )

  result <- read_export_csv(list.files(out_dir, pattern = "\\.zip$", full.names = TRUE))
  expect_equal(nrow(result), 1)
  expect_equal(result$sample_count, 14)
})

test_that("export_ae33_l2_from_l1b writes an L2 zip with hourly-aggregated data", {
  testthat::local_mocked_bindings(ae33_metadata = function(...) "mock metadata")

  dir      <- withr::local_tempdir()
  csv_path <- write_ae33_l1b(file.path(dir, "l1b.csv"), ae33_scans(jan_time, 35))
  zip_path <- zip_l1b_csv(csv_path, file.path(dir, "l1b.zip"))
  qc_path  <- write_ae33_qc(file.path(dir, "qc.csv"), ae33_empty_qc())
  out_dir  <- withr::local_tempdir()

  export_ae33_l2_from_l1b(
    l1b_zip        = zip_path,
    start_date     = as.Date("2023-01-01"),
    end_date       = as.Date("2023-01-31"),
    site           = "TestSite",
    manual_qc_file = qc_path,
    level          = "2",
    out_folder     = out_dir,
    con            = NULL
  )

  zips <- list.files(out_dir, pattern = "\\.zip$", full.names = TRUE)
  expect_length(zips, 1)
  expect_true(grepl("ASCENT_AE33_TestSite_2023-01-01_2023-01-31_L2\\.zip$", zips))

  result <- read_export_csv(zips)

  # AE33 requires >= 30 scans per hour for a valid value; 35 scans -> one row
  expect_equal(nrow(result), 1)
  expect_equal(result$sample_count, 35)
})

test_that("export_ae33_l2_from_l1b writes an L2N zip with native-resolution data", {
  testthat::local_mocked_bindings(ae33_metadata = function(...) "mock metadata")

  dir      <- withr::local_tempdir()
  csv_path <- write_ae33_l1b(file.path(dir, "l1b.csv"), ae33_scans(jan_time, 35))
  zip_path <- zip_l1b_csv(csv_path, file.path(dir, "l1b.zip"))
  qc_path  <- write_ae33_qc(file.path(dir, "qc.csv"), ae33_empty_qc())
  out_dir  <- withr::local_tempdir()

  export_ae33_l2_from_l1b(
    l1b_zip        = zip_path,
    start_date     = as.Date("2023-01-01"),
    end_date       = as.Date("2023-01-31"),
    site           = "TestSite",
    manual_qc_file = qc_path,
    level          = "2N",
    out_folder     = out_dir,
    con            = NULL
  )

  zips <- list.files(out_dir, pattern = "\\.zip$", full.names = TRUE)
  expect_length(zips, 1)
  expect_true(grepl("ASCENT_AE33_TestSite_2023-01-01_2023-01-31_L2_native\\.zip$", zips))

  result <- read_export_csv(zips)

  # Native output is un-aggregated: one row per scan
  expect_equal(nrow(result), 35)
})
