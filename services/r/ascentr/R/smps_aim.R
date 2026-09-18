#' Build AIM-format SMPS export files for all datasets in a time range
#'
#' Finds all SMPS instrument settings "datasets" active during the
#' requested time range and builds an AIM-format export file (see
#' [build_aim_file()]) for each, covering the overlap between the datasets
#' active period and the requested range.
#'
#' @param site ASCENT site code
#' @param start_dt Start date/datetime (inclusive) of the requested range
#' @param end_dt End date (inclusive) of the requested range
#' @param con A database connection, as returned by [get_db_connection()]
#'
#' @returns A list of character vectors, each the lines of an AIM-format
#'   file as produced by [build_aim_file()], one per dataset
#' @export
#'
#' @examples
build_aim_files <- function(site, start_dt, end_dt, con) {

  # find datasets in range
  datasets <- smps_datasets(site, start_dt, end_dt, con)

  # Get the columns for remapping
  cols <- smps_columns(con)

  params <- datasets |>
    # Find the time range that overlaps between the metadata and the requested dates
    mutate(start_time = pmax(as.POSIXct(start_dt, tz = "UTC"), dataset_start),
           end_time = pmin(as.POSIXct(end_dt, tz = "UTC"), dataset_end)) |>
    select(ds_value=value, start_time, end_time)

  # Iterate over the datasets
  aim_file_data <- function(ds_value, start_time, end_time, site, cols, con) {

    # get data and metadata
    meta <- smps_settings(site, start_time, end_time, con)
    df <- smps_data(site, start_time, end_time, con)
    # build aim file
    file <- build_aim_file(meta, df, ds_value, cols)
  }
  files <- purrr::pmap(params, aim_file_data, site = site, cols = cols, con = con)
}

#' Build a single AIM-format SMPS export file
#'
#' Reconstructs a file in the format produced by TSI's AIM software from
#' database records: expands the JSON-encoded particle count and raw
#' concentration columns into wide columns by size bin, injects the
#' dataset name into the instrument metadata, and assembles the header,
#' metadata, and data rows expected by AIM.
#'
#' @param metadata A data frame of instrument settings (name/value pairs),
#'   as returned by [smps_settings()]
#' @param df A data frame of SMPS sample analysis records, as returned by
#'   [smps_data()]
#' @param ds_value The dataset name to inject into the metadata
#' @param smps_cols A data frame of column name mappings, as returned by
#'   [smps_columns()]
#'
#' @returns A character vector of lines making up the AIM-format file, or
#'   `NULL` if `df` has no raw concentration records
#' @export
#'
#' @examples
build_aim_file <- function(metadata, df, ds_value, smps_cols) {


  # Dataset Name comes right after the first item (AIM Version). Everything else
  # is in the site_record_id order as a csv. There are empty columns to go the
  # entire width of the file. Full expansion of the json data is required to
  # determine the number of columns.

  # expand the json columns into a wide data frame - here yyjsonr is at least twice as fast as jsonlite
  counts <- purrr::map(df$concentration_json, \(x) as_tibble(yyjsonr::read_json_str(x))) |>
    purrr::list_rbind()

  safe_expand <- function(x) {
    tryJSON <- purrr::possibly(yyjsonr::read_json_str, NULL)
    j <- tryJSON(x)
    if (!is.null(j)) {
      return(as_tibble(j))
    } else {
      return(NULL)
    }
  }

  raw_concentration <- purrr::map(df$raw_concentration_json, \(x) safe_expand(x))
  if (length(raw_concentration) == 0) {
    return(NULL)
  }

  # Before rbinding, replace any nulls rows with a zero row.
  # Do we have any null rows?
  row_lengths <- purrr::map(raw_concentration, \(x) length(unlist(x)))
  min_row_length <- purrr::reduce(row_lengths, min)

  if (min_row_length == 0) {
    # we have some null rows, grab the first non-null row as a prototype
    max_row_length <- purrr::reduce(row_lengths, max)
    good_index <- which(row_lengths == max_row_length)[1]
    good_row <- unlist(raw_concentration[good_index])
    filler_row <- rep(0, max_row_length)
    names(filler_row) <- names(good_row)

    replace_empties <- function(x) {
      if (is.null(x)) {
        return(tibble::as_tibble_row(filler_row))
      } else {
        return(x)
      }
    }
    raw_concentration <- purrr::map(raw_concentration, replace_empties) |>
      purrr::list_rbind()

  } else {
    raw_concentration <- purrr::list_rbind(raw_concentration)
  }

  # Total number of cols is full width of counts and concentration, plus all
  # smps_cols
  width <- ncol(counts) + ncol(raw_concentration) + nrow(smps_cols)

  # inject the dataset name into the metadata in the right place
  set_name <- tibble(id = -1, site_record_id = -1, site_number = -1,
                     name = "Dataset Name", value = ds_value,
                     start_date = NA_real_, end_date = NA_real_, site_code = "")

  metadata <- insertRow2(metadata, set_name, 2)

  # Create the header
  padding <- paste0(rep(",", width - 3), collapse = "")
  metadata <- metadata |>
    mutate(header = paste0(name, ",", value, padding))
  header <- metadata$header

  # After the header are five rows of empty colums
  empty_pad <- paste0(padding, ",,")

  # The next row has the headers for the count and concentration data - the
  # first is after all of the smps cols, the second is after all the counts
  counts_name <- "Particle Concentration by Midpoint (nm)"
  conc_name <- "Raw Concentration by Midpoint (nm)"

  temp_pad <- paste0(rep(",", nrow(smps_cols)), collapse = "")
  counts_header <- paste0(temp_pad, counts_name)
  temp_pad <- paste0(rep(",", ncol(counts)), collapse = "")
  if (ncol(raw_concentration) > 0) {
    counts_header <- paste0(counts_header, temp_pad, conc_name)
    temp_pad <- paste0(rep(",", ncol(raw_concentration)-1), collapse = "")
    counts_header <- paste0(counts_header, temp_pad)
  }

  # Finally the data - Make one big data frame

  # If the aerosol_humidity and aerosol_temperature are all NaN, convert to text
  # "N/A" to look like the import.
  humidity_count <- length(which(!is.nan(df$aerosol_humidity)))
  temperature_count <- length(which(!is.nan(df$aerosol_temperature)))
  if (humidity_count == 0) {
    df <- mutate(df, aerosol_humidity = "N/A", .keep = "unused",
                 .after = total_concentration)
  }
  if (temperature_count == 0) {
    df <- mutate(df, aerosol_temperature = "N/A", .keep = "unused",
                 .after = aerosol_humidity)
  }

  # Format the date time to expected output
  df <- mutate(df, sample_start = strftime(sample_start, format = "%d/%m/%Y %T", tz = "UTC"),
               .keep = "unused", .after = scan_number)

  # For the operational data, first remove all of the db specific columns and
  # the json columns, then convert the names
  outdf <- df |>
    select(-id, -site_record_id, -site_number, -concentration_json,
           -raw_concentration_json, -site_code)
  names(outdf) <- smps_cols$file_column_name

  # Add the data - need to rename the raw concentration columns with an underscore
  if (ncol(raw_concentration) > 0) {
    conc_names <- paste0("_", names(raw_concentration))
    names(raw_concentration) <- conc_names
    outdf <- bind_cols(outdf, counts, raw_concentration)
  } else {
    outdf <- bind_cols(outdf, counts)
  }

  # Now we put it all together in line by line
  data_lines <- purrr::map(1:nrow(outdf), \(x) paste(outdf[x, ], collapse = ","))
  data_header <- paste(names(outdf), collapse = ",")

  c(header, rep(empty_pad, 5), counts_header, data_header, unlist(data_lines))

}


# Handy solution from SO: https://stackoverflow.com/a/11587051/5764101
insertRow2 <- function(existingDF, newrow, r) {
  existingDF <- rbind(existingDF,newrow)
  existingDF <- existingDF[order(c(1:(nrow(existingDF)-1),r-0.5)),]
  # Setting rownames on tibbles is deprecated
  #row.names(existingDF) <- 1:nrow(existingDF)
  return(existingDF)
}
