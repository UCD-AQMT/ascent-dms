
# Given a column name, the data frame, and a data frame describing units, attach the units
# to the column
attach_units <- function(col, df, df_fields) {

  unit_str <- df_fields$unit[df_fields$param == col]
  vec <- pull(df, col)

  if (!is.na(unit_str) & unit_str != "" & !lubridate::is.POSIXct(vec)) {

    with_units <- units::set_units(vec, unit_str, mode = "standard")

    this_row <- df[,col]
    this_row[,1] <- with_units
    ret <- this_row

  } else {
    ret <- df[,col]
  }
  ret
}


# Given a column, return a unit suffix to append to the field name
get_unit_suffix <- function(x) {

  # if this is not a field with a units class, skip
  if (is(x, "units")) {

    u <- units(x)

    # Separate any compounds units with dashes (e.g., Pa-s)
    if (length(u$numerator) > 0 & length(u$denominator) > 0) {
      suffix <- paste0("_", paste(u$numerator, collapse = "-"), "_",
                       paste(u$denominator, collapse = "-"))
    } else if (length(u$numerator) > 0) {
      suffix <- paste0("_", paste(u$numerator, collapse = "-"))
    } else if (length(u$denominator) > 0) {
      suffix <- paste0("_1_", paste(u$denominator, collapse = "-"))
    } else {
      suffix <- ""
    }
  } else {
    suffix <- ""
  }
  return(suffix)

}

# given a column, return units text within parantheses for metadata
get_unit_paren <- function(x) {


  # if this is not a field with a units class, skip
  if (is(x, "units")) {

    u <- units(x)

    # Separate any compounds units with dashes (e.g., Pa-s)
    if (length(u$numerator) > 0 & length(u$denominator) > 0) {
      suffix <- paste0(" (", paste(u$numerator, collapse = "-"), "/",
                       paste(u$denominator, collapse = "-"), ")")
    } else if (length(u$numerator) > 0) {
      suffix <- paste0(" (", paste(u$numerator, collapse = "-"), ")")
    } else {
      suffix <- paste0(" (1/", paste(u$denominator, collapse = "-"), ")")
    }
  } else {
    suffix <- ""
  }
  return(suffix)

}

# taken from SO: https://stackoverflow.com/questions/13673894/suppress-nas-in-paste
clean_paste <- function(x, collapse) {
  paste(x[!is.na(x)], collapse = collapse)
}

clean_paste_sep <- function(x, sep) {
  paste(x[!is.na(x)], sep = sep)
}
