# Utilities for pulling data from the AQS API to match with ASCENT

#' get_aqs_data
#'
#' @param site
#' @param parameters
#' @param start_dt
#' @param end_dt
#' @param config_name
#'
#' @returns
#' @export
#'
#' @examples
get_aqs_data <- function(site, parameters, start_dt, end_dt, config_name) {

  # Can only request 1 year or less
  configs <- config::get("aqs")
  base <- "https://aqs.epa.gov/data/api/sampleData/bySite"

  bdate <- as.Date(start_dt)
  edate <- as.Date(end_dt)
  if (edate - bdate > 365) {
    stop("AQS allows max of 365 days at a time")
  }

  aqs_site <- aqs_sites[site]
  if (is.na(aqs_site)) {
    stop(site, " does not have a paired speciation site in AQS")
  }

  s <- stringr::str_split_fixed(aqs_site, "-", 3)

  param <- paste(aqs_parameters[parameters], collapse = ",")

  q <- httr::GET(base, query = list(email = configs$email, key = configs$key,
                                    bdate = strftime(bdate, "%Y%m%d"),
                                    edate = strftime(edate, "%Y%m%d"),
                                    state = s[1], county = s[2], site = s[3],
                                    duration = 7,
                                    param = param))
  txt <- httr::content(q, "text")
  jsn <- yyjsonr::read_json_str(txt)
  if (jsn$Header$rows < 1) {
    return(jsn$Header$status)
  }
  df <- jsn$Data

}

aqs_parameters <- c("chl"=88203, "nh4"=88301, "no3"=88306, "OMC"=88350, "so4"=88403,
                    "EC"=88321, "OC"=88320, "Fabs"=63102,
                    "PM25"=88502, "PM10"=85101, "RCM"=88401,
                    "Al"=88104, "Si"=88165, "P"=88152, "S"=88169, "Cl"=88115, "K"=88180,
                    "Ca"=88111, "Ti"=88161, "V"=88164, "Cr"=88112, "Mn"=88132, "Fe"=88126,
                    "Co"=88113, "Ni"=88136, "Cu"=88114, "Zn"=88167, "As"=88103, "Se"=88154,
                    "Br"=88109, "Rb"=88176, "Sr"=88168, "Zr"=88185, "Ag"=88166, "Cd"=88110,
                    "In"=88131, "Sn"=88160, "Sb"=88102, "Cs"=88118, "Ba"=88107, "Ce"=88117,
                    "Pb"=88128)

aqs_sites <- c("LookRock"="47-009-0101", "CheekaPeak"="53-009-0013",
               "Rubidoux"="06-065-8001", "JoshuaTree"="06-071-9002",
               "Yellowstone"="56-039-9000", "LaCasa"="08-031-0026",
               "Lawrenceville"="42-003-0008", "QueensCollege"="36-081-0124",
               "SouthDeKalb"="13-089-0002")
