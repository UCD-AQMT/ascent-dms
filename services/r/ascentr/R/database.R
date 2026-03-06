
#' Title
#'
#' @param config_name
#'
#' @returns
#' @export
#'
#' @examples
get_db_connection <- function(config_name, ...) {
  args <- config::get(config_name, ...)
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = args$dbname,
                        user = args$user,
                        password = args$password,
                        host = args$host,
                        port = args$port
  )
}

#' Title
#'
#' @param config_name
#'
#' @returns
#' @export
#'
#' @examples
get_flux_client <- function(config_name) {
  args <- config::get(config_name)
  influxdbclient::InfluxDBClient$new(url = "https://eastus-1.azure.cloud2.influxdata.com",
                     token = args$influx_read_token,
                     org = "ascent")
}
