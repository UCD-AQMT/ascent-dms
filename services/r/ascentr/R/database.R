
#' Connect to the ASCENT Postgres database
#'
#' @param config_name Name of the `config.yml` configuration block holding
#'   the database `dbname`, `user`, `password`, `host`, and `port`
#' @param ... Further arguments passed to `config::get()`
#'
#' @returns A `DBIConnection` object connected to the ASCENT Postgres
#'   database
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

#' Connect to the ASCENT InfluxDB (Flux) instance
#'
#' @param config_name Name of the `config.yml` configuration block holding
#'   the `influx_read_token`
#'
#' @returns An `InfluxDBClient` connected to the ASCENT InfluxDB instance
#' @export
#'
#' @examples
get_flux_client <- function(config_name) {
  args <- config::get(config_name)
  influxdbclient::InfluxDBClient$new(url = "https://eastus-1.azure.cloud2.influxdata.com",
                     token = args$influx_read_token,
                     org = "ascent")
}
