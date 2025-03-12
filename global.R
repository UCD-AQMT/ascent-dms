
library(shiny)
library(shinydashboard)
library(bslib)
library(dplyr)
library(ggplot2)
library(dbplyr)
library(influxdbclient)
library(plotly)
library(shinycssloaders)

# Connection (dataconnection for production, dataconnection_dev for dev)
datacon <- "dataconnection"
args <- config::get(datacon)
con <- pool::dbPool(RPostgres::Postgres(),
                    dbname = args$dbname,
                    user = args$user,
                    password = args$password,
                    host = args$host,
                    port = args$port
                    )


# Keeping the test site for testing - remove on deploy
site_list <- tbl(con, I("common.sites")) |>
  filter(site_code != "Test") |>
  pull(site_code)

# This is for influxdb
read_token <- args$influx_read_token


acsm_colors <- c("chl"="violet", "nh4"="goldenrod1", "no3"="dodgerblue",
                 "org"="lightgreen", "so4"="tomato", "Xact (minus S)"="darkgray")

# Some formatting functions for plotly HTML
ugm3 <- function() {
  paste0("(", intToUtf8(0x03BC), "g/m<sup>3</sup>)")
}

# Shutdown Chores
onStop(function() {
  pool::poolClose(con)
})