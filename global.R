
library(shiny)
library(shinydashboard)
library(bslib)
library(dplyr)
library(ggplot2)
library(dbplyr)
library(influxdbclient)
library(plotly)
library(shinycssloaders)
library(patchwork)
library(thematic)
library(DT)

# Set themes for all plots
thematic_shiny(font = "auto")
theme_set(theme_minimal(base_size = 16))

# Enable bookmarking
enableBookmarking(store = "url")

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

# Common tables
tbl_sites <- tbl(con, I("common.sites"))

# Keeping the test site for testing - remove on deploy
site_list <- tbl_sites |>
  filter(site_code != "Test") |>
  pull(site_code)

# This is for influxdb
ae33_client <- InfluxDBClient$new(url = "https://eastus-1.azure.cloud2.influxdata.com",
                                  token = args$influx_read_token,
                                  org = "ascent")

# Rural sites have slower Xact data
rural_sites <- c("DeltaJunction", "Yellowstone", "LookRock",
                 "CheekaPeak", "JoshuaTree")

acsm_colors <- c("chl"="violet", "nh4"="goldenrod1", "no3"="dodgerblue",
                 "org"="lightgreen", "so4"="tomato", "Xact (minus S)"="darkgray")

# Some formatting functions for plotly HTML
ugm3 <- function() {
  paste0("(", intToUtf8(0x03BC), "g/m<sup>3</sup>)")
}
ctscm3 <- function() {
  paste0("(cts/cm<sup>3</sup>)")
}

# Shutdown Chores
onStop(function() {
  pool::poolClose(con)
})