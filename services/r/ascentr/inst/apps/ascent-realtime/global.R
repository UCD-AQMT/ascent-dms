
library(shiny)
library(bslib)
library(dplyr)
library(dbplyr)
library(influxdbclient)
library(bsicons)
library(units)
library(thematic)
library(shinycssloaders)
library(ggplot2)

# testing
library(ascentr)

# Minimum date for data viewing
minimum_date <- "2025-01-01"

# Set themes for all plots
# thematic_shiny seems to be breaking ggplot::geom_sf!! Try to make a reprex
thematic::thematic_shiny(font = "auto")
theme_set(theme_minimal(base_size = 18))

# Use a spinner that doesn't bounce the screen
options(spinner.type = 7)

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
  arrange(site_number) |>
  pull(site_code)

site_names <- tbl_sites |>
  filter(site_code != "Test") |>
  select(site_number, site_code, site_name) |>
  collect()


# This is for influxdb
ae33_con <- InfluxDBClient$new(url = "https://eastus-1.azure.cloud2.influxdata.com",
                                  token = args$influx_read_token,
                                  org = "ascent")


acsm_colors <- c("chl"="violet", "nh4"="goldenrod1", "no3"="dodgerblue",
                 "org"="lightgreen", "so4"="tomato", "Xact (minus S)"="darkgray",
                 "BC"="black")


# Shutdown Chores
onStop(function() {
  pool::poolClose(con)
})

ae33_channels <- tibble(parameter = c("EBC_1", "EBC_2", "EBC_3", "EBC_4", "EBC_5", "EBC_6", "EBC_7"),
                        wavelength = c(370, 470, 520, 590, 660, 880, 950))
