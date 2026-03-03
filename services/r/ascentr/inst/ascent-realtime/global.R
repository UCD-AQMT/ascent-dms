
library(shiny)
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
library(bsicons)
# library(ascentr)
library(units)

# Set themes for all plots
thematic_shiny(font = "auto")
theme_set(theme_minimal(base_size = 14))

# Use a spinner that doesn't bounce the screen
options(spinner.type = 7)

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
  arrange(site_number) |>
  pull(site_code)

site_names <- tbl_sites |>
  filter(site_code != "Test") |>
  select(site_number, site_code, site_name) |>
  collect()


# This is for influxdb
ae33_client <- InfluxDBClient$new(url = "https://eastus-1.azure.cloud2.influxdata.com",
                                  token = args$influx_read_token,
                                  org = "ascent")

# Rural sites have slower Xact data
rural_sites <- c("DeltaJunction", "Yellowstone", "LookRock",
                 "CheekaPeak", "JoshuaTree")

acsm_colors <- c("chl"="violet", "nh4"="goldenrod1", "no3"="dodgerblue",
                 "org"="lightgreen", "so4"="tomato", "Xact (minus S)"="darkgray",
                 "BC"="black")

# Some formatting functions for plotly HTML
ugm3 <- function() {
  paste0("(", intToUtf8(0x03BC), "g/m<sup>3</sup>)")
}
ctscm3 <- function() {
  paste0("(cts/cm<sup>3</sup>)")
}
ngm3 <- function() {
  "(ng/m<sup>3</sup>)"
}

# Shutdown Chores
onStop(function() {
  pool::poolClose(con)
})

# These are too slow to get dynamically from the influx database
ae33_fields <- c("BB", "EBC1_1",
                 "EBC1_2", "EBC1_3", "EBC1_4", "EBC1_5", "EBC1_6", "EBC1_7", "EBC2_1",
                 "EBC2_2", "EBC2_3", "EBC2_4", "EBC2_5", "EBC2_6", "EBC2_7", "EBC_1",
                 "EBC_2", "EBC_3", "EBC_4", "EBC_5", "EBC_6", "EBC_7", "STcnt", "STdet", 
                 "STinst",  "STled", "STvalv", "T_LED", "Tcntrl", "Tsupply", "att1_1", 
                 "att1_2", "att1_3", "att1_4", "att1_5", "att1_6", "att1_7", "att2_1",
                 "att2_2", "att2_3", "att2_4", "att2_5", "att2_6", "att2_7", "flow1",
                 "flow2", "flowC", "k_1", "k_2", "k_3", "k_4", "k_5", "k_6", "k_7",
                 "numflag", "ref_1", "ref_2", "ref_3", "ref_4", "ref_5", "ref_6", "ref_7",
                 "refpress", "reftemp", "sens1_1", "sens1_2", "sens1_3", "sens1_4",
                 "sens1_5", "sens1_6", "sens1_7", "sens2_1", "sens2_2", "sens2_3", 
                 "sens2_4", "sens2_5", "sens2_6", "sens2_7", "tpcnt")
