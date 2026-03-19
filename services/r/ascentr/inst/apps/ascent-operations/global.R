
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

# Set themes for all plots
thematic_shiny(font = "auto")
theme_set(theme_minimal(base_size = 14))

# Use a spinner that doesn't bounce the screen
options(spinner.type = 7)

# Enable bookmarking
enableBookmarking(store = "url")

# Read Xact insrument settings from CSV (until in database)
xact_settings <- read.csv("instrument_settings.csv") |>
  mutate(start_date = as.POSIXct(start_date, format = "%m/%d/%Y"),
         end_date = as.POSIXct(end_date, format = "%m/%d/%Y"),
         end_date = if_else(is.na(end_date), Sys.time(), end_date))

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

# Convert the AE33 STinst field from decimal to bits and return the statuses. These are
# described in the AE33 user's manual ver 1.59, page 53
parse_ae33_flags <- function(x) {
  
  # Have to convert to int first because influxdb returns an int64 which is not properly
  # converted by intToBits
  bits <- as.integer(intToBits(as.integer(x)))

  # Bits 0 and 1, operation
  chunk <- paste0(bits[1:2], collapse = "")
  operation <- case_match(chunk,
                          "00" ~ "OK:Taking measurement",
                          "01" ~ "info:Tape advance (fast cal., warm-up)",
                          "10" ~ "info:First measurement - obtaining ATN0",
                          "11" ~ "error:Stopped"
                          )
  operation <- unlist(strsplit(operation, split = ":", fixed = TRUE))
  
  # Bits 2 and 3, flow
  chunk <- paste0(bits[3:4], collapse = "")
  flow <- case_match(chunk,
                     "00" ~ "OK:Flow OK",
                     "01" ~ "warning:Flow low/high by more than 0.5 LPM or F1 < 0 or F2/F1 outside 0.2 - 0.75 range",
                     "10" ~ "warning:Check flow status history",
                     "11" ~ "warning:Flow low/high & check flow status history"
                     )
  flow <- unlist(strsplit(flow, split = ":", fixed = TRUE))
  
  # Bits 4 and 5, optical source
  chunk <- paste0(bits[5:6], collapse = "")
  optical <- case_match(chunk,
                        "00" ~ "OK:LEDs OK",
                        "01" ~ "info:Calibrating LED",
                        "10" ~ "warning:Calibration error (at least one channel OK)",
                        "11" ~ "error:LED error (all channels calibration error, COM error)"
                        )
  optical <- unlist(strsplit(optical, split = ":", fixed = TRUE))
  
  # Bit 6, chamber
  chunk <- as.character(bits[7])
  chamber <- case_match(chunk,
                        "0" ~ "OK:Chamber OK",
                        "1" ~ "error:Chamber error")
  chamber <- unlist(strsplit(chamber, split = ":", fixed = TRUE))
  
  # Bits 7 and 8, filter tape
  chunk <- paste0(bits[8:9], collapse = "")
  tape <- case_match(chunk,
                     "00" ~ "OK:Filter tape OK",
                     "01" ~ "warning:Tape warning (less than 30 spots left)",
                     "10" ~ "warning:Tape last warning (less than 5 spots left)",
                     "11" ~ "error:Tape error (tape not moving, end of tape)"
                     )
  tape <- unlist(strsplit(tape, split = ":", fixed = TRUE))
  
  # Bit 9, setup file
  chunk <- as.character(bits[10])
  setup <- case_match(chunk,
                      "0" ~ "OK:Setup file OK",
                      "1" ~ "warning:Setup warning")
  setup <- unlist(strsplit(setup, split = ":", fixed = TRUE))
  
  # Bits 10, 11, and 12, tests and procedures
  chunk <- paste0(bits[11:13], collapse = "") 
  tests <- case_match(chunk,
                      "000" ~ "OK:No test",
                      "001" ~ "info:Stability test",
                      "010" ~ "info:Clean air test",
                      "011" ~ "info:Change tape procedure",
                      "100" ~ "info:Optical test",
                      "110" ~ "info:Leakage test",
                      .default = "error:unknown test"
                      )
  tests <- unlist(strsplit(tests, split = ":", fixed = TRUE))
  
  # Bit 13, external device
  chunk <- as.character(bits[14])
  external_device <- case_match(chunk,
                                "0" ~ "OK:Connection OK",
                                "1" ~ "error:Connection Error")
  external_device <- unlist(strsplit(external_device, split = ":", fixed = TRUE))
  
  # Bit 14, auto clean air test status
  chunk <- as.character(bits[15])
  clean_air <- case_match(chunk,
                                "0" ~ "OK:Status OK or test not run yet",
                                "1" ~ "warning:Result of clean air test is not acceptable.\nService of the instrument is recommended")
  clean_air <- unlist(strsplit(clean_air, split = ":", fixed = TRUE))
  
  # Bit 15, CF card failure
  chunk <- as.character(bits[16])
  cf_card <- case_match(chunk,
                        "0" ~ "OK:CF card OK",
                        "1" ~ "error:Problem while saving or retrieving files to/from CF card")
  cf_card <- unlist(strsplit(cf_card, split = ":", fixed = TRUE))
  
  # Bit 16, database status
  chunk <- as.character(bits[17])
  database <- case_match(chunk,
                         "0" ~ "OK:Database status OK",
                         "1" ~ "warning:Database reaching limit warning (6e6 lines)")
  database <- unlist(strsplit(database, split = ":", fixed = TRUE))
  
  list(operation=operation, flow=flow, optical=optical, chamber=chamber, tape=tape,
       setup=setup, tests=tests, external_device=external_device, clean_air=clean_air,
       cf_card=cf_card,  database=database)
  
}
