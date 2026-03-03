# Upload monthly L1a files to gdex

#library(ascentr)
library(dplyr)
library(fs)
local_path <- "c:/Users/sraffuse/OneDrive - University of California, Davis/Documents/ASCENT/DataExport/"


# For now, only doing 2024

# AE33 L1a files
ae33 <- dir_ls(local_path, regexp = ".+AE33.+_2024.+L1a.+")
gdex_path <- "AE33/L1a"

upload_file <- function(file) {
  filename <- path_file(file)
  gdex_name <- path_join(c(gdex_path, filename))
  gdex_upload(file, gdex_name)
}

res <- purrr::map(ae33, upload_file, .progress = TRUE)

# Xact L1a files
xact <- dir_ls(local_path, regexp = ".+Xact.+_2024.+L1a.+")
gdex_path <- "Xact/L1a"
res <- purrr::map(xact, upload_file, .progress = TRUE)

# SMPS L1a files
smps <- dir_ls(local_path, regexp = ".+SMPS.+_2024.+L1a.+")
gdex_path <- "SMPS/L1a"
res <- purrr::map(smps, upload_file, .progress = TRUE)
