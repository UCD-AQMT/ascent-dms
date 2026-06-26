# Some test code for uploading data to NCAR GDEX

# library(curl)
#
# ?curl_upload
#
# h <- curl::new_handle()
# curl::handle_setheaders(
#   h,
#   "API-Key" = config::get("gdex")$api_key,
#   "Content-type" = "multipart/form-data"
# )
# curl::handle_setform(
#   h,
#   file = curl::form_file("c:/Users/sraffuse/OneDrive - University of California, Davis/Documents/ASCENT/DataExport/ASCENT_SMPS_CheekaPeak_20240701_20240731_L1a.zip"),
#   path = "test/SMPS/ASCENT_SMPS_CheekaPeak_20240701_20240731_L1a.zip"
# )
#
#
# req <- curl::curl_fetch_memory("https://api.gdex.ucar.edu/upload/", handle = h)
#
# jsonlite::prettify(rawToChar(req$content))
#
# # In think this worked, but can't tell
#
#
# # List Files:
# #   curl -H “API-Key: <api_key>” “https://api.gdex.ucar.edu/upload/?list=<path>”
#
# h2 <- curl::new_handle()
# curl::handle_setheaders(h2, "API-Key" = config::get("gdex")$api_key)
#
# r2 <- curl::curl_fetch_memory("https://api.gdex.ucar.edu/upload/?list=test/SMPS", handle = h2)
# rawToChar(r2$content)
# # works!

### This all worked with curl, can I do it with httr2?

### Yep, below works as well.... let's work from this

# library(httr2)
#
# url <- "https://api.gdex.ucar.edu/upload/"
# key <- config::get("gdex")$api_key
#
# gdex_path <- "test/AE33/ASCENT_AE33_CheekaPeak_20240101_20240131_L1a.zip"
# file_path <- "C:/Users/sraffuse/OneDrive - University of California, Davis/Documents/ASCENT/DataExport/ASCENT_AE33_CheekaPeak_20240101_20240131_L1a.zip"
#
# req <- request(url)
# req <- req |>
#   req_headers(
#     `api-key` = config::get("gdex")$api_key,
#     `content-type` = "multipart/form-data"
#   )
#
# req <- req |>
#   req_body_multipart(path = gdex_path, file = curl::form_file(file_path, type = "application/zip"))
#
# resp <- try(req |> req_perform())

gdex_upload <- function(local_name, gdex_name, api_key = gdex_get_api_key()) {

  httr2::request("https://api.gdex.ucar.edu/upload/") |>
    httr2::req_headers(
      `api-key` = api_key,
      `content-type` = "multipart/form-data"
    ) |>
    httr2::req_body_multipart(
      path = gdex_name,
      file = curl::form_file(local_name, type = "application/zip")
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_html()

}

gdex_delete <- function(gdex_name, api_key = gdex_get_api_key()) {
  
  httr2::request("https://api.gdex.ucar.edu/unlink/") |>
    httr2::req_headers(
      `api-key` = api_key,
      `content-type` = "multipart/form-data"
    ) |>
    httr2::req_body_multipart(
      path = gdex_name
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
  
}

# Get a list if files or folders in the GDEX archive
gdex_ls <- function(path = ".", echo = TRUE, api_key = gdex_get_api_key()) {
  resp <- httr2::request("https://api.gdex.ucar.edu/upload/") |>
    httr2::req_headers(`api-key` = api_key) |>
    httr2::req_url_query(list = path) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
  if (echo) {
    cat(resp)  
  }
  resp
}

# Create a list of full path filenames for a gdex path (nonrecursive)
gdex_ls_path <- function(path = ".", api_key = gdex_get_api_key()) {
  
  entries <- unlist(strsplit(gdex_ls(path = path, echo = FALSE, api_key), "\n"))
  
  # Folders end in forward slash, files do not
  files <- entries[!grepl("/", entries)]
  
  if (length(files) == 0) {
    warning("No files in path: ", path)
    return(NULL)
  } else {
    # remove the size and date info and isolate the file names - the last part after all spaces
    s <- strsplit(files, " ")
    l <- length(s[[1]])
    f <- purrr::map_chr(s, \(x) paste0(path, "/", x[l]))
    
  }
  
}

gdex_get_api_key <- function() {
  key <- Sys.getenv("GDEX_KEY")
  if (identical(key, "")) {
    stop("No API key found, please supply with `api_key` argument or with GDEX_KEY env var")
  }
  key
}

gdex_set_api_key <- function(key = NULL) {
  if (is.null(key)) {
    key <- askpass::askpass("Please enter your API key")
  }
  Sys.setenv("GDEX_KEY" = key)
}
