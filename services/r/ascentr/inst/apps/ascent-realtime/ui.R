
function(request) {
  page_navbar(
    title = "ASCENT Realtime",
    id = "tab",
    header = tags$head(tags$style(HTML(".bslib-value-box .value-box-value {font-size: 1.2rem;}")),
                       tags$style(HTML(".bslib-value-box .value-box-showcase {padding: 0rem; font-size: 1rem;}")),
                       tags$style(HTML("* {font-size: 0.9rem;}"))),
    nav_panel("Network", value = "network", networkUI("network")),
    nav_panel("Site View", value = "site", siteUI("site")),
    nav_panel("Download", value = "download", downloadUI("download")),
    nav_item(input_dark_mode(id = "mode"))
  )
}

