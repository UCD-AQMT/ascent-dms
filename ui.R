
function(request) {
  page_navbar(
    title = "ASCENT Operations",
    id = "tab",
    header = tags$head(tags$style(HTML(".bslib-value-box .value-box-value {font-size: 1.2rem;}")),
                       tags$style(HTML(".bslib-value-box .value-box-showcase {padding: 0rem; font-size: 1rem;}")),
                       tags$style(HTML("* {font-size: 0.9rem;}"))),
    nav_panel("Network", value = "network", networkUI("network")),
    nav_panel("Site View", value = "site", siteUI("site")),
    nav_panel("ACSM", value = "acsm", acsmUI("acsm")),
    nav_panel("AE33", value = "ae33", ae33UI("ae33")),
    nav_panel("SMPS", value= "smps", smpsUI("smps")),
    nav_panel("Xact", value = "xact", xactUI("xact")),
    nav_panel("Purple Air", value = "pa", paUI("pa")),
    nav_spacer(),
    nav_item(selectInput("site_all", "", choices = site_list)),
    nav_item(input_dark_mode(id = "mode"))
  )
}

