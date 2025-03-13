
function(request) {
  dashboardPage(
    dashboardHeader(title = "ASCENT Operations"),
    dashboardSidebar(
      sidebarMenu(
        id = "tab",
        menuItem("Network", tabName = "network"),
        menuItem("Site View", tabName = "site"),
        selectInput("site_all", "Site", choices = site_list),
        menuItem("ACSM", tabName = "acsm"),
        menuItem("Xact", tabName = "xact"),
        menuItem("SMPS", tabName = "smps"),
        menuItem("AE33", tabName = "ae33"),
        menuItem("Purple Air", tabName = "pa")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "network", networkUI("network")),
        tabItem(tabName = "site", siteUI("site")),
        tabItem(tabName = "acsm", acsmUI("acsm")),
        tabItem(tabName = "xact", xactUI("xact")),
        tabItem(tabName = "smps", smpsUI("smps")),
        tabItem(tabName = "ae33", ae33UI("ae33")),
        tabItem(tabName = "pa", paUI("pa"))
      )
    )
  )  
}

