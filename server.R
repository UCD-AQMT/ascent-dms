
shinyServer(function(input, output, session) {
  
  network <- networkServer("network")
  site <- siteServer("site", site = reactive(input$site_all))
  acsm <- acsmServer("acsm", site = reactive(input$site_all))
  # xact <- xactServer("xact")
  # ae33 <- ae33Server("ae33")
  # smps <- smpsServer("smps")
  # pa <- paServer("pa")
  
})