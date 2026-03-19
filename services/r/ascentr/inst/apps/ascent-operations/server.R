
shinyServer(function(input, output, session) {
  
  
  # bs_themer()
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['site']])) {
      updateSelectInput(session = session, 
                        inputId = "site_all",
                        selected = query[['site']])
    }
    if (!is.null(query[['tab']])) {
      updateTabItems(session = session,
                     inputId = "tab",
                     selected = query[['tab']])
    }
  })
  
  # Listen for the value returned by clicking on a cell on the Network page
  observe({
    if (!is.null(network())) {
      if (length(network() > 0)) {
    
        cl <- network()
        # If first column is clicked, go to the site view for the clicked site
        if (cl$col == 0) {
          nav_select(session = session,
                         id = "tab",
                         selected = "site")
          updateSelectInput(session = session, 
                            inputId = "site_all",
                            selected = site_list[cl$row])
        } else {
          # Otherwise, the use clicked on a specific instrument, so go to that page
          cols <- c("acsm", "ae33", "smps", "xact", "pa")
          tab_selected <- cols[cl$col]
          site_selected <- site_list[cl$row]
          nav_select(session = session,
                         id = "tab",
                         selected = tab_selected)
          updateSelectInput(session = session, 
                            inputId = "site_all",
                            selected = site_selected)
        }
      }
    }
    
    
  })
  
  network <- networkServer("network")
  site <- siteServer("site", site = reactive(input$site_all))
  acsm <- acsmServer("acsm", site = reactive(input$site_all))
  xact <- xactServer("xact", site = reactive(input$site_all))
  ae33 <- ae33Server("ae33", site = reactive(input$site_all))
  smps <- smpsServer("smps", site = reactive(input$site_all))
  pa <- paServer("pa", site = reactive(input$site_all))
  
})