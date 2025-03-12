
smpsUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    selectInput(ns("site"), "Site", choices = "UCD"),
    box("SMPS")
  )
}

smpsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}