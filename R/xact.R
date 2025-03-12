
xactUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    selectInput(ns("site"), "Site", choices = "UCD"),
    box("Xact")
  )
  
}

xactServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}