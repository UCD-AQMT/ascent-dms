
paUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    selectInput(ns("site"), "Site", choices = "UCD"),
    box("Purple Air")
  )
  
}

paServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}