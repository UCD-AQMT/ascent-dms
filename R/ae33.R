
ae33UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    selectInput(ns("site"), "Site", choices = "UCD"),
    box("AE33")
  )
  
}

ae33Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}