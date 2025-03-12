
networkUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    box("Network"),
    p("Home")
  )
  

}

networkServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    mtcars
    
  })
}