#' alteracion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_alteracion_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' alteracion Server Functions
#'
#' @noRd 
mod_alteracion_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_alteracion_ui("alteracion_1")
    
## To be copied in the server
# mod_alteracion_server("alteracion_1")
