#' importante UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_importante_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    tags$h3("Consideraciones para el uso correcto de la aplicación"),
    tags$span(
      "Este Dashboard esta elaborado con el objetivo de apoyar la elaboración del informe de experto referido al PAS150. Por favor considerar todas las intrucciones y consejos mencionados en esta página para evitar errores"
    ),
    tags$br(),
    tags$ul(
      tags$li(
        "Uno de los inputs más importante es la capa de cartorgafía de uso y vegetación. Por lo cual es importante que los datos que se ingresen no contenga errores. Para evitar algunos tipos de errores en la ventana de 'Check.cartografía' se muestran algunos resumenes de los datos para facilitar la detección de errores."
      )
    )

  )
}
    
#' importante Server Functions
#'
#' @noRd 
mod_importante_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_importante_ui("importante_1")
    
## To be copied in the server
# mod_importante_server("importante_1")
