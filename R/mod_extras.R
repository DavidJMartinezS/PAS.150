#' extras UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_extras_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::navset_card_underline(
      id = "navset_extras",
      bslib::nav_panel(
        title = "Descargas de capas y KMZ",
        bslib::layout_column_wrap(
          mod_ext_capas_carto_ui(ns("capas_carto")),
          mod_ext_supha_kmz_ui(ns("supha_kmz"))
        )
      ),
      bslib::nav_panel(
        title = "Alteración de hábitat",
        tags$h4("Proximamente...")
      )
    )
  )
}
    
#' extras Server Functions
#'
#' @noRd 
mod_extras_server <- function(id, rv) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    mod_ext_capas_carto_server(id = "capas_carto", rv = rv)
    mod_ext_supha_kmz_server(id = "supha_kmz", rv = rv)
  })
}
    
## To be copied in the UI
# mod_extras_ui("extras_1")
    
## To be copied in the server
# mod_extras_server("extras_1")
