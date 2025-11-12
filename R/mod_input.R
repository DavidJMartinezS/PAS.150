#' input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_ui <- function(id) {
  ns <- NS(id)
  bslib::sidebar(
    title = "Inputs",
    width = 420,
    bslib::accordion(
      bslib::accordion_panel(
        "Cartográficos",
        icon = bsicons::bs_icon("menu-app"),
        mod_basic_input_ui(ns("basic_input")),
        tags$hr(),
        tags$h5("Datos de terreno"),
        mod_parcelas_ui(ns("bd_parcelas")),
        # uiOutput(ns("densidad_ui")),
        tags$hr(),
        tags$h5("Bases cartográficas"),
        mod_bases_carto_ui(ns("bases_carto")),
        mod_get_down_carto_ui(ns("get_down_carto"))
      ),
      bslib::accordion_panel(
        "Apéndices bases de datos",
        icon = bsicons::bs_icon("file-spreadsheet"),
        mod_apendices_ui(ns("apendices"))
      )
    ),
    tags$img(
      src = "www/favicon.ico",
      width = "50%",
      height = "auto",
      style = "align: center; display: block; margin-left: auto; margin-right: auto;"
    )
  )
}
    
#' input Server Functions
#'
#' @noRd 
mod_input_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Basic_inputs
    mod_basic_input_server("basic_input", rv = rv)
    # BD parcelas
    mod_parcelas_server("bd_parcelas", rv = rv)
    # bases carto
    mod_bases_carto_server("bases_carto", rv = rv) 
    # Cartografia digital
    mod_get_down_carto_server("get_down_carto", rv = rv)
    # Apéndices
    mod_apendices_server("apendices", rv = rv)
  })
}
    
## To be copied in the UI
# mod_input_ui("input_1")
    
## To be copied in the server
# mod_input_server("input_1")
