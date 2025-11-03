#' bases_carto UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bases_carto_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::input_switch(
      ns("add_cam"),
      bslib::tooltip(
        trigger = list(
          "¿Crear capa de caminos?",
          bsicons::bs_icon("info-circle", class = "text-info")
        ),
        "Caminos serán creados a partir de la red vial del MOP actualizado al 07-02-2024"
      ),
      value = FALSE
    ),
    tags$div(style = "margin-top: -5px"),
    bslib::input_switch(
      id = ns("add_hidro"),
      label = bslib::tooltip(
        trigger = list(
          "¿Crear capa de Hidrografía?",
          bsicons::bs_icon("info-circle", class = "text-info")
        ),
        "Hidrografía será creada a partir de la hidrografía subida a Geoportal actualizada al 31-12-2022"
      ),
      value = FALSE
    ),
    conditionalPanel(
      condition = "input.add_hidro == true",
      ns = ns,
      tags$div(
        id = "inline",
        tags$div(style = "margin-top: -5px"),
        shinyWidgets::pickerInput(
          inputId = ns("fuente_hidro"),
          label = "Fuente",
          choices = c("MOP", "BCN"),
          selected = "MOP",
          options = shinyWidgets::pickerOptions(
            container = "body",
            style = "btn-outline-primary btn-sm"
          )
        ),
        tags$div(style = "margin-bottom: 5px")
      )
    ),
    # tags$div(style = "margin-top: -5px"),
    bslib::input_switch(
      ns("add_CN"),
      bslib::tooltip(
        trigger = list(
          "¿Crear capa de curvas de nivel?",
          bsicons::bs_icon("info-circle", class = "text-info")
        ),
        "Ingresar DEM. De lo contrario se realizará con modelo ALOS PALSAR de datos globales de OpenTopography API"
      ), 
      value = FALSE
    ),
    conditionalPanel(
      condition = "input.add_CN == true",
      ns = ns,
      tags$div(
        class = "btn-sm",
        tags$div(style = "margin-top: -5px"),
        fileInput(
          inputId = ns("dem"),
          label = "Ingresar DEM", 
          accept = c(".tif", ".tiff"), 
          buttonLabel = "Seleccionar",
          placeholder = "Archivo no seleccionado"
        )
      )
    )
  )
}
    
#' bases_carto Server Functions
#'
#' @noRd 
mod_bases_carto_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    bind_events(
      ids = c("add_cam", "add_hidro", "add_CN", "fuente_hidro", "dem"), 
      rv = rv, 
      parent_input = input
    )

  })
}
    
## To be copied in the UI
# mod_bases_carto_ui("bases_carto_1")
    
## To be copied in the server
# mod_bases_carto_server("bases_carto_1")
