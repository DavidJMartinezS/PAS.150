#' read_sf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_read_sf_ui <- function(id, label, required = FALSE, ...) {
  ns <- NS(id)
  
  display_label <- if (required) {
    with_red_star(label)
  } else {
    label
  }

  tags$div(
    shiny::fileInput(
      ns("sf_file"),
      label = display_label,
      accept = c('.shp','.dbf','.shx',".prj"),
      multiple = TRUE,
      buttonLabel = "Seleccionar",
      placeholder = "Archivo no seleccionado",
      ...
    ),
    tags$div(style = "margin-top: -10px")
  )
}
    
#' read_sf Server Functions
#'
#' @noRd 
mod_read_sf_server <- function(id, rv, i, fx = NULL, path = F, geometry_type = "POLYGON", ...){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$sf_print <- renderPrint({input$sf_file})
    observeEvent(input$sf_file, {
      shp <- sf$new(sf_file = input$sf_file, fx = fx, path = path, geometry_type = geometry_type)
      if(shp$validate_sf()){
        rv[[i]] <- shp$leer_sf(...)
      } else {
        print("invalidate")
        shp$invalidate_sf()
        rv[[i]] <- NULL
      }
    })
  })
}
    
## To be copied in the UI
# mod_read_sf_ui("read_sf_1")
    
## To be copied in the server
# mod_read_sf_server("read_sf_1")
