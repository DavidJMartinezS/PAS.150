#' view_table UI Function
#'
#' Módulo genérico para visualizar la tabla de atributos de un objeto 'sf'
#' en un modal, usando DT. Pensado para reutilizarse en varios botones "Ver".
#'
#' @param id,input,output,session Parámetros internos de {shiny}.
#' @param icon Ícono del botón.
#' @param class Clase css del botón.
#'
#' @noRd
mod_view_table_ui <- function(id, label = "", icon = bsicons::bs_icon("table"), class = "btn-secondary") {
  ns <- NS(id)
  shinyjs::disabled(
    actionButton(ns("view_btn"), tagList(icon, label), class = class)
  )
}
    
#' view_table Server Functions
#'
#' @noRd 
mod_view_table_server <- function(id, x, title = "Tabla de atributos"){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observe({
      if (isTruthy(x())) shinyjs::enable("view_btn") else shinyjs::disable("view_btn")
    })

    observeEvent(input$view_btn, {
      req(x())
      showModal(modalDialog(
        title = title,
        DT::DTOutput(ns("dt_table")),
        size = "xl",
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
    })

    output$dt_table <- DT::renderDT({
      req(x())
      DT::datatable(
        x() %>%
          sf::st_drop_geometry() %>%
          dplyr::mutate_if(is.character, as.factor),
        class = "cell-border stripe nowrap",
        rownames = FALSE,
        filter = "top",
        editable = "cell",
        options = list(
          dom = 't',
          pageLength = 100,
          scrollX = TRUE,
          scroller = TRUE
        )
      ) %>% 
        DT::formatStyle(
          columns = 1:5, 
          padding = '5px 4px', 
          `line-height` = '70%' 
        )
    })
 
  })
}
    
## To be copied in the UI
# mod_view_table_ui("view_table_1")
    
## To be copied in the server
# mod_view_table_server("view_table_1")
