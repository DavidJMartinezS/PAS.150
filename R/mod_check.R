#' check UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_check_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fillable(
    fill = T, 
    bslib::navset_card_underline(
      title = "Check Uso-Veg",
      bslib::nav_panel(title = "Tabla atributos", DT::DTOutput(ns("check_1"))),
      bslib::nav_panel(title = "Atributos por campo", verbatimTextOutput(ns("check_2"))),
      bslib::nav_panel(title = "Usos, Subusos, Formacion, F_ley20283", verbatimTextOutput(ns("check_3"))),
      bslib::nav_panel(title = "Uso, Tipo_for, Subtipo_fo, F_ley20283", verbatimTextOutput(ns("check_4"))),
      bslib::nav_panel(title = "F_ley20283, ECC, BNP_ECC", verbatimTextOutput(ns("check_5")))
    )
  )
}
    
#' check Server Functions
#'
#' @noRd 
mod_check_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$check_1 <- DT::renderDT(
      sf::st_drop_geometry(rv$uso_veg),
      class = "cell-border stripe", # style
      filter = "top",
      fillContainer = T,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
    output$check_2 <- renderPrint({
      req(rv$uso_veg)
      rv$uso_veg %>%
        dplyr::select(Uso, Subuso, Formacion, Tipo_for, Subtipo_fo, F_ley20283, BNP_ECC) %>%
        sf::st_drop_geometry() %>%
        sapply(function(x){sort(unique(x))})
    })
    output$check_3 <- renderPrint({
      req(rv$uso_veg)
      rv$uso_veg %>%
        sf::st_drop_geometry() %>%
        dplyr::count(Uso, Subuso, Formacion, F_ley20283) %>%
        as.data.frame()
    })
    output$check_4 <- renderPrint({
      req(rv$uso_veg)
      rv$uso_veg %>%
        sf::st_drop_geometry() %>%
        dplyr::count(Uso, Tipo_for, Subtipo_fo, F_ley20283) %>%
        as.data.frame()
    })
    output$check_5 <- renderPrint({
      req(rv$uso_veg)
      rv$uso_veg %>%
        sf::st_drop_geometry() %>%
        dplyr::count(Uso, F_ley20283, dplyr::across(contains("ecc")), BNP_ECC) %>%
        as.data.frame()
    })
 
  })
}
    
## To be copied in the UI
# mod_check_ui("check_1")
    
## To be copied in the server
# mod_check_server("check_1")
