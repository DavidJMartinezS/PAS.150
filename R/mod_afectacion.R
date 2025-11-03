#' afectacion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_afectacion_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fillable(
    bslib::navset_card_underline(
      full_screen = TRUE,
      title = "Afectación",
      bslib::nav_panel("Censo a Intervenir", gt::gt_output(ns("gt_c_inter"))),
      bslib::nav_panel("Censo a Alterar", gt::gt_output(ns("gt_c_alter"))),
      bslib::nav_panel("Superficies", gt::gt_output(ns("gt_sup_inter")))
    )
  )
}
    
#' afectacion Server Functions
#'
#' @noRd 
mod_afectacion_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$gt_c_inter <- gt::render_gt({
      validate(need(rv$carto_digital, "Requiere generar cartografía digital"))
      # req(rv$carto_digital)
      gt_ecc_inter(
        ecc_inter = rv$cato_digital$ECC_inter, 
        BNP_int_sin_censo = if(isTruthy(rv$cato_digital$BNP_int_sin_censo)) rv$cato_digital$BNP_int_sin_censo else NULL
      )
    })

    output$gt_c_alter <- gt::render_gt({
      validate(need(rv$carto_digital, "Requiere generar cartografía digital"))
      # req(rv$carto_digital)
      gt_ecc_alter(
        ecc_alter = rv$cato_digital$ECC_inter, 
        BNP_alt_sin_pto = if(isTruthy(rv$cato_digital$BNP_alt_sin_censo)) rv$cato_digital$BNP_alt_sin_censo else NULL 
      )
    })
    
    output$gt_sup_inter <- gt::render_gt({
      validate(
        need(rv$carto_digital, "Requiere generar cartografía digital")
      )
      # req(c(rv$obras, rv$carto_digital))
      gt_sup_inter(
        obras = rv$obras, 
        BNP_inter = rv$carto_digital$BNP_intervenir, 
        BNP_alterar = rv$carto_digital$BNP_alterar
      )
    })
  })
}
    
## To be copied in the UI
# mod_afectacion_ui("afectacion_1")
    
## To be copied in the server
# mod_afectacion_server("afectacion_1")
