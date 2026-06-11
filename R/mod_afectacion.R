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
      sidebar = bslib::sidebar(
        shinyWidgets::virtualSelectInput(
          inputId = ns("vars_obras"),
          label = "Agrupar por:",
          choices = NULL,
          multiple = TRUE,
          width = "100%",
          dropboxWrapper = "body"
        )
      ),
      bslib::nav_panel("Censo a Intervenir", div_gt(gt::gt_output(ns("gt_c_inter")))),
      bslib::nav_panel("Censo a Alterar", div_gt(gt::gt_output(ns("gt_c_alter")))),
      bslib::nav_panel("Superficies", div_gt(gt::gt_output(ns("gt_sup_inter")))),
    )
  )
}
    
#' afectacion Server Functions
#'
#' @noRd 
mod_afectacion_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observeEvent(rv$carto_digital, {
      opt <- purrr::keep(
        rv$carto_digital,
        .p = stringi::stri_detect_regex(
          names(rv$carto_digital),
          "[censo|estim].*[inter|alter]",
          case_insensitive = TRUE
        )
      ) %>%
        purrr::pluck() %>% 
        purrr::map(names) %>% 
        purrr::flatten_chr() %>% 
        unique() %>% 
        subset(stringi::stri_detect_regex(., "obra", case_insensitive = TRUE))

      shinyWidgets::updateVirtualSelect(
        session = session,
        inputId = "vars_obras",
        choices = opt,
        selected = opt[1]
      )
    })

    output$gt_c_inter <- gt::render_gt({
      validate(need(rv$carto_digital, "Requiere generar cartografía digital"))
      gt_ecc_inter(
        ecc_inter = purrr::keep(
          rv$carto_digital,
          .p = stringi::stri_detect_regex(
            names(rv$carto_digital),
            "Censo.*Inter",
            case_insensitive = T
          )
        ) %>% purrr::pluck(1),
        BNP_int_sin_censo = purrr::keep(
          rv$carto_digital,
          .p = stringi::stri_detect_regex(
            names(rv$carto_digital),
            "Estimación_Inter",
            case_insensitive = T
          )
        ) %>% purrr::pluck(1),
        sp = rv$sp,
        col_obras = input$vars_obras
      )
    })

    output$gt_c_alter <- gt::render_gt({
      if(!isTruthy(rv$carto_digital)) {
        validate(need(rv$carto_digital, "Requiere generar cartografía digital"))
      } else {
        validate(need(purrr::keep(
          rv$carto_digital, 
          .p = stringi::stri_detect_regex(
            names(rv$carto_digital), 
            "Censo.*Alter", 
            case_insensitive = T
          )
        ) %>% purrr::pluck(1), 
        sprintf("No existe alteración de individuos de %s", rv$sp)))
      }
      gt_ecc_alter(
        ecc_alter = purrr::keep(
          rv$carto_digital, 
          .p = stringi::stri_detect_regex(
            names(rv$carto_digital), 
            "Censo.*Alter", 
            case_insensitive = T
          )
        ) %>% purrr::pluck(1), 
        BNP_alt_sin_pto = purrr::keep(
          rv$carto_digital, 
          .p = stringi::stri_detect_regex(
            names(rv$carto_digital), 
            "Estimación_Alter", 
            case_insensitive = T
          )
        ) %>% purrr::pluck(1),
        sp = rv$sp,
        col_obras = input$vars_obras
      )
    })
    
    output$gt_sup_inter <- gt::render_gt({
      validate(need(rv$carto_digital, "Requiere generar cartografía digital"))
      gt_sup_inter(
        obras = rv$obras, 
        BNP_inter = purrr::keep(
          rv$carto_digital, 
          .p = stringi::stri_detect_regex(
            names(rv$carto_digital), 
            "BNP.*a_Inter", 
            case_insensitive = T
          )
        ) %>% purrr::pluck(1), 
        BNP_alterar = purrr::keep(
          rv$carto_digital, 
          .p = stringi::stri_detect_regex(
            names(rv$carto_digital), 
            "BNP.*a_Alter", 
            case_insensitive = T
          )
        ) %>% purrr::pluck(1),
        col_obras = input$vars_obras
      )
    })
  })
}
    
## To be copied in the UI
# mod_afectacion_ui("afectacion_1")
    
## To be copied in the server
# mod_afectacion_server("afectacion_1")
