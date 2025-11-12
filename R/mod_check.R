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
      title = "Check cartografía de uso y vegetación",
      bslib::nav_panel(title = "Tabla atributos", DT::DTOutput(ns("check_1"))),
      bslib::nav_panel(title = "Atributos por campo", verbatimTextOutput(ns("check_2"))),
      bslib::nav_panel(title = "Usos, Subusos, Formacion, F_ley20283", DT::DTOutput(ns("check_3"))),
      bslib::nav_panel(title = "Uso, Tipo_for, Subtipo_fo, F_ley20283", DT::DTOutput(ns("check_4"))),
      bslib::nav_panel(title = "F_ley20283, ECC, BNP_ECC", DT::DTOutput(ns("check_5")))
    )
  )
}
    
#' check Server Functions
#'
#' @noRd 
mod_check_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$check_1 <- DT::renderDT({
      req(rv$uso_veg)
      DT::datatable(
        sf::st_drop_geometry(rv$uso_veg) %>% 
          dplyr::select(req_names$uso_veg, dplyr::starts_with("ECC"), dplyr::matches("Sup_ha")) %>% 
          dplyr::mutate_if(is.character, as.factor),
        class = "cell-border stripe nowrap",
        filter = "top",
        editable = "cell",
        extensions = 'Buttons',
        options = list(
          dom = 'Brtip',
          autoWidth = TRUE,
          buttons = c('copy', 'excel'),
          pageLength = 15,
          scrollX = TRUE,
          scroller = TRUE
        )
      )
    })
    output$check_2 <- renderPrint({
      req(rv$uso_veg)
      rv$uso_veg %>%
        dplyr::select(Uso, Subuso, Formacion, Tipo_for, Subtipo_fo, F_ley20283, BNP_ECC) %>%
        sf::st_drop_geometry() %>%
        sapply(function(x){sort(unique(x))})
    })
    output$check_3 <- DT::renderDT({
      req(rv$uso_veg)
      DT::datatable(
        rv$uso_veg %>%
          sf::st_drop_geometry() %>%
          dplyr::count(Uso, Subuso, Formacion, F_ley20283) %>% 
          dplyr::mutate_if(is.character, as.factor),
        class = "cell-border nowrap",
        rownames = FALSE,
        filter = "top",
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
    output$check_4 <- DT::renderDT({
      req(rv$uso_veg)
      DT::datatable(
        rv$uso_veg %>%
          sf::st_drop_geometry() %>%
          dplyr::count(Uso, Tipo_for, Subtipo_fo, F_ley20283) %>% 
          dplyr::mutate_if(is.character, as.factor),
        class = "cell-border nowrap",
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
    output$check_5 <- DT::renderDT({
      req(rv$uso_veg)
      DT::datatable(
        rv$uso_veg %>%
          sf::st_drop_geometry() %>%
          dplyr::count(Uso, F_ley20283, dplyr::across(dplyr::starts_with("ECC")), BNP_ECC) %>%
          dplyr::mutate_if(is.character, as.factor),
        class = "cell-border nowrap",
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
          columns = 1:7, 
          padding = '5px 4px',
          `line-height` = '70%' 
        )
    })
  })
}
    
## To be copied in the UI
# mod_check_ui("check_1")
    
## To be copied in the server
# mod_check_server("check_1")
