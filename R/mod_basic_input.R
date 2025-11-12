#' basic_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_basic_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_read_sf_ui(ns("uso_veg"), label = "Uso-vegetación", required = TRUE),
    shinyWidgets::pickerInput(
      inputId = ns("sp"),
      label = "Especie:",
      choices = sort(req_names$sp),
      selected = "Porlieria chilensis",
      options = shinyWidgets::pickerOptions(
        container = "body",
        style = "btn-primary"
      )
    ),
    mod_read_sf_ui(ns("obras"), label = "Obras", required = TRUE),
    mod_read_sf_ui(ns("censo"), label = "Censos", required = TRUE),
    checkboxInput(ns("upto5m"), "Incluir individuos hasta 5 m.", value = T),
    tags$hr(),
    tags$h5("Definir afectación (Opcional)"),
    mod_read_sf_ui(ns("bnp_inter"), "BNP a intervenir"),
    mod_read_sf_ui(ns("bnp_alter"), "BNP a alterar"),
    checkboxInput(ns("listo_bnp_alter"), "Capa de alteración lista"),
    uiOutput(ns("densidad_ui"))
  )
}
    
#' basic_input Server Functions
#'
#' @noRd 
mod_basic_input_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    bind_events(
      ids = c("sp", "upto5m", "listo_bnp_alter", "densidad"), 
      rv = rv, 
      parent_input = input
    )

    observeEvent({rv$BNP_inter; rv$BNP_alter}, {
      if("Censado" %in% union(names(rv$BNP_inter), names(rv$BNP_alter))) {
        output$densidad_ui <- renderUI({
          tags$div(
            numericInput(ns("densidad"), label = "Densidad para estimación (ind/ha)", value = 0)
          )
        })
      }
    })

    observeEvent(rv$densidad_bd, {
      updateNumericInput(session = session, inputId = "densidad", value = rv$densidad_bd)
    })

    output$densidad_bd_out <- renderPrint({rv$densidad_bd})
    output$densidad_out <- renderPrint({rv$densidad})
    
    # Uso_veg ----
    mod_read_sf_server("uso_veg", rv = rv, i = "uso_veg", fx = prepare_uso_veg)
    
    observeEvent(rv$uso_veg, {
      req(rv$uso_veg)
      ok <- check_input(
        rv= rv,
        x = "uso_veg",
        names_req = req_names$uso_veg,
        id_reset = "uso_veg-sf_file"
      )
      if (isTruthy(rv$uso_veg) && ok) {
        rv[["cuenca"]] <- get_cuenca(rv$uso_veg)
        rv[["BNP_cuenca"]] <- get_BNP_cuenca(uso_veg = rv$uso_veg, sp = input$sp)
      } else {
        rv$cuenca <- NULL
        rv$BNP_cuenca <- NULL
      }
    })

    # Input obras ----
    mod_read_sf_server("obras", rv = rv, i = "obras")
    observeEvent(rv$obras, {
      check_input(
        rv = rv,
        x = "obras",
        names_req = req_names$obras,
        id_reset = "obras-sf_file"
      )
    })

    # Input censo ----
    mod_read_sf_server("censo", rv = rv, i = "censo", geometry_type = "POINT")
    observeEvent(rv$censo, {
      check_input(
        rv = rv,
        x = "censo",
        names_req = req_names$censo,
        id_reset = "censo-sf_file",
        geometry = "POINT"
      )
    })

    # Inputs afectacion ----
    mod_read_sf_server("bnp_inter", rv = rv, i = "BNP_inter", fx = prepare_uso_veg)

    observeEvent(rv$BNP_inter, {
      check_input(
        rv = rv,
        x = "BNP_inter",
        names_req = req_names$BNP_afect,
        id_reset = "bnp_inter-sf_file"
      )
      if(!is.null(rv$BNP_inter)) {
        if(!all(rv$BNP_inter$BNP_ECC %>% stringi::stri_detect_fixed(rv$sp, case_insensitive = T))) {
          shinyalert::shinyalert(
            title = "Existen BNP sin la especie objetivo",
            text = tags$p(
              "Las siguientes entidades no tienen la especie objetivo en el campo 'BNP_ECC'",
              tags$br(),
              which(rv$BNP_inter$BNP_ECC %>% stringi::stri_detect_fixed(rv$sp, case_insensitive = T, negate = T)) %>% 
                paste(collapse = ", ")
            ),
            html = TRUE,
            type = "warning",
            closeOnEsc = T,
            showConfirmButton = T,
            confirmButtonCol = "#6FB58F",
            animation = T
          )
        }
      }
    })

    mod_read_sf_server("bnp_alter", rv = rv, i = "BNP_alter", fx = prepare_uso_veg)
    
    observeEvent(rv$BNP_alter, {
      check_input(
        rv = rv,
        x = "BNP_alter",
        names_req = "BNP_ECC",
        id_reset = "bnp_alter-sf_file"
      )
      if(!is.null(rv$BNP_alter)) {
        if(!all(rv$BNP_alter$BNP_ECC %>% stringi::stri_detect_fixed(rv$sp, case_insensitive = T))) {
          shinyalert::shinyalert(
            title = "Existen BNP sin la especie objetivo",
            text = tags$p(
              "Las siguientes entidades no tienen la especie objetivo en el campo 'BNP_ECC'",
              tags$br(),
              which(rv$BNP_alter$BNP_ECC %>% stringi::stri_detect_fixed(rv$sp, case_insensitive = T, negate = T)) %>% 
                paste(collapse = ", ")
            ),
            html = TRUE,
            type = "warning",
            closeOnEsc = T,
            showConfirmButton = T,
            confirmButtonCol = "#6FB58F",
            animation = T
          )
        }
      }
    })
  })
}
    
## To be copied in the UI
# mod_basic_input_ui("basic_input_1")
    
## To be copied in the server
# mod_basic_input_server("basic_input_1")
