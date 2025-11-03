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
    checkboxInput(ns("listo_bnp_alter"), "Capa de alteración lista")
  )
}
    
#' basic_input Server Functions
#'
#' @noRd 
mod_basic_input_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    bind_events(
      ids = c("sp", "upto5m", "listo_bnp_alter"), 
      rv = rv, 
      parent_input = input
    )
    
    # Uso_veg ----
    mod_read_sf_server("uso_veg", rv = rv, i = "uso_veg", fx = function(x) {
      x %>%
        dplyr::rename_all(~ ifelse(
          . == "geometry", ., 
          stringi::stri_trans_totitle(stringi::stri_trans_general(., "Latin-ASCII"), type = "sentence")
        )) %>%
        dplyr::rename_at(dplyr::vars(dplyr::contains("ecc")), stringi::stri_trans_toupper) %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^uso.*tierra", case_insensitive = T), ~ "Uso") %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^sub.*uso", case_insensitive = T), ~ "Subuso") %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^tipo.*for", case_insensitive = T), ~ "Tipo_for") %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^sub.*tipo.*fo", case_insensitive = T), ~ "Subtipo_fo") %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("ley.*20283", case_insensitive = T), ~ "F_ley20283") %>%
        dplyr::mutate_at(dplyr::vars(dplyr::starts_with("ECC")), tidyr::replace_na, "-") %>%
        {if(names(.) %>% stringi::stri_detect_regex("^ECC") %>% any()) {
          .[] %>%
            dplyr::mutate(
              BNP_ECC = purrr::pmap_chr(
                dplyr::select(sf::st_drop_geometry(.), dplyr::starts_with("ECC")),
                function(...) {
                  ecc <- list(...)
                  if (all(ecc == "-")) {
                    "-"
                  } else if (any(ecc != "-")) {
                    ecc %>% subset(. != "-") %>% unique() %>% paste(collapse = " - ")
                  }
                }
              )
          )
        } else .} %>%
        dplyr::mutate(Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2))
    })
    
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
    mod_read_sf_server("bnp_inter", rv = rv, i = "BNP_inter")
    observeEvent(rv$BNP_inter, {
      check_input(
        rv = rv,
        x = "BNP_inter",
        names_req = req_names$BNP_afect,
        id_reset = "bnp_inter-sf_file"
      )
      if(!is.null(rv$BNP_inter)) {
        if(rv$BNP_inter$BNP_ECC %>% stringi::stri_detect_fixed(sp, case_insensitive = T) %>% all()) {
          shinyalert::shinyalert(
            title = "Existen BNP sin la especie objetivo",
            text = tags$p(
              "Las siguientes entidades no tienen la especie objetivo en el campo 'BNP_ECC'",
              tags$br(),
              which(rv$BNP_inter$BNP_ECC %>% stringi::stri_detect_fixed(sp, case_insensitive = T)) %>% 
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
    mod_read_sf_server("bnp_alter", rv = rv, i = "BNP_alter")
    observeEvent(rv$BNP_alter, {
      check_input(
        rv = rv,
        x = "BNP_alter",
        names_req = "BNP_ECC",
        id_reset = "bnp_alter-sf_file"
      )
      if(!is.null(rv$BNP_alter)) {
        if(rv$BNP_alter$BNP_ECC %>% stringi::stri_detect_fixed(sp, case_insensitive = T) %>% all()) {
          shinyalert::shinyalert(
            title = "Existen BNP sin la especie objetivo",
            text = tags$p(
              "Las siguientes entidades no tienen la especie objetivo en el campo 'BNP_ECC'",
              tags$br(),
              which(rv$BNP_alter$BNP_ECC %>% stringi::stri_detect_fixed(sp, case_insensitive = T)) %>% 
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
