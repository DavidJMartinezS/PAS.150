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
      choices = c(sort(df_sp$Especie), "Agregar nueva..."),
      selected = "Porlieria chilensis",
      options = shinyWidgets::pickerOptions(
        container = "body",
        style = "btn-primary",
        showSubtext = TRUE
      ),
      choicesOpt = list(
        subtext = c(df_sp$Desc, ""),
        style = c(rep("", length(df_sp$Especie)), "color: #3d93ab; font-weight: bold; border-top: 1px solid #ddd")
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

    prev_sp <- reactiveVal("Porlieria chilensis")

    observeEvent(rv$df_sp, {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "sp",
        choices = c(sort(rv$df_sp$Especie), "Agregar nueva..."),
        choicesOpt = list(
          subtext = c(rv$df_sp$Desc, ""),
          style = c(rep("", length(rv$df_sp$Especie)), "color: #3d93ab; font-weight: bold; border-top: 1px solid #ddd")
        ),
        selected = input$sp
      )
    })

    observeEvent(input$sp, {
      if (isTruthy(input$sp) && input$sp == "Agregar nueva...") {
        showModal(modalDialog(
          title = "Agregar nueva especie",
          textInput(ns("new_sp_name"), "Nombre de la especie:"),
          selectInput(
            ns("new_sp_rce"), "Clasificación RCE:",
            choices = c("VU", "EN", "CR")
          ),
          radioButtons(
            ns("new_sp_tipo_rep"), "Tipo de reproducción:",
            choices = c("dioica", "monoica"),
            inline = TRUE
          ),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("confirm_add_sp"), "Agregar", class = "btn-primary")
          ),
          easyClose = TRUE
        ))
        shinyWidgets::updatePickerInput(session, "sp", selected = prev_sp())
      } else if (isTruthy(input$sp)) {
        prev_sp(input$sp)
      }
    })

    observeEvent(input$confirm_add_sp, {
      req(input$new_sp_name)
      new_sp_val <- input$new_sp_name
      
      # Agregar a la tabla de metadatos si no existe
      if (!new_sp_val %in% rv$df_sp$Especie) {
        new_row <- tibble::tibble(
          Especie = new_sp_val,
          RCE = input$new_sp_rce,
          Tipo_rep = input$new_sp_tipo_rep,
          Desc = paste0("RCE: ", RCE, " | Tipo: ", Tipo_rep)
        )
        
        rv$df_sp <- dplyr::bind_rows(rv$df_sp, new_row) %>% 
          dplyr::arrange(Especie)
      }
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "sp",
        selected = new_sp_val
      )
      
      removeModal()
    })

    # Actualizar metadatos reactivosde la especie
    observeEvent(input$sp, {
      req(input$sp)
      # Si es una especie válida (no la opción de agregar)
      if (input$sp != "Agregar nueva...") {
        # Buscamos la info en nuestra tabla reactiva de especies
        info <- rv$df_sp %>% dplyr::filter(Especie == input$sp)
        
        if (nrow(info) > 0) {
          rv$rce <- info$RCE
          rv$tipo_rep <- info$Tipo_rep
        }
      }
    })

    # Afectacion
    observe({
      if ("Censado" %in% names(rv$BNP_inter) || "Censado" %in% names(rv$BNP_alter)) {
        output$densidad_ui <- renderUI({
          tags$div(
            numericInput(ns("densidad"), label = "Densidad para estimación (ind/ha)", value = 0)
          )
        })
      } else {
        output$densidad_ui <- renderUI(NULL)
      }
    })

    observeEvent(rv$densidad_bd, {
      updateNumericInput(session = session, inputId = "densidad", value = rv$densidad_bd)
    })

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
        rv[["cuenca"]] <- NULL
        rv[["BNP_cuenca"]] <- NULL
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
      if(isTruthy(rv$BNP_cuenca) && isTruthy(rv$obras)) {
        rv[["BNP_intervenir"]] <- get_BNP_intervencion(BNP_cuenca = rv$BNP_cuenca, obras = rv$obras, BNP_inter = rv$BNP_inter)
      } else {
        rv[["BNP_intervenir"]] <- NULL
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
      if(isTruthy(rv$BNP_cuenca) && isTruthy(rv$BNP_alter)) {
        rv[["BNP_alterar"]] <- get_BNP_alterar(BNP_alter = rv$BNP_alter, BNP_cuenca = rv$BNP_cuenca, alt_ok = rv$listo_bnp_alter)
      } else {
        rv[["BNP_alterar"]] <- NULL
      }
    })
  })
}
    
## To be copied in the UI
# mod_basic_input_ui("basic_input_1")
    
## To be copied in the server
# mod_basic_input_server("basic_input_1")
