#' apendices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_apendices_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      id = "inline",
      shinyWidgets::pickerInput(
        inputId = ns("portada"),
        label = "Seleccionar portada: ",
        choices = c("default", "KIM753", "MLP612", "otra"),
        selected = "default",
        width = "150px",
        options = shinyWidgets::pickerOptions(
          container = "body",
          style = "btn-outline-primary"
        )
      )
    ),
    tags$div(style = "margin-top: 10px"),
    conditionalPanel(
      condition = "input.portada == 'otra'",
      ns = ns,
      tags$div(
        shinyWidgets::pickerInput(
          inputId = ns("tipo_proj"),
          label = "Tipo de proyecto",
          choices = c("EIA", "DIA"),
          selected = "EIA",
          width = "150px",
          options = shinyWidgets::pickerOptions(
            container = "body",
            style = "btn-outline-primary"
          )
        ),
        textInput(
          inputId = ns("nom_proj"),
          label = "Nombre de proyecto",
          width = "250px",
          placeholder = "Ingrese nombre del proyecto"
        ),
        fileInput(
          inputId = ns("logo"),
          label = "Logo del cliente",
          multiple = FALSE,
          accept = "image/*"
        )
      )
    ),
    tags$h5("Análisis de Biodiversidad"),
    tags$div(
      id = "flex",
      actionButton(
        inputId = ns("get_flora_btn"),
        label = "Generar BD Biodiversidad",
        icon = icon("pagelines"),
        class = "btn-outline-info btn-sm"
      ),
      mod_downfiles_ui(ns("down_bd_bio"))
    ),
    tags$hr(),
    tags$h5("Análisis de Inventarios forestales"),
    tags$div(
      id = "flex",
      actionButton(
        inputId = ns("get_fore_btn"),
        label = "Generar BD Inventario forestal",
        icon = icon("tree"),
        class = "btn-outline-info btn-sm"
      ),
      mod_downfiles_ui(ns("down_bd_fore"))
    )
  )
}
    
#' apendices Server Functions
#'
#' @noRd 
mod_apendices_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    bind_events(
      ids = c("portada", "tipo_proj", "nom_proj", "logo"), 
      rv = rv, 
      parent_input = input
    )

    observe({
      if(all(c(
        isTruthy(rv$BD_flora), isTruthy(rv$cuenca), isTruthy(rv$sp), isTruthy(rv$portada) 
      ))) {
        shinyjs::enable("get_flora_btn")
      } else {
        shinyjs::disable("get_flora_btn")
      }
    })
    observeEvent(input$get_flora_btn, {
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#6FB58F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Generando Apéndice de Biodiversidad",
            tags$br(),
            "Espere unos segundos..."
          )
        )
      )
      on.exit({
        shinybusy::remove_modal_spinner()
      }, add = TRUE)

      rv$BD_biodiversidad <- tryCatch({
        BD_biodiversidad(
          BD_flora = rv$BD_flora,
          sp = rv$sp,
          nom_ssubc = rv$cuenca$NOM_SSUBC,
          portada = rv$portada,
          portada_opts = portada_opts(
            tipo_proj = rv$tipo_proj,
            nom_proj = rv$nom_proj,
            logo = rv$logo
          )
        )
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Error al generar la el Apéndice de Biodiversidad!",
          text = as.character(e$message),
          html = TRUE,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
        return(NULL)
      })
      if (!is.null(rv$BD_biodiversidad)) {
        shinybusy::report_success(
          title = "Listo!",
          text = "Se ha generado el Apéndice de Biodiversidad",
          button = "Ok"
        )
      }
    })
    mod_downfiles_server(
      "down_bd_bio", 
      x = reactive(rv$BD_biodiversidad$wb), 
      name_save = sprintf("BD Biodiversidad %s", stringi::stri_extract_first_words(rv$sp))
    )

    observe({
      if(all(c(
        isTruthy(rv$BD_fore), isTruthy(rv$BNP_cuenca), isTruthy(rv$sp), isTruthy(rv$portada) 
      ))) {
        shinyjs::enable("get_fore_btn")
      } else {
        shinyjs::disable("get_fore_btn")
      }
    })
    observeEvent(input$get_fore_btn, {
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#6FB58F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Generando Apéndice de inventarios forestales",
            tags$br(),
            "Espere unos segundos..."
          )
        )
      )
      on.exit({
        shinybusy::remove_modal_spinner()
      }, add = TRUE)

      rv$BD_inv_forestales <- tryCatch({
        BD_inventarios(
          BD_fore = rv$BD_fore, 
          BNP_cuenca = rv$BNP_cuenca, 
          sp = rv$sp,
          portada = rv$portada,
          portada_opts = portada_opts(
            tipo_proj = rv$tipo_proj,
            nom_proj = rv$nom_proj,
            logo = rv$logo
          )
        )
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Error al generar la el Apéndice de Inventarios forestales!",
          text = as.character(e$message),
          html = TRUE,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
        return(NULL)
      })
      if (!is.null(rv$BD_inv_forestales)) {
        shinybusy::report_success(
          title = "Listo!",
          text = "Se ha generado el Apéndice de Inventarios forestales",
          button = "Ok"
        )
      }
    })
    mod_downfiles_server(
      "down_bd_fore", 
      x = reactive(rv$BD_inv_forestales$wb), 
      name_save = sprintf("BD Inventarios forestales BNP %s", stringi::stri_extract_first_words(rv$sp))
    )

  })
}
    
## To be copied in the UI
# mod_apendices_ui("apendices_1")
    
## To be copied in the server
# mod_apendices_server("apendices_1")
