#' fragmentacion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_fragmentacion_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fillable(
    bslib::navset_card_underline(
      id = "navset_frag",
      selected = "Ajuste de parámetros", 
      bslib::nav_panel(
        title = tags$img(
          height = "20px",
          src = "https://www.fragstats.org/images/FragstatsWebLOGO-70.png"
        ),
        mod_fragstats_ui(ns("fragstats"))
      ),
      bslib::nav_panel(
        title = "Ajuste de parámetros",
        bslib::input_switch(
          id = ns("switch_sf_frag"),
          label = "Realizar análisis de fragmentación independiente de los inputs",
          value = F,
          width = "fit-content"
        ),
        conditionalPanel(
          condition = "input.switch_sf_frag == true",
          ns = ns,
          tags$div(
            tags$div(
              class = "warning wrn_alert",
              tags$div(
                class = "wrn_content",
                tags$div(
                  class = "icon",
                  svg_icon_warning
                ),
                tags$p(
                  "Al generar el apéndice de fragmentación independiente de los inputs de la aplicación, 
                debe saber que en el caso de haber generado los apéndices de inventarios forestales y/o de biodiversidad,
                estos no se tendrán en consideración para la evaluación de los 5 parámetros en donde se utilizan.
                Los valores de estos parámetros quedarán con el valor mínimo (1), y se encontrarán destacados en el excel."
                )
              )
            ),
            mod_read_sf_ui(
              id = ns("uso_frag"),
              label = "Usos de suelo de la cuenca",
              required = T,
              width = "600px"
            )
          )
        ),
        tags$div(style = "margin-top: -10px"),
        shinyWidgets::pickerInput(
          inputId = ns("select_subusos"),
          label = "Seleccionar subusos que no sean de vegetación (incluir terrenos agrícolas)",
          choices = NULL,
          multiple = TRUE,
          width = "600px",
          options = shinyWidgets::pickerOptions(
            container = "body",
            style = "btn-outline-primary btn-sm"
          )
        ),
        checkboxInput(
          ns("alt_bd_frag"),
          label = "Existe alteración",
          value = T
        ),
        tags$p(
          "Ingrese excel con los resultados del FragStats. La primera y segunda hoja del documento, deben corresponder a las condiciones ANTES y DESPUES del proyecto, respectivamente.",
          style = "font-weight: bold;"
        ),
        tags$div(style = "margin-top: -45px"),
        tags$div(
          id = "flex",
          style = "align-items: center",
          fileInput(ns("path_frag"), "", accept = c(".xls", ".xlsx")),
          actionButton(
            class = "div_med",
            ns("get_bd_frag"),
            label = "Generar análisis de amenazas",
            class = "btn-outline-info"
          ),
          mod_downfiles_ui(
            ns("down_bd_frag"),
            label = "Descargar apéndice de fragmentación"
          )
        )
      ),
      bslib::nav_panel(
        title = "Resultados",
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::layout_column_wrap(
            width = 1,
            bslib::card(
              full_screen = T,
              bslib::card_header("Matriz del paisaje"),
              bslib::card_body(gt::gt_output(ns("gt_frag_matriz")))
            ),
            bslib::card(
              full_screen = T,
              bslib::card_header("Parámetros de fragmentación"),
              bslib::card_body(gt::gt_output(ns("gt_frag_param")))
            )
          ),
          bslib::card(
            full_screen = T,
            bslib::card_header("Resultados análisis de amenazas"),
            bslib::card_body(gt::gt_output(ns("gt_frag_resultados")))
          )
        )
      )
    )
  )
}
    
#' fragmentacion Server Functions
#'
#' @noRd 
mod_fragmentacion_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Datos y parámetros ----
    mod_read_sf_server(id = "uso_frag", rv = rv, i = "uso_frag")
    observeEvent(rv$uso_frag, {
      check_input(
        rv = rv,
        x = "uso_frag",
        names_req = "Subuso",
        id_reset = "uso_frag-sf_file"
      )
    })
    sf_uso <- reactive({
       if(input$switch_sf_frag) rv$uso_frag else rv$uso_veg
    })

    observeEvent(rv$uso_veg, {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "select_subusos",
        label = "Seleccionar subusos que no sean de vegetación (incluir terrenos agrícolas)", 
        choices = sort(unique(rv$uso_veg$Subuso))
      )
    })

    observe({
      if (
        all(c(
          isTruthy(sf_uso()),
          isTruthy(rv$obras),
          isTruthy(input$path_frag$datapath)
        ))
      ) {
        shinyjs::enable("get_bd_frag")
      } else {
        shinyjs::disable("get_bd_frag")
      }
    })

    observeEvent(input$get_bd_frag, {
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#6FB58F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Generando Apéndice de Fragmentación",
            tags$br(),
            "Espere unos momento..."
          )
        )
      )
      on.exit({
        shinybusy::remove_modal_spinner()
      }, add = TRUE)

      rv$BD_fragmentacion <- tryCatch({
        BD_fragmentacion(
          sf_uso = sf_uso(), 
          sf_obras = rv$obras,
          path_frag = input$path_frag$datapath,
          ECC = rv$sp,
          subusos_noveg = input$select_subusos,
          alteracion = input$alt_bd_frag, 
          spp_acomp = if(!is.null(rv$BD_biodiversidad)) NULL else rv$BD_biodiversidad$spp_acomp, 
          prop = if(!is.null(rv$BD_biodiversidad)) NULL else rv$BD_biodiversidad$spp_acomp,
          estadisticos = rv$estadisticos, 
          portada = rv$portada,
          portada_opts = portada_opts(
            tipo_proj = rv$tipo_proj,
            nom_proj = rv$nom_proj,
            logo = rv$logo$datapath
          )
        )
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Error al generar la el Apéndice de Fragmentación!",
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
      if (!is.null(rv$BD_fragmentacion)) {
        shinybusy::report_success(
          title = "Listo!",
          text = "Se ha generado el Apéndice de Fragmentación",
          button = "Ok"
        )
      }
    })
    mod_downfiles_server(
      "down_bd_frag", 
      x = reactive(rv$BD_fragmentacion$wb), 
      name_save = sprintf("BD Tabla análisis de fragmentación %s", stringi::stri_extract_first_words(rv$sp))
    )
    
    # Outputs ----
    output$gt_frag_matriz <- gt::render_gt({
      validate(need(rv$BD_fragmentacion, "Requiere haber generardo la BD de inventarios forestales"))
      gt_frag_matriz(matriz_paisaje = rv$BD_fragmentacion$matriz_paisaje)
    })
    output$gt_frag_param <- gt::render_gt({
      validate(need(rv$BD_fragmentacion, "Requiere haber generardo la BD de inventarios forestales"))
      gt_frag_param(df = rv$BD_fragmentacion$df)
    })
    output$gt_frag_resultados <- gt::render_gt({
      validate(need(rv$BD_fragmentacion, "Requiere haber generardo la BD de inventarios forestales"))
      gt_frag_resultados(tabla_eval = rv$BD_fragmentacion$tabla_eval)
    })
    
    # Instrucciones FragStats ----
    mod_fragstats_server(id = "fragstats", rv = rv)
    
  })
}
    
## To be copied in the UI
# mod_fragmentacion_ui("fragmentacion_1")
    
## To be copied in the server
# mod_fragmentacion_server("fragmentacion_1")
