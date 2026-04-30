#' ext_supha_kmz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ext_supha_kmz_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
  bslib::card_header("Crear KMZ"),
  tags$head(
    tags$style(".multi-wrapper {height: 500px;}"), 
    tags$style(".multi-wrapper .non-selected-wrapper, .multi-wrapper .selected-wrapper {height: 100%;}")
  ),
  tags$div(
    class = "d-flex align-items-start gap-3 mb-1",
    shinyFiles::shinyDirButton(
      ns("dir_carto"),
      label = "Directorio",
      title = "Seleccionar directorio",
      multiple = FALSE,
      icon = icon("folder"),
      viewtype = "detail",
      class = "btn-sm",
      style = "padding: 7px 10px; background-color: #f5fa8cff; border-radius: 10px;"
    ),
    verbatimTextOutput(outputId = ns("dir_selected"), placeholder = T) %>% 
      tagAppendAttributes(style = "margin-bottom: 0; flex-grow: 1; padding: 5px 10px; font-size: 0.85rem;")
  ),
  checkboxInput(ns("recursive"), "Recursivo", value = F),
  tags$div(
    actionButton(inputId = ns("all"), label = "Selecionar todo", class = "btn-sm"),
    actionButton(inputId = ns("none"), label = "Deseleccionar todo", class = "btn-sm"),
  ),
  tags$div(
    style = "height: 550px",
    shinyWidgets::multiInput(
      inputId = ns("capas"), 
      label = "Seleccionar capas:",
      choices = as.character(0),
      selected = as.character(0),
      width = "100%",
      options = list(
        enable_search = FALSE,
        non_selected_header = "Elegir entre:",
        selected_header = "Seleccionadas:"
      )
    )
  ),
  tags$div(
    id = "flex",
    actionButton(
      inputId = ns("trans_sup_ha"), 
      label = 'Formatear "Sup_ha"', 
      icon = icon("draw-polygon"), 
      class = "btn-outline-info"
    ),
    tags$div(style = "margin-left: 25px;"),
    actionButton(
      inputId = ns("create_kmz"),
      label = "Generar KMZ",
      icon = icon("layer-group"),
      class = "btn-outline-info"
    )
  )
)
}
    
#' ext_supha_kmz Server Functions
#'
#' @noRd 
mod_ext_supha_kmz_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    capas <- reactiveVal(0)
    is_config <- reactiveVal(SupHA::setup_arcpy_env())

    # Directorio ----
    roots <- c(wd = path.expand("~"))
    shinyFiles::shinyDirChoose(
      input,
      id = "dir_carto",
      roots = roots,
      updateFreq = 0,
      session,
      defaultPath = "",
      defaultRoot = NULL,
      allowDirCreate = TRUE
    )
    
    directorio <- reactive({
      if(all(c("root", "path") %in% names(input$dir_carto))){
        selected_path <- do.call(file.path, c(roots[input$dir_carto$root], input$dir_carto$path)) %>% tools::file_path_as_absolute()
      } else {
        selected_path <- nullfile()
      }
      return(selected_path)
    })

    output$dir_selected <- renderPrint({
      req(directorio())
      if (dir.exists(directorio())) {
        directorio()
      } else {
        "Directorio no seleccionado"
      }
    })
    
    # multiinput ----
    observeEvent(eventExpr = {
      directorio()
      input$recursive
    }, handlerExpr = {
      capas_dir <- list.files(directorio(), pattern = ".shp$", full.names = F, recursive = input$recursive)
      capas(capas_dir)
      shinyWidgets::updateMultiInput(
        session = session, 
        inputId = "capas", 
        choices = capas()
      )
    })
    observeEvent(input$all, {
      shinyWidgets::updateMultiInput(session, "capas", selected = capas())
    })
    observeEvent(input$none, {
      shinyWidgets::updateMultiInput(session, "capas", selected = NA_character_)
    })
    
    # Format Sup_ha ----
    observe({
      if(isTruthy(directorio())) shinyjs::enable("trans_sup_ha") else shinyjs::disable("trans_sup_ha")
    }) 
    observeEvent(input$trans_sup_ha, {
      # Determinar estado actual para la UI
      config_ok <- is_config()
      status_icon <- if(config_ok) icon("check-circle", class = "text-success") else icon("times-circle", class = "text-danger")
      status_label <- if(config_ok) " Entorno configurado" else " Entorno no configurado"

      showModal(modalDialog(
        title = "Configuración de Formateo Sup_ha",
        tags$p("Esta función formatea la precisión del campo ", tags$code("Sup_ha"), " a dos decimales."),
        tags$div(
          class = "alert alert-warning mb-3",
          icon("exclamation-triangle"), 
          " Requiere tener instalado ", tags$b("ArcGIS Pro"), " en el equipo."
        ),
        tags$div(
          class = "mb-3",
          tags$b("Estado: "),
          tags$span(id = ns("status_text"), status_icon, status_label)
        ),
        hr(),
        textInput(
          ns("python_exe"), 
          "Ruta del ejecutable Python de ArcGIS Pro:", 
          value = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe"
        ),
        actionButton(ns("setup_env"), "Configurar Entorno", class = "btn-info btn-sm", icon = icon("gears")),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("run_format"), "Ejecutar Formateo", class = "btn-success", icon = icon("play")) %>% 
            tagAppendAttributes(disabled = if(config_ok) NULL else NA)
        ),
        size = "m",
        easyClose = TRUE
      ))
    })

    # Configurar el entorno de ArcPy
    observeEvent(input$setup_env, {
      tryCatch({
        SupHA::setup_arcpy_env(python_path = input$python_exe)
        is_config(TRUE)
        shinyjs::enable("run_format")
        shinyjs::html("status_text", as.character(tagList(icon("check-circle", class = "text-success"), " Entorno configurado")))
        shinybusy::notify_success("Entorno configurado correctamente.")
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Error de Configuración",
          text = e$message,
          type = "error"
        )
      })
    })

    # Ejecutar la función después de configurar
    observeEvent(input$run_format, {
      req(input$capas)
      removeModal()
        
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#6FB58F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Formateando precision del campo Sup_ha a 2 decimales"
          )
        )
      )
      
      tryCatch({
        input$capas %>% 
          file.path(directorio(), .) %>% 
          purrr::map(SupHA::st_trans_sup_ha)
        
        shinybusy::remove_modal_spinner()
        shinybusy::notify_success("Proceso completado con éxito.", timeout = 3000, position = "right-bottom")
      }, error = function(e) {
        shinybusy::remove_modal_spinner()
        shinyalert::shinyalert(
          title = 'Error al ejecutar formateo',
          text = as.character(e$message), type = "error"
        )
      })
    })

    # crear kmz ----
    observe({
      if(isTruthy(directorio())) shinyjs::enable("create_kmz") else shinyjs::disable("create_kmz")
    }) 
    observeEvent(input$create_kmz, {
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#6FB58F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Generando kmz... Espere uno momento..."
          )
        )
      )
      on.exit({
        shinybusy::remove_modal_spinner()
      }, add = TRUE)

      tryCatch({
        input$capas %>% 
          purrr::map(function(x){
            sf::read_sf(file.path(directorio(), x)) %>% prepare_kml(basename = tools::file_path_sans_ext(basename(x))) %>% 
              sf::write_sf(file.path(directorio(), paste0(tools::file_path_sans_ext(x), ".kml")))
            zip::zip(
              zipfile = file.path(directorio(), paste0(tools::file_path_sans_ext(x), ".kmz")), 
              files = file.path(directorio(), paste0(tools::file_path_sans_ext(x), ".kml"))
            )
            file.remove(file.path(directorio(), paste0(tools::file_path_sans_ext(x), ".kml")))
          })
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Error al generar kmz",
          text = as.character(e$message),
          html = TRUE,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
      })
    })

  })
}
    
## To be copied in the UI
# mod_ext_supha_kmz_ui("ext_supha_kmz_1")
    
## To be copied in the server
# mod_ext_supha_kmz_server("ext_supha_kmz_1")
