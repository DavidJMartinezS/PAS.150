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
    tags$div(
      style = "display: flex; align-items: start; gap: 25px;",
      # class = "d-flex align-items-start",
      checkboxInput(ns("recursive"), "Recursivo", value = FALSE, width = "100px"),
      tags$div(
        id = "inline",
        radioButtons(
          inputId = ns("filetype"), 
          label = "Extensión:", 
          choices = c(".shp", "todos"), 
          selected = ".shp", 
          inline = TRUE
        )
      )
    ),
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
      style = "display: flex; align-items: flex-end; gap: 15px;",
      textInput(
        inputId = ns("presufijo"), 
        label = "Prefijo o sufijo", 
        placeholder = "Ingrese texto...",
        width = "150px"
      ),
      shinyWidgets::pickerInput(
        inputId = ns("sep"),
        label = "Separador:",
        choices = c(
          'Sin sep.' = "", 
          'Espacio' = " ", 
          '"-"' = "-", 
          '"_"' = "_",
          '" - "' = " - " 
        ),
        selected = "_",
        width = "100px",
        options = shinyWidgets::pickerOptions(
          container = "body",
          style = "btn-outline-primary"
        )
      ),
      tags$div(
        style = "margin-bottom: 15px;gap: 25px;",
        actionButton(
          inputId = ns("add_prefijo"), 
          label = "Añadir prefijo", 
          icon = icon("angle-left"), 
          class = "btn-outline-info"
        ),
        actionButton(
          inputId = ns("add_sufijo"), 
          label = "Añadir sufijo", 
          icon = icon("angle-right"), 
          class = "btn-outline-info"
        )
      )
    ),
    tags$div(
      id = "flex",
      actionButton(
        inputId = ns("format_sup_ha"), 
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
    rv_ext <- reactiveValues(
      capas_dir = NULL
    )

    # Directorio ----
    roots <- c(
      "Documentos" = path.expand("~"),
      "OneDrive_personal" = Sys.getenv("OneDriveConsumer"),
      "OneDrive_empresarial" = Sys.getenv("OneDriveCommercial"),
      "Escritorio" = Sys.getenv("USERPROFILE") %>% file.path("Desktop")
    ) %>% 
      c(., get_volumes()) %>% 
      .[nchar(.) > 0] %>%
      .[purrr::map_lgl(., dir.exists)]

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
    
    dir_carto <- reactive({
      if(all(c("root", "path") %in% names(input$dir_carto))){
        selected_path <- do.call(file.path, c(roots[input$dir_carto$root], input$dir_carto$path)) %>% tools::file_path_as_absolute()
      } else {
        selected_path <- nullfile()
      }
      return(selected_path)
    })

    output$dir_selected <- renderPrint({
      req(dir_carto())
      if (dir.exists(dir_carto())) {
        tools::file_path_as_absolute(dir_carto())
      } else {
        "Directorio no seleccionado"
      }
    })
    
    # Multiinput ----
    observeEvent(eventExpr = {
      dir_carto()
      input$recursive
      input$filetype
    }, handlerExpr = {
      pattern <- if (input$filetype == ".shp") "\\.shp$" else NULL
      rv_ext$capas_dir <- list.files(dir_carto(), pattern = pattern, full.names = F, recursive = input$recursive)
      shinyWidgets::updateMultiInput(
        session = session, 
        inputId = "capas", 
        choices = rv_ext$capas_dir
      )
    })
    observeEvent(input$all, {
      shinyWidgets::updateMultiInput(session, "capas", selected = rv_ext$capas_dir)
    })
    observeEvent(input$none, {
      shinyWidgets::updateMultiInput(session, "capas", selected = NA_character_)
    })
    
    observe({
      if(isTruthy(dir_carto()) && isTruthy(input$capas)) {
        shinyjs::enable("format_sup_ha") 
        shinyjs::enable("create_kmz") 
      } else {
        shinyjs::disable("format_sup_ha")
        shinyjs::disable("create_kmz")
      }
    }) 

    # Prefijos y sufijos ----
    observe({
      condition <- (input$filetype == "todos" && isTruthy(input$capas) && nzchar(input$presufijo))
      if (condition) {
        shinyjs::enable("add_prefijo")
        shinyjs::enable("add_sufijo")
      } else {
        shinyjs::disable("add_prefijo")
        shinyjs::disable("add_sufijo")
      }
    })

    observeEvent(input$add_prefijo, {
      req(input$capas, input$presufijo, dir_carto())
      tryCatch({
        paths <- file.path(dir_carto(), input$capas)
        purrr::walk(paths, agregar_prefijo, prefijo = input$presufijo, sep = input$sep)
        
        # pattern <- if (input$filetype == ".shp") "\\.shp$" else NULL
        # rv_ext$capas_dir <- list.files(dir_carto(), pattern = pattern, full.names = F, recursive = input$recursive)
        # shinyWidgets::updateMultiInput(session, "capas", choices = rv_ext$capas_dir, selected = character(0))
        
        shinybusy::notify_success("Prefijo añadido con éxito", position = "right-bottom")
      }, error = function(e) {
        shinyalert::shinyalert("Error", as.character(e$message), type = "error")
      })
    })

    observeEvent(input$add_sufijo, {
      req(input$capas, input$presufijo, dir_carto())
      tryCatch({
        paths <- file.path(dir_carto(), input$capas)
        purrr::walk(paths, agregar_sufijo, sufijo = input$presufijo, sep = input$sep)
        
        # pattern <- if (input$filetype == ".shp") "\\.shp$" else NULL
        # rv_ext$capas_dir <- list.files(dir_carto(), pattern = pattern, full.names = F, recursive = input$recursive)
        # shinyWidgets::updateMultiInput(session, "capas", choices = rv_ext$capas_dir, selected = character(0))
        
        shinybusy::notify_success("Sufijo añadido con éxito", position = "right-bottom")
      }, error = function(e) {
        shinyalert::shinyalert("Error", as.character(e$message), type = "error")
      })
    })

    updateTextInput(session, "presufijo", value = "")

    # Format Sup_ha ----
    observeEvent(input$format_sup_ha, {
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#6FB58F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Formateando precision del campo Sup_ha a 2 decimales... Espere un momento..."
          )
        )
      )
      on.exit({
        shinybusy::remove_modal_spinner()
      }, add = TRUE)
      
      tryCatch({
        input$capas %>% 
          file.path(dir_carto(), .) %>% 
          purrr::map(function(x) {
            tryCatch(format_sup_ha(x), error = function(e) {
              message("Error al formatear '", basename(x), "': ", e$message)
              NULL
            })
          })
        
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
          file.path(dir_carto(), .) %>% 
          purrr::map(function(x) {
            tryCatch({
              asign_name_folder_kmz(shp = x) %>% 
                do.call("shp2kmz", .)
            }, error = function(e) {
              message("Error al generar KMZ para '", basename(x), "': ", e$message)
              NULL 
            })
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
