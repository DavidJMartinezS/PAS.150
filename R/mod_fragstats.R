#' fragstats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom bslib layout_columns card card_header card_body
#' @importFrom fs dir_tree
#' @importFrom shiny NS tagList 
#' @importFrom shinyalert shinyalert 
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
#' @importFrom shinyFiles shinyDirButton shinyDirChoose
#' @importFrom shinyjs enable disable
#' @importFrom purrr map_lgl
#' @importFrom tools file_path_as_absolute
#' @importFrom utils browseURL
mod_fragstats_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    id = "steps_frag",
    tags$div(
      class = "warning wrn_alert",
      tags$div(
        class = "wrn_content",
        tags$div(
          class = "icon",
          svg_icon_warning
        ),
        tags$p(
          "Es necesario tener instalado previamente el software FragStats. Si no lo tiene, puede descargarlo con el siguiente ",
          actionLink(ns("link_fragstats"), "enlace"), ".\n",
          "Siga atentamente las instrucciones para evitar errores."
        )
      )
    ),
    tags$h3("Pasos para generar análisis de fragmentación con FragStats"),
    tags$br(),
    bslib::layout_columns(
      col_widths = c(8, 4),
      tags$ol(
        tags$li(
          tags$span(
            "Definir directorio donde ubicar las carpetas con los resultados y cargar los archivos de BNP antes y despues del proyecto",
            rep_br(2),
            tags$div(
              style = "color: #3d93abff; font-size: 16px",
              icon("circle-info"),
              tags$span(
                # style = "color: ",
                "Debe tener en cuenta que el nombre de los archivos que ingrese a continuación serán utilizados para crear el resto de los archivos.
                Preocuparse de que tenga el nombre que desee."
              )
            )
          ),
          tags$div(
            style = "display:flex; align-items: center; margin-top: 20px",
            fileInput(
              inputId = ns("BNP_antes"),
              label = with_red_star("BNP antes del proyecto"),
              multiple = TRUE,
              accept = c(".shp", ".shx", ".dbf", ".prj"),
              width = "350px",
              buttonLabel = "Seleccionar",
              placeholder = "Archivo no seleccionado"
            ),
            tags$div(style = "margin-right:25px"),
            fileInput(
              inputId = ns("BNP_despues"),
              label = with_red_star("BNP después del proyecto"),
              multiple = TRUE,
              accept = c(".shp", ".shx", ".dbf", ".prj"),
              width = "350px",
              buttonLabel = "Seleccionar",
              placeholder = "Archivo no seleccionado"
            ),
            tags$div(style = "margin-right:25px"),
            shinyFiles::shinyDirButton(
              ns("dir_frag"),
              label = "Directorio",
              title = "Seleccionar directorio",
              multiple = FALSE,
              icon = icon("folder"),
              viewtype = "detail",
              style = "padding: 7px 10px; background-color: #f5fa8cff; border-radius: 10px;"
            )
          )
        ),
        tags$li(
          tags$span("Configurar parámetros de borde ("),
          tags$div(
            class = "compact-inline-input", style = "display: inline-block",
            numericInput(inputId = ns("edge"), label = NULL, value = 10, width = "65px")
          ),
          tags$span("m) y proximidad ("),
          tags$div(
            class = "compact-inline-input", style = "display: inline-block",
            numericInput(inputId = ns("prox"), label = NULL, value = 1000, width = "80px")
          ),
          tags$span("m) del modelo")
        ),
        tags$li(
          tags$span(
            "Ejecutar modelo FragStats ──► ",
            actionButton(inputId = ns("frag_run"), label = "Run FragStats", class = "btn-primary btn-sm")
          )
        )
      ),
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-between",
          "Estructura de Directorio", 
          actionButton(ns("refresh"), label = "", icon = icon("arrows-rotate"))
        ),
        bslib::card_body(verbatimTextOutput(ns("dir_tree")), height = "450px")
      )
    )
  )
}
    
#' fragstats Server Functions
#'
#' @noRd 
mod_fragstats_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    refresh_tree <- reactiveVal(0)

    # Downloads ----
    observeEvent(input$link_fragstats, {
      utils::browseURL("https://www.fragstats.org/download/frg4.2.681[x64].zip")
    })

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
      id = "dir_frag",
      roots = roots,
      updateFreq = 0,
      session,
      defaultPath = "",
      defaultRoot = NULL,
      allowDirCreate = TRUE
    )
    
    dir_frag <- reactive({
      if(all(c("root", "path") %in% names(input$dir_frag))){
        selected_path <- do.call(file.path, c(roots[input$dir_frag$root], input$dir_frag$path)) %>% tools::file_path_as_absolute()
      } else {
        selected_path <- nullfile()
      }
      return(selected_path)
    })
    
    output$dir_tree <- renderPrint({
      refresh_tree()
      if (dir.exists(dir_frag())) {
        fs::dir_tree(path = tools::file_path_as_absolute(dir_frag()))
      } else {
        "Directorio no seleccionado"
      }
    })

    observeEvent(input$refresh,{
      refresh_tree(refresh_tree() + 1)
    })
    
    # BNP_antes ----
    observeEvent(input$BNP_antes, {
      shp <- ReadSfPath$new(sf_file = input$BNP_antes)
      if(shp$validate_sf()){
        rv[["BNP_antes"]] <- shp$path_shp
      } else {
        print("invalidate")
        shp$invalidate_sf()
        rv[["BNP_antes"]] <- NULL
      }
    })
    
    # BNP despues ----
    observeEvent(input$BNP_despues, {
      shp <- ReadSfPath$new(sf_file = input$BNP_despues)
      if(shp$validate_sf()){
        rv[["BNP_despues"]] <- shp$path_shp
      } else {
        print("invalidate")
        shp$invalidate_sf()
        rv[["BNP_despues"]] <- NULL
      }
    })

    # Fragstats process ----
    observe({
      if(all(c(
        isTruthy(dir.exists(dir_frag())), 
        isTruthy(rv$BNP_antes), 
        isTruthy(rv$BNP_despues)
      ))) {
        shinyjs::enable("frag_run")
      } else shinyjs::disable("frag_run")
    })

    observeEvent(input$frag_run, {
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#6FB58F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Ejecutando modelo FragStats... Espere unos minutos..."
          )
        )
      )
      on.exit({
        shinybusy::remove_modal_spinner()
      }, add = TRUE)

      tryCatch({
        FragStats(
          dir = dir_frag(), 
          path_antes = rv$BNP_antes, 
          path_despues = rv$BNP_despues, 
          edge = input$edge,
          prox = input$prox
        )
        refresh_tree(refresh_tree() + 1)
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Error al correr el modelo FragStats",
          text = as.character(e$message),
          html = TRUE,
          type = "error",
          closeOnEsc = TRUE,
          showConfirmButton = TRUE,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
      })
    })

  })
}
    
## To be copied in the UI
# mod_fragstats_ui("fragstats_1")
    
## To be copied in the server
# mod_fragstats_server("fragstats_1")
