#' fragstats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
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
          actionLink(ns("link_fragstats"), "enlace", .noWS = "outside"), ".\n",
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
            "Definir directorio donde ubicar las carpetas con los resultados y cargar los archivos de BNP antes y   despues del proyecto",
            rep_br(2),
            tags$div(
              style = "color: #3d93abff; font-size: 16px",
              icon("circle-info"),
              tags$span(
                # style = "color: ",
                "Debe tener en cuenta que el nombre de los archivos que ingrese a continuación serán utilizados para   crear el resto de los archivos.
                Preocuparse de que tenga el nombre que desee."
              )
            )
          ),
          tags$div(
            # id = "flex", 
            style = "display:flex; align-items: center; margin-top: 20px",
            fileInput(
              inputId = ns("BNP_antes"),
              label = with_red_star("BNP antes del proyecto"),
              multiple = T,
              accept = c(".shp", ".shx", ".dbf", ".prj"),
              width = "350px",
              buttonLabel = "Seleccionar",
              placeholder = "Archivo no seleccionado"
            ),
            tags$div(style = "margin-right:25px"),
            fileInput(
              inputId = ns("BNP_despues"),
              label = with_red_star("BNP después del proyecto"),
              multiple = T,
              accept = c(".shp", ".shx", ".dbf", ".prj"),
              width = "350px",
              buttonLabel = "Seleccionar",
              placeholder = "Archivo no seleccionado"
            ),
            tags$div(style = "margin-right:25px"),
            shinyFiles::shinyDirButton(
              ns("directory"),
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
          tags$span(
            "Preparación de carpetas y archivos para FragStats ──► ",
            actionButton(inputId = ns("frag_pre"), label = "pre-FragStats", class = "btn-primary btn-sm")
          )
        ),
        tags$li(
          tags$span(
            "Abrir el archivo '.fca' disponible en el siguiente ", 
            downloadLink(ns("download_model"), "enlace", .noWS = "outside"),
            ", correspondiente al modelo a ejecutar en FragStats. Este ya contiene las configuraciones necesarias, solo 
            debe cargar el archivo '.tif' del BNP antes del proyecto en el FragStats de la carpeta 'IMG'."
          )
        ),
        tags$li(
          tags$span(
           "Guardar la salida en la carpeta de 'RESULTADOS' y luego limpiar los inputs y outputs. 
          A continuación ejecutar el mismo modelo, ahora para el '.tif' del BNP despues del proyecto."
          )
        ),
        tags$li(
          tags$span(
            "Terminar de generar los resultados archivos para FragStats ──► ",
            actionButton(inputId = ns("frag_post"), label = "post-FragStats", class = "btn-primary btn-sm")
          )
        )
      ),
      bslib::card(
        bslib::card_header("Estructura de Directorio"),
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
    
    # Downloads ----
    observeEvent(input$link_fragstats, {
      browseURL("https://www.fragstats.org/download/frg4.2.681[x64].zip")
    })

    output$download_model <- downloadHandler(
      filename = function() {
        "FragStats.fca"
      },
      content = function(file) {
        file.copy(app_sys("app/www/FragStats.fca"), file)
      }
    )
    
    # Directorio ----
    roots <- c(wd = path.expand("~"))
    shinyFiles::shinyDirChoose(
      input,
      id = "directory",
      roots = roots,
      updateFreq = 0,
      session,
      defaultPath = "",
      defaultRoot = NULL,
      allowDirCreate = TRUE
    )
    
    directorio <- reactive({
      if(all(c("root", "path") %in% names(input$directory))){
        selected_path <- do.call(file.path, c(roots[input$directory$root], input$directory$path))
      } else {
        selected_path <- nullfile()
      }
      return(selected_path)
    })
    
    output$dir_tree <- renderPrint({
      if (dir.exists(directorio())) {
        fs::dir_tree(path = tools::file_path_as_absolute(directorio()))
      } else {
        "Directorio no seleccionado"
      }
    })
    
    # BNP_antes ----
    observeEvent(input$BNP_antes, {
      shp <- sf_path$new(sf_file = input$BNP_antes)
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
      shp <- sf_path$new(sf_file = input$BNP_despues)
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
        isTruthy(dir.exists(directorio())), 
        isTruthy(rv$BNP_antes), 
        isTruthy(rv$BNP_despues)
      ))) {
        shinyjs::enable("frag_pre")
      } else shinyjs::disable("frag_pre")
    }) 
    observeEvent(input$frag_pre, {
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#6FB58F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Preparando carpetas... Espere unos segundos..."
          )
        )
      )
      on.exit({
        shinybusy::remove_modal_spinner()
      }, add = TRUE)

      tryCatch({
        FragStats_pre(dir = directorio(), path_antes = rv$BNP_antes, path_despues = rv$BNP_despues)
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Error al preparar las carpetas",
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

    observe({
      if(all(c(
        isTruthy(directorio()), 
        isTruthy(rv$BNP_antes), 
        isTruthy(rv$BNP_despues)
      ))) {
        shinyjs::enable("frag_post")
      } else shinyjs::disable("frag_post")
    }) 
    observeEvent(input$frag_post, {
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#6FB58F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Generando insumos del FragStats... Espere uno momento..."
          )
        )
      )
      on.exit({
        shinybusy::remove_modal_spinner()
      }, add = TRUE)

      tryCatch({
        FragStats_post(dir = directorio(), path_antes = rv$BNP_antes, path_despues = rv$BNP_despues)
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Error en procesos post-FragStats",
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
# mod_fragstats_ui("fragstats_1")
    
## To be copied in the server
# mod_fragstats_server("fragstats_1")
