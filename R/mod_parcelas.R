#' parcelas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_parcelas_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(
      inputId = ns("bd_flora"),
      label = "Parcelas de inventario florístico",
      accept = c(".xlsx")
    ),
    tags$div(style = "margin-top: -10px"),
    tags$div(
      id = "flex",
      style = "display: flex; align-items: center; justify-content: space-between;",
      splitLayout(
        cellWidths = c("25%", "50%", "25%"),
        checkboxInput(ns("listo_bd_flora"), "BD lista"),
        checkboxInput(ns("in_bnp_obra"), "En BNP con obras", value = F),
        actionButton(
          ns("check_bd_flora"),
          label = "Check",
          icon = icon("circle-check"),
          class = "btn-primary btn-sm"
        )
      )
    ),
    fileInput(
      inputId = ns("bd_fore"),
      label = "Ingrese parcelas de inventario forestal",
      accept = c(".xlsx")
    ),
    tags$div(style = "margin-top: -10px"),
    tags$div(
      id = "flex",
      style = "display: flex; align-items: center; justify-content: space-between;",
      splitLayout(
        cellWidths = c("25%", "75%"),
        checkboxInput(ns("listo_bd_fore"), "BD lista"),
        actionButton(
          ns("check_bd_fore"),
          label = "Check",
          icon = icon("circle-check"),
          class = "btn-primary btn-sm"
        )
      )
    )
  )
}
    
#' parcelas Server Functions
#'
#' @noRd 
mod_parcelas_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    bind_events(
      ids = c("listo_bd_flora", "listo_bd_fore", "in_bnp_obra"), 
      rv = rv, 
      parent_input = input
    )

    # BD flora ----
    observe({
      if (isTruthy(rv$obras)) {
        shinyjs::enable("in_bnp_obra")
        updateCheckboxInput(session, "in_bnp_obra", value = T)
      } else {
        shinyjs::disable("in_bnp_obra")
      }
    })

    observeEvent(input$listo_bd_flora, {
      if (input$listo_bd_flora) {
        freezeReactiveValue(input, "in_bnp_obra")
        updateCheckboxInput(session, "in_bnp_obra", value = FALSE)
      }
    })
    observeEvent(input$in_bnp_obra, {
      if (input$in_bnp_obra) {
        freezeReactiveValue(input, "listo_bd_flora")
        updateCheckboxInput(session, "listo_bd_flora", value = FALSE)
      }
    })

    toListen_bd_flora <- reactive({
      list(input$bd_flora, input$listo_bd_flora, rv$BNP_cuenca, input$in_bnp_obra, rv$obras)
    })

    observeEvent(
      toListen_bd_flora(), 
      ignoreNULL = TRUE, {
      req(input$bd_flora)
      
      rv$BD_flora <- tryCatch({
        prepare_bd_flora_150(
          BD = input$bd_flora$datapath, 
          bd_lista = input$listo_bd_flora, 
          BNP_cuenca = rv$BNP_cuenca,
          in_bnp_obra = input$in_bnp_obra, 
          obras = rv$obras
        ) %>% 
          suppressWarnings()
      }, error = function(e) {
        shinyjs::reset(id = "bd_flora")
        shinyalert::shinyalert(
          title = "Error con la BD de flora!",
          text = e$message,
          html = F,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
        return(NULL)
      })
    })

    observe({
      if (isTruthy(rv$BD_flora)) shinyjs::enable("check_bd_flora") else shinyjs::disable("check_bd_flora")
    })

    observeEvent(input$check_bd_flora, {
      req(rv$BD_flora)
      check_bd_flora(rv$BD_flora)
    })
    
    # BD fore ----
    toListen_bd_fore <- reactive({
      list(input$bd_fore, input$listo_bd_fore, rv$BNP_cuenca)
    })

    observeEvent(
      toListen_bd_fore(), 
      ignoreNULL = TRUE, {
      req(input$bd_fore)
      req(rv$BNP_cuenca)

      rv$BD_fore <- tryCatch({
        prepare_bd_fore_150(
          BD = input$bd_fore$datapath,
          bd_lista = input$listo_bd_fore, 
          BNP_cuenca = rv$BNP_cuenca
        ) %>% 
          suppressWarnings()
      }, error = function(e) {
        shinyjs::reset(id = "bd_fore")
        shinyalert::shinyalert(
          title = "Error con la BD de inventarios!",
          text = e$message,
          html = F,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
        return(NULL)
      })
    })

    observe({
      if (isTruthy(rv$BD_fore)) shinyjs::enable("check_bd_fore") else shinyjs::disable("check_bd_fore")
    })

    observeEvent(input$check_bd_fore, {
      req(rv$BD_fore)
      check_bd_fore(rv$BD_fore)
    })

    observeEvent(eventExpr = {
      rv$BD_fore
      rv$sp
    }, handlerExpr = {
      req(rv$BD_fore)
      rv$densidad <- get_densidad(rv$BD_fore, rv$sp)
    })

  })
}
    
## To be copied in the UI
# mod_parcelas_ui("parcelas_1")
    
## To be copied in the server
# mod_parcelas_server("parcelas_1")
