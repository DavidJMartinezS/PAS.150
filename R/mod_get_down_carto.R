#' get_down_carto UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_get_down_carto_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      id = "flex",
      actionButton(
        inputId = ns("get_carto_btn"),
        label = "Generar cartografía digital",
        icon = icon("map"),
        class = "btn-outline-info btn-sm"
      ),
      mod_downfiles_ui(ns("down_carto"))
    )
  )
}
    
#' get_down_carto Server Functions
#'
#' @noRd 
mod_get_down_carto_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observe({
      if(all(c(
        isTruthy(rv$uso_veg), isTruthy(rv$sp), isTruthy(rv$obras), isTruthy(rv$censo)
      ))) shinyjs::enable("get_carto_btn") else 
      shinyjs::disable("get_carto_btn")
    })

    observeEvent(input$get_carto_btn, {
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#6FB58F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Generando cartografía digital.",
            tags$br(),
            " Puede ir a prepararse un café, esto tardarará unos minutos"
          )
        )
      )
      on.exit({
        shinybusy::remove_modal_spinner()
      }, add = TRUE)

      rv$carto_digital <- tryCatch({
        get_carto_digital(
          uso_veg = rv$uso_veg,
          sp = rv$sp,
          obras = rv$obras,
          censo = rv$censo,
          upto5m = rv$upto5m,
          BNP_inter = rv$BNP_inter,
          BNP_alter = rv$BNP_alter,
          alt_ok = rv$listo_bnp_alter,
          densidad = rv$densidad,
          BD_flora = rv$BD_flora,
          BD_fore = rv$BD_fore,
          bd_flora_lista = rv$listo_bd_flora,
          bd_fore_lista = rv$listo_bd_fore,
          in_bnp_obra = rv$in_bnp_obra,
          add_cam = rv$add_cam,
          add_hidro = rv$add_hidro,
          fuente_hidro = if (rv$add_hidro) rv$fuente_hidro,
          add_CN = rv$add_CN,
          dem = rv$dem
        )
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Error al generar la cartografía digitial!",
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

      if (!is.null(rv$carto_digital)) {
        shinybusy::report_success(
          title = "Listo!",
          text = "Se ha generado la cartografía digital",
          button = "Ok"
        )
      }
    })
    
    carto_names <- reactive({list(Cartografia_digital = names(rv$carto_digital))})
    mod_downfiles_server(
      id = "down_carto", 
      x = reactive(rv$carto_digital), 
      name_save = carto_names()
    )

  })
}
    
## To be copied in the UI
# mod_get_down_carto_ui("get_down_carto_1")
    
## To be copied in the server
# mod_get_down_carto_server("get_down_carto_1")
