#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  options(
    # shiny.fullstacktrace = T,
    # shiny.trace = T,
    shiny.maxRequestSize = 4000 * 1024^2,
    timeout = 600
  )
  rv <- reactiveValues(
    uso_veg = NULL,
    sp = NULL,
    obras = NULL, 
    censo = NULL, 
    upto5m = NULL,
    cuenca = NULL,
    BNP_cuenca = NULL, 
    BNP_inter = NULL,
    BNP_alter = NULL,
    listo_bnp_alter = NULL,
    BD_flora = NULL,
    listo_bd_flora = NULL,
    in_bnp_obra = NULL,
    BD_fore = NULL,
    densidad = NULL,
    listo_bd_fore = NULL,
    add_cam = NULL,
    add_hidro = NULL,
    add_CN = NULL,
    dem = NULL,
    fuente_hidro = NULL,
    carto_digital = NULL,
    portada = NULL,
    tipo_proj = NULL,
    nom_proj = NULL,
    logo = NULL,
    BD_biodiversidad = NULL,
    BD_inv_forestales = NULL,
    BD_fragmentacion = NULL,
    uso_frag = NULL,
    estadisticos = NULL,
    BNP_antes = NULL,
    BNP_despues = NULL
  )

  mod_input_server("input_carto", rv = rv)
  mod_check_server("check_carto", rv = rv)
  mod_info_cuenca_server("info_cuenca", rv = rv)
  mod_uso_suelo_server("uso_suelo", rv = rv)
  mod_vegetacion_server("vegetacion", rv = rv)
  mod_inv_for_server("inventarios", rv = rv)
  mod_afectacion_server("afectacion", rv = rv)
  mod_biodiversidad_server("biodiversidad", rv = rv)
  mod_fragmentacion_server("fragmentacion", rv = rv)

  observeEvent(input$link_github, {
    browseURL("https://github.com/DavidJMartinezS")
  })
  observeEvent(input$link_geobiota, {
    browseURL("https://www.geobiota.com/")
  })
}
