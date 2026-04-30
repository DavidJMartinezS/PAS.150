#' ext_capas_carto UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ext_capas_carto_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("Crear capas de la Cartografía Digital"),
    full_screen = TRUE,
    uiOutput(ns("tabla_carto_ui"))
  )
}
    
#' ext_capas_carto Server Functions
#'
#' @noRd 
mod_ext_capas_carto_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    viewing_layer <- reactiveVal(NULL)

    capas_meta <- tibble::tibble(
      id = c("cuenca", "ubic", "obras", "bnp_c", "bnp_i", "bnp_a", "censo_i", "censo_a", "uso", "veg", "frag_a", "frag_d", "est_a", "est_i", "caminos", "hidro", "curvas", "inv_fl", "inv_fo", "registros"),
      label = c("Cuenca de estudio", "Area de proyecto ubicación", "Area de proyecto obras", "BNP de la Cuenca", "BNP a intervenir", "BNP a alterar", "Censo a intervenir", "Censo a alterar", "Uso actual de la tierra", "Vegetación en la cuenca", "BNP fragmentación Antes", "BNP fragmentación Después", "Estimación del BNP a alterar", "Estimación del BNP a intervenir", "Caminos de la cuenca", "Hidrografía de la cuenca", "Curvas de nivel de la cuenca", "Parcelas de inventarios florísticos", "Parcelas de inventarios forestales", "Registros de la especie"),
      key_pattern = c("Cuenca_de_estudio", "Area_de_proyecto_Ubicación", "Area_de_proyecto_Obras", "BNP_%s_Cuenca", "BNP_%s_a_Intervenir", "BNP_%s_a_Alterar", "Censo_%s_a_Intervenir", "Censo_%s_a_Alterar", "Uso_actual_de_la_tierra", "Vegetación_en_la_cuenca", "BNP_fragmentación_%s_Antes", "BNP_fragmentación_%s_Después", "Estimación_Alteración_%s", "Estimación_Intervención_%s", "Caminos_cuenca", "Hidrografía_cuenca", "Curvas_de_Nivel_cuenca", "UTM_Inventarios_Flora", "UTM_Inventarios_Forestales", "UTM_Registros_%s"),
      fun = c("get_cuenca", "get_ubicacion", "get_obras", "get_BNP_cuenca", "get_BNP_intervencion", "get_BNP_alterar", "get_ECC_int", "get_ECC_alt", "get_uso", "get_veg", "get_BNP_antes", "get_BNP_despues", "get_BNP_alt_sin_censo", "get_BNP_int_sin_censo", "get_caminos", "get_hidro", "get_CN", "get_inv_flora", "get_inv_fores", "get_prospeccion"),
      args = list(
        list(uso_veg = "uso_veg"), # cuenca estudio
        list(obras = "obras", cuenca = "cuenca"), # ubicacion area de proyecto 
        list(obras = "obras", cuenca = "cuenca"), # obras area de proyecto
        list(uso_veg = "uso_veg", sp = "sp"), # BNP de la cuenca
        list(BNP_cuenca = "BNP_cuenca", obras = "obras", BNP_inter = "BNP_inter"), # BNP intervenir
        list(BNP_alter = "BNP_alter", BNP_cuenca = "BNP_cuenca", alt_ok = "listo_bnp_alter"), # BNP alterar
        list(censo = "censo", sp = "sp", BNP_inter = "BNP_intervenir", BNP_alter = "BNP_alterar", upto5m = "upto5m"), # Censo a intervenir
        list(BNP_alter = "BNP_alter", censo = "censo", sp = "sp"), # Censo a alterar
        list(uso_veg = "uso_veg"), # Uso cuenca
        list(uso_veg = "uso_veg"), # Vegetacion cuenca
        list(BNP_cuenca = "BNP_cuenca"), # BNP antes
        list(BNP_cuenca = "BNP_cuenca", BNP_inter = "BNP_intervenir", BNP_alter = "BNP_alterar"), # BNP despues
        list(BNP_alter = "BNP_alterar", densidad = "densidad"), # Estimacion censo alter
        list(BNP_inter = "BNP_intervenir", densidad = "densidad"), # Estimacion censo inter
        list(cuenca = "cuenca"), # Caminos
        list(cuenca = "cuenca", fuente_hidro = "fuente_hidro"), # Hidrografia
        list(cuenca = "cuenca", dem = "dem"), # Curvas de nivel
        list(BD_flora = "BD_flora", BNP_cuenca = "BNP_cuenca", bd_lista = "listo_bd_flora", in_bnp_obra = "in_bnp_obra", obras = "obras"), # UTM flora
        list(BD_fore = "BD_fore", BNP_cuenca = "BNP_cuenca", bd_lista = "listo_bd_fore"), # UTM fore
        list(BD_flora = "BD_flora", BD_fore = "BD_fore", censo = "censo", sp = "sp", BNP_cuenca = "BNP_cuenca") # UTM registros
      ),
      req = list(
        c("uso_veg"), # cuenca estudio
        c("obras", "uso_veg"), # ubicacion area de proyecto 
        c("obras", "uso_veg"), # obras area de proyecto
        c("uso_veg", "sp"), # BNP de la cuenca
        c("obras", "uso_veg"), # BNP intervenir
        c("uso_veg", "BNP_alter"), # BNP alterar
        c("censo", "sp", "BNP_intervenir"), # Censo a intervenir
        c("censo", "sp"), # Censo a alterar
        c("uso_veg"), # Uso cuenca
        c("uso_veg"), # Vegetacion cuenca
        c("BNP_cuenca"), # BNP antes
        c("BNP_cuenca", "BNP_inter"), # BNP despues
        c("BNP_alter", "densidad"), # Estimacion censo alter
        c("BNP_inter", "densidad"), # Estimacion censo inter
        c("cuenca"), # Caminos
        c("cuenca"), # Hidrografia
        c("cuenca", "dem"), # Curvas de nivel
        c("BD_flora", "BNP_cuenca"), # UTM flora
        c("BD_fore", "BNP_cuenca"), # UTM fore
        c("BD_flora", "BD_fore", "censo", "sp", "BNP_cuenca") # UTM registros
      )
    )

    # render tabla
    output$tabla_carto_ui <- renderUI({
      # n <- nrow(capas_meta)
      # mid <- ceiling(n / 2)

      make_table <- function(df) {
        tags$table(
          class = "table table-sm table-hover align-middle",
          tags$thead(tags$tr(tags$th("Capa"), tags$th("Acciones", class="text-center"))),
          tags$tbody(
            purrr::pmap(df, function(id, label, req, ...) {
              tags$tr(
                tags$td(
                  bslib::tooltip(
                    trigger = list(
                      label,
                      bsicons::bs_icon("info-circle", class = "text-info ms-1", style = "cursor: help;")
                    ),
                    paste("Requiere:", paste(req, collapse = ", "))
                  )
                ),
                tags$td(
                  class = "d-flex justify-content-center align-items-center gap-2",
                  actionButton(
                    ns(paste0("btn_run_", id)), 
                    label = "Crear", 
                    class = "btn-sm btn-outline-primary"
                  ) %>% tagAppendAttributes(disabled = NA),
                  mod_downfiles_ui(ns(paste0("btn_dl_", id)), class = "btn-sm btn-success"),
                  actionButton(
                    ns(paste0("btn_see_", id)), 
                    label = "", 
                    icon = shiny::icon("eye"), 
                    class = "btn-outline-info btn-sm"
                  ) %>% tagAppendAttributes(disabled = NA)
                )
              )
            })
          )
        )
      }

      make_table(capas_meta)
    })

    # Manejar todos los eventos
    purrr::pwalk(capas_meta, function(id, label, key_pattern, fun, args, req) {
      observe({
        req_met <- purrr::every(req, ~ shiny::isTruthy(rv[[.x]]))
        if (req_met) {
          shinyjs::enable(paste0("btn_run_", id))
        } else {
          shinyjs::disable(paste0("btn_run_", id))
        }
      })

      observeEvent(input[[paste0("btn_run_", id)]], {
        call_args <- purrr::map(args, ~ rv[[.x]])

        withProgress(message = paste('Generando', label), {
          # Calculamos la clave de destino en la lista de cartografía
          sp_code <- if(!is.null(rv$sp)) stringi::stri_extract_first_words(rv$sp) else ""
          target_key <- if(grepl("%s", key_pattern)) sprintf(key_pattern, sp_code) else key_pattern
          
          res <- try(do.call(fun, call_args), silent = TRUE)
          
          if (!inherits(res, "try-error")) {
            if(is.null(rv$carto_digital_list)) rv$carto_digital_list <- list()
            rv$carto_digital_list[[target_key]] <- res
            shinyjs::enable(paste0("btn_see_", id))
          }
        })
        mod_downfiles_server(
          id = paste0("btn_dl_", id), 
          x = reactive(rv$carto_digital_list[[target_key]]), 
          name_save = target_key, 
          create_kmz = TRUE
        )
      })

      # modal visualizacion
      observeEvent(input[[paste0("btn_see_", id)]], {
        # Calculamos la clave de destino para buscar en la lista
        sp_code <- if(!is.null(rv$sp)) stringi::stri_extract_first_words(rv$sp) else ""
        target_key <- if(grepl("%s", key_pattern)) sprintf(key_pattern, sp_code) else key_pattern
        
        layer_data <- rv$carto_digital_list[[target_key]]
        
        if (is.null(layer_data)) {
          showNotification(paste("La capa", label, "no ha sido generada aún."), type = "warning")
          return()
        }
        
        # Actualizamos el valor reactivo y abrimos el modal
        viewing_layer(list(data = layer_data, label = label))
        
        showModal(modalDialog(
          title = paste("Vista previa:", label),
          leaflet::leafletOutput(ns("modal_map"), height = "600px"),
          size = "xl",
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        ))
      })

    })

    # leaflet modal
    output$modal_map <- leaflet::renderLeaflet({
      req(viewing_layer())
      dat <- viewing_layer()$data
      
      # Transformación a WGS84 y remoción de geometrías vacías
      dat_4326 <- dat %>% 
        sf::st_transform(4326) %>% 
        dplyr::filter(!sf::st_is_empty(.))
      
      map <- leaflet::leaflet(dat_4326) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
        leaflet::addLayersControl(
          baseGroups = c("CartoDB.Positron", "Esri.WorldImagery"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )

      geom_type <- as.character(sf::st_geometry_type(dat_4326, by_geometry = FALSE))
      
      if (any(grepl("POINT", geom_type))) {
        map <- map %>% leaflet::addCircleMarkers(
          color = "#920000", radius = 6, stroke = TRUE, fillOpacity = 0.8,
          popup = leafpop::popupTable(sf::st_drop_geometry(dat_4326))
        )
      } else if (any(grepl("LINESTRING", geom_type))) {
        map <- map %>% leaflet::addPolylines(
          color = "#00A087FF", weight = 3,
          popup = leafpop::popupTable(sf::st_drop_geometry(dat_4326))
        )
      } else {
        map <- map %>% leaflet::addPolygons(
          fillColor = "#84BD00FF", fillOpacity = 0.5, color = "black", weight = 1,
          popup = leafpop::popupTable(sf::st_drop_geometry(dat_4326))
        )
      }
      map
    })

  })
}
    
## To be copied in the UI
# mod_ext_capas_carto_ui("ext_capas_carto_1")
    
## To be copied in the server
# mod_ext_capas_carto_server("ext_capas_carto_1")
