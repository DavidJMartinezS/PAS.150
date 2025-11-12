#' inv_for UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_inv_for_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_column_wrap(
    width = 1/2,
    bslib::layout_column_wrap(
      width = 1,
      heights_equal = "row",
      bslib::card(
        height = "450px", 
        full_screen = T,
        bslib::card_header("Tabla IVI"),
        bslib::card_body(gt::gt_output(ns("gt_ivi")))
      ),
      bslib::card(
        height = "450px",
        full_screen = T,
        bslib::card_header("Estadísticos"),
        bslib::card_body(gt::gt_output(ns("gt_estadisticos")))
      )
    ),
    bslib::layout_column_wrap(
      width = 1,
      heights_equal = "row",
      bslib::card(
        height = "380px",
        full_screen = T,
        bslib::card_header("Intervalos de confianza de la densidad media por estado de desarrollo"),
        bslib::card_body(gt::gt_output(ns("gt_IC_prop")))
      ),
      bslib::card(
        height = "550px",
        full_screen = T,
        bslib::card_header("Mapa BNP y parcelas forestales"),
        bslib::card_body(leaflet::leafletOutput(ns("leaf_inv_fore"), height = "540px"))
      )
    )
  )
}

#' inv_for Server Functions
#'
#' @noRd
mod_inv_for_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    estadisticos <- reactive({
      req(rv$BD_inv_forestales)
      req(c(rv$BD_fore, rv$BNP_cuenca, rv$sp))
      estadisticos_PAS150(
        a = rv$BD_fore %>% dplyr::count(Parcela, Sup_parcela) %>% dplyr::count(Sup_parcela) %>% dplyr::slice_head(n = 1) %>% .[[1]] %>% .[]/10000,
        A = rv$BNP_cuenca$Sup_ha %>% sum(),
        data = rv$BD_inv_forestales$BD_Nha,
        sp = rv$sp
      )
    })
    observeEvent(estadisticos(), {
      rv[["estadisticos"]] <- estadisticos()
    })

    output$gt_ivi <- gt::render_gt({
      validate(need(rv$BD_inv_forestales, "Requiere haber generardo la BD de inventarios forestales"))
      gt_IVI(df_ivi = rv$BD_inv_forestales$IVI)
    })

    output$gt_estadisticos <- gt::render_gt({
      validate(need(rv$BD_inv_forestales, "Requiere haber generardo la BD de inventarios forestales"))
      gt_estadisticos(df_est = estadisticos())
    })

    output$gt_IC_prop <- gt::render_gt({
      validate(need(rv$BD_inv_forestales, "Requiere haber generardo la BD de inventarios forestales"))
      gt_prop(df_est = estadisticos(), df_prop = rv$BD_inv_forestales$prop, BNP_cuenca = rv$BNP_cuenca, sp = rv$sp)
    })

    output$leaf_inv_fore <- leaflet::renderLeaflet({
      validate(need(rv$BD_inv_forestales, "Requiere haber generardo la BD de inventarios forestales"))
      req(c(rv$BNP_cuenca, rv$cuenca, rv$sp))

      leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.DeLorme, group = "Esri.DeLorme") %>%
        leaflet::addPolygons(
          data = rv$cuenca %>% sf::st_transform(4326),
          fillOpacity = 0,
          weight = 2,
          color = "#00A087FF",
          dashArray = "10,5"
        ) %>%
        leaflet::addPolygons(
          data = rv$BNP_cuenca %>% sf::st_transform(4326),
          fillColor = "#84BD00FF",
          fillOpacity = 0.8,
          group = "BNP",
          weight = 2,
          color = "snow",
        ) %>%
        leaflet::addCircles(
          data = rv$BD_inv_forestales$BD_Nha %>% 
            dplyr::select(1, 3:4, rv$sp, Nha_Total) %>% 
            dplyr::rename(ECC = 4) %>% 
            dplyr::mutate(popup = paste0(
              "<h2 style='font-weight: bold;text-align: center;'>", Parcela, "</h2>",
              "UTM E: ", UTM_E, "<br/>",
              "UTM N: ", UTM_N, "<br/>",
              "NHA ECC: ", ECC, "<br/>",
              "NHA Total: ", Nha_Total
            )) %>% 
            sf::st_as_sf(coords = c("UTM_E", "UTM_N"), crs = get_utm_epsg(rv$BNP_cuenca), remove = F) %>% 
            sf::st_transform(4326),
          group = "PTS",
          color = "#920000",
          popup = ~ popup
        ) %>%
        leaflet.extras::addFullscreenControl(position = "topleft") %>%
        leaflet.extras::addResetMapButton() %>%
        leaflet::addMiniMap(
          position = "topright",
          tiles = leaflet::providers$Esri.WorldStreetMap,
          toggleDisplay = TRUE,
          minimized = FALSE
        ) %>%
        leaflet::addMeasure(
          position = "topleft",
          primaryLengthUnit = "meters",
          secondaryLengthUnit = "kilometers",
          primaryAreaUnit = "sqmeters"
        ) %>%
        leaflet::addScaleBar(
          position = "bottomright",
          options = leaflet::scaleBarOptions(imperial = FALSE)
        ) %>%
        leaflet::addLegend(
          title = "Legend",
          colors = c("#84BD00FF","#920000","#00A087FF"),
          labels = c(paste0("Bosque Nativo de Preservación de ", rv$sp), "Unidades de muestreo","Cuenca"),
          opacity = c(1,1,1),
          group = 'legend',
          position = "bottomleft"
        ) %>%
        leaflet::addLayersControl(
          overlayGroups = c("BNP", "PTS", "legend"),
          baseGroups = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery", "Esri.DeLorme"),
          options = leaflet::layersControlOptions(collapsed = T)
        )
    })
  })
}

## To be copied in the UI
# mod_inv_for_ui("inv_for_1")

## To be copied in the server
# mod_inv_for_server("inv_for_1")
