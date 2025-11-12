#' vegetacion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_vegetacion_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fillable(
    bslib::layout_columns(
      col_widths = c(6, 6), 
      tagList(
        bslib::layout_columns(
          col_widths = c(6, 6), 
          bslib::value_box(
            max_height = "200px",
            title = "Superficie de BNP de la especie (ha)",
            value = textOutput(ns("vb_sup_bnp")),
            showcase = icon("pagelines"),
            theme = bslib::value_box_theme(bg = "#f0f7f5ff", fg = "#638b66")
          ),
          bslib::value_box(
            max_height = "200px",
            title = "Superficie formaciones xerofíticas (ha)",
            value = textOutput(ns("vb_sup_fx")),
            showcase = icon("sun-plant-wilt"),
            theme = bslib::value_box_theme(bg = "#f0f7f5ff", fg = "#fbb04e")
          )
        ),
        bslib::card(
          full_screen = T,
          bslib::card_header("Tipos forestales"),
          gt::gt_output(ns("gt_tf"))
        )
      ),
      bslib::layout_column_wrap(
        width = 1,
        bslib::card(
          full_screen = T,
          bslib::card_header("Mapa de vegetación"),
          leaflet::leafletOutput(ns("leaf_bnp"))
        )
      )
    )
  )
}
    
#' vegetacion Server Functions
#'
#' @noRd 
mod_vegetacion_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$vb_sup_bnp <- renderText({
      rv$BNP_cuenca$Sup_ha %>% sum() %>% 
        format(decimal.mark = ",", big.mark = ".") %>% 
        paste0(" ha ", ((rv$BNP_cuenca$Sup_ha %>% sum()) / (rv$uso_veg$Sup_ha %>% sum()) * 100) %>% janitor::round_half_up(2) %>% paste0("(",.," %)"))
    })

    output$vb_sup_fx <- renderText({
      rv$uso_veg %>%
        dplyr::filter(stringi::stri_detect_regex(F_ley20283, "xero", case_insensitive = T)) %>%
        dplyr::pull(Sup_ha) %>%
        sum() %>%
        format(decimal.mark = ",", big.mark = ".") %>%
        paste0(
          " ha ",
          ((rv$uso_veg %>%
            dplyr::filter(stringi::stri_detect_regex(F_ley20283, "xero", case_insensitive = T)) %>%
            dplyr::pull(Sup_ha) %>%
            sum()) / (rv$uso_veg$Sup_ha %>% sum()) * 100) %>%
            janitor::round_half_up(2) %>%
            format(decimal.mark = ",") %>%
            paste0("(", ., " %)")
        )
    })
    
    output$gt_tf <- gt::render_gt({
      req(c(rv$uso_veg, rv$sp))
      gt_tipos_forestales(uso_veg = rv$uso_veg, sp = rv$sp)
    })
    
    output$leaf_bnp <- leaflet::renderLeaflet({
      req(c(rv$uso_veg, rv$BNP_cuenca, rv$cuenca))

      rv$uso_veg %>%
        dplyr::filter(stringi::stri_detect_regex(F_ley20283, "xero", case_insensitive = T)) %>%
        sf::st_transform(4326) %>%
        leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.DeLorme, group = "Esri.DeLorme") %>%
        leaflet::addPolygons(
          fillColor = "#FFDC91FF",
          fillOpacity = 0.9,
          group = "FX",
          weight = 1,
          color = "gray40",
          highlightOptions = leaflet::highlightOptions(
            color = "gray20",
            weight = 2.5,
            bringToFront = TRUE,
          )
        ) %>%
        leaflet::addPolygons(
          data = rv$uso_veg %>%
            dplyr::filter(F_ley20283 %>% stringi::stri_cmp_equiv('bosque nativo', strength = 1)) %>%
            sf::st_transform(4326),
          fillColor = "#84BD00FF",
          fillOpacity = 0.9,
          group = "BN",
          weight = 1,
          color = "gray40",
          highlightOptions = leaflet::highlightOptions(
            color = "gray20",
            weight = 2.5,
            bringToFront = TRUE,
          )
        ) %>%
        leaflet::addPolygons(
          data = rv$BNP_cuenca %>%
            dplyr::filter(F_ley20283 %>% stringi::stri_detect_regex('preser', case_insensitive = T)) %>% 
            sf::st_transform(4326),
          fillColor = "#20854EFF",
          fillOpacity = 0.9,
          group = "BNP",
          weight = 1,
          color = "gray40",
          highlightOptions = leaflet::highlightOptions(
            color = "gray20",
            weight = 2.5,
            bringToFront = TRUE,
          )
        ) %>%
        leaflet::addPolygons(
          data = rv$cuenca %>% sf::st_transform(4326),
          fillColor = "transparent",
          weight = 1,
          color = "#00A087FF",
          dashArray = "10,5"
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
          colors = c("#FFDC91FF", "#84BD00FF","#20854EFF","#00A087FF"),
          labels = c("Formaciones Xerofíticas", "Bosque Nativo","Bosque Nativo de Preservación","Cuenca"),
          opacity = c(1,1,1,1),
          group = 'legend',
          position = "bottomleft"
        ) %>%
        leaflet::addLayersControl(
          overlayGroups = c("FX","BN","BNP","legend"),
          baseGroups = c("CartoDB.Positron","OpenStreetMap","Esri.WorldImagery","Esri.DeLorme"),
          options = leaflet::layersControlOptions(collapsed = T)
        )
    })
  })
}
    
## To be copied in the UI
# mod_vegetacion_ui("vegetacion_1")
    
## To be copied in the server
# mod_vegetacion_server("vegetacion_1")
