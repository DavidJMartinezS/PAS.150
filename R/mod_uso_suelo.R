#' uso_suelo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_uso_suelo_ui <- function(id) {
  ns <- NS(id)
  
  bslib::page_fillable(
    bslib::layout_column_wrap(
      bslib::card(
        bslib::card_header("Tabla usos y sub-usos"),
        full_screen = T,
        gt::gt_output(ns("gt_subusos"))
      ),
      bslib::card(
        bslib::card_header("Mapa sub-usos"),
        full_screen = T,
        leaflet::leafletOutput(ns("leaf_subusos"))
      )
    )
  )
}
    
#' uso_suelo Server Functions
#'
#' @noRd 
mod_uso_suelo_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$gt_subusos <- gt::render_gt({
      req(rv$uso_veg)
      gt_usos(uso_veg = rv$uso_veg)
    })
    output$leaf_subusos <- leaflet::renderLeaflet({
      req(rv$uso_veg)
      colf_sub <-
        leaflet::colorFactor(
          ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 20`$value[1:(rv$uso_veg$Subuso %>% unique() %>% length())],
          rv$uso_veg$Subuso
        )
      rv$uso_veg %>% 
        sf::st_transform(4326) %>% 
        leaflet::leaflet() %>% 
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group ="CartoDB.Positron") %>% 
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group ="OpenStreetMap") %>% 
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group ="Esri.WorldImagery") %>% 
        leaflet::addProviderTiles(leaflet::providers$Esri.DeLorme, group ="Esri.DeLorme") %>% 
        leaflet::addPolygons(
          fillColor = ~colf_sub(Subuso),
          fillOpacity = 0.9,
          group = "Subusos",
          weight = 1,
          color = "black",
          label = ~ Subuso,
          highlightOptions = leaflet::highlightOptions(
            color = "black",
            weight = 2.5,
            bringToFront = TRUE
          )
        ) %>% 
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
          title = "Subusos",
          pal = colf_sub, 
          values = ~Subuso, 
          opacity = 1,
          position =  "bottomright",
          group = 'legend'
        ) %>% 
        leaflet::addLayersControl(
          overlayGroups = c("Subusos","legend"),
          baseGroups = c("CartoDB.Positron","OpenStreetMap","Esri.WorldImagery","Esri.DeLorme"),
          options = leaflet::layersControlOptions(collapsed = T)
        ) 
    })
  })
}
    
## To be copied in the UI
# mod_uso_suelo_ui("uso_suelo_1")
    
## To be copied in the server
# mod_uso_suelo_server("uso_suelo_1")
