#' check_bd UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_explore_bd_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fillable(
    fill = T,
    bslib::navset_card_underline(
      full_screen = TRUE,
      bslib::nav_panel(
        title = "Exploracion BD de flora",
        bslib::layout_columns(
          col_widths = c(8,4), 
          fill = T,
          DT::DTOutput(ns("explore_bd_flora")),
          leaflet::leafletOutput(ns("leaf_pts_flora"))
       )
      ),
      bslib::nav_panel(
        title = "Exploracion BD de inventarios forestales",
        bslib::layout_columns(
          col_widths = c(8,4), 
          fill = T,
          DT::DTOutput(ns("explore_bd_fore")),
          leaflet::leafletOutput(ns("leaf_pts_fore"))
        ) 
      )
    )
  )
}
    
#' check_bd Server Functions
#'
#' @noRd 
mod_explore_bd_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # DT Flora ----
    output$explore_bd_flora <- DT::renderDT({
      validate(
        need(rv$BD_flora, "Requiere haber ingresado la cartografía de uso y vegetación y la BD de flora")
      )
      DT::datatable(
        rv$BD_flora %>% 
          dplyr::select(req_names$BD_flora) %>% 
          dplyr::mutate_if(is.character, as.factor),
        class = "cell-border stripe nowrap",
        filter = "top",
        editable = "cell",
        extensions = 'Buttons',
        options = list(
          dom = 'Brtip',
          autoWidth = TRUE,
          buttons = c('copy', 'excel'),
          pageLength = 15,
          scrollX = TRUE,
          scroller = TRUE
        )
      )
    })
    
    # Leaflet Flora ----
    icon_flora <- leaflet::makeAwesomeIcon(icon = 'leaf', markerColor = "lightred", iconColor = 'white')
    output$leaf_pts_flora <- leaflet::renderLeaflet({
      req(rv$BD_flora)

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
        {if(isTruthy(rv$in_bnp_obra)){
          leaflet::addPolygons(.,
            data = rv$BNP_cuenca %>% sf::st_filter(rv$obras, .predicate = sf::st_relate, pattern = "T********") %>% sf::st_transform(4326),
            fillColor = "#f1c40f",
            fillOpacity = 0.9,
            group = "BNP c/obras",
            weight = 1,
            color = "gray40",
            highlightOptions = highlightOptions(
              color = "gray20",
              weight = 2.5,
              bringToFront = TRUE,
            )
          ) %>%
          leaflet::addPolygons(
            data = rv$BNP_cuenca %>% 
              dplyr::filter(!lengths(sf::st_relate(rv$BNP_cuenca, rv$obras, , pattern = "T********")) > 0) %>% 
              sf::st_transform(4326),
            fillColor = "#84BD00FF",
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
            data = rv$obras %>% sf::st_intersection(sf::st_union(rv$cuenca)) %>% sf::st_transform(4326), 
            fillColor = "transparent",
            group = "obras",
            color = "#000000", 
            opacity = 1, 
            stroke = TRUE,
            weight = 1
          ) 
        } else {
          leaflet::addPolygons(.,
            data = rv$BNP_cuenca %>% sf::st_transform(4326),
            fillColor = "#84BD00FF",
            fillOpacity = 0.9,
            group = "BNP",
            weight = 1,
            color = "gray40",
            highlightOptions = leaflet::highlightOptions(
              color = "gray20",
              weight = 2.5,
              bringToFront = TRUE,
            )
          )
        }} %>% 
        leaflet::addAwesomeMarkers(
          data = rv$BD_flora %>% 
            dplyr::count(Parcela, UTM_E, UTM_N) %>% 
            dplyr::select(-n) %>% 
            dplyr::mutate(popup = paste0(
              "<h2 style='font-weight: bold;text-align: center;'>", Parcela, "</h2>",
              "UTM E: ", UTM_E, "<br/>",
              "UTM N: ", UTM_N, "<br/>"
            )) %>% 
            sf::st_as_sf(coords = c("UTM_E", "UTM_N"), crs = get_utm_epsg(rv$BNP_cuenca), remove = F) %>% 
            sf::st_transform(4326),
          group = "Parcelas",
          popup = ~ popup,
          icon = icon_flora
        ) %>%
        leaflet.extras::addFullscreenControl(position = "topleft") %>%
        leaflet.extras::addResetMapButton() %>%
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
        {if(isTruthy(rv$in_bnp_obras)) {
          leaflet::addLegend(.,
            title = "Legend",
            colors = c("#f1c40f","#84BD00FF","lightred","#000000","#00A087FF"),
            labels = c(paste0("Bosque Nativo de Preservación de ", rv$sp, " con obras"), paste0("Bosque Nativo de Preservación de ", rv$sp), "Parcelas", "Obras", "Cuenca"),
            opacity = c(1,1,1,1),
            group = 'legend',
            position = "bottomleft"
          ) %>%
          leaflet::addLayersControl(
            overlayGroups = c("BNP","BNP c/obras","obras","Parcelas","legend"),
            baseGroups = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery", "Esri.DeLorme"),
            options = leaflet::layersControlOptions(collapsed = T)
          )
        } else {
          leaflet::addLegend(.,
            title = "Legend",
            colors = c("#84BD00FF","lightred","#00A087FF"),
            labels = c(paste0("Bosque Nativo de Preservación de ", rv$sp), "Parcelas", "Cuenca"),
            opacity = c(1,1,1),
            group = 'legend',
            position = "bottomleft"
          ) %>%
          leaflet::addLayersControl(
            overlayGroups = c("BNP", "Parcelas", "legend"),
            baseGroups = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery", "Esri.DeLorme"),
            options = leaflet::layersControlOptions(collapsed = T)
          )
        }} 
    })

    # DT fore ----
    output$explore_bd_fore <- DT::renderDT({
      validate(
        need(rv$BD_fore, "Requiere haber ingresado la cartografía de uso y vegetación y la BD de flora")
      )
      DT::datatable(
        rv$BD_fore %>% 
          dplyr::select(req_names$BD_fore) %>% 
          dplyr::mutate_if(is.character, as.factor),
        class = "cell-border stripe nowrap",
        filter = "top",
        editable = "cell",
        extensions = 'Buttons',
        options = list(
          dom = 'Brtip',
          autoWidth = TRUE,
          buttons = c('copy', 'excel'),
          pageLength = 15,
          scrollX = TRUE,
          scroller = TRUE
        )
      )
    })

    # Leaflet fore ----
    icon_fore <- leaflet::makeAwesomeIcon(icon = 'tree-deciduous', markerColor = 'cadetblue', iconColor = 'white')
    output$leaf_pts_fore <- leaflet::renderLeaflet({
      req(rv$BD_fore)

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
        leaflet::addAwesomeMarkers(
          data = rv$BD_fore %>% 
            dplyr::count(Parcela, UTM_E, UTM_N) %>% 
            dplyr::select(-n) %>% 
            dplyr::mutate(popup = paste0(
              "<h2 style='font-weight: bold;text-align: center;'>", Parcela, "</h2>",
              "UTM E: ", UTM_E, "<br/>",
              "UTM N: ", UTM_N, "<br/>"
            )) %>% 
            sf::st_as_sf(coords = c("UTM_E", "UTM_N"), crs = get_utm_epsg(rv$BNP_cuenca), remove = F) %>% 
            sf::st_transform(4326),
          group = "Parcelas",
          popup = ~ popup,
          icon = icon_fore
        ) %>%
        leaflet.extras::addFullscreenControl(position = "topleft") %>%
        leaflet.extras::addResetMapButton() %>%
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
          colors = c("#84BD00FF","cadetblue","#00A087FF"),
          labels = c(paste0("Bosque Nativo de Preservación de ", rv$sp), "Parcelas","Cuenca"),
          opacity = c(1,1,1),
          group = 'legend',
          position = "bottomleft"
        ) %>%
        leaflet::addLayersControl(
          overlayGroups = c("BNP", "Parcelas", "legend"),
          baseGroups = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery", "Esri.DeLorme"),
          options = leaflet::layersControlOptions(collapsed = T)
        )
    })
  })
}
