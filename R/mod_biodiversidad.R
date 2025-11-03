#' biodiversidad UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_biodiversidad_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fillable(
    bslib::layout_columns(
      col_widths = c(6, 6), 
      fillable = T, 
      tagList(
        bslib::card(
          full_screen = T,
          bslib::card_header("Frecuencia de especies acompañantes"),
          bslib::card_body(gt::gt_output(ns("gt_spp_acomp")))
        ),
        # Un layout de columnas anidado para poner los value_box en una fila
        bslib::layout_columns(
          col_widths = c(6, 6), 
          fill = F,
          bslib::value_box(
            max_height = "150px",
            title = "N° de parcelas floristicas en BNP",
            value = textOutput(ns("vb_n_parc")),
            showcase = icon("plant-wilt"),
            theme = bslib::value_box_theme(bg = "#f0f7f5ff", fg = "#1d8a66ff")
          ),
          bslib::value_box(
            max_height = "150px",
            title = "Riqueza floristica de los BNP en el área de proyecto",
            value = textOutput(ns("vb_riqueza")),
            showcase = icon("pagelines"),
            theme = bslib::value_box_theme(bg = "#f0f7f5ff", fg = "#65bd31ff")
          )
        ),
        bslib::navset_card_underline(
          full_screen = T, 
          bslib::nav_panel(title = "Hábito", plotly::plotlyOutput(ns("plt_habito"))),
          bslib::nav_panel(title = "Origen", plotly::plotlyOutput(ns("plt_origen")))
        )
      ),
      bslib::layout_column_wrap(
        width = 1,
        bslib::card(
          full_screen = T,
          bslib::card_header("Índices de biodiversidad"),
          bslib::card_body(gt::gt_output(ns("gt_indices")))
        ),
        bslib::card(
          full_screen = T,
          bslib::card_header("mapa parcelas de flora"),
          bslib::card_body(leaflet::leafletOutput(ns("leaf_flora")))
        )
      )
    )
  )
}
    
#' biodiversidad Server Functions
#'
#' @noRd 
mod_biodiversidad_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$gt_spp_acomp <- gt::render_gt({
      req(rv$BD_biodiversidad)
      gt_spp_acomp(rv$BD_biodiversidad$spp_acomp)
    })

    output$vb_n_parc <- renderText({
      req(rv$BD_biodiversidad)
      rv$BD_biodiversidad$localizacion %>% nrow()
    })

    output$vb_riqueza <- renderText({
      req(rv$BD_biodiversidad)
      rv$BD_biodiversidad$composicion %>% nrow()
    })

    output$plt_habito <- plotly::renderPlotly({
      req(rv$BD_biodiversidad)
      habito <- rv$BD_biodiversidad$composicion %>%
        dplyr::count(Habito) %>% 
      paleta_habito <- ggthemes::ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]$`Miller Stone` %>% slice(1:nrow(habito)) %>% .$value
      plotly::plot_ly(
        type = 'pie',
        labels = habito$Habito,
        values = habito$n,
        textinfo = 'label+percent',
        insidetextfont = list(color = 'black'),
        marker = list(colors = paleta_habito,line = list(color = '#FFFFFF', width = 1)),
        showlegend = FALSE
      ) %>% 
        plotly::layout(title = 'Proporcion de especies por hábito en el área de proyecto')
    })

    output$plt_origen <- plotly::renderPlotly({
      req(rv$BD_biodiversidad)
      origen <- rv$BD_biodiversidad$composicion %>%
        dplyr::count(Origen) 
      paleta_origen <- ggthemes::ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]$`Nuriel Stone` %>% dplyr::slice(1:nrow(origen)) %>% .$value
      plotly::plot_ly(
        type = 'pie',
        labels = origen$Origen,
        values = origen$n,
        textinfo = 'label+percent',
        insidetextfont = list(color = 'black'),
        marker = list(colors = paleta_origen, line = list(color = '#FFFFFF', width = 1)),
        showlegend = FALSE
      ) %>% 
        plotly::layout(title = 'Proporcion de especies por Origen en el área de proyecto')
    })

    output$gt_indices <- gt::render_gt({
      req(rv$BD_biodiversidad)
      gt_indices(rv$BD_biodiversidad$tabla_indices)
    })

    output$leaf_flora <- leaflet::renderLeaflet({
      req(c(rv$BNP_cuenca, rv$obras, rv$cuenca, rv$sp))
      req(rv$BD_biodiversidad)

      leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group ="CartoDB.Positron") %>%
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group ="OpenStreetMap") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group ="Esri.WorldImagery") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.DeLorme, group ="Esri.DeLorme") %>%
        leaflet::addPolygons(
          data = rv$cuenca %>% sf::st_transform(4326),
          fillColor = "transparent",
          weight = 1,
          color = "#00A087FF",
          dashArray = "10,5"
        ) %>%
        leaflet::addPolygons(
          data = rv$BNP_cuenca %>% sf::st_filter(obras, .predicate = sf::st_relate, pattern = "T********") %>% sf::st_transform(4326),
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
        ) %>% 
        leaflet::addCircles(
          data = rv$BD_biodiversidad$localizacion %>% 
            dplyr::mutate(
              popup = paste0(
                "<h2 style='font-weight: bold;text-align: center;'>",Parcela,"</h2>",
                "UTM E: ", janitor::round_half_up(UTM_E), "<br/>",
                "UTM N: ", janitor::round_half_up(UTM_N), "<br/>"
              )
            ) %>% 
            sf::st_as_sf(coords = c("UTM_E", "UTM_N"), crs = 32719, remove = F) %>% 
            sf::st_transform(4326),
          group = "PTS",
          color = "#920000",
          popup = ~popup
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
          colors = c("#f1c40f","#84BD00FF","#920000","#000000","#00A087FF"),
          labels = c(paste0("Bosque Nativo de Preservación de ", rv$sp, " con obras"), paste0("Bosque Nativo de Preservación de ", rv$sp), "Parcelas de flora", "Obras", "Cuenca"),
          opacity = c(1,1,1,1),
          group = 'legend',
          position = "bottomleft"
        ) %>%
        leaflet::addLayersControl(
          overlayGroups = c("BNP","BNP c/obras","obras","PTS","legend"),
          baseGroups = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery", "Esri.DeLorme"),
          options = leaflet::layersControlOptions(collapsed = T)
        )
      
    })
  })
}
    
## To be copied in the UI
# mod_biodiversidad_ui("biodiversidad_1")
    
## To be copied in the server
# mod_biodiversidad_server("biodiversidad_1")
