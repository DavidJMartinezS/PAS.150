#' info_cuenca UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_info_cuenca_ui <- function(id) {
  ns <- NS(id)
   bslib::page_fillable(
    bslib::layout_column_wrap(
      fill = FALSE,
      bslib::value_box(
        max_height = "250px",
        title = "Especie",
        value = textOutput(ns("vb_sp")),
        showcase = icon("leaf"),
        theme = bslib::value_box_theme(bg = "#f0f7f5ff", fg = "#32a521")
      ),
      bslib::value_box(
        max_height = "250px",
        title = "Nombre de la cuenca",
        value = textOutput(ns("vb_nom_ssubc")),
        showcase = bsicons::bs_icon("pin-map-fill"),
        theme = bslib::value_box_theme(bg = "#f0f7f5ff", fg = "#39737c")
      ),
      bslib::value_box(
        max_height = "250px",
        title = "División Politico-Administrativa",
        value = textOutput(ns("vb_localizacion")),
        showcase = icon("book-atlas"),
        theme = bslib::value_box_theme(bg = "#f0f7f5ff", fg = "#f47942")
      ),
      bslib::value_box(
        max_height = "250px",
        title = "Superficie de la cuenca (ha) - Porcentaje región (%)",
        value = textOutput(ns("vb_sup_ssubc")),
        showcase = icon("map"),
        theme = bslib::value_box_theme(bg = "#f0f7f5ff", fg = "#b66353")
      )
    ),
    bslib::card(
      bslib::card_header("Mapa subsubcuenca"),
      leaflet::leafletOutput(ns("leaf_cuenca")),
      full_screen = T
    )
  )
}
    
#' info_cuenca Server Functions
#'
#' @noRd 
mod_info_cuenca_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    out_info <- reactive({
      req(c(rv$uso_veg, rv$cuenca))

      comunas <- sf::read_sf(
        system.file("Comunas.gdb", package = "dataPAS"),
        wkt_filter = sf::st_as_text(sf::st_geometry(
          sf::st_transform(sf::st_union(rv$cuenca), 5360)
        ))
      ) %>%
        sf::st_set_geometry("geometry") %>%
        sf::st_transform(sf::st_crs(rv$cuenca))

      nom_cuenca_vb <- rv$cuenca %>% dplyr::pull(NOM_SSUBC)

      info_cuenca <-
        sf::st_intersection(rv$cuenca, comunas) %>%
        dplyr::mutate(
          Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2)
        ) %>%
        dplyr::group_by(REGION, PROVINCIA, COMUNA) %>%
        dplyr::summarise(Sup_ha = sum(Sup_ha), .groups = "drop") %>%
        dplyr::mutate(label_div = purrr::pmap_chr(list(REGION, PROVINCIA, COMUNA), function(.x, .y, .z) paste(.x, .y, .z, sep = "-")))

      sup_cuenca_vb <-
        paste0((rv$uso_veg$Sup_ha %>% sum()) %>% format(decimal.mark = ",", big.mark = "."),
              " (",
              ((rv$uso_veg$Sup_ha %>% sum()) /
                 (
                   comunas %>%
                     dplyr::filter(REGION == info_cuenca %>% dplyr::slice_max(Sup_ha) %>% dplyr::pull(REGION)) %>%
                     sf::st_make_valid() %>%
                     dplyr::count(REGION) %>%
                     dplyr::mutate(Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2)) %>%
                     sf::st_drop_geometry() %>%
                     dplyr::pull(Sup_ha)
                 )
              ) %>%
                `*`(100) %>% janitor::round_half_up(2) %>% format(decimal.mark = ",", big.mark = "."),
              "%)"
        )
      colf_com <- leaflet::colorFactor(ggthemes::ggthemes_data$tableau$`color-palettes`$regular$`Tableau 20`$value[1:(info_cuenca$COMUNA %>% length())], info_cuenca$COMUNA)
      return(
        list(
          nom_cuenca_vb = nom_cuenca_vb,
          info_cuenca = info_cuenca,
          sup_cuenca_vb = sup_cuenca_vb,
          colf_com = colf_com
        )
      )
    })

    # Especie 
    output$vb_sp <- renderText({
      rv$sp
    })
    # Nombre ssubc
    output$vb_nom_ssubc <- renderText({
      out_info()$nom_cuenca_vb
    })
    # Localizacion
    output$vb_localizacion <- renderText({
      out_info()$info_cuenca %>% dplyr::slice_max(Sup_ha) %>% dplyr::pull(label_div)
    })
    # Superficie ssubc
    output$vb_sup_ssubc <- renderText({
      out_info()$sup_cuenca_vb
    })
    # mapa cuenca
    output$leaf_cuenca <- leaflet::renderLeaflet({
      out_info()$info_cuenca %>% 
        sf::st_transform(4326) %>% 
        leaflet::leaflet() %>% 
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group ="CartoDB.Positron") %>% 
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group ="OpenStreetMap") %>% 
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group ="Esri.WorldImagery") %>% 
        leaflet::addProviderTiles(leaflet::providers$Esri.DeLorme, group ="Esri.DeLorme") %>% 
        leaflet::addPolygons(
          fillColor = ~out_info()$colf_com(COMUNA),
          fillOpacity = .5,
          group = "Comunas",
          weight = 1,
          color = "black",
          label = ~ COMUNA,
          highlightOptions = leaflet::highlightOptions(
            color = "black",
            weight = 2.5,
            bringToFront = TRUE
          )
        ) %>% 
        leaflet::addLayersControl(
          overlayGroups = c("Comunas"),
          baseGroups = c("CartoDB.Positron","OpenStreetMap","Esri.WorldImagery","Esri.DeLorme"),
          options = leaflet::layersControlOptions(collapsed = T)
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
        )
    })
  })
}
    
## To be copied in the UI
# mod_info_cuenca_ui("info_cuenca_1")
    
## To be copied in the server
# mod_info_cuenca_server("info_cuenca_1")
