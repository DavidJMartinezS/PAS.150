#' medidas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_medidas_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fillable(
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        tags$div(
          style = "display:flex; align-items: flex-top; gap: 10px;",
          mod_read_sf_ui(id = ns("sitio_medidas"), label = "Sitio medidas:", width = "500px")
        ),
        leaflet::leafletOutput(ns("leaf_medidas")),
        downloadButton(
          outputId = ns("down_medidas"), 
          label = "Descargar shapefile", 
          class = "btn-primary", 
          style = "width: fit-content; white-space: nowrap;"
        ),
        DT::dataTableOutput(ns("dt_medidas"))
      ),
      bslib::navset_card_underline(
        full_screen = TRUE, 
        bslib::nav_panel(
          title = "Superficie Medidas", 
          tags$div(
            style = "display: flex; justify-content: space-between; gap: 2em; width: 100%; align-items: flex-end;",
            
            tags$div(
              style = "display: flex; flex-direction: column; align-items: center; flex: 1;",
              tags$label(
                "Afectación", 
                style = "width: 100%; text-align: center; margin-bottom: 5px; font-weight: bold; border-bottom: 2px solid #555; padding-bottom: 3px;"
              ),
              tags$div(
                style = "display: flex; gap: 1em; width: 100%;",
                tags$div(style = "flex: 1;", numericInput(ns("sup_inter"), "Intervención (ha):", NULL, width = "100%")),
                tags$div(style = "flex: 1;", numericInput(ns("sup_alter"), "Alteración (ha):", NULL, width = "100%"))
              )
            ),

            tags$div(
              style = "display: flex; flex-direction: column; align-items: center; flex: 1;",
              tags$label(
                "Reposición", 
                style = "width: 100%; text-align: center; margin-bottom: 5px; font-weight: bold; border-bottom: 2px solid #555; padding-bottom: 3px;"
              ),
              tags$div(
                style = "display: flex; gap: 1em; width: 100%;",
                tags$div(style = "flex: 1;", numericInput(ns("sup_inter_med"), "Intervención (ha):", NULL, width = "100%")),
                tags$div(style = "flex: 1;", numericInput(ns("sup_alter_med"), "Alteración (ha):", NULL, width = "100%"))
              )
            )
          ),
          downloadButton(
            outputId = ns("down_plot_sup_med"), 
            label = "Descargar gráfico", 
            class = "btn-primary",
            style = "width: fit-content; white-space: nowrap;"
          ),
          plotOutput(ns("plot_sup_med"))
        ),
        bslib::nav_panel(
          title = "Individuos Medidas", 
          tags$div(
            style = "display: flex; gap: 1em; width: 100%;",
            div(style = "flex: 1;", numericInput(ns("n_afect"), "N° ind. a afectar:", value = NULL, width = "100%")),
            div(style = "flex: 1;", numericInput(ns("n_repo"), "N° ind. a reponer:", value = NULL, width = "100%"))
          ),
          downloadButton(
            outputId = ns("down_plot_ind_med"), 
            label = "Descargar gráfico", 
            class = "btn-primary", 
            style = "width: fit-content; white-space: nowrap;"
          ),
          plotOutput(ns("plot_ind_med"))
        )
      )
    )
  )
}
    
#' medidas Server Functions
#'
#' @noRd 
mod_medidas_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    rv_med <- reactiveValues(
      sitio_medidas = NULL, 
      df_medidas = NULL
    )

    mod_read_sf_server(id = "sitio_medidas", rv = rv_med, i = "sitio_medidas", fx = function(x) {
      x %>%
        dplyr::rename_all(~ ifelse(. == "geometry", .,
          stringi::stri_trans_totitle(stringi::stri_trans_general(., "Latin-ASCII"), type = "sentence")
        )) %>% 
        dplyr::rename_at(dplyr::vars(dplyr::contains("ecc")), stringi::stri_trans_toupper) %>% 
        {if(!"Nom_ssubc" %in% names(.[])) .[] %>% dplyr::mutate(Nom_ssubc =  "") else .[]} %>% 
        {if(!"ECC" %in% names(.[])) .[] %>% dplyr::mutate(ECC = rv$sp) else .[]} %>% 
        {if(!"Nom_medida" %in% names(.[])) .[] %>% dplyr::mutate(Nom_medida = "") else .[]} %>% 
        {if(!"Nom_predio" %in% names(.[])) .[] %>% dplyr::mutate(Nom_predio = "") else .[]} %>% 
        st_add_sup_ha() %>% 
        dplyr::select(Nom_ssubc, ECC, Nom_medida, Nom_predio, Sup_ha)
    })

    output$leaf_medidas <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
        leaflet::setView(lng = -71.00, lat = -35.00, zoom = 7)
    })
    outputOptions(output, "leaf_medidas", suspendWhenHidden = FALSE)

    observeEvent(rv_med$sitio_medidas, {
      req(rv_med$sitio_medidas)
      
      medidas_base <- rv_med$sitio_medidas %>%
        sf::st_drop_geometry() %>% 
        dplyr::mutate(
          lng = NA_real_,
          lat = NA_real_
        )
        
      rv_med$df_medidas <- medidas_base

      medidas_sf <- rv_med$sitio_medidas %>% 
        sf::st_transform(4326) %>% 
        dplyr::mutate(
          popup = tabla_popup(sf::st_drop_geometry(.), cols = c(dplyr::matches("Nom_medidas|Sup_ha")))
        )
      bbox <- sf::st_bbox(medidas_sf)

      leaflet::leafletProxy("leaf_medidas", session) %>%
        leaflet::clearGroup("medidas") %>%
        leaflet::addPolygons(
          data = medidas_sf,
          popup = ~popup,
          group = "medidas",
          color = "blue",
          opacity = 0.8,
          weight = 1.5
        ) %>%
        leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
    })

    observeEvent(input$btn_goto, {
      req(rv_med$sitio_medidas)

      id_medidas <- as.integer(input$btn_goto)
      medidas_sel <- rv_med$sitio_medidas %>%
        sf::st_transform(4326) %>%
        dplyr::slice(id_medidas)

      req(nrow(medidas_sel) > 0)
      
      bbox <- sf::st_bbox(medidas_sel)
      leaflet::leafletProxy("leaf_medidas", session) %>%
        leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
    })

    # datatable ----
    output$dt_medidas <- DT::renderDataTable({
      req(rv_med$df_medidas)
      df <- rv_med$df_medidas
      
      df$Action <- sprintf(
        '<button class="btn btn-sm btn-outline-info" onclick="Shiny.setInputValue(\'%s\', \'%s\', {priority: \'event\'})" title="Zoom al sitio"><i class="fa fa-magnifying-glass-location"></i></button>',
        ns("btn_goto"), seq_len(nrow(df))
      )
      
      df_show <- df %>% 
        dplyr::select(Nom_ssubc, ECC, Nom_medida, Nom_predio, Sup_ha, Action)
      
      DT::datatable(
        df_show,
        callback = DT::JS(sprintf(
          "$('#%s div.dwnld').append($('#%s'));",
          ns("dt_medidas"), ns("down_medidas")
        )),
        rownames = FALSE,
        escape = FALSE,
        colnames = c("Nombre subsubcuenca", "ECC", "Nombre medida",
                    "Nombre predio", "Superficie (ha)", "Zoom"),
        selection = "none",
        editable = list(target = "cell", disable = list(columns = c(1, 4, 5))),
        options = list(
          dom = '<"dwnld">rtip',   # div vacío donde cae el botón; sin 'B'
          pageLength = 5,
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        )
      ) %>% 
        DT::formatRound(columns = c("Sup_ha"), digits = 0)
    })

    observeEvent(input$dt_medidas_cell_edit, {
      rv_med$df_medidas <- DT::editData(
        rv_med$df_medidas, input$dt_medidas_cell_edit, rownames = FALSE
      )
    })

    # descargar shp ----
    output$down_medidas <- downloadHandler(
      filename = function() {
        paste0("Localización_sitio_medidas", ".zip")
      },
      content = function(file) {
        req(rv_med$sitio_medidas, rv_med$df_medidas)

        tmp <- file.path(tempdir(), paste0("shp_", as.integer(Sys.time())))
        dir.create(tmp, recursive = TRUE)
        on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

        out <- rv_med$sitio_medidas
        ed  <- rv_med$df_medidas
        cols <- setdiff(intersect(names(ed), names(out)), attr(out, "sf_column"))
        for (cl in cols) out[[cl]] <- ed[[cl]]

        sf::st_write(out, file.path(tmp, "Localización_sitio_medidas.shp"))
        format_sup_ha(file.path(tmp, "Localización_sitio_medidas.shp"))
        shp2kmz(file.path(tmp, "Localización_sitio_medidas.shp"), name = "Nom_medida")

        zip::zip(file, files = list.files(tmp, pattern = "Localización_sitio_medidas"), root = tmp)
      }
    )

    # Figuras ----
    output$plot_sup_med <- renderPlot({
      req(input$sup_inter, input$sup_alter, input$sup_inter_med, input$sup_alter_med)
      grafico_medidas_superficie(input$sup_inter, input$sup_alter, input$sup_inter_med, input$sup_alter_med)
    }, res = 110)

    output$down_plot_sup_med <- downloadHandler(
      filename = function() paste0("grafico_superficie_medidas", ".png"),
      content  = function(file) {
        png(file, width = 8.5, height = 10, units = "in", res = 300)
        on.exit(dev.off())
        grafico_medidas_superficie(input$sup_inter, input$sup_alter, input$sup_inter_med, input$sup_alter_med)
      }
    )

    output$plot_ind_med <- renderPlot({
      req(input$n_afect, input$n_repo)
      grafico_medidas_individuos(input$n_afect, input$n_repo)
    }, res = 110)

    output$down_plot_ind_med <- downloadHandler(
      filename = function() paste0("grafico_individuos_medidas", ".png"),
      content  = function(file) {
        png(file, width = 8.5, height = 10, units = "in", res = 300)
        on.exit(dev.off())
        grafico_medidas_individuos(input$n_afect, input$n_repo)
      }
    )

  })
}
    
## To be copied in the UI
# mod_medidas_ui("medidas_1")
    
## To be copied in the server
# mod_medidas_server("medidas_1")
