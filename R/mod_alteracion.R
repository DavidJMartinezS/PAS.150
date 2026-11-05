#' alteracion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_alteracion_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fillable(
    bslib::layout_columns(
      col_widths = c(7,5), 
      bslib::card(
        tags$h4("Alteración basada en BNP remanente", style = "font-weight: bold;"),
        text_box("Esta función permite crear la capa de alteración de hábitat correspondiente a aquellos polígonos de BNP que, por causa de las obras del proyecto, se subdividen dejando un remanente con una superficie inferior a la establecida. Esta superficie umbral podría ser la superficie minima viable para la presencia de BNP en la zona, o bien la superficie bajo la cual el bosque pierde su protección legal"),
        tags$div(
          style = "display:flex; align-items: flex-end; gap: 15px;",
          numericInput(ns("sup_ha_max"), "Superficie umbral (ha):", value = 0.5, min = 0, step = 0.1, width = "200px"),
          tags$div(
            style = "display:flex; margin-bottom: 16px; gap: 15px;",
            actionButton(ns("run_rem"), "Generar", icon = icon("play"), class = "btn-primary"),
            mod_view_table_ui(ns("view_rem")),
            mod_downfiles_ui(ns("down_rem"), style = "")
          )
        ),
        tags$hr(),
        tags$h4("Alteración basada en buffer", style = "font-weight: bold;"),
        tags$div(
          id = "inline",
          shinyWidgets::pickerInput(
            inputId = ns("tipo_buffer"), 
            label = "Tipo buffer:",
            choices = c(
              "Buffer fijo" = "buff_fijo", 
              "Buffer variable" = "buff_variable",
              "Buffer a individuos" = "buff_ind"
            ),
            selected = "buff_fijo",
            width = "200px",
            options = shinyWidgets::pickerOptions(
              container = "body",
              style = "btn-outline-primary"
            )
          )
        ),
        conditionalPanel(
          condition = "input.tipo_buffer == 'buff_fijo'", 
          ns = ns,
          tags$div(
            text_box("Esta función permite crear un área buffer a una distancia fija alrededor del BNP a intervenir."),
            tags$div(
              id = "inline",
              numericInput(ns("buffer_fijo"), label = "Distancia buffer (m):", value = 10, min = 0)
            )
          )
        ),
        conditionalPanel(
          condition = "input.tipo_buffer == 'buff_variable'", 
          ns = ns,
          tags$div(
            text_box("Esta función permite crear un área buffer variable alrededor del BNP a intervenir, basado en los radios de copa, el clima y la pendiente, de acuerdo a la guía de conaf 'Propuesta de criterios y variables para la evaluación de la alteración de hábitat'."),
            tags$div(
              style = "display:flex; align-items: flex-top; gap: 10px;",
              fileInput(
                inputId = ns("dem"),
                label = "Ingresar DEM", 
                accept = c(".tif", ".tiff"), 
                width = "650px",
                buttonLabel = "Seleccionar",
                placeholder = "Archivo no seleccionado"
              ),
              radioButtons(
                inputId = ns("stat_slope"), "Pendiente:", 
                choices = c("Mediana" = "median", "Promedio" = "mean"), 
                selected = "median"
              ),
              numericInput(ns("radio_copa"), "Radio de copas (m):", value = 1.5, min = 0),
              numericInput(ns("clima"), "Clima:", value = 4, min = 2, max = 5),
              numericInput(ns("decimal_buffer"), "Decimales buffer:", value = 1, min = 0)
            )
          )
        ),
        conditionalPanel(
          condition = "input.tipo_buffer == 'buff_fijo' || input.tipo_buffer == 'buff_variable'",
          ns = ns,
          tags$div(
            style = "display:flex; align-items: flex-end; gap: 15px;",
            shinyWidgets::virtualSelectInput(
              inputId = ns("nom_obras"), "Campo Obra:",
              choices = NULL, width = "30%", dropboxWrapper = "body"
            ),
            shinyWidgets::virtualSelectInput(
              inputId = ns("ord_obras"), "Orden obras:", choices = NULL,
              multiple = TRUE, width = "70%", dropboxWrapper = "body"
            )
          )
        ),
        conditionalPanel(
          condition = "input.tipo_buffer == 'buff_ind'", 
          ns = ns,
          tags$div(
            text_box("Esta función permite crear un área de alteración de hábitat de BNP alrededor de los individuos ubicados a una cierta distancia de las obras, formando un polígono convexo o cóncavo en torno a ellos."),
            shinyWidgets::chooseSliderSkin("Flat", color = "LightSeaGreen"),
            tags$div(
              style = "display:flex; align-items: flex-end; gap: 15px;",
              numericInput(ns("distancia"), "Distancia a obra (m):", value = 5, min = 0),
              numericInput(ns("buffer"), "Radio buffer (m):", value = 5, min = 0),
              numericInput(ns("dTolerance"), "Tolerancia (decimales):", value = 2, min = 0),
              sliderInput(ns("radio"), "Concavidad (radio):", min = 0, max = 1, value = 1, step = 0.1)
            )
          )
        ),
        tags$div(
          style = "display:flex; gap: 15px;",
          actionButton(ns("run_buff"), "Generar", icon = icon("play"), class = "btn-primary"),
          mod_view_table_ui(ns("view_buff")),
          mod_downfiles_ui(ns("down_buff"), style = "")
        ),
        tags$hr(),
        tags$h4("Unión de Criterios de Alteración", style = "font-weight: bold;"),
        checkboxInput(ns("include_inter"), label = "Incluir intervención", value = FALSE),
        radioButtons(
          ns("merge_type"), 
          label = "Criterio de unión:",
          choices = c(
            "Permitir solape" = "u_solape",
            "Priorizar remanentes (sin solape)" = "u_rem",
            "Union de geometrias (mantiene ambas sin solape)" = "u_geom"
          )
        ),
        tags$div(
          style = "display:flex; gap: 15px;",
          actionButton(ns("run_merge"), "Combinar Capas", icon = icon("object-ungroup"), class = "btn-primary"),
          mod_view_table_ui(ns("view_rem")),
          mod_downfiles_ui(ns("down_merge"), style = "")
        )
      ),
      # Card 2: Alteración Buffer Censo
      bslib::card(
        bslib::card_header("Mapa alteración"),
        full_screen = TRUE,
        leaflet::leafletOutput(ns("leaf_alteracion"), height = "650px")
      )
    )
  )
}
    
#' alteracion Server Functions
#'
#' @noRd 
mod_alteracion_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    local_rv <- reactiveValues(
      inter = NULL,
      alter_rem = NULL,
      alter_buff = NULL,
      alter_merge = NULL
    )

    observeEvent(rv$obras, {
      req(rv$obras)

      df <- sf::st_drop_geometry(rv$obras)
      fields <- names(df)[stringi::stri_detect_regex(names(df), "obra", case_insensitive = TRUE)]

      shinyWidgets::updateVirtualSelect(
        session = session,
        inputId = "nom_obras",
        choices = fields,
        selected = fields[1]
      )
    })

    observeEvent({rv$nom_obras; rv$obras; rv$BNP_cuenca}, {
      req(rv$obras, input$nom_obras, rv$BNP_cuenca)

      values <- rv$obras[rv$BNP_cuenca,] %>%
        sf::st_drop_geometry() %>%
        dplyr::pull(.data[[input$nom_obras]]) %>%
        unique() %>%
        stats::na.omit()

      shinyWidgets::updateVirtualSelect(
        session = session,
        inputId = "ord_obras",
        choices = values,
        selected = NA_character_
      )
    }, ignoreNULL = FALSE)

    # Ejecutar Remanente ----
    observe({
      if(all(c(isTruthy(rv$BNP_cuenca), isTruthy(rv$obras), isTruthy(rv$sp)))) {
        shinyjs::enable("run_rem") 
      } else {
        shinyjs::disable("run_rem")
      }
    })
    
    observeEvent(input$run_rem, {
      req(rv$BNP_cuenca, rv$obras, rv$sp)
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#6FB58F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Generando capa de alteración",
            tags$br(),
            "Espere un momento..."
          )
        )
      )
      on.exit({
        shinybusy::remove_modal_spinner()
      }, add = TRUE)

      local_rv$alter_rem <- tryCatch({
        get_alter_bnp_remanente(
          BNP = rv$BNP_cuenca,
          obras = rv$obras,
          sp = rv$sp,
          sup_ha_max = input$sup_ha_max
        )
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Error al generar la capa de alteración!",
          text = as.character(e$message),
          html = TRUE,
          type = "error",
          closeOnEsc = TRUE,
          showConfirmButton = TRUE,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
        return(NULL)
      })

    })

    # Ejecutar Buffer ----
    observe({
      cond <- switch(
        input$tipo_buffer,
        "buff_fijo"     = all(isTruthy(rv$BNP_cuenca), isTruthy(rv$obras), isTruthy(input$nom_obras)),
        "buff_variable" = all(isTruthy(rv$BNP_cuenca), isTruthy(rv$BNP_inter), isTruthy(rv$obras), isTruthy(input$dem), isTruthy(input$nom_obras)),
        "buff_ind"      = all(isTruthy(rv$BNP_cuenca), isTruthy(rv$obras), isTruthy(rv$censo), isTruthy(rv$sp)),
        FALSE
      )
      shinyjs::toggleState("run_buff", condition = cond)
    })
    observeEvent(input$run_buff, {
      req(rv$BNP_cuenca, rv$obras, rv$censo, rv$sp)
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#6FB58F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Generando capa de alteración",
            tags$br(),
            "Espere un momento..."
          )
        )
      )
      on.exit({
        shinybusy::remove_modal_spinner()
      }, add = TRUE)

      local_rv$alter_buff <- tryCatch({
        if(input$tipo_buffer == "buff_fijo") {
          get_alter_buffer_fijo(
            BNP = rv$BNP_cuenca,
            obras = rv$obras,
            buffer = input$buffer_fijo,
            nombre_obras = input$nom_obras,
            orden_obras = input$ord_obras
          )
        } else if(input$tipo_buffer == "buff_variable") {
          get_alter_buffer_variable(
            BNP_inter = rv$BNP_inter,
            BNP = rv$BNP_cuenca,
            obras = rv$obras,
            radio = input$radio_copa,
            clima = input$clima,
            dem = input$dem,
            stat_pend = input$stat_slope,
            decimales_buffer = input$decimal_buffer,
            nombre_obras = input$nom_obras,
            orden_obras = input$ord_obras
          )
        } else if(input$tipo_buffer == "buff_ind") {
          get_alter_censo_buffer(
            BNP = rv$BNP_cuenca,
            obras = rv$obras,
            censos = rv$censo,
            sp = rv$sp,
            distancia = input$distancia,
            buffer = input$buffer,
            dTolerance = input$dTolerance,
            radio = input$radio
          )
        }
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Error al generar la capa de alteración!",
          text = as.character(e$message),
          html = TRUE,
          type = "error",
          closeOnEsc = TRUE,
          showConfirmButton = TRUE,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
        return(NULL)
      })
    })

    # Unión de alteración ----
    observe({
      if(all(c(isTruthy(rv$BNP_cuenca), isTruthy(rv$obras), isTruthy(local_rv$alter_rem), isTruthy(local_rv$alter_buff)))) {
        shinyjs::enable("run_merge") 
      } else {
        shinyjs::disable("run_merge")
      }
    })
    observeEvent(input$run_merge, {
      req(any(
        shiny::isTruthy(local_rv$alter_rem), 
        shiny::isTruthy(local_rv$alter_buff)
      ))
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#6FB58F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Uniendo capas de alteración",
            tags$br(),
            "Espere un momento..."
          )
        )
      )
      on.exit({
        shinybusy::remove_modal_spinner()
      }, add = TRUE)

      if(input$include_inter){
        local_rv$inter <- get_BNP_intervencion(
          BNP_cuenca = rv$BNP_cuenca, 
          obras = rv$obras
        ) %>% 
          dplyr::mutate(Afectacion = "Intervencion")
      }

      inter <- local_rv$inter
      rem <- local_rv$alter_rem
      buf <- local_rv$alter_buff

      if (input$merge_type == "u_solape") {
        local_rv$alter_merge <- dplyr::bind_rows(inter, rem, buf)
      } else if (input$merge_type == "u_rem") {
        local_rv$alter_merge <- dplyr::bind_rows(inter, rem, buf) %>% 
          sf::st_difference() %>% 
          clean_geom()
      } else {
        local_rv$alter_merge <- dplyr::bind_rows(inter, st_union_no_overlaps(buf, rem))
      }
    })

    # Módulos de descarga ----
    sp_abrv <- reactive({stringi::stri_extract_all_words(rv$sp) %>% purrr::as_vector() %>% paste0(collapse = "_")})
    
    mod_downfiles_server(
      "down_rem", 
      x = reactive({ local_rv$alter_rem }), 
      name = reactive(sprintf("Alteracion_Remanente_%s", sp_abrv()))
    )
    mod_downfiles_server(
      "down_buff", 
      x = reactive({ local_rv$alter_buff }), 
      name = reactive(sprintf("Alteracion_Buffer_%s", sp_abrv()))
    )
    mod_downfiles_server(
      "down_merge", 
      x = reactive({ local_rv$alter_merge }), 
      name = reactive(sprintf("Alteracion_Consolidada_%s", sp_abrv()))
    )

    # Mapa base (se crea una sola vez, al tener la cuenca) ----
    # Estilos centralizados (para reusar en capas, leyenda y control) ----
    colores_capas <- c(
      "Cuenca"                  = "#00A087FF",
      "BNP ECC"                 = "#84BD00FF",
      "Obras"                   = "#000000",
      "Censo"                   = "#920000",
      "Alteración remanente"    = "#f1c40f",
      "Alteración buffer"       = "#e67e22",
      "Alteración consolidada"  = "#00fff2"
    )

    # Reactivo central: qué capas existen en este momento ----
    capas_activas <- reactive({
      c(
        "Cuenca"                 = isTruthy(rv$cuenca),
        "BNP ECC"                = isTruthy(rv$BNP_cuenca),
        "Obras"                  = isTruthy(rv$obras) && isTruthy(rv$cuenca),
        "Censo"                  = isTruthy(rv$censo) && isTruthy(rv$cuenca),
        "Alteración remanente"   = isTruthy(local_rv$alter_rem),
        "Alteración buffer"      = isTruthy(local_rv$alter_buff),
        "Alteración consolidada" = isTruthy(local_rv$alter_merge)
      )
    })
    output$leaf_alteracion <- leaflet::renderLeaflet({
      req(rv$cuenca, rv$BNP_cuenca)
      leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
        leaflet.extras::addFullscreenControl(position = "topleft") %>%
        leaflet.extras::addResetMapButton() %>%
        leaflet::addPolygons(
          data = rv$cuenca %>% sf::st_transform(4326),
          fillColor = "transparent",
          weight = 1,
          color = "#00A087FF",
          dashArray = "10,5",
          group = "Cuenca"
        ) %>%
        leaflet::addPolygons(
          data = rv$BNP_cuenca %>% sf::st_transform(4326),
          fillColor = "#84BD00FF",
          fillOpacity = 0.5,
          weight = 1.5,
          color = "snow",
          group = "BNP ECC"
        ) %>%
        leaflet::addMeasure(
          position = "topleft",
          primaryLengthUnit = "meters",
          secondaryLengthUnit = "kilometers",
          primaryAreaUnit = "sqmeters"
        )
        # leaflet::addLegend(
        #   title = "Legend",
        #   colors = c("#00A087FF", "#84BD00FF", "#000000", "#920000", "#f1c40f", "#e67e22", "#00fff2"),
        #   labels = c("Cuenca", "BNP ECC", "Obras", "Censo", "Alteración remanente", "Alteración buffer", "Alteración consolidada"),
        #   opacity = 1,
        #   position = "bottomleft",
        #   group = "legend"
        # ) %>% 
        # leaflet::addLayersControl(
        #   baseGroups = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery"),
        #   overlayGroups = c("Cuenca", "BNP ECC", "Obras", "Censo", "Alteración remanente", "Alteración buffer", "Alteración consolidada", "legend"),
        #   options = leaflet::layersControlOptions(collapsed = TRUE)
        # )
    })

    # Capas que se van agregando/actualizando vía proxy (sin resetear el mapa) ----
    observe({
      req(c(rv$obras, rv$cuenca))
      proxy <- leaflet::leafletProxy("leaf_alteracion", session) %>% leaflet::clearGroup("Obras")
      if (isTruthy(rv$obras) && isTruthy(rv$cuenca)) {
        proxy %>% 
          leaflet::addPolygons(
            data = rv$obras[rv$cuenca, ] %>% 
              sf::st_transform(4326) %>% 
              dplyr::mutate(popup = tabla_popup(sf::st_drop_geometry(.), cols = c(dplyr::contains("obra")))),
            popup = ~popup,
            fillColor = "transparent",
            color = "#000000", 
            opacity = 1, 
            stroke = TRUE,
            weight = 1,
            group = "Obras"
          )
      }
    })

    observe({
      proxy <- leaflet::leafletProxy("leaf_alteracion", session) %>% leaflet::clearGroup("Censo")
      if (isTruthy(rv$censo) && isTruthy(rv$cuenca)) {
        proxy %>% leaflet::addCircleMarkers(
          data = rv$censo[rv$cuenca, ] %>% sf::st_transform(4326),
          popup = ~Especie,
          radius = 3,
          color = "#920000",
          stroke = TRUE,
          fillOpacity = 0.8,
          group = "Censo"
        )
      }
    })

    observe({
      proxy <- leaflet::leafletProxy("leaf_alteracion", session) %>% leaflet::clearGroup("Alteración remanente")
      if (isTruthy(local_rv$alter_rem)) {
        proxy %>% leaflet::addPolygons(
          data = local_rv$alter_rem %>% 
            sf::st_transform(4326) %>% 
            dplyr::mutate(popup = tabla_popup(sf::st_drop_geometry(.))),
          popup = ~popup,
          fillColor = "#f1c40f",
          fillOpacity = 0.5,
          weight = 1.5,
          color = "snow",
          group = "Alteración remanente"
        )
      }
    })

    observe({
      proxy <- leaflet::leafletProxy("leaf_alteracion", session) %>% leaflet::clearGroup("Alteración buffer")
      if (isTruthy(local_rv$alter_buff)) {
        proxy %>% leaflet::addPolygons(
          data = local_rv$alter_buff %>% 
            sf::st_transform(4326) %>% 
            dplyr::mutate(popup = tabla_popup(sf::st_drop_geometry(.))),
          popup = ~popup,
          fillColor = "#e67e22",
          fillOpacity = 0.5,
          weight = 1.5,
          color = "snow",
          group = "Alteración buffer"
        )
      }
    })

    observe({
      proxy <- leaflet::leafletProxy("leaf_alteracion", session) %>% leaflet::clearGroup("Alteración consolidada")
      if (isTruthy(local_rv$alter_merge)) {
        proxy %>% leaflet::addPolygons(
          data = local_rv$alter_merge %>% 
            sf::st_transform(4326) %>% 
            dplyr::mutate(popup = tabla_popup(sf::st_drop_geometry(.))),
          popup = ~popup,
          fillColor = "#00fff2",
          fillOpacity = 0.5,
          weight = 1.5,
          color = "#0514ef",
          group = "Alteración consolidada"
        )
      }
    })

    # Leyenda dinámica (solo capas presentes) ----
    observe({
      activas <- capas_activas()
      nombres_activos <- names(activas)[activas]

      proxy <- leaflet::leafletProxy("leaf_alteracion", session) %>%
        leaflet::removeControl(layerId = "legend_alteracion")

      if (length(nombres_activos) > 0) {
        proxy %>%
          leaflet::addLegend(
            layerId = "legend_alteracion",
            title = "Legend",
            colors = unname(colores_capas[nombres_activos]),
            labels = nombres_activos,
            opacity = 1,
            position = "bottomleft"
          )
      }
    })

    # Control de capas dinámico (solo capas presentes) ----
    observe({
      activas <- capas_activas()
      nombres_activos <- names(activas)[activas]

      proxy <- leaflet::leafletProxy("leaf_alteracion", session) %>%
        leaflet::removeLayersControl()

      if (length(nombres_activos) > 0) {
        proxy %>%
          leaflet::addLayersControl(
            baseGroups = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery"),
            overlayGroups = nombres_activos,
            options = leaflet::layersControlOptions(collapsed = TRUE)
          )
      }
    })

    # Tablas de atributos en modal (DT) ----
    mod_view_table_server("view_rem", x = reactive(local_rv$alter_rem), title = "Tabla de atributos - Alteración remanente")
    mod_view_table_server("view_buff", x = reactive(local_rv$alter_buff), title = "Tabla de atributos - Alteración buffer")
    mod_view_table_server("view_merge", x = reactive(local_rv$alter_merge), title = "Tabla de atributos - Alteración consolidada")
 
  })
}
    
## To be copied in the UI
# mod_alteracion_ui("alteracion_1")
    
## To be copied in the server
# mod_alteracion_server("alteracion_1")
