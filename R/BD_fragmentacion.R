#' Title
#'
#' @param sf_uso objeto sf con uso de suelo
#' @param sf_obras objeto sf con las obras
#' @param path_frag ruta del archivo `xls` o `xlsx` con los resultados del FragStats 
#' @param ECC especie objeto del informe
#' @param subusos_noveg subusos que no corresponden a vegetación 
#' @param alteracion valor lógico. \code{TRUE} si existe alteración de hábitat
#' @param spp_acomp data.frame con especies acompañantes. obtener con `BD_biodiversidad()`
#' @param prop data.frame con las proporciones por estado de desarrollo de la especie objetivo. obtener con `BD_inventarios()`
#' @param estadisticos data.frame con los estadísticos de muestreo. obtener con `estadisticos_PAS150()`
#' @param portada portada para los apendices. Disponible: `default`, `MLP612`, `KIM753` u `otra`.
#' @param portada_opts opciones para personalizar una portada. ver `details`.
#'
#' @returns lista con resultados asociados al apéndice de fragmentacion
#'
#' @export

# sf_uso <- sf::read_sf("N:/SGA890 - PAS150 DIN Lo Aguirre/009_Desarrollo/02.PAS150/01. EIA - INFORME DE EXPERTO/APÉNDICE 3. Cartografía digital/02.Uso Actual de la Tierra/SHP/SGA890_Uso_actual_de_la_tierra.shp") %>% sf::st_transform(32719)
# sf_obras = sf::read_sf("N:/SGA890 - PAS150 DIN Lo Aguirre/009_Desarrollo/02.PAS150/Insumos/03. Obras/SGA890_Obras_areales_sin_traslapes_20251014.shp") %>% 
#     dplyr::filter(!Obra == "Caminos existente")
# nom_ssubc <- "Estero Lampa Entre Estero Colina y Rio Mapocho"
# path_frag = file.path("~/SGA890/Fragstats/TABLA/JOIN_PATCH25_PROX1000.xlsx")
# ECC = "Porlieria chilensis"
# subusos_noveg = sort(unique(sf_uso$Subuso)) %>% 
#   subset(stringi::stri_detect_regex(stringi::stri_trans_general(., "Latin-ASCII"), "rocoso|camino|zona|rio|lago|mineria|agricola|otros|ambiental", case_insensitive = T))
# alteracion = T
# spp_acomp = NULL
# prop = NULL
# estadisticos = NULL

get_lsm_analisis <- function(
  sf_uso, 
  sf_obras, 
  path_frag,
  ECC,
  subusos_noveg,
  alteracion = T, 
  spp_acomp = NULL, 
  prop = NULL,
  estadisticos = NULL,
  portada = "default",
  portada_opts = NULL
){
  valid_input(sf_uso, inherit = "sf", names = "Subuso", geometry = "POLYGON")
  valid_input(sf_obras, inherit = "sf", geometry = "POLYGON")
  valid_input(cuenca, inherit = "sf", geometry = "POLYGON") 
  stopifnot("Extensión de archivos no válida. 'xls' o 'xlsx'." = tools::file_ext(path_frag) %in% c("xls", "xlsx"))
  valid_input(ECC, inherit = "character")
  valid_input(subusos_noveg, inherit = c("character", "NULL"))
  if(is.null(subusos_noveg)) {
    subusos_noveg <- sort(unique(sf_uso$Subuso)) %>%
      subset(stringi::stri_detect_regex(
        stringi::stri_trans_general(., "Latin-ASCII"),
        "rocoso|camino|zona|rio|lago|mineria|agricola|otros|ambiental",
        case_insensitive = T
      ))
  }
  valid_input(alteracion, inherit = "logical")
  if(!is.null(spp_acomp)) valid_df(df = spp_acomp)
  if(!is.null(prop)) valid_df(df = prop, names = c("Proporciones", "Estado"))
  if(!is.null(estadisticos)) valid_df(df = estadisticos, names = c("Variable"))
  
  # FragStats ----
  results_antes <- {if (tools::file_ext(path_frag) == "xls"){
    readxl::read_xls(path_frag, sheet = 1)
  } else {
    readxl::read_xlsx(path_frag, sheet = 1)
  }} %>% 
    dplyr::mutate(CA = AREA, NP = 1, PID = as.character(PID)) %>% 
    dplyr::select(PID, NP, CA, AREA, SHAPE, FRAC, CORE, NCORE, PROX, ENN)
  
  results_despues <- {if (tools::file_ext(path_frag) == "xls"){
    readxl::read_xls(path_frag, sheet = 2)
  } else {
    readxl::read_xlsx(path_frag, sheet = 2)
  }} %>% 
    dplyr::mutate(CA = AREA, NP = 1, PID = as.character(PID)) %>% 
    dplyr::select(PID, NP, CA, AREA, SHAPE, FRAC, CORE, NCORE, PROX, ENN)
  
  add_total_lsm <- function(x, name_tot){
    round_df_fun <- function(df){
      df %>% 
        dplyr::mutate_at(dplyr::vars(AREA, CA, CORE), janitor::round_half_up, 2) %>% 
        dplyr::mutate_at(dplyr::vars(ENN), janitor::round_half_up, 1) %>% 
        dplyr::mutate_at(dplyr::vars(SHAPE, FRAC, PROX), janitor::round_half_up, 3)
    }
    x %>% 
      dplyr::select(PID, NP, CA, AREA:ENN) %>% 
      dplyr::bind_rows(
        x %>% 
          dplyr::summarise_at(dplyr::vars(NP, CA, CORE, NCORE), .funs = sum) %>% 
          dplyr::bind_cols(
            x %>% 
              dplyr::summarise_at(dplyr::vars(AREA, SHAPE, FRAC, PROX, ENN), .funs = mean) %>% 
              dplyr::mutate(PID = name_tot) 
          ) %>% 
          round_df_fun() %>% 
          dplyr::select(PID, NP, CA, AREA, SHAPE, FRAC, CORE, NCORE, PROX, ENN)
      )
  }
  
  antes <- results_antes %>% add_total_lsm(name_tot = 'Total Antes')
  despues <- results_despues %>% add_total_lsm(name_tot = 'Total Después')
  
  eval_frag <- function(SIGLA,DIFF,CLASE){
    DIFF <- abs(DIFF)
    if(SIGLA == 'NP') val <- ifelse(CLASE == "Aumento", ifelse(DIFF > 10, 1, ifelse(DIFF > 5, 2, ifelse(DIFF > 1, 4, 8))), 8)
    if(SIGLA %in%  c('CA', 'PROX')) val <- ifelse(CLASE == "Reducción", ifelse(DIFF > 5, 1, ifelse(DIFF > 1, 2, ifelse(DIFF < 1, 3, 4))), 4)
    if(SIGLA == 'AREA') val <- ifelse(CLASE == "Reducción", ifelse(DIFF > 5, 1,ifelse(DIFF > 1, 2, ifelse(DIFF < 1, 2, 4))), 4)
    if(SIGLA %in% c('SHAPE', 'FRAC', 'NCORE', 'ENN')) val <- ifelse(CLASE == "Aumento", ifelse(DIFF > 5, 1, ifelse(DIFF > 1, 2, ifelse(DIFF < 1, 3, 4))), 4)
    if(SIGLA == 'CORE') val <- ifelse(CLASE == "Reducción", ifelse(DIFF > 5, 1, ifelse(DIFF > 1, 2, ifelse(DIFF < 1, 3, 6))), 6)
    return(val)
  }
  
  df <- data.frame(
    SIGLA = names(antes)[-1],
    ANTES = antes %>% dplyr::slice_tail() %>% as.vector() %>% unlist() %>% unname() %>% .[-1],
    `DESPUÉS` = despues %>% dplyr::slice_tail() %>% as.vector() %>% unlist() %>% unname() %>% .[-1]
  ) %>%
    dplyr::mutate_at(2:3, as.numeric) %>%
    dplyr::mutate(
      `DIFERENCIA(%)` = (`DESPUÉS` - ANTES)/ANTES * 100,
      CLASE = ifelse(ANTES == `DESPUÉS`, "Igual", ifelse(ANTES < `DESPUÉS`, "Aumento", "Reducción"))
    ) %>% 
    dplyr::mutate_at(2:3, janitor::round_half_up, 3) %>% mutate_at(4, janitor::round_half_up, 2) %>% 
    dplyr::mutate(
      VALOR = purrr::pmap_dbl(list(SIGLA,`DIFERENCIA(%)`,CLASE), eval_frag)
    ) %>% 
    dplyr::rename(`Valoración` = VALOR, Sigla = SIGLA) %>% 
    dplyr::inner_join(tabla_guia_fragmentacion[3:6]) %>% 
    dplyr::relocate(Parámetro, .after = Sigla) %>% 
    dplyr::rename_all(stringr::str_to_upper)
  
  # Matriz de paisaje ----
  Usos <- sf_uso %>% 
    dplyr::mutate(Subuso = ifelse(
      Subuso %in% c(subusos_noveg),
      'Otros usos sin vegetación',
      Subuso
    ))   
  
  sf_obras <- sf_obras[Usos, ]
  
  matriz_paisaje <- Usos %>% 
    sf::st_drop_geometry() %>% 
    dplyr::filter(Subuso != 'Otros usos sin vegetación') %>% 
    dplyr::group_by(Subuso) %>% 
    dplyr::summarise(Antes = sum(Sup_ha)) %>% # sup antes por subuso 
    dplyr::full_join(
      Usos %>% 
        sf::st_intersection(sf::st_union(sf_obras)) %>% 
        sf::st_collection_extract("POLYGON") %>% 
        dplyr::filter(Subuso != 'Otros usos sin vegetación') %>% 
        dplyr::count(Subuso) %>% 
        dplyr::select(-n) %>%
        dplyr::mutate(Intervención = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2)) %>%
        sf::st_drop_geometry()
    ) %>% # añadir sup a intervenir por subuso
    dplyr::mutate_at('Intervención', ~ ifelse(is.na(.), 0, .)) %>% 
    janitor::adorn_totals(name = 'Total vegetación') %>% # añadir fila total vegetacion
    dplyr::mutate(Después = Antes - Intervención) %>% # añadir sup después
    dplyr::bind_rows(
      Usos %>% 
        sf::st_drop_geometry() %>% 
        dplyr::filter(Subuso == 'Otros usos sin vegetación') %>% 
        dplyr::group_by(Subuso) %>% 
        dplyr::summarise(Antes = sum(Sup_ha)) %>% 
        dplyr::full_join(
          Usos %>% 
            sf::st_intersection(sf::st_union(sf_obras)) %>% 
            sf::st_collection_extract("POLYGON") %>%
            dplyr::filter(Subuso == 'Otros usos sin vegetación') %>% 
            dplyr::count(Subuso) %>% 
            dplyr::select(-n) %>%
            dplyr::mutate(Intervención = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2)) %>%
            sf::st_drop_geometry()
        ) %>% 
        dplyr::mutate_at('Intervención', ~ ifelse(is.na(.), 0, .)) %>% 
        dplyr::mutate(Después = Antes - Intervención)
    ) %>% # añadir sup de otros usos
    dplyr::bind_rows(
      data.frame(
        Subuso = 'Total',
        Antes = .[] %>% dplyr::slice_tail(n = 2) %>% .$Antes %>% sum(),
        Intervención = .[] %>% dplyr::slice_tail(n = 2) %>% .$Intervención %>% sum(),
        Después = .[] %>% dplyr::slice_tail(n = 2) %>% .$Después %>% sum()
      )
    ) %>% 
    dplyr::mutate(Tasa = (log(Después/Antes)*100) %>% janitor::round_half_up(2)) # añadir tasa
  
  densidades_prop <- if (is.null(prop)) {NULL} else {
    prop %>% 
      dplyr::mutate(
        Promedio = (estadisticos[estadisticos$Variable == ECC, ][["Promedio"]] * Proporciones) %>% janitor::round_half_up(),
        E_rel = estadisticos[estadisticos$Variable == ECC,][["E_rel"]],
        Int_inf = janitor::round_half_up(Promedio - Promedio * E_rel / 100),
        Int_sup = janitor::round_half_up(Promedio + Promedio * E_rel / 100)
      ) %>% 
      dplyr::select(Estado, Int_inf)
  }
  
  # Tabla Evaluación ----
  eval_param <- function(param, valor){
    val <- valor
    if(param == 'Matriz del paisaje (ha)'){
      val <- ifelse(abs(matriz_paisaje[matriz_paisaje$Subuso == 'Total vegetación', 'Tasa']) > 5, 1,
                    ifelse(abs(matriz_paisaje[matriz_paisaje$Subuso == 'Total vegetación', 'Tasa']) > 1, 3, 6))
    } 
    if(param == 'Hábitat natural') val <- 8
    if(param == 'Riqueza de especies'){
      val <- if(is.null(spp_acomp)) {1} else {
        dplyr::case_when(
          nrow(spp_acomp) == 0 ~ 3,
          nrow(spp_acomp) == 1 ~ 2,
          .default = 3,
        )
      }
    } 
    if(param == 'Abundancia de especies') val <- if(is.null(spp_acomp)) {1} else {ifelse(nrow(spp_acomp) >= 1, 1, 3)}
    if(param == 'Regeneración') {
      if (is.null(densidades_prop)) {
        val <- 1
      } else if (densidades_prop$Estado %>% stringi::stri_detect_regex("regenera", case_insensitive = T) %>% any()) {
        val <- ifelse(densidades_prop[densidades_prop$Estado %>% stringr::str_detect("regenera", case_insensitive = T), "Int_inf"] > 300, 10,
                      ifelse(densidades_prop[densidades_prop$Estado %>% stringr::str_detect("regenera", case_insensitive = T), "Int_inf"] > 100, 5, 1))
      } else {
        val <- 1
      }
    }
    if(param == 'Brinzales') {
      if (is.null(densidades_prop)) {
        val <- 1
      } else if(densidades_prop$Estado %>% stringi::stri_detect_regex("brinzal", case_insensitive = T) %>% any()) {
        val <- ifelse(densidades_prop[densidades_prop$Estado %>% stringi::stri_detect_regex("brinzal", case_insensitive = T), "Int_inf"] > 250, 9,
                      ifelse(densidades_prop[densidades_prop$Estado %>% stringi::stri_detect_regex("brinzal", case_insensitive = T), "Int_inf"] > 75, 5, 1))
      } else {
        val <- 1
      }
    }
    if(param == 'Árboles adultos') {
      val <- if (is.null(densidades_prop)) {1} else {
        ifelse(densidades_prop[densidades_prop$Estado == "Adulto","Int_inf"] > 150, 9,
               ifelse(densidades_prop[densidades_prop$Estado == "Adulto","Int_inf"] > 50, 5, 1))
      }
    }
    if(param == 'Alteración') val <- ifelse(alteracion, 1, 2)
    if(param == 'Extensión de la presencia') val <- 3
    if(param == 'Sistema reproductivo') val <- ifelse(ECC == 'Carica chilensis', 2, 4)
    return(val)
  }
  
  tabla_eval <- tabla_guia_fragmentacion %>% 
    dplyr::count(N, Escala_espacial, Parámetro) %>% 
    dplyr::select(-n) %>% 
    dplyr::rename_all(stringi::stri_trans_toupper) %>% 
    merge(df[, c(2,7:8)], by = 'PARÁMETRO', all=T) %>% 
    dplyr::mutate(PONDERACIÓN = ifelse(ESCALA_ESPACIAL == 'PAISAJE', 0.2, ifelse(ESCALA_ESPACIAL == 'HÁBITAT', 0.5, 0.3))) %>% 
    dplyr::select(N, ESCALA_ESPACIAL, PARÁMETRO, `VALORACIÓN`, PONDERACIÓN) %>% 
    dplyr::arrange(N) %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(`VALORACIÓN` = purrr::map2_dbl(PARÁMETRO, `VALORACIÓN`, eval_param)) %>% 
    dplyr::inner_join(tabla_guia_fragmentacion[, c(3,5,6)] %>% dplyr::rename_all(stringi::stri_trans_toupper), by = c('PARÁMETRO','VALORACIÓN')) %>% 
    dplyr::rename_all(stringi::stri_trans_totitle, type = "sentence") %>% 
    dplyr::rename(`Categoría observada` = Categorías, Valores = Valoración) %>% 
    dplyr::mutate(`Valor ponderado` = Valores * Ponderación) %>% 
    dplyr::select(Parámetro, `Categoría observada`, Valores, Ponderación, `Valor ponderado`) 
  
  ### EXCEL ----
  title_color <- openxlsx2::wb_color(hex = "#62A39F")
  header_color <- openxlsx2::wb_color(hex = "#DFECEB")
  # Portada ----
  opts <- switch(
    portada,
    "default" = portada_opts(plantilla = "default"),
    "MLP612" = portada_opts(plantilla = "MLP612"),
    "KIM753" = portada_opts(plantilla = "KIM753"),
    "otra" = if (is.null(portada_opts)) {
      portada_opts()
    } else {
      do.call(portada_opts, portada_opts)
    }
  )
  
  nom_ssubc <- get_cuenca(sf_uso) %>% dplyr::pull(NOM_SSUBC)
  wb <- openxlsx2::wb_workbook(theme = "Integral") %>%
    wb_portada_PAS150(
      apendice = "fragmentacion",
      nom_ssubc = nom_ssubc,
      opts = opts
    )
  
  # Presentación ----
  table.fun <- function(x){
    x %>%
      stringi::stri_split_regex(pattern = '\n', simplify = T) %>%
      t() %>%
      stringi::stri_trim_both() %>%
      tibble::as_tibble() %>%
      tidyr::separate(value, into = c("Campo", "Desc"), sep = ":") %>%
      dplyr::mutate_at("Campo", paste0, ":") %>%
      dplyr::mutate_all(stringi::stri_trim_both) %>%
      suppressWarnings()
  }
  wb <- wb %>% 
    openxlsx2::wb_add_worksheet("Presentación", grid_lines = F) %>% 
    openxlsx2::wb_set_col_widths(cols = 1:2, widths = c(18, 60)) %>% 
    openxlsx2::wb_page_setup(paper_size = 1)
  
  desc_general <- c("Análisis de fragmentación y métricas de paisaje","La siguiente memoria de calculos presenta los resultados  de las métricas de paisajes obtenidas por el software FragStats, junto con el análisis de fragmentación de acuerdo a la 'Guía para la solicitud de excepcionalidad del artículo 19 de la Ley N.º 20.283 sobre Recuperación del Bosque Nativo y Fomento Forestal'. A continuación el detalle sobre el contenido de cada una de las hojas de este Excel.") %>% tibble::as_tibble_col()
  wb <- wb %>% openxlsx2::wb_add_data(x = desc_general, start_col = 1, start_row = 1, col_names = F)
  for (i in 1:2) wb <- wb %>% openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(cols = 1:2, rows = i))
  desc_h1 <- c(
    "Resultados_FragStats: Corresponde a los resultados de lás métricas de parche arrojadas por el software FragStats anterior y posterior al proyecto, con sus totales. Además se presenta una tabla con la valoración obtenida de acuerdo a la Guía antes mencionada. Los detalles de algunos campos a continuación.
SIGLA: Siglas de los parámetros indicados en el campo PARÁMETRO
ANTES: Cifras totales de los resultados de fragmentación antes del proyecto
DESPUÉS: Cifras totales de los resultados de fragmentación después del proyecto
DIFERENCIA(%): Porcentaje de cambio. Calculado como (DESPUÉS-ANTES)/ANTES
CLASE: Campo que indica si hubo un aumento o reducción de la metrica luego del proyecto
VALORACIÓN; CATEGORÍAS: Valores con sus respectivas categorías de acuerdo con la indicado en la Guía antes mencionada"
  ) %>% table.fun()
  wb <- wb %>% openxlsx2::wb_add_data(x = desc_h1, start_col = 1, start_row = nrow(desc_general) + 2, col_names = F)
  nota <- "*Los calculos de los totales varían entre la suma o el promedio, de acuerdo a la métrica de fragmentación. Suma: NP, CA, CORE, NCORE. Promedio: AREA, SHAPE, FRAC, PROX, ENN."
  wb <- wb %>% 
    openxlsx2::wb_add_data(x = nota, start_col = 1, start_row = nrow(desc_general) + 2 + nrow(desc_h1), col_names = F) %>% 
    openxlsx2::wb_add_font(dims = openxlsx2::wb_dims(cols = 1:2, rows = nrow(desc_general) + 2 + nrow(desc_h1)), italic = T, size = 9) %>% 
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(cols = 1:2, rows = nrow(desc_general) + 2 + nrow(desc_h1)), solve = T)
  desc_h2 <- c(
    "Matriz de Paisaje: En esta pestaña se presentan las superficies por subuso del suelo en la cuenca, antes y después del proyecto, a partir de los cuales se obtiene una tasa de pérdida detallada a continuación.
Tasa: Porcentaje de pérdida de superficie. Calculado como log(Después/Antes)*100."
  ) %>% table.fun()
  wb <- wb %>% 
    openxlsx2::wb_add_data(x = desc_h2, start_col = 1, start_row = nrow(desc_general) + 2 + nrow(desc_h1) + 2, col_names = F)
  desc_h3 <- c(
    "Tabla de Evaluación: En esta pestaña se muestra la tabla de evaluación de cada uno de los 19 parámetros indicados en la Guía, señalando el valor del parámetro, la ponderación, y la suma ponderada."
  ) %>% table.fun()
  wb <- wb %>% openxlsx2::wb_add_data(x = desc_h3,start_col = 1,start_row = nrow(desc_general) + 2 + nrow(desc_h1) + 2 + nrow(desc_h2) + 1, col_names = F)
  
  dim_1 <- c(
    1,
    nrow(desc_general)+2,
    nrow(desc_general)+2+nrow(desc_h1)+2,
    nrow(desc_general)+2+nrow(desc_h1)+2+nrow(desc_h2)+1
  ) %>% stringr::str_c("A",., collapse = ";")
  dim_2 <- dim_1 %>% stringr::str_replace_all("A", "B")
  dim_3 <- list(desc_general, desc_h1, nota %>% tibble::as_tibble(), desc_h2, desc_h3) %>%
    purrr::map(function(x){
      x %>% tibble::add_row() %>% dplyr::mutate(n = dplyr::row_number())
    }) %>% 
    dplyr::bind_rows() %>%
    dplyr::slice(-11) %>% 
    tibble::rownames_to_column("row") %>%
    dplyr::filter(n != 1 & !is.na(Campo)) %>% dplyr::pull(row) %>% 
    as.numeric() %>% 
    stringr::str_c("A",.,collapse = ";")
  
  wb <- wb %>% 
    openxlsx2::wb_add_cell_style(dims = dim_1 %>% stringr::str_c(dim_2,sep = ";"), horizontal = "center", vertical = "center") %>% 
    openxlsx2::wb_add_font(dims = dim_1, bold = T, size = 12, underline = "single") %>% 
    openxlsx2::wb_add_fill(dims = dim_1 %>% stringr::str_c(dim_2,sep = ";"), color = openxlsx2::wb_color(hex = "#D9D9D9")) %>% 
    openxlsx2::wb_add_cell_style(dims = dim_3, vertical = "center") %>% 
    openxlsx2::wb_add_font(dims = dim_3, bold = T) %>% 
    openxlsx2::wb_add_cell_style(dims = openxlsx2::wb_dims(rows = 1:40, cols = 1:2), wrap_text = "1")
  
  # Resultados fragstats ----
  wb <- wb %>% 
    openxlsx2::wb_add_worksheet("Resultados_FragStats", zoom = 90) %>% 
    # Titulo antes
    openxlsx2::wb_add_data(x = "ANTES (SIN PROYECTO)", dims = "A1") %>% 
    openxlsx2::wb_add_font(dims = "A1", color = openxlsx2::wb_color("white"), bold = T) %>% 
    openxlsx2::wb_add_fill(dims = "A1", color = title_color) %>% 
    openxlsx2::wb_add_cell_style(dims = "A1", horizontal = "center") %>% 
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(cols = 1:ncol(results_antes), rows = 1), solve = T) %>% 
    # Datos antes
    openxlsx2::wb_add_data(x = results_antes, start_col = 1, start_row = 2) %>% 
    openxlsx2::wb_add_font(dims = openxlsx2::wb_dims(rows = 2, cols = 1:ncol(results_antes)), bold = T) %>% 
    openxlsx2::wb_add_fill(dims = openxlsx2::wb_dims(rows = 2, cols = 1:ncol(results_antes)), color = header_color) %>% 
    openxlsx2::wb_add_cell_style(dims = openxlsx2::wb_dims(rows = 2, cols = 1:ncol(results_antes)), horizontal = "center") %>% 
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(cols = 1:ncol(results_antes), rows = 1:(nrow(results_antes)+3)),
      inner_hgrid = "thin", 
      inner_vgrid = "thin", 
      inner_hcolor = openxlsx2::wb_color(hex = "#6F7B62"), 
      inner_vcolor = openxlsx2::wb_color(hex = "#6F7B62")
    ) %>% 
    openxlsx2::wb_add_data(x = "Total antes", start_col = 1, start_row = nrow(results_antes)+3)
  for (i in c(2:10)) {
    j <- LETTERS[i]
    fx <- ifelse(i %in% c(2,3,7,8), "SUM", "AVERAGE")
    red <- ifelse(i %in% c(3,4,7), "2", ifelse(i %in% c(10), "1", ifelse(i %in% c(5,6,9), "3", "0")))
    numfmt <- ifelse(i %in% c(3,4,7), "#,##0.00", ifelse(i %in% c(10), "#,##0.0", ifelse(i %in% c(5,6,9), "#,##0.000", "0")))
    wb <- wb %>% 
      openxlsx2::wb_add_formula(
        x = sprintf("ROUND(%s(%s3:%s%s),%s)", fx, j, j, nrow(results_antes)+2, red), 
        dims = sprintf("%s%s", j, nrow(results_antes)+3)
      ) %>% 
      openxlsx2::wb_add_numfmt(
        dims = openxlsx2::wb_dims(
          x = results_antes %>% janitor::adorn_totals(), 
          from_row = 2, 
          select = "data", 
          cols = i
        ), 
        numfmt = numfmt
      ) 
  }
  wb <- wb %>% 
    openxlsx2::wb_add_font(dims = openxlsx2::wb_dims(cols = 1:ncol(results_antes), rows = nrow(results_antes) + 3), bold = T) %>% 
    # Titulo después
    openxlsx2::wb_add_data(x = "DSPUÉS (CON PROYECTO)", dims = "L1") %>% 
    openxlsx2::wb_add_font(dims = "L1", color = openxlsx2::wb_color("white"), bold = T) %>% 
    openxlsx2::wb_add_fill(dims = "L1", color = title_color) %>% 
    openxlsx2::wb_add_cell_style(dims = "L1", horizontal = "center") %>% 
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(cols = 1:ncol(results_despues) + 11, rows = 1), solve = T) %>% 
    # Datos después
    openxlsx2::wb_add_data(x = results_despues, start_col = 12, start_row = 2) %>% 
    openxlsx2::wb_add_font(dims = openxlsx2::wb_dims(rows = 2, cols = 1:ncol(results_despues), from_col = "L"), bold = T) %>% 
    openxlsx2::wb_add_fill(dims = openxlsx2::wb_dims(rows = 2, cols = 1:ncol(results_despues), from_col = "L"), color = header_color) %>% 
    openxlsx2::wb_add_cell_style(dims = openxlsx2::wb_dims(rows = 2, cols = 1:ncol(results_despues), from_col = "L"), horizontal = "center") %>% 
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(cols = 1:ncol(results_despues) + 11, rows = 1:(nrow(results_despues)+3)),
      inner_hgrid = "thin", 
      inner_vgrid = "thin", 
      inner_hcolor = openxlsx2::wb_color(hex = "#6F7B62"), 
      inner_vcolor = openxlsx2::wb_color(hex = "#6F7B62")
    ) %>% 
    openxlsx2::wb_add_data(x = "Total después", start_col = 12, start_row = nrow(results_despues)+3)
  for (i in c(2:10)) {
    j <- LETTERS[i+11]
    fx <- ifelse(i %in% c(2, 3, 7, 8), "SUM", "AVERAGE")
    red <- ifelse(i %in% c(3, 4, 7), "2", ifelse(i %in% c(10), "1", ifelse(i %in% c(5, 6, 9), "3", "0")))
    numfmt <- ifelse(i %in% c(3, 4, 7), "0.00", ifelse(i %in% c(10), "0.0", ifelse(i %in% c(5, 6, 9), "0.000", "0")))
    wb <- wb %>% 
      openxlsx2::wb_add_formula(
        x = sprintf("ROUND(%s(%s3:%s%s),%s)", fx, j, j, nrow(results_despues) + 2, red), 
        dims = sprintf("%s%s", j, nrow(results_despues)+3)
      ) %>% 
      openxlsx2::wb_add_numfmt(
        dims = openxlsx2::wb_dims(
          x = results_despues %>% janitor::adorn_totals(),
          from_row = 2,
          select = "data",
          cols = i + 11
        ),
        numfmt = numfmt
      )
  }
  wb <- wb %>% 
    openxlsx2::wb_add_font(dims = openxlsx2::wb_dims(cols = 1:ncol(results_despues) + 11, rows = nrow(results_despues) + 3), bold = T) %>% 
    # Parametros
    openxlsx2::wb_add_data(
      x = df %>% dplyr::mutate_at(3:6, ~ifelse(is.numeric(.), NA, NA_character_)), 
      dims = openxlsx2::wb_dims(rows = nrow(results_antes) + 5, cols = 1)
    ) %>% 
    openxlsx2::wb_add_font(dims = openxlsx2::wb_dims(cols = seq_len(ncol(df)), rows = nrow(results_antes) + 5), bold = T) %>% 
    openxlsx2::wb_add_fill(dims = openxlsx2::wb_dims(cols = seq_len(ncol(df)), rows = nrow(results_antes) + 5), color = header_color) %>% 
    openxlsx2::wb_add_cell_style(dims = openxlsx2::wb_dims(cols = seq_len(ncol(df)), rows = nrow(results_antes) + 5), horizontal = "center") %>% 
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(cols = 1:ncol(df), rows = 1:(nrow(df) + 1), from_row = nrow(results_antes)+5),
      inner_hgrid = "thin", 
      inner_vgrid = "thin", 
      inner_hcolor = openxlsx2::wb_color(hex = "#6F7B62"), 
      inner_vcolor = openxlsx2::wb_color(hex = "#6F7B62")
    ) %>% 
    openxlsx2::wb_add_formula(
      x = sprintf("TRANSPOSE(B%s:J%s)", nrow(results_antes)+3, nrow(results_antes)+3),
      dims = openxlsx2::wb_dims(x = df, from_row = nrow(results_antes)+5, cols = 3), 
      array = T, cm = T
    ) %>% 
    openxlsx2::wb_add_formula(
      x = sprintf("TRANSPOSE(M%s:U%s)", nrow(results_despues)+3, nrow(results_despues)+3),
      dims = openxlsx2::wb_dims(x = df, from_row = nrow(results_antes)+5, cols = 4), 
      array = T, cm = T
    ) %>% 
    openxlsx2::wb_add_formula(
      x = sprintf(
        "ROUND((%s-%s)/%s*100,2)", 
        openxlsx2::wb_dims(x = df, from_row = nrow(results_antes)+5, cols = 4), 
        openxlsx2::wb_dims(x = df, from_row = nrow(results_antes)+5, cols = 3), 
        openxlsx2::wb_dims(x = df, from_row = nrow(results_antes)+5, cols = 3)
      ),
      dims = openxlsx2::wb_dims(x = df, from_row = nrow(results_antes)+5, cols = 5), 
      array = T, 
      cm = T
    ) %>% 
    openxlsx2::wb_add_formula(
      x = sprintf(
        'IF(%s=%s,"Igual",IF(%s>%s,"Aumento","Reduccion"))', 
        openxlsx2::wb_dims(x = df, from_row = nrow(results_antes)+5, cols = 3), 
        openxlsx2::wb_dims(x = df, from_row = nrow(results_antes)+5, cols = 4), 
        openxlsx2::wb_dims(x = df, from_row = nrow(results_antes)+5, cols = 4),
        openxlsx2::wb_dims(x = df, from_row = nrow(results_antes)+5, cols = 3)
      ),
      dims = openxlsx2::wb_dims(x = df, from_row = nrow(results_antes)+5, cols = 6), 
      array = T, 
      cm = T
    ) %>% 
    openxlsx2::wb_set_col_widths(cols = c(1:10,12:21), widths = "auto")
  
  # Matriz de paisaje ----
  wb <- wb %>% 
    openxlsx2::wb_add_worksheet("Matriz de Paisaje") %>% 
    openxlsx2::wb_add_data(x = "Superficies (ha) x Subuso de suelo", dims = "A1") %>% 
    openxlsx2::wb_add_font(dims = "A1", color = openxlsx2::wb_color("white"), bold = T) %>% 
    openxlsx2::wb_add_fill(dims = "A1", color = title_color) %>% 
    openxlsx2::wb_add_cell_style(dims = "A1", horizontal = "center") %>% 
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(cols = 1:ncol(matriz_paisaje), rows = 1), solve = T) %>% 
    openxlsx2::wb_add_data(x = matriz_paisaje %>% dplyr::mutate_at(4:5, ~ifelse(is.numeric(.), NA, NA_character_)), start_row = 2) %>% 
    openxlsx2::wb_add_font(dims = openxlsx2::wb_dims(rows = 2, cols = 1:ncol(matriz_paisaje)), bold = T) %>% 
    openxlsx2::wb_add_fill(dims = openxlsx2::wb_dims(rows = 2, cols = 1:ncol(matriz_paisaje)), color = header_color) %>% 
    openxlsx2::wb_add_cell_style(dims = openxlsx2::wb_dims(rows = 2, cols = 1:ncol(matriz_paisaje)), horizontal = "center") %>% 
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(cols = 1:ncol(matriz_paisaje), rows = 1:(nrow(matriz_paisaje)+2)),
      inner_hgrid = "thin", 
      inner_vgrid = "thin", 
      inner_hcolor = openxlsx2::wb_color(hex = "#6F7B62"), 
      inner_vcolor = openxlsx2::wb_color(hex = "#6F7B62")
    ) %>% 
    openxlsx2::wb_add_formula(
      x = sprintf("%s-%s", openxlsx2::wb_dims(x = matriz_paisaje, cols = 2, from_row = 2), openxlsx2::wb_dims(x = matriz_paisaje, cols = 3, from_row = 2)),
      dims = openxlsx2::wb_dims(x = matriz_paisaje, cols = 4, from_row = 2), array = T, cm = T
    ) %>% 
    openxlsx2::wb_add_formula(
      x = sprintf("ROUND(LN(%s/%s)*100,2)", openxlsx2::wb_dims(x = matriz_paisaje, cols = 4, from_row = 2), openxlsx2::wb_dims(x = matriz_paisaje, cols = 2, from_row = 2)),
      dims = openxlsx2::wb_dims(x = matriz_paisaje, cols = 5, from_row = 2), array = T, cm = T
    ) 
  for (i in c(2:3)) {
    wb <- wb %>% 
      openxlsx2::wb_add_formula(
        x = sprintf("SUM(%s)", openxlsx2::wb_dims(x = matriz_paisaje %>% dplyr::slice(1:(nrow(matriz_paisaje)-3)), from_row = 2, cols = i)),
        dims = openxlsx2::wb_dims(x = matriz_paisaje, from_row = 2, cols = i, rows = which(matriz_paisaje$Subuso == 'Total vegetación'))
      ) %>% 
      openxlsx2::wb_add_formula(
        x = sprintf(
          "SUM(%s,%s)",
          openxlsx2::wb_dims(x = matriz_paisaje, from_row = 2, cols = i, rows = which(matriz_paisaje$Subuso == 'Total vegetación')),
          openxlsx2::wb_dims(x = matriz_paisaje, from_row = 2, cols = i, rows = which(matriz_paisaje$Subuso == 'Otros usos sin vegetación'))
        ),
        dims = openxlsx2::wb_dims(x = matriz_paisaje, from_row = 2, cols = i, rows = which(matriz_paisaje$Subuso == 'Total'))
      )
  }
  wb <- wb %>% 
    openxlsx2::wb_add_font(
      dims = openxlsx2::wb_dims(
        x = matriz_paisaje,
        cols = seq_len(ncol(matriz_paisaje)),
        rows = which(grepl('Total', matriz_paisaje$Subuso)),
        from_row = 2
      ),
      bold = T
    ) %>% 
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(
        x = matriz_paisaje,
        cols = ncol(matriz_paisaje),
        rows = which(matriz_paisaje$Subuso == 'Total vegetación'),
        from_row = 2
      ),
      color = openxlsx2::wb_color(hex = "#D3EBDA")
    ) %>% 
    openxlsx2::wb_add_numfmt(dims = openxlsx2::wb_dims(cols = 2:5, rows = 1:nrow(matriz_paisaje)+2), numfmt = "#,##0.00") %>% 
    openxlsx2::wb_set_col_widths(cols = 1, widths = "auto") %>% 
    openxlsx2::wb_set_col_widths(cols = 2:5, widths = 11) 
  
  # Tabla de Evaluación
  wb <- wb %>% 
    openxlsx2::wb_add_worksheet("Tabla de Evaluación") %>% 
    openxlsx2::wb_add_data(x = "Tabla de evaluación de los 19 parámetros del análisis de amenazas para la especie en la cuenca", dims = "A1") %>% 
    openxlsx2::wb_add_font(dims = "A1", color = openxlsx2::wb_color("white"), bold = T) %>% 
    openxlsx2::wb_add_fill(dims = "A1", color = title_color) %>% 
    openxlsx2::wb_add_cell_style(dims = "A1", horizontal = "center") %>% 
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(cols = 1:ncol(tabla_eval), rows = 1), solve = T) %>%
    openxlsx2::wb_add_data(x = tabla_eval %>% dplyr::mutate_at(5, ~ifelse(is.numeric(.),NA,NA_character_)), start_col = 1, start_row = 2) %>% 
    openxlsx2::wb_add_font(dims = openxlsx2::wb_dims(rows = 2, cols = 1:ncol(tabla_eval)), bold = T) %>% 
    openxlsx2::wb_add_fill(dims = openxlsx2::wb_dims(rows = 2, cols = 1:ncol(tabla_eval)), color = header_color) %>% 
    openxlsx2::wb_add_cell_style(dims = openxlsx2::wb_dims(rows = 2, cols = 1:ncol(tabla_eval)), horizontal = "center") %>% 
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(cols = 1:ncol(tabla_eval), rows = 1:(nrow(tabla_eval)+2)),
      inner_hgrid = "thin", 
      inner_vgrid = "thin", 
      inner_hcolor = openxlsx2::wb_color(hex = "#6F7B62"), 
      inner_vcolor = openxlsx2::wb_color(hex = "#6F7B62")
    ) %>% 
    openxlsx2::wb_add_data(x = "Suma ponderada", dims = "C22") %>% 
    openxlsx2::wb_add_formula(x = "C3:C21*D3:D21", dims = "E3:E21", array = T) %>% 
    openxlsx2::wb_add_formula(x = "SUM(E3:E21)", dims = "E22") %>% 
    openxlsx2::wb_add_font(dims = "C22:E22", bold = T) %>% 
    openxlsx2::wb_add_fill(dims = "C22:E22", color = header_color) %>% 
    openxlsx2::wb_add_cell_style(dims = "C22", horizontal = "center") %>% 
    openxlsx2::wb_add_border(
      dims = "C22:E22", inner_hgrid = "thin", 
      inner_vgrid = "thin", 
      inner_hcolor = openxlsx2::wb_color(hex = "#6F7B62"), 
      inner_vcolor = openxlsx2::wb_color(hex = "#6F7B62")
    ) %>% 
    openxlsx2::wb_merge_cells(dims = "C22:D22", solve = T) %>% 
    openxlsx2::wb_set_col_widths(cols = 1:5, widths = c(36, 85, 10, 12, 16))
  
  if (is.null(spp_acomp)) {
    wb <- wb %>% 
      openxlsx2::wb_add_fill(sheet = "Tabla de Evaluación", dims = "C14:C15", color = openxlsx2::wb_color("yellow")) 
  }
  if (is.null(prop)) {
    wb <- wb %>% 
      openxlsx2::wb_add_fill(sheet = "Tabla de Evaluación", dims = "C18:C20", color = openxlsx2::wb_color("yellow"))
  }
  
  return(
    list(
      df = df,
      matriz_paisaje = matriz_paisaje,
      tabla_eval = tabla_eval,
      wb = wb
    )
  )
}


