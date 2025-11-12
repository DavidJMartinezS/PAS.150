#' Apéndice BD inventarios forestales
#'
#' @param portada portada para los apendices. Disponible: `default`, `MLP612`, `KIM753` u `otra`.
#' @param portada_opts opciones para personalizar una portada. ver `details`.
#' @inheritParams carto_digital
#' 
#' @details 
#' En el parametro `portada_opts` debe ingresar una lista con los argumentos de la funcion `portada_opts()`.
#'
#' @returns lista con resultados asociados al apendices de inventarios forestales
#'
#' @export
BD_inventarios <- function(
  BD_fore,
  BNP_cuenca,
  sp,
  portada = "default",
  portada_opts = NULL
) {
  valid_input(BD_fore, inherit = "data.frame", names = req_names$BD_fore)
  valid_input(BNP_cuenca, inherit = "sf")
  valid_input(sp, inherit = "character")
  portada <- match.arg(portada, c("default", "MLP612", "KIM753", "otra"))
  valid_input(portada_opts, inherit = c("NULL", "list"))

  # Datos ----
  clase_corte <- BD_fore %>%
    dplyr::pull(DAP) %>%
    na.omit() %>%
    stats::quantile(c(.99), na.rm = T) %>%
    plyr::round_any(., accuracy = 10, f = ceiling)

  clase_max <- BD_fore %>%
    dplyr::pull(DAP) %>%
    na.omit() %>%
    max() %>%
    plyr::round_any(., accuracy = 10, f = ceiling)

  BD <- BD_fore %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(
      Nha = N_ind * 10000 / Sup_parcela,
      Clase = cut(
        DAP,
        c(seq(0, clase_corte, 10), if (clase_corte != clase_max) clase_max),
        include.lowest = F,
        right = F
      ) %>%
        forcats::fct_na_value_to_level(""),
      Marca_clase = purrr::map_dbl(Clase, function(x){
        x %>% 
          stringi::stri_extract_all_regex("\\d+") %>% 
          unlist() %>% as.numeric() %>% mean()
      })
    ) %>%
    tibble::rowid_to_column('Correlativo') %>%
    dplyr::mutate(AB = 'IFERROR(PI()*(BD[[#This Row], [DAP]]/200)^2, "")')
  class(BD$AB) <- "formula"

  BD_Nha <- BD %>%
    dplyr::group_by(Parcela, Sup_parcela, UTM_E, UTM_N, Especie) %>%
    dplyr::summarise(n = sum(N_ind, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n = (n * 10000 / Sup_parcela)) %>%
    tidyr::pivot_wider(names_from = Especie, values_from = n) %>%
    dplyr::mutate(Nha_Total = rowSums(dplyr::select(., -c(1:4)), na.rm = TRUE)) %>%
    dplyr::mutate_at(-c(1:4), ~ ifelse(is.na(.), 0, .)) %>%
    dplyr::mutate_if(is.numeric, janitor::round_half_up)

  nha_sp <- BD_Nha %>% dplyr::pull(sp) %>% mean() %>% janitor::round_half_up()
  f_row_ivi <- 3
  n_estado <- BD %>%
    dplyr::filter(
      Especie == sp, 
      stringi::stri_detect_regex(Estado, "adulto|brinzal|regenera", case_insensitive = T)
    ) %>%
    dplyr::pull(Estado) %>%
    unique() %>%
    length()
  n_spp <- BD$Especie %>% unique() %>% length()
  n_par <- BD$Parcela %>% unique() %>% length()
  n_par_sp <- BD %>%
    dplyr::filter(
      Especie == sp, 
      stringi::stri_detect_regex(Estado, "adulto|brinzal|regenera", case_insensitive = T)
    ) %>%
    dplyr::pull(Parcela) %>%
    unique() %>%
    length()
  FE <- 10000 /
    (BD %>%
      dplyr::count(Parcela, Sup_parcela) %>%
      dplyr::pull(Sup_parcela) %>%
      sum())
  info_prop <- tibble::tibble(
    v1 = c("Superficie BNP cuenca", "NHA", "Individuos totales"),
    v2 = c(BNP_cuenca$Sup_ha %>% sum(), nha_sp, NA)
  )

  frec_rel <- BD %>%
    dplyr::group_by(Especie, Parcela) %>%
    dplyr::summarise(n = sum(N_ind, na.rm = T)) %>%
    dplyr::group_by(Especie) %>%
    dplyr::count(name = 'Count') %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Frecuencia_relativa = (Count / sum(Count) * 100) %>% janitor::round_half_up(1)
    ) %>%
    dplyr::select(Especie, Frecuencia_relativa)

  IVI <- BD %>%
    dplyr::mutate(G = pi * (DAP / 200)^2) %>%
    dplyr::group_by(Especie) %>%
    dplyr::summarise(
      Nha = (sum(N_ind) * janitor::round_half_up(FE, 3)) %>% janitor::round_half_up(0),
      Gha = (sum(G, na.rm = T) * janitor::round_half_up(FE, 3)) %>% janitor::round_half_up(3),
      H = mean(Altura, na.rm = T) %>% janitor::round_half_up(1)
    ) %>%
    dplyr::mutate(
      Densidad_relativa = (Nha / sum(Nha) * 100) %>% janitor::round_half_up(1),
      Dominancia_relativa = (Gha / sum(Gha) * 100) %>% janitor::round_half_up(1)
    ) %>%
    dplyr::left_join(frec_rel) %>%
    dplyr::mutate(
      IVI = (Densidad_relativa + Dominancia_relativa + Frecuencia_relativa)
    ) %>%
    janitor::adorn_totals()

  IVI[nrow(IVI), "H"] <- BD %>% dplyr::pull(Altura) %>% mean() %>% janitor::round_half_up(1)
  IVI[nrow(IVI), 5:8] <- IVI[nrow(IVI), 5:8] %>% janitor::round_half_up()

  prop <- BD %>%
    dplyr::filter(
      Especie == sp, 
      stringi::stri_detect_regex(Estado, "adulto|brinzal|regenera", case_insensitive = T)
    ) %>%
    dplyr::group_by(Parcela, Sup_parcela, UTM_E, UTM_N, Estado) %>%
    dplyr::summarise(n = sum(Nha, na.rm = T)) %>%
    dplyr::ungroup() %>%
    # dplyr::mutate(nha = (n * 10000 / Sup_parcela)) %>%
    tidyr::pivot_wider(names_from = Estado, values_from = n) %>%
    dplyr::mutate(Total = rowSums(dplyr::select(., -c(1:4)), na.rm = TRUE)) %>%
    dplyr::mutate_at(-c(1:4), ~ ifelse(is.na(.), 0, .)) %>%
    dplyr::mutate(
      dplyr::across(
        -c(Parcela, Sup_parcela, UTM_E, UTM_N, Total),
        ~ . / Total,
        .names = "{col}_rel"
      )
    )

  area_x_meanTot <- janitor::round_half_up(
    sf::st_area(sf::st_union(BNP_cuenca)) %>%
      units::set_units(ha) %>%
      units::drop_units() %>%
      janitor::round_half_up(2) *
      nha_sp
  )

  prop_2 <- prop %>%
    tidyr::pivot_longer(names_to = 'Estado', values_to = 'val', dplyr::ends_with('_rel')) %>%
    dplyr::group_by(Estado) %>%
    dplyr::summarise(Proporciones = mean(val) %>% janitor::round_half_up(3)) %>%
    dplyr::mutate(
      Individuos_totales = (Proporciones * area_x_meanTot) %>% janitor::round_half_up()
    ) %>%
    dplyr::mutate_at('Estado', stringi::stri_replace_first_fixed, '_rel', '')

  # EXCEL ----
  DT_style <- "TableStyleLight14"
  PT_style <- "TableStyleMedium7"
  title_color <- openxlsx2::wb_color(hex = "#62A39F")
  header_color <- openxlsx2::wb_color(hex = "#DFECEB")
  
  # Portada ----
  opts <- switch(
    portada,
    "default" = portada_opts(plantilla = "default"),
    "MLP612" = portada_opts(plantilla = "MLP612"),
    "KIM753" = portada_opts(plantilla = "KIM753"),
    "otra" = if (is.null(portada_opts)) portada_opts() else do.call(PAS.150::portada_opts, portada_opts)
  )

  nom_ssubc <- get_cuenca(BNP_cuenca) %>% dplyr::pull(NOM_SSUBC)
  wb <- openxlsx2::wb_workbook(theme = "Integral") %>%
    wb_portada_PAS150(
      apendice = "inventarios",
      nom_ssubc = nom_ssubc,
      opts = opts 
    )

  # Presentacion ----
  table.fun <- function(x) {
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

  desc_general <- c(
    "Base de datos de Inventario forestal",
    paste0(
      "La siguiente base de datos presenta los datos, conceptos y cálculos empleados para estimar la media poblacional del número de ejemplares de ",
      sp,
      " en la superficie cubierta con bosque nativo de preservación de la especie en la cuenca de estudio. Para ello en cada una de las pestañas se emplea los siguientes conceptos:"
    )
  ) %>%
    tibble::as_tibble_col()
  wb <- wb %>%
    openxlsx2::wb_add_data(
      x = desc_general,
      start_col = 1,
      start_row = 1,
      col_names = F
    )
  for (i in 1:2) {
    wb <- wb %>% openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(cols = 1:2, rows = i))
  }
  desc_h1 <- c(
    "BD_Inv.For:	Corresponde a la base de datos de inventario forestal registrada para cada una de las parcelas realizadas en las formaciones de bosque nativo de preservación, donde:
Parcela:	Nombre de la unidad de muestreo.
UTM_E:	Coordenada central Este de la parcela de muestreo en el sistema geodésico WGS84.
UTM_N:	Coordenada central Norte de la parcela de muestreo en el sistema geodésico WGS84.
Clase:	Clase diamétrica del Diametro equivalente
Cod_ssubc:	Código de la DGA para la subsubcuenca."
  ) %>%
    table.fun()
  wb <- wb %>%
    openxlsx2::wb_add_data(
      x = desc_h1,
      start_col = 1,
      start_row = nrow(desc_general) + 2,
      col_names = F
    )
  desc_h2 <- c(
    'Frec. sp_x_parc: En esta se muestra la tabla con las frecuencias (n° de repeticiones) de cada especie arbórea. En esta ultima se calculan los siguientes datos por especie:
Count: Indica el número de parcelas donde se registró la especie.
Frec_abs: Frecuencia absoluta. Calculada como Count / n° total de parcelas * 100.
Frec_rel: Frecuencia relativa. Calculada como Frec_abs / suma total(Count) * 100.'
  ) %>%
    table.fun()
  wb <- wb %>%
    openxlsx2::wb_add_data(
      x = desc_h2,
      start_col = 1,
      start_row = nrow(desc_general) + 2 + nrow(desc_h1) + 1,
      col_names = F
    )
  desc_h3 <- c(
    "Tabla IVI: En esta sección, se presentan los calculos para obtener el índice del valor de importancia (IVI) de cada especie arbórea. En la tabla se muestra la densidad por ha (Nha), área basal por ha (Gha), Altura promedio (H) y Frecuencia relativa por especie, además de otros campos que se detallan a continuación:
Densidad_rel: Frecuencia relativa del Nha. Nha/suma(Nha)*100.
Dominancia_rel: Frecuencia relativa del Gha. Gha/suma(Gha)*100.
IVI: Valor de importancia."
  ) %>%
    table.fun()
  wb <- wb %>%
    openxlsx2::wb_add_data(
      x = desc_h3,
      start_col = 1,
      start_row = nrow(desc_general) +
        2 +
        nrow(desc_h1) +
        1 +
        nrow(desc_h2) +
        1,
      col_names = F
    )
  desc_h4 <- paste0(
    "BD Nha: Corresponde a la base de datos del número de árboles por hectárea por cada parcela y especie. En ella se utilizan conceptos previamente descritos en las pestañas anteriores"
  ) %>%
    table.fun()
  wb <- wb %>%
    openxlsx2::wb_add_data(
      x = desc_h4,
      start_col = 1,
      start_row = nrow(desc_general) +
        2 +
        nrow(desc_h1) +
        1 +
        nrow(desc_h2) +
        1 +
        nrow(desc_h3) +
        1,
      col_names = F
    )
  desc_h5 <- c(
    "Proporciones: Este segmento se refiere a las proporciones relacionadas con los distintos estados de desarrollo de la especie en cuestión, tal como se ha detallado en el informe de expertos. Esta hoja presenta una tabla con las proporciones de los estados de desarrollo de la especie, por cada parcela de inventario, las cuales se calcularon a partir del Nha por cada estado de desarrollo. Aparte se muestra una tabla con las proporciones promedio de todas las parcelas y su extrapolación a la superficie total de BNP."
  ) %>%
    table.fun()
  wb <- wb %>%
    openxlsx2::wb_add_data(
      x = desc_h5,
      start_col = 1,
      start_row = nrow(desc_general) +
        2 +
        nrow(desc_h1) +
        1 +
        nrow(desc_h2) +
        1 +
        nrow(desc_h3) +
        1 +
        nrow(desc_h4) +
        1,
      col_names = F
    )
  desc_h6 <- c(
    "Estadígrafos: Corresponde a los estadígrafos del número de individuos por hectárea de las parcelas de inventario forestal presentadas en la hoja 'BD_Nha'. También se muestran algunos estadísticos por estado de desarrollo de la especie objeto del informe, los cuales son utilizados para el análisis de amenazas."
  ) %>%
    table.fun()
  wb <- wb %>%
    openxlsx2::wb_add_data(
      x = desc_h6,
      start_col = 1,
      start_row = nrow(desc_general) +
        2 +
        nrow(desc_h1) +
        1 +
        nrow(desc_h2) +
        1 +
        nrow(desc_h3) +
        1 +
        nrow(desc_h4) +
        1 +
        nrow(desc_h5) +
        1,
      col_names = F
    )
  dim_1 <- c(
    1,
    nrow(desc_general) + 2,
    nrow(desc_general) + 2 + nrow(desc_h1) + 1,
    nrow(desc_general) + 2 + nrow(desc_h1) + 1 + nrow(desc_h2) + 1,
    nrow(desc_general) +
      2 +
      nrow(desc_h1) +
      1 +
      nrow(desc_h2) +
      1 +
      nrow(desc_h3) +
      1,
    nrow(desc_general) +
      2 +
      nrow(desc_h1) +
      1 +
      nrow(desc_h2) +
      1 +
      nrow(desc_h3) +
      1 +
      nrow(desc_h4) +
      1,
    nrow(desc_general) +
      2 +
      nrow(desc_h1) +
      1 +
      nrow(desc_h2) +
      1 +
      nrow(desc_h3) +
      1 +
      nrow(desc_h4) +
      1 +
      nrow(desc_h5) +
      1
  ) %>%
    paste0("A", ., collapse = ";")
  dim_2 <- dim_1 %>% stringi::stri_replace_all_fixed("A", "B")
  dim_3 <- list(desc_h1, desc_h2, desc_h3, desc_h4, desc_h5) %>%
    purrr::map(function(x) {
      x %>% dplyr::add_row() %>% dplyr::mutate(n = dplyr::row_number())
    }) %>%
    dplyr::bind_rows() %>%
    tibble::rownames_to_column("row") %>%
    dplyr::filter(n != 1 & !is.na(Campo)) %>%
    dplyr::pull(row) %>%
    as.numeric() %>%
    {.[] + 3} %>%
    paste0("A", ., collapse = ";")

  wb <- wb %>%
    openxlsx2::wb_add_cell_style(
      dims = dim_1 %>% paste(dim_2, sep = ";"),
      horizontal = "center",
      vertical = "center"
    ) %>%
    openxlsx2::wb_add_font(dims = dim_1, bold = T, underline = "single", size = 12) %>%
    openxlsx2::wb_add_fill(
      dims = dim_1 %>% paste(dim_2, sep = ";"),
      color = openxlsx2::wb_color(hex = "#D9D9D9")
    ) %>%
    openxlsx2::wb_add_cell_style(dims = dim_3, vertical = "center") %>%
    openxlsx2::wb_add_font(dims = dim_3, bold = T) %>%
    openxlsx2::wb_add_cell_style(dims = openxlsx2::wb_dims(rows = 1:40, cols = 1:2), wrap_text = "1")

  # BD inv ----
  wb <- wb %>%
    openxlsx2::wb_add_worksheet("BD_Inv.For") %>%
    openxlsx2::wb_add_data_table(
      x = BD,
      first_column = T,
      table_style = DT_style,
      table_name = "BD",
      na.strings = ""
    )
  BD_wb <- openxlsx2::wb_data(wb, sheet = "BD_Inv.For")

  # Frec. spp_x_parc ----
  wb <- wb %>%
    openxlsx2::wb_add_worksheet("Frec.spp_x_parc") %>%
    openxlsx2::wb_add_pivot_table(
      x = BD_wb,
      sheet = "Frec.spp_x_parc",
      dims = "A1",
      rows = "Especie",
      cols = "Parcela",
      data = "N_ind",
      fun = "sum",
      params = list(
        row_grand_totals = FALSE,
        col_grand_totals = FALSE,
        table_style = PT_style
      )
    ) %>%
    openxlsx2::wb_add_data(x = "Cuenta", dims = sprintf("%s2", openxlsx2::int2col(n_par + 2)))

  for (i in seq_len(n_spp) + 2) {
    wb <- wb %>%
      openxlsx2::wb_add_formula(
        x = sprintf("COUNTA(B%s:%s%s)", i, openxlsx2::int2col(n_par + 1), i),
        dims = sprintf("%s%s", openxlsx2::int2col(n_par + 2), i)
      )
  }

  wb <- wb %>%
    openxlsx2::wb_add_data(x = "Frec_abs", dims = sprintf("%s2", openxlsx2::int2col(n_par + 3))) %>%
    openxlsx2::wb_add_formula(
      x = sprintf(
        "ROUND(MMULT(%s3:%s%s,1/COUNTA(B2:%s2))*100,1)",
        openxlsx2::int2col(n_par + 2),
        openxlsx2::int2col(n_par + 2),
        n_spp + 2,
        openxlsx2::int2col(n_par + 1)
      ),
      dims = sprintf(
        "%s3:%s%s",
        openxlsx2::int2col(n_par + 3),
        openxlsx2::int2col(n_par + 3),
        n_spp + 2
      ),
      array = T
    ) %>%
    openxlsx2::wb_add_data(x = "Frec_rel", dims = sprintf("%s2", openxlsx2::int2col(n_par + 4))) %>%
    openxlsx2::wb_add_formula(
      x = sprintf(
        "ROUND(MMULT(%s3:%s%s,1/SUM(%s3:%s%s))*100,1)",
        openxlsx2::int2col(n_par + 3),
        openxlsx2::int2col(n_par + 3),
        n_spp + 2,
        openxlsx2::int2col(n_par + 3),
        openxlsx2::int2col(n_par + 3),
        n_spp + 2
      ),
      dims = sprintf(
        "%s3:%s%s",
        openxlsx2::int2col(n_par + 4),
        openxlsx2::int2col(n_par + 4),
        n_spp + 2
      ),
      array = T
    ) %>%
    openxlsx2::wb_add_font(
      dims = sprintf("%s2:%s2", openxlsx2::int2col(n_par + 2), openxlsx2::int2col(n_par + 4)),
      bold = T,
      color = openxlsx2::wb_color("white")
    ) %>%
    openxlsx2::wb_add_fill(
      dims = sprintf("%s2:%s2", openxlsx2::int2col(n_par + 2), openxlsx2::int2col(n_par + 4)),
      color = title_color
    ) %>%
    openxlsx2::wb_add_cell_style(
      dims = sprintf("%s2:%s2", openxlsx2::int2col(n_par + 2), openxlsx2::int2col(n_par + 4)),
      horizontal = "center"
    ) %>%
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(
        rows = 1:n_spp,
        cols = 1:3,
        from_row = 3,
        from_col = n_par + 2
      ),
      right_border = NULL,
      bottom_border = NULL,
      top_border = NULL
    ) %>%
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(rows = 2, cols = n_par + 2),
      right_border = NULL,
      top_border = NULL,
      bottom_border = NULL
    )

  # Tabla IVI ----
  wb <- wb %>%
    openxlsx2::wb_add_worksheet("Tabla IVI") %>%
    openxlsx2::wb_add_data(x = "Factor de expansión", dims = "A1") %>%
    openxlsx2::wb_add_font(dims = "A1", bold = T) %>%
    openxlsx2::wb_add_formula(
      x = sprintf(
        "10000/SUM(BD_Nha!%s)",
        openxlsx2::wb_dims(x = BD_Nha, cols = "Sup_parcela")
      ),
      dims = "B1"
    ) %>%
    openxlsx2::wb_add_numfmt(dims = "B1", numfmt = "0.000") %>%
    openxlsx2::wb_add_border(
      dims = "A1:B1",
      inner_hgrid = "thin",
      inner_vgrid = "thin"
    ) %>% 
    openxlsx2::wb_add_data(
    x = t(tibble::as_tibble_col(c(
      "Especie",
      "Suma de N_ind",
      "Suma de AB",
      "Promedio de Altura"
    ))),
    dims = "A3",
    col_names = F
  ) %>%
  openxlsx2::wb_add_formula(
    x = "_xlfn.UNIQUE(BD[Especie])",
    dims = "A4",
    array = T,
    cm = T
  ) %>%
  openxlsx2::wb_add_data(x = "Total general", dims = sprintf("A%s", n_spp + 4)) %>% 
  openxlsx2::wb_add_formula(
    x = sprintf(
      "SUMIFS(BD[N_ind], BD[Especie], %s)",
      openxlsx2::wb_dims(rows = 1:n_spp, from_row = 4)
    ),
    dims = openxlsx2::wb_dims(
      rows = 1:n_spp,
      from_row = 4,
      from_col = 2
    ),
    array = T,
    cm = TRUE
  ) %>%
  openxlsx2::wb_add_formula(
    x = sprintf("SUM(B4:B%s)", n_spp + 3),
    dims = sprintf("B%s", n_spp + 4)
  ) %>% 
  openxlsx2::wb_add_numfmt(
    dims = openxlsx2::wb_dims(rows = 1:(n_spp + 1), from_row = 4, from_col = 2),
    numfmt = "#,##0"
  ) %>%
  openxlsx2::wb_add_formula(
    x = sprintf(
      "SUMIFS(BD[AB], BD[Especie], %s)",
      openxlsx2::wb_dims(rows = 1:n_spp, from_row = 4)
    ),
    dims = openxlsx2::wb_dims(
      rows = 1:n_spp,
      from_row = 4,
      from_col = 3
    ),
    array = T,
    cm = TRUE
  ) %>%
  openxlsx2::wb_add_formula(
    x = sprintf("SUM(C4:C%s)", n_spp + 3),
    dims = sprintf("C%s", n_spp + 4)
  ) %>% 
  openxlsx2::wb_add_numfmt(
    dims = openxlsx2::wb_dims(rows = 1:(n_spp + 1), from_row = 4, from_col = 3),
    numfmt = "#,##0.000"
  ) %>%
  openxlsx2::wb_add_formula(
    x = sprintf(
      "AVERAGEIFS(BD[Altura], BD[Especie], %s)",
      openxlsx2::wb_dims(rows = 1:n_spp, from_row = 4)
    ),
    dims = openxlsx2::wb_dims(
      rows = 1:n_spp,
      from_row = 4,
      from_col = 4
    ),
    array = T,
    cm = TRUE
  ) %>%
  openxlsx2::wb_add_formula(
    x = sprintf("SUM(D4:D%s)", n_spp + 3),
    dims = sprintf("D%s", n_spp + 4)
  ) %>% 
  openxlsx2::wb_add_numfmt(
    dims = openxlsx2::wb_dims(rows = 1:(n_spp + 1), from_row = 4, from_col = 4),
    numfmt = "#,##0.0"
  ) %>%
  openxlsx2::wb_add_data(x = "Nha (Ind/ha)", dims = "E3") %>%
  openxlsx2::wb_add_formula(
    x = sprintf("ROUND(MMULT(B4:B%s,B1),0)", n_spp + 4),
    dims = sprintf("E4:E%s", n_spp + 4),
    array = T,
    cm = TRUE
  ) %>%
  openxlsx2::wb_add_data(x = "Gha (m2/ha)", dims = "F3") %>%
  openxlsx2::wb_add_formula(
    x = sprintf("ROUND(MMULT(C4:C%s,B1),3)", n_spp + 3),
    dims = sprintf("F4:F%s", n_spp + 3),
    array = T,
    cm = TRUE
  ) %>%
  openxlsx2::wb_add_formula(
    x = sprintf("ROUND(SUM(F4:F%s),3)", n_spp + 3),
    dims = sprintf("F%s", n_spp + 4)
  ) %>%
  openxlsx2::wb_add_data(x = "Densidad_rel", dims = "G3") %>%
  openxlsx2::wb_add_formula(
    x = sprintf(
      "ROUND(MMULT(E4:E%s,1/E%s)*100,1)",
      n_spp + 3,
      n_spp + 4
    ),
    dims = sprintf("G4:G%s", n_spp + 3),
    array = T,
    cm = TRUE
  ) %>%
  openxlsx2::wb_add_data(x = "Dominancia_rel", dims = "H3") %>%
  openxlsx2::wb_add_formula(
    x = sprintf(
      "ROUND(MMULT(F4:F%s,1/F%s)*100,1)",
      n_spp + 3,
      n_spp + 4
    ),
    dims = sprintf("H4:H%s", n_spp + 3),
    array = T,
    cm = TRUE
  ) %>%
  openxlsx2::wb_add_data(x = "Frecuencia_rel", dims = "I3") %>%
  openxlsx2::wb_add_formula(
    x = sprintf(
      "MMULT(Frec.spp_x_parc!%s3:%s%s,1)",
      openxlsx2::int2col(n_par + 4),
      openxlsx2::int2col(n_par + 4),
      n_spp + 2
    ),
    dims = sprintf("I4:I%s", n_spp + 3),
    array = T,
    cm = TRUE
  ) %>%
  openxlsx2::wb_add_data(x = "IVI", dims = "J3") %>%
  openxlsx2::wb_add_cell_style(dims = "E3:J3", horizontal = "center") %>%
  openxlsx2::wb_set_col_widths(cols = 1:4, widths = "auto") %>%
  openxlsx2::wb_set_col_widths(cols = 5:9, widths = 13.5)

  for (i in seq_len(n_spp) + 3) {
    wb <- wb %>%
      openxlsx2::wb_add_formula(
        x = sprintf("SUM(G%s:I%s)", i, i),
        dims = sprintf("J%s", i)
      )
  }
  for (i in LETTERS[7:10]) {
    wb <- wb %>%
      openxlsx2::wb_add_formula(
        x = sprintf("ROUND(SUM(%s4:%s%s),0)", i, i, n_spp + f_row_ivi),
        dims = sprintf("%s%s", i, n_spp + f_row_ivi + 1)
      )
  }
  for (i in c(3, n_spp + 4)) {
    wb <- wb %>%
      openxlsx2::wb_add_font(dims = sprintf("A%s:J%s", i, i), bold = T) %>%
      openxlsx2::wb_add_border(
        dims = sprintf("A%s:J%s", i, i),
        left_border = NULL,
        right_border = NULL
      )
  }

  # BD NHA ----
  wb <- wb %>%
    openxlsx2::wb_add_worksheet("BD_Nha") %>%
    openxlsx2::wb_add_data_table(
      x = BD_Nha,
      first_column = T,
      table_style = DT_style,
      table_name = "BD_Nha"
    ) %>%
    openxlsx2::wb_add_numfmt(
      dims = openxlsx2::wb_dims(x = BD_Nha, select = "data", cols = 5:ncol(BD_Nha)),
      numfmt = "#,##0"
    ) %>%
    openxlsx2::wb_set_col_widths(cols = seq_len(length(BD_Nha)), widths = "auto") %>%
    openxlsx2::wb_set_col_widths(cols = 3:4, widths = 9)

  # Proporciones ----
  wb <- wb %>%
    openxlsx2::wb_add_worksheet("Proporciones") %>%
    openxlsx2::wb_add_pivot_table(
      x = BD_wb,
      sheet = "Proporciones",
      filter = "Especie",
      rows = "Parcela",
      cols = "Estado",
      data = "Nha",
      params = list(
        table_style = PT_style,
        outline = FALSE, 
        row_grand_totals = FALSE,
        numfmt = c(formatCode = c("#,##0")),
        choose = c(
          Especie = sprintf("x == '%s'", sp),
          Estado = 'purrr::map_lgl(stringi::stri_detect_regex(x, "adulto|brinzal|regenera", case_insensitive = T), shiny::isTruthy)'
        )
      )
    ) 
    
  
  # agregar densidades relativas por estado de desarrollo
  for (i in seq_len(n_estado) + 1) {
    wb <- wb %>%
      openxlsx2::wb_add_formula(
        sheet = "Proporciones",
        x = sprintf("=%s4&%s", openxlsx2::int2col(i), '"_rel"'),
        dims = sprintf("%s4", openxlsx2::int2col(n_estado + i + 1))
      ) %>%
      openxlsx2::wb_add_font(
        dims = openxlsx2::wb_dims(rows = 4, cols = n_estado + i + 1),
        bold = T,
        color = openxlsx2::wb_color("white")
      ) %>%
      openxlsx2::wb_add_fill(
        dims = openxlsx2::wb_dims(rows = 4, cols = n_estado + i + 1),
        color = title_color
      ) %>%
      openxlsx2::wb_add_cell_style(
        dims = openxlsx2::wb_dims(rows = 4, cols = n_estado + i + 1),
        horizontal = "center",
        vertical = "bottom"
      )
  }
  for (i in seq_len(n_par_sp) + 4) {
    for (j in seq_len(n_estado) + 1) {
      wb <- wb %>%
        openxlsx2::wb_add_formula(
          sheet = "Proporciones",
          x = sprintf("%s%s/%s%s", openxlsx2::int2col(j), i, openxlsx2::int2col(n_estado + 2), i),
          dims = sprintf("%s%s", openxlsx2::int2col(n_estado + j + 1), i)
        )
    }
  }
  for (i in seq_len(n_estado) + n_estado + 2) {
    wb <- wb %>%
      openxlsx2::wb_add_formula(
        x = sprintf(
          "ROUND(AVERAGE(%s5:%s%s),3)",
          openxlsx2::int2col(i),
          openxlsx2::int2col(i),
          n_par_sp + 4
        ),
        dims = sprintf("%s%s", openxlsx2::int2col(i), n_par_sp + 5)
      ) %>%
      openxlsx2::wb_add_font(
        dims = sprintf("%s%s", openxlsx2::int2col(i), n_par_sp + 5),
        bold = T
      ) %>%
      openxlsx2::wb_add_border(
        dims = sprintf("%s%s", openxlsx2::int2col(i), n_par_sp + 5),
        left_border = NULL,
        right_border = NULL,
        top_border = "thin",
        bottom_border = "thin"
      )
  }

  wb <- wb %>%
    openxlsx2::wb_add_numfmt(
      sheet = "Proporciones",
      dims = openxlsx2::wb_dims(
        rows = seq_len(n_par_sp),
        from_row = 5,
        cols = seq_len(n_estado),
        from_col = n_estado + 3
      ),
      numfmt = c("#,##0.00")
    ) %>%
    openxlsx2::wb_set_col_widths(
      cols = seq_len(n_estado) + n_estado + 2,
      widths = 14.22
    ) %>%
    openxlsx2::wb_add_data(
      x = info_prop,
      start_col = n_estado * 2 + 4,
      start_row = n_estado + 7,
      col_names = F,
      na.strings = ""
    ) %>%
    openxlsx2::wb_add_formula(
      x = sprintf(
        "ROUND(%s%s*%s%s,0)",
        openxlsx2::int2col(n_estado * 2 + 5),
        n_estado + 7,
        openxlsx2::int2col(n_estado * 2 + 5),
        n_estado + 8
      ),
      dims = sprintf("%s%s", openxlsx2::int2col(n_estado * 2 + 5), n_estado + 9)
    ) %>%
    openxlsx2::wb_add_font(
      dims = sprintf(
        "%s%s:%s%s",
        openxlsx2::int2col(n_estado * 2 + 4),
        n_estado + 7,
        openxlsx2::int2col(n_estado * 2 + 4),
        n_estado + 9
      ),
      bold = T
    ) %>%
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(
        rows = seq_len(nrow(info_prop)),
        cols = seq_len(length(info_prop)),
        from_row = n_estado + 7,
        from_col = n_estado * 2 + 4
      ),
      inner_hgrid = "thin",
      inner_vgrid = "thin"
    ) %>%
    openxlsx2::wb_add_numfmt(
      dims = sprintf("%s%s", openxlsx2::int2col(n_estado * 2 + 5), n_estado + 7),
      numfmt = 4
    ) %>%
    openxlsx2::wb_add_numfmt(
      dims = sprintf(
        "%s%s:%s%s",
        openxlsx2::int2col(n_estado * 2 + 5),
        n_estado + 8,
        openxlsx2::int2col(n_estado * 2 + 5),
        n_estado + 9
      ),
      numfmt = 3
    ) %>%
    openxlsx2::wb_add_data(
      x = "Proporciones e individuos totales x estado de desarrollo",
      start_col = n_estado * 2 + 4,
      start_row = 3
    ) %>%
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(rows = 3, cols = n_estado * 2 + 4),
      color = title_color
    ) %>%
    openxlsx2::wb_add_data(
      x = list("Estado", "Proporciones", "Densidad", "Individuos totales"),
      start_col = n_estado * 2 + 4,
      start_row = 4
    ) %>%
    openxlsx2::wb_add_font(
      dims = openxlsx2::wb_dims(
        rows = 1,
        cols = 1:4,
        from_row = 3,
        from_col = n_estado * 2 + 4
      ),
      bold = T,
      color = openxlsx2::wb_color("white")
    ) %>%
    openxlsx2::wb_add_font(
      dims = openxlsx2::wb_dims(
        rows = 2,
        cols = 1:4,
        from_row = 3,
        from_col = n_estado * 2 + 4
      ),
      bold = T
    ) %>%
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(
        rows = 2,
        cols = 1:4,
        from_row = 3,
        from_col = n_estado * 2 + 4
      ),
      color = header_color
    ) %>%
    openxlsx2::wb_add_cell_style(
      dims = openxlsx2::wb_dims(
        rows = 1:2,
        cols = 1:4,
        from_row = 3,
        from_col = n_estado * 2 + 4
      ),
      horizontal = "center",
      vertical = "bottom"
    ) %>%
    openxlsx2::wb_merge_cells(
      dims = openxlsx2::wb_dims(rows = 3, cols = 1:4, from_col = n_estado * 2 + 4),
      solve = T
    ) %>%
    openxlsx2::wb_add_formula(
      x = sprintf("TRANSPOSE(B4:%s4)", openxlsx2::int2col(n_estado + 1)),
      dims = openxlsx2::wb_dims(
        rows = seq_len(n_estado),
        cols = 1,
        from_col = n_estado * 2 + 4,
        from_row = 5
      ),
      array = T,
      cm = T
    ) %>%
   openxlsx2::wb_add_formula(
      x = sprintf(
        "TRANSPOSE(%s%s:%s%s)",
        openxlsx2::int2col(n_estado + 3),
        n_par_sp + 5,
        openxlsx2::int2col(n_estado * 2 + 2),
        n_par_sp + 5
      ),
      dims = openxlsx2::wb_dims(
        rows = seq_len(n_estado),
        cols = 1,
        from_col = n_estado * 2 + 5,
        from_row = 5
      ),
      array = T,
      cm = T
    ) %>%
    openxlsx2::wb_add_formula(
      x = sprintf(
        "ROUND(MMULT(%s5:%s%s,%s%s),0)",
        openxlsx2::int2col(n_estado * 2 + 5),
        openxlsx2::int2col(n_estado * 2 + 5),
        n_estado + 4,
        openxlsx2::int2col(n_estado * 2 + 5),
        n_estado + 8
      ),
      dims = openxlsx2::wb_dims(
        rows = seq_len(n_estado),
        cols = 1,
        from_col = n_estado * 2 + 6,
        from_row = 5
      ),
      array = T,
      cm = T
    ) %>%
    openxlsx2::wb_add_formula(
      x = sprintf(
        "ROUND(MMULT(%s5:%s%s,%s%s),0)",
        openxlsx2::int2col(n_estado * 2 + 6),
        openxlsx2::int2col(n_estado * 2 + 6),
        n_estado + 4,
        openxlsx2::int2col(n_estado * 2 + 5),
        n_estado + 7
      ),
      dims = openxlsx2::wb_dims(
        rows = seq_len(n_estado),
        cols = 1,
        from_col = n_estado * 2 + 7,
        from_row = 5
      ),
      array = T,
      cm = T
    ) %>%
    openxlsx2::wb_add_numfmt(
      dims = openxlsx2::wb_dims(
        rows = seq_len(n_estado),
        cols = 1,
        from_col = n_estado * 2 + 7,
        from_row = 5
      ),
      numfmt = "#,##0"
    ) %>%
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(
        rows = seq_len(n_estado + 2),
        cols = 1:4,
        from_row = 3,
        from_col = n_estado * 2 + 4
      ),
      inner_hgrid = "thin",
      inner_vgrid = "thin"
    ) %>%
    openxlsx2::wb_set_col_widths(
      cols = (n_estado * 2 + 4):(n_estado * 2 + 7),
      widths = c(19.11, 11.22, 11, 15)
    )

  # Estadigrafos ----
  df_sup <- tibble::tibble(
    x1 = c("sup. Muestreo (ha)", "sup. Población (BNP de la cuenca, ha)"),
    x2 = c(as.numeric(names(which.max(table(BD$Sup_parcela)))) / 10000, sum(BNP_cuenca$Sup_ha))
  )
  class(df_sup$x2) <- "formula"

  estadisticos <- tibble::tibble(
    Parametros = c(
      "Rango (ind/ha)",
      "Tamaño de la muestra (n)",
      "Nha (ind/ha)",
      "Varianza",
      "Coeficiente de variación (%)",
      "Error absoluto (ind/ha)",
      "Error relativo (%)",
      "Intervalo de confianza (95%) (ind/ha)"
    ),
    Total = c(
      'CONCATENATE(MIN(BD_Nha[[#All],[Nha_Total]]), " - ", MAX(BD_Nha[[#All],[Nha_Total]]))',
      'COUNTA(BD_Nha[[#Data],[Nha_Total]])',
      'ROUND(AVERAGE(BD_Nha[[#All],[Nha_Total]]), 0)',
      'ROUND(((1 - (B7 * B1 / B2)) * (STDEVA(BD_Nha[[#All],[Nha_Total]])^2 / B7)), 2)',
      'ROUND(((SQRT(B9) / B8) * 100), 1)',
      'ROUND(TINV(0.05, B7 - 1) * SQRT(B9), 0)',
      'ROUND(((B11 / B8) * 100), 1)',
      'CONCATENATE(B8 - B11," - ", B8 + B11)'
    ),
    SP = c(
      sprintf('CONCATENATE(MIN(BD_Nha[[#All],[%s]]), " - ", MAX(BD_Nha[[#All],[%s]]))', sp, sp),
      sprintf('COUNTA(BD_Nha[[#Data],[%s]])', sp),
      sprintf('ROUND(AVERAGE(BD_Nha[[#All],[%s]]),0)', sp),
      sprintf(
        'ROUND(((1 - (C7 * B1 / B2)) * (STDEVA(BD_Nha[[#All],[%s]])^2 / C7)), 2)',
        sp
      ),
      'ROUND(((SQRT(C9) / C8) * 100), 1)',
      'ROUND(TINV(0.05, C7 - 1) * SQRT(C9), 0)',
      'ROUND(((C11 / C8) * 100), 1)',
      'CONCATENATE(C8 - C11, " - ", C8 + C11)'
    )
  )

  class(estadisticos$Total) <- "formula"
  class(estadisticos$SP) <- "formula"
  names(estadisticos) <- c("Parámetros", "Conjunto de especies arbóreas", sp)
  wb <- wb %>%
    openxlsx2::wb_add_worksheet("Estadígrafos") %>%
    openxlsx2::wb_add_data(x = df_sup, dims = "A1", col_names = F) %>%
    openxlsx2::wb_add_numfmt(dims = "B2", numfmt = "#,##0.00") %>%
    openxlsx2::wb_add_font(dims = "A1:A2", bold = T) %>%
    openxlsx2::wb_add_border(
      dims = "A1:B2",
      inner_hcolor = openxlsx2::wb_color(hex = "#BEC6B7"),
      inner_vcolor = openxlsx2::wb_color(hex = "#BEC6B7")
    ) %>%
    # tabla estadisticos
    openxlsx2::wb_add_data(x = "Estadígrafos muestreo de Nha", dims = "A4") %>%
    openxlsx2::wb_add_font(dims = "A4", bold = T, color = openxlsx2::wb_color("white")) %>%
    openxlsx2::wb_add_fill(dims = "A4", color = title_color) %>%
    openxlsx2::wb_add_cell_style(dims = "A4", horizontal = "center") %>%
    openxlsx2::wb_merge_cells(dims = "A4:C4", solve = T) %>%
    openxlsx2::wb_add_data(x = estadisticos, start_col = 1, start_row = 5) %>%
    openxlsx2::wb_add_font(dims = "C5", italic = T) %>%
    openxlsx2::wb_add_font(dims = "A5:C5", bold = T) %>%
    openxlsx2::wb_add_fill(dims = "A5:C5", color = header_color) %>%
    openxlsx2::wb_add_cell_style(dims = "A5;B5:C13", horizontal = "center") %>%
    openxlsx2::wb_add_border(
      dims = "A4:C13",
      inner_hcolor = openxlsx2::wb_color(hex = "#BEC6B7"),
      inner_hgrid = "medium"
    ) %>%
    openxlsx2::wb_set_col_widths(cols = 1:3, widths = c(31, 28, 20)) %>%
    # tabla estadisticos x estado de desarrollo
    openxlsx2::wb_add_data(
      x = paste(
        openxlsx2::fmt_txt(
          "Densidad, error e intervalos de confianza por estado de desarrollo de "
        ),
        openxlsx2::fmt_txt(sp, italic = T, color = openxlsx2::wb_color("white"))
      ),
      dims = "E4"
    ) %>%
    openxlsx2::wb_add_font(dims = "E4", bold = T, color = openxlsx2::wb_color("white")) %>%
    openxlsx2::wb_add_fill(dims = "E4", color = title_color) %>%
    openxlsx2::wb_add_cell_style(dims = "E4", horizontal = "center") %>%
    openxlsx2::wb_merge_cells(dims = "E4:K4", solve = T) %>%
    openxlsx2::wb_add_data(
      x = rep(NA, 7) %>%
        t() %>%
        tibble::as_tibble() %>%
        `names<-`(
          c(
            "ECC/Estado de desarrollo",
            "Parámetro poblacional estimado",
            "Estimación (ind/ha)",
            "Error relativo (%)",
            "Intervalo de confianza (ind/ha)",
            ".",
            "Total poblacional"
          )
        ) %>%
        dplyr::mutate_at(5, ~"Inf") %>%
        dplyr::mutate_at(6, ~"Sup"),
      start_col = 5,
      start_row = 5
    ) %>%
    openxlsx2::wb_set_col_widths(cols = 5:11, widths = c(22, 19, 11, 11, 14, 14, 16)) %>%
    openxlsx2::wb_add_font(dims = "E5:K6", bold = T) %>%
    openxlsx2::wb_add_fill(dims = "E5:K6", color = header_color) %>%
    openxlsx2::wb_add_cell_style(dims = "E5:K6", horizontal = "center", wrap_text = T) %>%
    openxlsx2::wb_merge_cells(dims = "I5:J5", solve = T)

  df_est_prop <- tibble::tibble(
    x1 = c(sp, prop_2$Estado),
    x2 = c(
      "Densidad media",
      prop_2 %>%
        janitor::adorn_pct_formatting(digits = 1, , , Proporciones) %>%
        dplyr::mutate_at(
          2,
          ~ paste0("Proporción (", stringi::stri_replace_all_regex(., "\\.", ","), ")")
        ) %>%
        dplyr::pull(Proporciones)
    ),
    x3 = c(
      "C8",
      sprintf(
        "ROUND(G7 * Proporciones!%s%s,0)",
        openxlsx2::int2col(n_estado * 2 + 5),
        seq_len(n_estado) + 4
      )
    ),
    x4 = c("C12", rep(NA, n_estado)),
    x5 = c(
      "C8 - C11",
      sprintf(
        "ROUND(G%s-G%s*H7/100,0)",
        seq_len(n_estado) + 7,
        seq_len(n_estado) + 7
      )
    ),
    x6 = c(
      "C8 + C11",
      sprintf(
        "ROUND(G%s+G%s*H7/100,0)",
        seq_len(n_estado) + 7,
        seq_len(n_estado) + 7
      )
    ),
    x7 = c(
      sprintf("Proporciones!%s%s", openxlsx2::int2col(n_estado * 2 + 5), n_estado + 9),
      sprintf(
        "Proporciones!%s%s",
        openxlsx2::int2col(n_estado * 2 + 7),
        seq_len(n_estado) + 4
      )
    )
  )
  class(df_est_prop$x3) <- "formula"
  class(df_est_prop$x4) <- "formula"
  class(df_est_prop$x5) <- "formula"
  class(df_est_prop$x6) <- "formula"
  class(df_est_prop$x7) <- "formula"

  for (i in LETTERS[c(5:8, 11)]) {
    wb <- wb %>%
      openxlsx2::wb_merge_cells(dims = sprintf("%s5:%s6", i, i), solve = T)
  }

  wb <- wb %>%
    openxlsx2::wb_add_data(
      x = df_est_prop,
      start_col = 5,
      start_row = 7,
      col_names = F
    ) %>%
    openxlsx2::wb_add_cell_style(
      dims = "H7",
      vertical = "center",
      horizontal = "center"
    ) %>%
    openxlsx2::wb_merge_cells(dims = sprintf("H7:H%s", n_estado + 7), solve = T) %>%
    openxlsx2::wb_add_border(
      dims = sprintf("E4:K%s", n_estado + 7),
      inner_hcolor = openxlsx2::wb_color(hex = "#BEC6B7"),
      inner_hgrid = "medium"
    ) %>%
    openxlsx2::wb_add_numfmt(
      dims = openxlsx2::wb_dims(
        x = df_est_prop,
        x = "data",
        from_col = 5,
        from_row = 6,
        cols = "x7"
      ),
      numfmt = "#,##0"
    )
  
  # Salida ----
  return(
    list(
      IVI = IVI,
      BD_Nha = BD_Nha,
      prop = prop_2,
      wb = wb
    )
  )
}

