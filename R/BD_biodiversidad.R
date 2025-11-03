#' Apéndice BD biodiversidad
#'
#' @param nom_ssubc Nombre de la subsubcuenca
#' @param portada portada para los apendices. Disponible: `default`, `MLP612`, `KIM753` u `otra`.
#' @param portada_opts opciones para personalizar una portada. ver `details`.
#' @inheritParams carto_digital
#'
#' @details 
#' En el parametro `portada_opts` debe ingresar una lista con los argumentos de la funcion `portada_opts()`.
#' 
#' @returns lista con resultados asociados al apendice de biodiversidad
#'
#' @export

# BNP_cuenca <- sf::read_sf("~/KIM735_ADENDA/BDD con R/C1/OUTPUTS/Compilado_Carto_digital_1727895600.67695/BNP_Porlieria_Cuenca.shp")
# nom_ssubc <- BNP_cuenca$NOM_SSUBC %>% unique()
# obras <- sf::read_sf("~/KIM735_ADENDA/BDD con R/KIM753_A1_Area_Intervencion_por_Obra_20240823.shp")
# BD_flora <- prepare_bd_flora_150("~/KIM735_ADENDA/BDD con R/BDD_Inventarios_Flora_CXX.xlsx", BNP_cuenca, bd_lista = F, in_bnp_obra = T, obras = obras)
# sp <- "Porlieria chilensis"
# portada = "default"
# portada_opts = NULL

BD_biodiversidad <- function(
  BD_flora,
  sp,
  nom_ssubc,
  portada = "default",
  portada_opts = NULL
) {
  valid_df(BD_flora, req_names$BD_flora)
  valid_input(sp, inherit = "character")
  portada <- match.arg(portada, c("default", "MLP612", "KIM753", "otra"))
  valid_input(portada_opts, inherit = c("NULL", "list"))

  # DATOS ----
  BD_indices <- BD_flora %>%
    dplyr::group_by(Especie) %>%
    dplyr::summarise(n = sum(N_ind, na.rm = T)) %>%
    dplyr::mutate(n = dplyr::if_else(n == 0, 1, n)) %>%
    dplyr::mutate(
      p = n / sum(n),
      `Ln(p)` = log(p),
      `p x Ln(p)` = p * `Ln(p)`
    )
  sc <- function(list) {
    list %>% purrr::map_dbl(~ .x^2) %>% sum()
  } # función suma cuadrados
  tabla_indices_R <- tibble::tibble(
    Indices = c("Simpson", "Div. Simpson", "Shannon-Weaver"),
    Valor = c(
      janitor::round_half_up(sc(BD_indices$p), 3),
      1 - janitor::round_half_up(sc(BD_indices$p), 3),
      janitor::round_half_up(1 - sum(BD_indices$`p x Ln(p)`), 3)
    )
  ) 

  # EXCEL ----
  DT_style <- "TableStyleLight14"
  PT_style <- "TableStyleMedium7"
  title_color <- wb_color(hex = "#62A39F")

  ## Portada ----
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

  wb <- openxlsx2::wb_workbook(theme = "Integral") %>%
    wb_portada_PAS150(
      apendice = "biodiversidad",
      nom_ssubc = nom_ssubc,
      opts = opts
    )

  ## Presentación ----
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
    "Base de datos de flora y biodiversidad en el área del proyecto",
    "La siguiente base de datos expone los criterios y cálculos para cuantificar la biodiversidad de la cuenca de estudio en las áreas de bosque nativo de preservación. Para poder lograr dicho objetivo, el proceso consta de varios procesos, explicados en las siguientes pestañas de cáculo:"
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
    wb <- wb %>%
      openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(cols = 1:2, rows = i))
  }
  desc_h1 <- paste0(
    "BD_Flora: Corresponde a la base de datos flora para la subsubcuenca de estudio, donde se detalla la categoría taxonómica de cada una de las especies junto con su RCE y piso vegetacional:
Parcela: Parcela de muestreo.
UTM_E: Coordenada central Este de la parcela de muestreo en el sistema geodésico WGS84.
UTM_N: Coordenada central Norte de la parcela de muestreo en el sistema geodésico WGS84.
N_ind: Número de ejemplares encontrados en la parcela de 1000 m2.
Cob_bb: Cobertura vegetacional según Braun-Blanquet.
RCE: Reglamento para Clasificar Especies según estado de conservación.
DS_68: Indica si se encuentra en el listado de especies dentro del DS 68."
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
    'Localización UTM: Corresponde a la localización de cada una de las parcelas de inventario florístico levantado en cada una de las unidades de bosque nativo de preservación'
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
    "Catálogo florístico: Corresponde a un resumen de la base de datos de flora, donde se muestra para cada una de las especie, el hábito, origen, categoría de conservación y decretos que la sustentan."
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
  desc_h4 <- c(
    "Índices de Biodiversidad: Esta pestaña muestra el calculo de los índices de Simpson y Shannon-Weaver, calculados a partir de la proporción de cada especie en los BNP dentro del área de proyecto.
  n: Total de indivduos muestreados de la especie (Suma Count_500m2 x especie).
  p: Proporción de la presencia de la especie (n/suma total(n)).
  Ln(p): Logaritmo natural de p.
  p x Ln(p): Producto entre p y su logaritmo natural."
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
    "Frecuencia Sp. Acompañantes: Por último, se presenta cada una de las especies presentes en la subsubcuenca de estudio, junto su frecuencia absoluta (n) y relativa (%)."
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
    {
      .[] + 3
    } %>%
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
    openxlsx2::wb_add_cell_style(
      dims = openxlsx2::wb_dims(rows = 1:40, cols = 1:2),
      wrap_text = "1"
    )

  ## BD Flora ----
  wb <- wb %>%
    openxlsx2::wb_add_worksheet("BD Flora") %>%
    openxlsx2::wb_add_data_table(
      x = BD_flora,
      table_name = "BD_flora",
      table_style = DT_style,
      first_column = T
    ) %>%
    openxlsx2::wb_set_col_widths(cols = 1:ncol(BD_flora), widths = "auto") %>%
    openxlsx2::wb_set_col_widths(cols = 5:6, widths = 9)
  BD_wb <- wb_data(wb, sheet = "BD Flora")

  ## Localización UTM ----
  localizacion <- BD_flora %>%
    dplyr::count(Parcela, UTM_E, UTM_N) %>%
    dplyr::select(-n)
  wb <- wb %>%
    openxlsx2::wb_add_worksheet("Localización UTM") %>%
    openxlsx2::wb_add_data_table(
      x = localizacion,
      table_name = "localizacion",
      table_style = DT_style,
      first_column = T
    ) %>%
    openxlsx2::wb_set_col_widths(cols = 1, widths = "auto") %>%
    openxlsx2::wb_set_col_widths(cols = 2:3, widths = 12)

  ## Catalogo floristico ----
  composicion <- BD_flora %>%
    dplyr::count(Especie, Habito, Origen, RCE, DS_68) %>%
    dplyr::select(-n)
  wb <- wb %>%
    openxlsx2::wb_add_worksheet("Catálogo floristico") %>%
    openxlsx2::wb_add_data_table(
      x = composicion,
      table_name = "composicion",
      table_style = DT_style,
      first_column = T
    ) %>%
    openxlsx2::wb_add_font(
      dims = openxlsx2::wb_dims(
        x = composicion,
        select = "data",
        cols = "Especie"
      ),
      italic = T
    ) %>%
    openxlsx2::wb_set_col_widths(cols = 1:ncol(composicion), widths = "auto")

  ## Indices de Biodiversidad ----
  row_i <- 4
  rows_filtered <- BD_wb %>% 
    dplyr::filter(!stringi::stri_detect_regex(Habito, "hierba|herb", case_insensitive = T)) %>% 
    dplyr::filter(!stringi::stri_detect_regex(Cob_BB, "-|fp", case_insensitive = T)) %>% 
    dplyr::pull(Especie) %>% unique() %>% length()
  wb <- wb %>%
    openxlsx2::wb_add_worksheet("Indices de Biodiversidad") %>%
    openxlsx2::wb_add_pivot_table(
      sheet = "Indices de Biodiversidad",
      dims = sprintf("A%s", row_i),
      x = BD_wb,
      filter = c("Habito", "Cob_BB"),
      rows = "Especie",
      data = "N_ind",
      params = list(
        apply_font_formats = T,
        col_grand_totals = FALSE,
        table_style = PT_style,
        choose = c(
          Habito = '!stringi::stri_detect_regex(x, "hierba|herb", case_insensitive = T)',
          Cob_BB = '!stringi::stri_detect_regex(x, "-|fp", case_insensitive = T)'
        )
      )
    ) %>%
    openxlsx2::wb_add_font(
      dims = sprintf(
        "A%s:A%s",
        row_i + 1,
        row_i + length(unique(BD_wb$Especie))
      ),
      italic = T,
      bold = F
    ) %>%
    openxlsx2::wb_add_data(
      x = c("p", "LN(p)", "p * LN(p)"),
      dims = sprintf("C%s:E%s", row_i, row_i)
    ) %>%
    openxlsx2::wb_add_font(
      dims = sprintf("C%s:E%s", row_i, row_i),
      bold = T,
      color = openxlsx2::wb_color("white")
    ) %>%
    openxlsx2::wb_add_fill(
      dims = sprintf("C%s:E%s", row_i, row_i),
      color = title_color
    ) %>%
    openxlsx2::wb_add_border(
      dims = sprintf("C%s:E%s", row_i, row_i),
      left_border = NULL,
      right_border = NULL,
      bottom_border = NULL,
      top_color = title_color
    ) %>%
    openxlsx2::wb_add_cell_style(
      dims = sprintf("C%s:E%s", row_i, row_i),
      horizontal = "center"
    ) %>% 
    openxlsx2::wb_add_formula(
      x = sprintf("MMULT(B5:B%s, 1/B%s)", row_i + rows_filtered, row_i + rows_filtered + 1),
      dims = openxlsx2::wb_dims(rows = 1:rows_filtered, from_row = 5, from_col = 3), array = T, cm = T
    ) %>% 
    openxlsx2::wb_add_formula(
      x = sprintf("LN(C5:C%s)", row_i + rows_filtered),
      dims = openxlsx2::wb_dims(rows = 1:rows_filtered, from_row = 5, from_col = 4), array = T, cm = T
    ) %>% 
    openxlsx2::wb_add_formula(
      x = sprintf("C5:C%s * D5:D%s", row_i + rows_filtered, row_i + rows_filtered),
      dims = openxlsx2::wb_dims(rows = 1:rows_filtered, from_row = 5, from_col = 5), array = T, cm = T
    )

  tabla_indices <- tibble::tibble(
    `Índices` = c("Simpson", "Div. Simpson", "Shannon-Weaver"),
    Valor = rep(NA, 3)
  ) 
  tabla_eq <- tibble::tibble(
    sym = c("p", "LN", "p * LN", "∑"),
    nom = c(
      "Abundancia relativa",
      "Logaritmo natural",
      "Multiplo de frecuencia relativa x logaritmo natural de p",
      "Sumatoria de p con logaritmo natural de p"
    ),
    equ = rep("=", 4),
    ecu = c(
      "Abundancia sp/Abundancia total sp",
      "LN(p)",
      "p * LN(p)",
      "∑(p * LM(p))"
    )
  )
  wb <- wb %>%
    openxlsx2::wb_add_numfmt(
      dims = sprintf(
        "C%s:E%s",
        row_i + 1,
        rows_filtered + row_i
      ),
      numfmt = "0.0000"
    ) %>%
    # Indices
    openxlsx2::wb_add_data(
      x = tabla_indices,
      start_col = "G",
      start_row = row_i,
      with_filter = F
    ) %>%
    openxlsx2::wb_add_font(
      dims = sprintf("G%s:H%s", row_i, row_i),
      bold = T,
      color = wb_color("white")
    ) %>%
    openxlsx2::wb_add_fill(
      dims = sprintf("G%s:H%s", row_i, row_i),
      color = title_color
    ) %>%
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(
        x = tabla_indices,
        from_col = "G",
        from_row = row_i
      ),
      inner_hgrid = "thin",
      inner_vgrid = "thin",
      inner_hcolor = openxlsx2::wb_color(hex = "#6F7B62"),
      inner_vcolor = openxlsx2::wb_color(hex = "#6F7B62")
    ) %>%
    openxlsx2::wb_add_cell_style(
      dims = openxlsx2::wb_dims(rows = row_i, cols = "G:H"),
      horizontal = "center"
    ) %>%
    openxlsx2::wb_add_formula(
      x = sprintf(
        "ROUND(SUMSQ(C%s:C%s),3)",
        row_i + 1,
        row_i + length(unique(BD_wb$Especie))
      ),
      dims = sprintf("H%s", row_i + 1)
    ) %>%
    openxlsx2::wb_add_formula(
      x = sprintf("1-H%s", row_i + 1),
      dims = sprintf("H%s", row_i + 2)
    ) %>%
    openxlsx2::wb_add_formula(
      x = sprintf(
        "ROUND(1-SUM(E%s:E%s),3)",
        row_i + 1,
        row_i + length(unique(BD_wb$Especie))
      ),
      dims = sprintf("H%s", row_i + 3)
    ) %>%
    openxlsx2::wb_add_numfmt(
      dims = sprintf("H%s", row_i + 3),
      numfmt = "0.000"
    ) %>%
    openxlsx2::wb_set_col_widths(cols = c(3:5), widths = 11) %>%
    # Ecuacion
    openxlsx2::wb_add_image(
      dims = sprintf("G%s", row_i + 5),
      file = app_sys("app/www/Sh-W.png"),
      width = 8.4,
      height = 2.51,
      units = "cm"
    ) %>%
    openxlsx2::wb_add_data(
      x = "Componenetes de la ecuación",
      dims = sprintf("G%s", row_i + 11)
    ) %>%
    openxlsx2::wb_add_font(
      dims = sprintf("G%s", row_i + 11),
      bold = T,
      color = wb_color("white")
    ) %>%
    openxlsx2::wb_add_fill(
      dims = sprintf("G%s", row_i + 11),
      color = title_color
    ) %>%
    openxlsx2::wb_add_cell_style(
      dims = sprintf("G%s", row_i + 11),
      horizontal = "center"
    ) %>%
    openxlsx2::wb_merge_cells(
      dims = sprintf("G%s:J%s", row_i + 11, row_i + 11),
      solve = T
    ) %>%
    openxlsx2::wb_add_data(
      x = tabla_eq,
      start_col = "G",
      start_row = row_i + 12,
      col_names = F
    ) %>%
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(x = tabla_eq, from_row = row_i + 11, from_col = "G"),
      inner_hgrid = "thin",
      inner_vgrid = "thin",
      inner_hcolor = openxlsx2::wb_color(hex = "#6F7B62"),
      inner_vcolor = openxlsx2::wb_color(hex = "#6F7B62")
    ) %>%
    openxlsx2::wb_set_col_widths(cols = c(7:10), widths = c(13, 46, 2, 31))

  ## Frecuencia Especies Acompañantes ----
  tabla_frec <- BD_flora %>%
    dplyr::count(Parcela, Especie) %>%
    dplyr::mutate(n = 1)
  frecuencias <- BD_flora %>%
    dplyr::count(Parcela, Especie) %>%
    dplyr::count(Especie, sort = T) %>%
    dplyr::mutate(Frec = n / nrow(localizacion)) %>%
    dplyr::mutate(
      Frecuencia = "frecuencias[[#This Row], [n]]/ROWS(localizacion[])"
    ) %>%
    tibble::as_tibble()
  class(frecuencias$Frecuencia) <- "formula"

  spp_acomp <- frecuencias %>%
    dplyr::filter(Frec >= 0.25, Especie != sp) %>%
    dplyr::select(Especie, Frec) %>%
    dplyr::rename(Frecuencia = Frec)

  wb <- wb %>%
    openxlsx2::wb_add_worksheet("Frecuencia Sp. Acompañantes") %>%
    openxlsx2::wb_add_data_table(
      x = tabla_frec,
      table_style = DT_style,
      first_column = T,
      with_filter = F
    )
  frac_wb <- openxlsx2::wb_data(wb, sheet = "Frecuencia Sp. Acompañantes")

  wb <- wb %>%
    openxlsx2::wb_add_pivot_table(
      sheet = "Frecuencia Sp. Acompañantes",
      dims = "E1",
      x = frac_wb,
      rows = "Especie",
      data = "n",
      params = list(
        table_style = PT_style,
        col_grand_totals = FALSE,
        row_grand_totals = FALSE
      )
    ) %>%
    openxlsx2::wb_add_data_table(
      x = frecuencias %>% dplyr::select(-Frec),
      dims = "H1",
      table_name = "frecuencias",
      table_style = DT_style,
      first_column = T,
      with_filter = F
    ) %>%
    openxlsx2::wb_set_col_widths(cols = c(1, 2, 5, 8), widths = "auto") %>%
    openxlsx2::wb_set_col_widths(cols = c(3, 9), widths = 5) %>%
    openxlsx2::wb_add_numfmt(
      dims = openxlsx2::wb_dims(x = frecuencias, from_col = "H", cols = 3),
      numfmt = "0%"
    ) %>%
    openxlsx2::wb_add_font(dims = openxlsx2::wb_dims(x = tabla_frec, cols = 2), italic = T) %>%
    openxlsx2::wb_add_font(
      dims = openxlsx2::wb_dims(x = frecuencias, cols = 1, from_col = "E"),
      italic = T
    ) %>%
    openxlsx2::wb_add_font(
      dims = openxlsx2::wb_dims(x = frecuencias, cols = 1, from_col = "H"),
      italic = T,
      bold = F
    ) %>%
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(
        x = frecuencias %>% dplyr::filter(Frec >= 0.25),
        cols = 1:3,
        from_col = "H"
      ),
      color = openxlsx2::wb_color(hex = "#E9EBE7")
    )

  ## Outputs ----
  return(
    list(
      localizacion = localizacion,
      composicion = composicion,
      spp_acomp = spp_acomp,
      tabla_indices = tabla_indices_R,
      wb = wb
    )
  )
}
