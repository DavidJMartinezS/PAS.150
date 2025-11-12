#' @noRd
gt_tipos_forestales <- function(uso_veg, sp) {
  valid_input(uso_veg, inherit = "data.frame", names = c("F_ley20283", "Tipo_for", "Sup_ha"))

  sup_cuenca <- uso_veg$Sup_ha %>% sum()
  TF_BN <- uso_veg %>% 
    sf::st_drop_geometry() %>% 
    dplyr::filter(F_ley20283 %>% stringi::stri_cmp_equiv('bosque nativo', strength = 1)) %>% 
    dplyr::group_by(Tipo_for) %>% 
    dplyr::summarise(Sup_ha_BN = sum(Sup_ha), .groups = 'drop') %>% 
    dplyr::mutate(P_BN = Sup_ha_BN / sup_cuenca)
  
  TF_BNP <- uso_veg %>% 
    sf::st_drop_geometry() %>% 
    dplyr::filter(
      !Tipo_for %>% stringi::stri_cmp_equiv('no aplica', strength = 1), 
      F_ley20283 %>% stringi::stri_detect_regex('preser', case_insensitive = T)
    ) %>% 
    dplyr::mutate(BNP = dplyr::if_else(BNP_ECC %>% stringi::stri_detect_regex(sp, case_insensitive = T), stringi::stri_sub(sp, 1, 4), 'Otras')) %>% 
    dplyr::group_by(Tipo_for, BNP) %>% 
    dplyr::summarise(Sup_ha = sum(Sup_ha), .groups = 'drop') %>% 
    dplyr::mutate(P = Sup_ha / sup_cuenca) 

  if ('Otras' %in% c(TF_BNP$BNP)) {
    TF_BNP <- TF_BNP %>%
      tidyr::pivot_wider(id_cols = Tipo_for, names_from = BNP,values_from = c(Sup_ha, P),names_sep = "_") %>% 
      dplyr::select(Tipo_for, dplyr::ends_with(stringi::stri_sub(sp, 1, 4)), dplyr::ends_with('Otras'))
  } else {
    TF_BNP <- TF_BNP %>%
      dplyr::bind_rows(
        uso_veg %>% 
          sf::st_drop_geometry() %>% 
          dplyr::filter(
            !Tipo_for %>% stringi::stri_cmp_equiv('no aplica', strength = 1), 
            F_ley20283 %>% stringi::stri_detect_regex('preser', case_insensitive = T)
          ) %>% 
          dplyr::group_by(Tipo_for) %>% 
          dplyr::tally() %>% dplyr::select(-n) %>% 
          dplyr::mutate(BNP = 'Otras', Sup_ha = 0, P = 0)
      ) %>% 
      tidyr::pivot_wider(id_cols = Tipo_for, names_from = BNP, values_from = c(Sup_ha,P), names_sep = "_") %>% 
      dplyr::select(Tipo_for, dplyr::ends_with(stringi::stri_sub(sp, 1, 4)), dplyr::ends_with('Otras'))
  }

  TF_Total <- uso_veg %>% 
    sf::st_drop_geometry() %>% 
    dplyr::filter(F_ley20283 %>% stringi::stri_cmp_equiv('Bosque nativo', strength = 1)) %>% 
    dplyr::group_by(Tipo_for) %>% 
    dplyr::summarise(Sup_Total = sum(Sup_ha)) %>% 
    dplyr::mutate(P_Total = Sup_Total / sup_cuenca)

  TF_tabla <- dplyr::full_join(TF_BN, TF_BNP) %>%
    dplyr::full_join(TF_Total) %>%
    dplyr::mutate_all(tidyr::replace_na, 0) %>%
    janitor::adorn_totals(name = "Total") %>%
    gt::gt(rowname_col = "Tipo_for") %>%
    gt::tab_header(
      title = "Superficie (ha) según tipo forestal, bosque nativo y bosque nativo de preservación en la cuenca"
    ) %>%
    gt::tab_stubhead(label = "Tipo Forestal") %>%
    gt::tab_footnote(
      footnote = paste0(
        "Porcentajes respecto del de la superficie total de la cuenca (",
        sup_cuenca %>% format(decimal.mark = ",", big.mark = "."),
        " ha)"
      )
    ) %>%
    gt::tab_spanner(
      label = gt::md(paste0(
        "*",
        stringi::stri_extract_first_words(sp, 1) %>% stringi::stri_sub(1, 1) %>% paste0('. '),
        stringi::stri_extract_last_words(sp, 2),
        "*"
      )),
      columns = 4:5,
      id = 'ecc'
    ) %>%
    gt::tab_spanner(label = "Otras", columns = 6:7, id = 'otras') %>%
    gt::tab_spanner(
      label = "Bosque nativo de preservación",
      spanners = c("ecc", "otras")
    ) %>%
    gt::tab_spanner(label = "Bosque nativo", columns = 2:3, level = 1) %>%
    gt::tab_spanner(label = "Total", columns = 8:9) %>%
    gt::cols_label(
      gt::matches("Sup") ~ "Sup (ha)",
      gt::starts_with("P_") ~ "%"
    ) %>%
    gt::cols_align(align = "center") %>%
    gt::fmt_percent(
      columns = seq(3, 9, 2),
      decimals = 2,
      dec_mark = ','
    ) %>%
    gt::fmt_number(
      columns = seq(2, 8, 2),
      sep_mark = '.',
      dec_mark = ','
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "#A0968C", alpha = 0.2),
      locations = list(
        gt::cells_column_labels(gt::everything()),
        gt::cells_column_spanners(gt::everything()),
        gt::cells_stubhead()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold"
      ),
      locations = list(
        gt::cells_column_labels(gt::everything()),
        gt::cells_column_spanners(gt::everything()),
        gt::cells_stubhead()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = "medium"),
      locations = list(
        gt::cells_title(),
        gt::cells_column_labels(),
        gt::cells_column_spanners(),
        gt::cells_body(),
        gt::cells_stubhead(),
        gt::cells_stub()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = "small"),
      locations = list(gt::cells_footnotes())
    ) %>% 
    suppressMessages() %>% 
    suppressWarnings()
  return(TF_tabla)
}

#' @noRd
gt_usos <- function(uso_veg) {
  valid_input(uso_veg, inherit = "data.frame", names = c("Uso", "Subuso", "Sup_ha"))

  uso_veg %>% 
    sf::st_drop_geometry() %>% 
    dplyr::group_by(Uso, Subuso) %>% 
    dplyr::summarise(Superficie_ha = sum(Sup_ha), .groups = "drop") %>% 
    dplyr::mutate(Representatividad = Superficie_ha / sum(Superficie_ha)) %>%  
    dplyr::bind_rows(
      uso_veg %>% 
        sf::st_drop_geometry() %>% 
        dplyr::group_by(Uso) %>% 
        dplyr::summarise(Superficie_ha = sum(Sup_ha), .groups = "drop") %>% 
        dplyr::mutate(
          Representatividad = Superficie_ha / sum(Superficie_ha),
          Subuso = paste0('Subtotal ', Uso)
        ) 
    ) %>% 
    dplyr::bind_rows(
      uso_veg %>%
        sf::st_drop_geometry() %>% 
        dplyr::summarise(Superficie_ha = sum(Sup_ha)) %>%
        dplyr::mutate(
          Representatividad = Superficie_ha / sum(Superficie_ha),
          Uso = "Total",
          Subuso = "Total"
        )
    ) %>%
    dplyr::mutate_if(is.character, stringi::stri_trans_general, "Latin-ASCII") %>% 
    dplyr::arrange(Uso) %>% 
    gt::gt() %>% 
    gt::fmt_percent(
      columns = Representatividad,
      decimals = 2,
      dec_mark = ','
    ) %>% 
    gt::fmt_number(
      columns = Superficie_ha,
      sep_mark = '.',
      dec_mark = ','
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold"
      ),
      locations = list(
        gt::cells_body(
          rows = stringi::stri_detect_regex(Subuso, 'total', case_insensitive = T)
        )
      )
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold",
        align = "center",
        v_align = "middle"
      ),
      locations = gt::cells_column_labels()
    ) %>% 
    gt::tab_style(
      style = gt::cell_fill(color = "#A0968C",alpha = 0.1),
      locations = list(gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(size = "medium"),
      locations = list(gt::cells_body(),gt::cells_column_labels())
    ) %>% 
    gt::cols_label(
      Uso = "Uso de la tierra",
      Subuso = 'Sub-uso',
      Superficie_ha = "Superficie (ha)",
      Representatividad = gt::md("Representatividad<br>en la cuenca (%)")
    ) 
}

#' @noRd
gt_IVI <- function(df_ivi) {
  df_ivi %>% 
    dplyr::rename_at(5:7, stringi::stri_replace_first_regex, "_", " ") %>% 
    gt::gt() %>% 
    gt::fmt_number(
      columns = 3,
      decimals = 3,
      sep_mark = '.',
      dec_mark = ','
    ) %>% 
    gt::fmt_number(
      columns = 4:8,
      decimals = 1,
      sep_mark = '.',
      dec_mark = ','
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold"
      ),
      locations = list(
        gt::cells_body(
          columns = gt::everything(),
          rows = stringi::stri_detect_regex(Especie, 'Total')
        ),
        gt::cells_column_labels(gt::everything())
      )
    ) %>% 
    gt::tab_style(
      style = gt::cell_fill(color = "#A0968C", alpha = 0.1),
      locations = list(gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(size = "small"),
      locations = list(gt::cells_body(), gt::cells_column_labels())
    )
}

#' @noRd
gt_estadisticos <- function(df_est) {
  df_est %>%
    dplyr::mutate_all(as.character) %>%
    `names<-`(c(
      "Variable",
      "Rango (ind/ha)",
      "Tamaño de la muestra (n)",
      "Nha (ind/ha)",
      "Varianza",
      "Coeficiente de variación (%)",
      "Error absoluto (ind/ha)",
      "Error relativo (%)",
      "Intervalo de confianza (95%) (ind/ha)"
    )) %>%
    tidyr::pivot_longer(
      names_to = "Parámetros",
      values_to = "Val",
      cols = -Variable
    ) %>%
    dplyr::mutate_at("Parámetros", forcats::as_factor) %>%
    tidyr::spread(key = Variable, value = Val) %>%
    dplyr::rename("Conjunto de especies arbóreas" = Nha_Total) %>%
    dplyr::rename_at(3, stringi::stri_replace_all_regex, "\\.", " ") %>%
    gt::gt() %>%
    gt::cols_align(align = "center", columns = 2:3) %>%
    gt::cols_align(align = "left", columns = 1) %>%
    gt::fmt_number(
      columns = 2:3,
      sep_mark = '.',
      dec_mark = ','
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "#A0968C", alpha = 0.1),
      locations = list(gt::cells_column_labels())
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = "small"),
      locations = list(gt::cells_body(), gt::cells_column_labels())
    ) %>%
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold",
        align = "center"
      ),
      locations = list(
        gt::cells_column_labels(gt::everything())
      )
    )
}

#' @noRd
gt_prop <- function(df_est, df_prop, BNP_cuenca, sp) {
  df_est %>%
    dplyr::filter(Variable == sp) %>%
    dplyr::select(Variable, Promedio, E_rel, Int_conf) %>%
    tidyr::separate_wider_delim(
      Int_conf,
      delim = " - ",
      names = c("Int_inf", "Int_sup")
    ) %>%
    dplyr::mutate(
      Individuos_totales = df_prop$Individuos_totales %>% sum(),
      param = "Densidad media"
    ) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Int")), as.double) %>%
    dplyr::select(
      Variable,
      param,
      Promedio,
      E_rel,
      Int_inf,
      Int_sup,
      Individuos_totales
    ) %>%
    dplyr::bind_rows(
      df_prop %>%
        dplyr::mutate(
          param = paste0("Proporción (p=", Proporciones, ")"),
          Promedio = (df_est[df_est$Variable == sp, ][["Promedio"]] *
            Proporciones) %>%
            janitor::round_half_up(),
          E_rel = df_est[df_est$Variable == sp, ][["E_rel"]],
          Int_inf = janitor::round_half_up(Promedio - Promedio * E_rel / 100),
          Int_sup = janitor::round_half_up(Promedio + Promedio * E_rel / 100)
        ) %>%
        dplyr::rename(Variable = Estado) %>%
        dplyr::select(
          Variable,
          param,
          Promedio,
          E_rel,
          Int_inf,
          Int_sup,
          Individuos_totales
        )
    ) %>%
    `names<-`(c(
      "ECC/Estado de desarrollo",
      "Parámetro poblacional estimado",
      "Estimación (ind/ha)",
      "Error relativo (%)",
      "Inf",
      "Sup",
      "Total poblacional* (individuos)"
    )) %>%
    gt::gt() %>%
    gt::cols_width(
      c(1,2) ~ gt::px(150),
      c(3:4, 7) ~ gt::px(100),
      5:6 ~ gt::px(80)
    ) %>%
    gt::tab_header(
      title = "Estimación puntual e intervalos de confianza para la densidad media de la especie en categoría de conservación"
    ) %>%
    gt::tab_footnote(
      footnote = paste0(
        "* Total obtenido multiplicando la densidad por la superficie total de la cuenca (",
        BNP_cuenca$Sup_ha %>%
          sum() %>%
          format(decimal.mark = ",", big.mark = "."),
        " ha)"
      )
    ) %>%
    gt::tab_spanner(
      label = "Intervalos de confianza (ind/ha)",
      columns = 5:6
    ) %>%
    gt::fmt_number(
      columns = c(4, 7),
      sep_mark = '.',
      dec_mark = ',',
      drop_trailing_zeros = T
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "#A0968C", alpha = 0.1),
      locations = list(gt::cells_column_labels(), gt::cells_column_spanners())
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = "small"),
      locations = list(
        gt::cells_title(),
        gt::cells_body(),
        gt::cells_column_labels(),
        gt::cells_column_spanners()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = "x-small"),
      locations = list(gt::cells_footnotes())
    ) %>%
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold",
        align = "center"
      ),
      locations = list(
        gt::cells_column_labels(gt::everything()),
        gt::cells_column_spanners()
      )
    )
}

#' @noRd
gt_ecc_inter <- function(ecc_inter, BNP_int_sin_censo, col_obras = NULL) {
  sp <- ecc_inter$Especie %>% unique() %>% .[1]
  vars_obras <- if(is.null(col_obras)) {
    dplyr::syms(dplyr::select(ecc_inter, dplyr::contains("obra")) %>% names() %>% .[1])
  } else {
    dplyr::syms(col_obras)
  }
  ecc_inter %>% 
    sf::st_drop_geometry() %>% 
    dplyr::count(!!!vars_obras, Especie, Afectacion) %>% 
    {if(!is.null(BNP_int_sin_censo)) {
      .[] %>% dplyr::bind_rows(
        BNP_int_sin_censo %>% 
          sf::st_drop_geometry() %>% 
          dplyr::mutate(Especie = sp) %>% 
          dplyr::select(!!!vars_obras, Especie, Afectacion, Ind_interv) %>% 
          dplyr::rename(n = Ind_interv)
      )
    } else .[]} %>% 
    dplyr::group_by(!!!vars_obras, Especie, Afectacion) %>% 
    dplyr::summarise(n = sum(n)) %>% 
    janitor::adorn_totals(name = "Total individuos a intervenir") %>%
    gt::gt() %>% 
    gt::tab_style(
      style = gt::cell_fill(color = "#A0968C", alpha = 0.1),
      locations = list(gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(size = "large"),
      locations = list(gt::cells_title(), gt::cells_body(), gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold",
        align = "center",
        v_align = "middle"
      ),
      locations = list(gt::cells_column_labels(gt::everything()))) %>% 
    gt::tab_style(
      style = gt::cell_text(style = "italic"),
      locations = list(gt::cells_body(columns = "Especie"))
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = Obra %>% stringi::stri_detect_regex("Total"))
    ) %>% 
    gt::cols_label(
      Afectacion = "Tipo (corta, eliminación, destrucción, descepado)",
      n = "N° individuos por intervenir"
    )
}

#' @noRd
gt_ecc_alter <- function(ecc_alter, BNP_alt_sin_pto, col_obras = NULL) {
  sp <- ecc_alter$Especie %>% unique() %>% .[1]
  vars_obras <- if(is.null(col_obras)) {
    dplyr::syms(dplyr::select(ecc_alter, dplyr::contains("obra")) %>% names() %>% .[1])
  } else {
    dplyr::syms(col_obras)
  }
  ecc_alter %>% 
    sf::st_drop_geometry() %>% 
    dplyr::count(!!!vars_obras, Especie) %>% 
    {if(!is.null(BNP_alt_sin_pto)) {
      .[] %>% 
        dplyr::bind_rows(
          BNP_alt_sin_pto %>% 
            sf::st_drop_geometry() %>% 
            dplyr::mutate(Especie = sp) %>% 
            dplyr::select(!!!vars_obras, Especie, Ind_alterar) %>% 
            dplyr::rename(n = Ind_alterar)
        ) 
    } else .[]} %>% 
    dplyr::group_by(!!!vars_obras, Especie) %>% 
    dplyr::summarise(n = sum(n), .groups = "drop") %>%
    dplyr::mutate(Componente = "") %>% 
    janitor::adorn_totals(name = "Total individuos a alterar su hábitat") %>% 
    gt::gt() %>% 
    gt::tab_style(
      style = gt::cell_fill(color = "#A0968C",alpha = 0.1),
      locations = list(gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(size = "large"),
      locations = list(gt::cells_title(), gt::cells_body(), gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold",
        align = "center",
        v_align = "middle"
      ),
      locations = list(gt::cells_column_labels(gt::everything()))) %>% 
    gt::tab_style(
      style = gt::cell_text(style = "italic"),
      locations = list(gt::cells_body(columns = "Especie"))
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = Obra %>% stringi::stri_detect_regex("Total"))
    ) %>% 
    gt::cols_label(
      Componente = "Componente del ambiente alterado",
      n = "N° individuos cuyo hábitat será alterado"
    )
}

#' @noRd
gt_sup_inter <- function(obras, BNP_inter, BNP_alterar, col_obras = NULL) {
  if(is.null(col_obras)) {
    vars_obras <- dplyr::syms(dplyr::select(obras, dplyr::contains("obra")) %>% names() %>% .[1])
  } else {
    vars_obras <- dplyr::syms(col_obras)
  }
  last_var <- vars_obras[length(vars_obras)]
  df_1 <- obras %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(!!!vars_obras) %>%
    dplyr::summarise(Sup_obra = sum(Sup_ha), .groups = "drop") %>%
    dplyr::left_join(
      BNP_inter %>%
        sf::st_drop_geometry() %>%
        dplyr::group_by(!!!vars_obras) %>%
        dplyr::summarise(Sup_bn_obra = sum(Sup_ha), .groups = "drop")
    ) %>%
    dplyr::left_join(
      BNP_inter %>%
        sf::st_drop_geometry() %>%
        dplyr::group_by(!!!vars_obras) %>%
        dplyr::summarise(Sup_int = sum(Sup_ha), .groups = "drop")
    ) %>%
    dplyr::left_join(
      BNP_alterar %>%
        sf::st_drop_geometry() %>%
        dplyr::group_by(!!!vars_obras) %>%
        dplyr::summarise(Sup_alt = sum(Sup_ha), .groups = "drop")
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(!!!last_var), 
      ~dplyr::case_when(
        dplyr::if_all(
          c("Sup_bn_obra", "Sup_int", "Sup_alt"),
          ~ is.na(.)
        ) ~ "Otras obras",
        .default = .
      )
    ) 
  
  if(length(vars_obras) > 1) {
    for(i in vars_obras %>% dplyr::setdiff(last_var) %>% length() %>% seq_len() %>% sort(decreasing = T)) {
      current_var <- vars_obras[[i]]
      next_var <- vars_obras[[i+1]]
      df_1 <- df_1 %>%
        dplyr::group_by(!!current_var) %>%
        dplyr::mutate("{current_var}" := if (all(!!next_var == "Otras obras")) "Otras obras" else !!current_var) %>%
        dplyr::ungroup()
    }
  }
  
  gt <- df_1 %>% 
    dplyr::group_by(!!!vars_obras) %>%
    dplyr::summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::across(!!!vars_obras[1]) == "Otras obras") %>% 
    janitor::adorn_totals() %>%
    dplyr::mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>% 
    gt::gt() %>%
    gt::fmt_number(
      columns = c(1:4 + length(vars_obras)),
      sep_mark = '.',
      dec_mark = ',',
      decimals = 2
    ) %>%
    gt::tab_spanner(
      label = "Obra o actividad",
      columns = 1:(1 + length(vars_obras))
    ) %>%
    gt::tab_spanner(
      label = "Bosque nativo de preservación (ha)",
      columns = 2:4 + length(vars_obras)
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "#A0968C", alpha = 0.1),
      locations = list(gt::cells_column_labels(), gt::cells_column_spanners())
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = "medium"),
      locations = list(
        gt::cells_title(),
        gt::cells_body(),
        gt::cells_column_labels(),
        gt::cells_column_spanners()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold",
        align = "center",
        v_align = "middle"
      ),
      locations = list(gt::cells_column_labels(), gt::cells_column_spanners())
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = Obra %>% stringi::stri_detect_regex("Total"))
    ) %>%
    gt::cols_label(
      Sup_obra = "Superficie (ha)",
      Sup_bn_obra = "Superficie en la obra o actividad",
      Sup_int = "Superficie de intervención",
      Sup_alt = "Superficie de alteración de hábitat"
    ) 
  return(gt)
}

#' @noRd
gt_spp_acomp <- function(spp_acomp) {
  spp_acomp %>% 
    gt::gt() %>% 
    gt::fmt_percent(
      columns = 2,
      decimals = 1,
      dec_mark = ","
    )  %>% 
    gt::tab_style(
      style = gt::cell_text(style = "italic"),
      locations = list(gt::cells_body(columns = 1))
    ) %>% 
    gt::tab_style(
      style = gt::cell_fill(color = "#A0968C", alpha = 0.1),
      locations = list(gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(size = "medium"),
      locations = list(gt::cells_body(), gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold",
        align = "center",
        v_align = "middle"
      ),
      locations = list(gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_body(columns = 2)
    ) %>% 
    gt::cols_label(
      Especie = "Especies acompañantes",
      Frecuencia = "Frecuencia (%)"
    )
}

#' @noRd
gt_indices <- function(tabla_indices) {
  tabla_indices %>% 
    tidyr::pivot_wider(names_from = `Índices`, values_from = Valor) %>% 
    dplyr::mutate(Sector = "BNP en área de proyecto") %>% 
    dplyr::relocate(Sector) %>% 
    gt::gt() %>% 
    gt::cols_label(
      `Div. Simpson` = "Simpson (1-𝝀)",
      `Shannon-Weaver` = "Shannon-Weaver (H')"
    ) %>% 
    gt::fmt_number(
      columns = 2:4,
      decimals = 3,
      sep_mark = '.',
      dec_mark = ","
    ) %>% 
    gt::tab_header(
      title = "Índices de diversidad del bosque nativo de preservación en el área de proyecto"
    ) %>% 
    gt::tab_style(
      style = gt::cell_fill(color = "#A0968C",alpha = 0.1),
      locations = list(gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(size = "medium"),
      locations = list(gt::cells_title(), gt::cells_body(), gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold",
        align = "center",
        v_align = "middle"
      ),
      locations = list(gt::cells_column_labels())
    )
}

#' @noRd
gt_frag_matriz <- function(matriz_paisaje) {
  matriz_paisaje %>% 
    gt::gt() %>% 
    gt::fmt_number(
      columns = 2:5,
      decimals = 2,
      sep_mark = '.',
      dec_mark = ","
    ) %>% 
    gt::tab_spanner(
      label = "Superficie (ha)",
      columns = 2:4
    ) %>% 
    gt::tab_style(
      style = gt::cell_fill(color = "#A0968C",alpha = 0.1),
      locations = list(gt::cells_column_spanners(), gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(size = "medium"),
      locations = list(gt::cells_body(), gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold",
        align = "center",
        v_align = "middle"
      ),
      locations = list(gt::cells_column_spanners(), gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = Subuso %>% stringi::stri_detect_regex("Total"))
    ) %>% 
    gt::tab_style(
      style = gt::cell_fill("yellow"),
      locations = gt::cells_body(columns = 5, rows = Subuso %>% stringi::stri_detect_regex("Total veg"))
    )
}

#' @noRd
gt_frag_param <- function(df) {
  df %>% 
    gt::gt() %>% 
    gt::fmt_number(
      columns = 3:4,
      decimals = 2,
      sep_mark = '.',
      dec_mark = ",",
      drop_trailing_zeros = T
    ) %>% 
    gt::fmt_number(
      columns = 5,
      decimals = 2,
      sep_mark = '.',
      dec_mark = ",",
    ) %>% 
    gt::tab_style(
      style = gt::cell_fill(color = "#A0968C",alpha = 0.1),
      locations = list(gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(size = "x-small"),
      locations = list(gt::cells_body(), gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold",
        align = "center",
        v_align = "middle"
      ),
      locations = list(gt::cells_column_labels())
    )
}

#' @noRd
gt_frag_resultados <- function(tabla_eval) {
  tabla_eval %>% 
    dplyr::mutate_at(4, ~stringi::stri_replace_all_regex(.,"\\.",",")) %>% 
    janitor::adorn_totals() %>% 
    gt::gt() %>% 
    gt::fmt_number(
      columns = 5,
      decimals = 2,
      sep_mark = '.',
      dec_mark = ",",
      drop_trailing_zeros = T
    ) %>% 
    gt::tab_style(
      style = gt::cell_fill(color = "#A0968C",alpha = 0.1),
      locations = list(gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(size = "small"),
      locations = list(gt::cells_body(), gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold",
        align = "center",
        v_align = "middle"
      ),
      locations = list(gt::cells_column_labels())
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = `Parámetro` %>% stringi::stri_detect_regex("Total"))
    ) 
}