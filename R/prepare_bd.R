#' Preparar datos
#'
#' @param BD dataframe o ruta con la base de datos de las parcelas de inventarios.
#' @param bd_lista valor logico. \code{TRUE} si los datos ingresados estan listos.
#' @param BNP_cuenca capa vectorial con los BNP en la cuenca. Obtener con `get_BNP_cuenca()`.
#' @param in_bnp_obra valor logico. \code{TRUE} para filtrar los puntos dentro de BNP con obras.
#' @param obras capa vectorial con las obras del proyecto.
#'
#' @returns tibble con los datos preparados
#' @rdname prepare_bd
#' @export
prepare_bd_flora_150 <- function(BD, bd_lista = TRUE, BNP_cuenca = NULL, in_bnp_obra = FALSE, obras = NULL) {
  valid_df(BD)
  if (!bd_lista) valid_input(BNP_cuenca, inherit = "sf", geometry = "POLYGON")
  if (in_bnp_obra) valid_input(obras, inherit = "sf", geometry = "POLYGON")

  bd <- tryCatch({
    (if (inherits(BD, "data.frame")) {
      BD
    } else {
      switch(
        tools::file_ext(BD),
        "xls" = readxl::read_xls(BD),
        "xlsx" = readxl::read_xlsx(BD)
      )
    })
  }, error = function(e) stop("Error en prepare_bd_flora: No se pudo leer el archivo", call. = F))

  BD <- tryCatch({
    bd %>%
      dplyr::rename_all(~ stringi::stri_trans_totitle(
        stringi::stri_trans_general(., "Latin-ASCII"),
        type = "sentence"
      )) %>%
      dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("punto", strength = 1), ~ "Parcela") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("cob_bb", strength = 1), ~ "Cob_BB") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("500", case_insensitive = T), ~ "N_ind") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("ds.*68", case_insensitive = T), ~ "DS_68") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("forma.*vida|habito", case_insensitive = T), ~ "Habito") %>%
      dplyr::rename_at(
        dplyr::vars(dplyr::matches("^rce"), dplyr::contains("utm")),
        stringi::stri_trans_toupper
      ) %>% 
      {if (!"Sup_parcela" %in% names(.[])) {
        .[] %>% tibble::add_column(Sup_parcela = 500)
      } else .} 
  }, error = function (e) {
    stop(
      as.character(e) %>%
        stringi::stri_replace_all_fixed(
          pattern = c(
            "in `rename()`",
            "Names must be unique",
            "These names are duplicated",
            "at locations",
            "and"
          ),
          replacement = c(
            "al renombrar los campos",
            "Los campos deben ser unicos",
            "Los siguientes campos estan duplicados",
            "en las columnas",
            "y"
          ),
          vectorize_all = F
        ) %>%
        gsub("\033\\[[0-9;]*m", "", .) %>%
        gsub("([!✖*])\\s+", "", .)
    )
  })
  valid_input(BD, names = req_names$BD_flora)

  if (!bd_lista) {
    ptos_todos <- BD %>% 
      dplyr::count(Parcela, UTM_E, UTM_N) %>% 
      na.omit() %>% 
      sf::st_as_sf(coords = c('UTM_E', 'UTM_N'), crs = get_utm_epsg(BNP_cuenca))
    if (in_bnp_obra) {
      ptos <- ptos_todos %>% 
        sf::st_intersection(sf::st_geometry(
          BNP_cuenca %>% 
            sf::st_filter(obras, .predicate = sf::st_relate, pattern = "T********")
        )) %>% 
        sf::st_drop_geometry() %>% 
        dplyr::pull(Parcela)
    } else {
      ptos <- ptos_todos %>% 
        sf::st_intersection(sf::st_union(BNP_cuenca)) %>% 
        sf::st_drop_geometry() %>% 
        dplyr::pull(Parcela)
    }
    BD <- BD %>% dplyr::filter(Parcela %in% ptos) 
  }

  BD %>%
    dplyr::mutate_if(is.character, stringi::stri_replace_all_regex, "\\s+", " ") %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::contains("Cob_BB")),
      ~ stringi::stri_trim(stringi::stri_trans_tolower(.))
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::matches("Especie")),
      ~ stringi::stri_trim(stringi::stri_trans_totitle(., type = "sentence"))
    ) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("UTM")), janitor::round_half_up) %>% 
    dplyr::filter(!Especie %>% stringi::stri_detect_regex("identificac|indeter", case_insensitive = T)) %>%
    dplyr::mutate_at("N_ind", as.integer) %>% 
    dplyr::mutate(N_ind = dplyr::case_when(Cob_BB == "fp" ~ 0.5, .default = N_ind)) %>% 
    tibble::as_tibble()
}

#' @rdname prepare_bd
#' @export
prepare_bd_fore_150 <- function(BD, BNP_cuenca, bd_lista = FALSE) {
  valid_df(BD)
  
  bd <- tryCatch({
    (if (inherits(BD, "data.frame")) {
      BD
    } else {
      switch(
        tools::file_ext(BD),
        "xls" = readxl::read_xls(BD),
        "xlsx" = readxl::read_xlsx(BD)
      )
    })
  }, error = function(e) stop("Error en prepare_bd_flora: No se pudo leer el archivo xlsx", call. = F))

  BD <- tryCatch({
    bd %>%
      dplyr::rename_all(~ stringi::stri_trans_totitle(
        stringi::stri_trans_general(., "Latin-ASCII"),
        type = "sentence")
      ) %>%
      dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("punto", strength = 1), ~ "Parcela") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("H.*(m)", strength = 1), ~ "Altura") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("D.*equiv", strength = 1), ~ "DAP") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("500", case_insensitive = T), ~ "N_ind") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("ds.*68", case_insensitive = T), ~ "DS_68") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("forma.*vida|habito", case_insensitive = T), ~ "Habito") %>%
      dplyr::rename_at(
        dplyr::vars(dplyr::matches("^rce"), dplyr::contains("UTM"), dplyr::matches("dap")),
        stringi::stri_trans_toupper
      )
  }, error = function(e) {
    stop(
      as.character(e) %>%
        stringi::stri_replace_all_fixed(
          pattern = c(
            "in `rename()`",
            "Names must be unique",
            "These names are duplicated",
            "at locations",
            "and"
          ),
          replacement = c(
            "al renombrar los campos",
            "Los campos deben ser unicos",
            "Los siguientes campos estan duplicados",
            "en las columnas",
            "y"
          ),
          vectorize_all = F
        ) %>%
        gsub("\033\\[[0-9;]*m", "", .) %>%
        gsub("([!✖*])\\s+", "", .)
    )
  })
  valid_input(BD, names = req_names$BD_fore)
  
  if (!bd_lista) {
    pto_bnp <- BD %>%
      dplyr::count(Parcela, UTM_E, UTM_N) %>% 
      sf::st_as_sf(coords = c("UTM_E","UTM_N"), crs = get_utm_epsg(BNP_cuenca)) %>%
      sf::st_intersection(sf::st_union(BNP_cuenca)) %>%
      .$Parcela
    BD <- BD %>% dplyr::filter(Parcela %in% pto_bnp) 
  } 

  BD %>%
    dplyr::mutate_if(is.character, stringi::stri_replace_all_regex, "\\s+", " ") %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("Estado")), stringi::stri_trans_general, "Latin-ASCII") %>% 
    dplyr::mutate_at(
      dplyr::vars(dplyr::matches("Especie"), dplyr::matches("Estado")),
      ~ stringi::stri_trim(stringi::stri_trans_totitle(., type = "sentence"))
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::matches("N_ind"), dplyr::matches("Altura"), dplyr::matches("DAP")),
      as.numeric
    ) %>% 
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("UTM")), janitor::round_half_up) %>% 
    dplyr::mutate_at(dplyr::vars(dplyr::contains("Altura")), ~ as.numeric(stringi::stri_replace_all_regex(., ",| ", "\\."))) %>% 
    dplyr::filter(!Especie %>% stringi::stri_detect_regex("identificac|indeter", case_insensitive = T)) %>%
    dplyr::mutate_at("N_ind", as.integer) %>% 
    tibble::as_tibble()
}