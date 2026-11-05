#' Identifica y calcula la superficie de alteración en fragmentos remanentes de BNP
#'
#' Esta función procesa la capa de Bosque Nativo de Preservación (BNP) para identificar
#' fragmentos que, tras ser seccionados por las obras del proyecto (intervención),
#' resultan en superficies remanentes menores a un umbral específico. Estos fragmentos
#' se clasifican como "Alteración BNP remanente" debido a que su pequeño tamaño
#' compromete su viabilidad biológica o conectividad.
#'
#' @param BNP Objeto `sf` (polígonos) que representa el Bosque Nativo de Preservación.
#'   Debe contener al menos las columnas `Nom_ssubc` y `BNP_ECC`.
#' @param obras Objeto `sf` (polígonos) que representa las áreas de intervención de las obras.
#' @param sp String con el nombre científico de la especie objeto de protección (ECC)
#'   para filtrar en la columna `BNP_ECC`.
#' @param sup_ha_max Valor numérico que define el umbral máximo de superficie (en hectáreas)
#'   para que un fragmento seccionado sea considerado "alterado". Por defecto es 0.5 ha. (Solo para `get_alter_bnp_remanente`).
#' @param censos Objeto `sf` (puntos) con los registros de censo. (Solo para `get_alter_censo_buffer`).
#' @param distancia Valor numérico. Distancia máxima (en metros) desde un individuo censado hasta una obra para ser considerado en la zona de alteración por buffer. (Solo para `get_alter_censo_buffer`).
#' @param buffer Valor numérico. Radio del buffer (en metros) a aplicar a los individuos censados cercanos a las obras. (Solo para `get_alter_censo_buffer`).
#' @param dTolerance Valor numérico. Número de decimales para redondear las distancias calculadas. (Solo para `get_alter_censo_buffer`).
#' @param radio Valor numérico entre 0 y 1. Parámetro para controlar la concavidad del casco cóncavo (concave hull) generado alrededor de los puntos agrupados. Un valor de 1 produce un casco convexo, mientras que valores menores permiten formas más cóncavas. (Solo para `get_alter_censo_buffer`).
#' @examples
#' \dontrun{
#'   alter_1 <- get_alter_bnp_remanente(
#'     BNP = sf::read_sf("ruta/capa/BNP.shp"),
#'     obras = sf::read_sf("ruta/capa/Obras.shp"),
#'     sp = "Porlieria chilensis",
#'     sup_ha_max = 0.5
#'   )
#'   alter_2 <- get_alter_bnp_remanente(
#'     BNP = sf::read_sf("ruta/capa/BNP.shp"),
#'     obras = sf::read_sf("ruta/capa/Obras.shp"),
#'     censos = sf::read_sf("ruta/capa/Censos.shp"),
#'     sp = "Porlieria chilensis"
#'   )
#' }
#' 
#' @return Un objeto `sf` con los polígonos de BNP remanente que cumplen con el criterio
#'   de tamaño máximo, etiquetados con la columna `Afectacion`. Si no se encuentran
#'   remanentes que cumplan los criterios, devuelve `NULL` de forma invisible.
#' 
#' @name Alteracion
#' @export
get_alter_bnp_remanente <- function(BNP, obras, sp, sup_ha_max = 0.5) {
  BNP <- tryCatch(
    preparar_uso_veg(x = BNP),
    error = function(e) stop("Error al preparar la capa de BNP", e$message, call. = FALSE)
  )
  if (!inherits(BNP, "sf")) stop("El argumento 'BNP' debe ser un objeto 'sf'.", call. = FALSE)
  if (!all(c("Nom_ssubc", "BNP_ECC") %in% names(BNP))) stop("BNP debe contener las columnas: 'Nom_ssubc' y 'BNP_ECC'.", call. = FALSE)
  if (!inherits(obras, "sf")) stop("El argumento 'obras' debe ser un objeto 'sf'.", call. = FALSE)
  if (!is.character(sp)) stop("El argumento 'sp' debe ser una cadena de texto.", call. = FALSE)

  obras <- obras %>% sf::st_transform(sf::st_crs(BNP))
  obras_union <- sf::st_union(obras[BNP, ])
  
  bnp_sp <- BNP %>% dplyr::filter(stringi::stri_detect_regex(BNP_ECC, sp, case_insensitive = TRUE))

  remanentes <- BNP %>% 
    dplyr::filter(BNP_ECC %>% stringi::stri_detect_regex(sp, case_insensitive = T)) %>% 
    dplyr::count(Nom_ssubc) %>% 
    sf::st_collection_extract("POLYGON") %>% 
    sf::st_cast("POLYGON") %>% 
    sf::st_filter(obras, .predicate = sf::st_relate, pattern = "T********") %>% 
    tibble::rowid_to_column("ID") %>% 
    sf::st_difference(sf::st_union(obras[BNP, ])) %>% 
    sf::st_collection_extract("POLYGON") %>% 
    sf::st_cast("POLYGON") %>% 
    dplyr::select(-n) %>% 
    dplyr::add_count(ID) %>% 
    dplyr::filter(
      n > 1,
      janitor::round_half_up(units::drop_units(sf::st_area(geometry)), 1) > 0
    ) %>% 
    dplyr::select(-c(ID, n)) %>%
    st_add_sup_ha() %>% 
    dplyr::filter(Sup_ha < sup_ha_max) %>% 
    dplyr::arrange(Sup_ha) %>% 
    dplyr::relocate(geometry, .after = dplyr::last_col()) %>% 
    sf::st_join(
      obras %>% dplyr::select(dplyr::contains("obra")), 
      join = sf::st_nearest_feature
    ) %>% 
    suppressMessages() %>% 
    suppressWarnings()
  
  if (nrow(remanentes) == 0) {
    return(invisible(NULL))
  }
  
  alteracion <- remanentes %>% 
    sf::st_intersection(
      bnp_sp %>% dplyr::select(-dplyr::any_of(names(sf::st_drop_geometry(remanentes))))
    ) %>% 
    sf::st_collection_extract("POLYGON") %>% 
    sf::st_make_valid() %>% 
    sf::st_collection_extract("POLYGON") %>% 
    sf::st_cast("POLYGON") %>% 
    st_add_sup_ha() %>% 
    dplyr::mutate(
      Afectacion = "Alteración BNP remanente",
    ) %>% 
    dplyr::arrange(Sup_ha) %>% 
    dplyr::select(dplyr::all_of(names(sf::st_drop_geometry(BNP))), dplyr::contains("obra"), Afectacion, Sup_ha) %>%
    dplyr::relocate(geometry, .after = dplyr::last_col()) %>% 
    suppressMessages() %>% 
    suppressWarnings()
  
  return(alteracion)
}

#' @rdname Alteracion
#' @export
get_alter_censo_buffer <- function(BNP, obras, censos, sp, distancia = 5, buffer = 5, dTolerance = 2, radio = 1){
  BNP <- tryCatch(
    preparar_uso_veg(x = BNP),
    error = function(e) stop("Error al preparar la capa de BNP", e$message, call. = FALSE)
  )
  if (!inherits(BNP, "sf")) stop("El argumento 'BNP' debe ser un objeto 'sf'.", call. = FALSE)
  if (!inherits(obras, "sf")) stop("El argumento 'obras' debe ser un objeto 'sf'.", call. = FALSE)
  if (!is.character(sp)) stop("El argumento 'sp' debe ser una cadena de texto.", call. = FALSE)
  if (!is.numeric(distancia)) stop("El argumento 'distancia' debe ser un valor numérico.", call. = FALSE)
  if (!is.numeric(dTolerance) || dTolerance < 0) {
    stop("El argumento 'dTolerance' debe ser un número entero mayor o igual a 0.", call. = FALSE)
  }
  if (!is.numeric(radio) || radio < 0 || radio > 1) {
    stop("El argumento 'radio' debe ser un valor numérico entre 0 y 1.", call. = FALSE)
  }
  
  ind <- censos %>%
    dplyr::filter(Especie %>% stringi::stri_detect_regex(sp, case_insensitive = T)) %>% 
    dplyr::select(Especie) %>% 
    sf::st_filter(BNP) %>% 
    dplyr::mutate(
      Distancia = sf::st_distance(
        geometry,
        obras[sf::st_nearest_feature(geometry, obras), ],
        by_element = TRUE
      ) %>%
        units::drop_units() %>% 
        janitor::round_half_up(dTolerance)
    ) %>%
    dplyr::filter(Distancia <= distancia) %>% 
    sf::st_difference(sf::st_union(obras[BNP,])) %>%  
    suppressMessages() %>% 
    suppressWarnings()

  alter_buffer <- ind %>% 
    sf::st_buffer(buffer) %>% 
    dplyr::mutate(group = group_by_distance(geometry, buffer)) %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarise(geometry = sf::st_union(geometry), n = n()) %>% 
    sf::st_collection_extract("POLYGON") %>% 
    sf::st_cast("MULTIPOINT") %>% 
    sf::st_concave_hull(ratio = radio) %>% 
    # smoothr::smooth(method = "ksmooth") %>% 
    sf::st_make_valid() %>% 
    sf::st_collection_extract("POLYGON") %>% 
    dplyr::relocate(geometry, .after = dplyr::last_col()) %>% 
    dplyr::select(-c(1,2)) %>% 
    sf::st_difference(sf::st_union(obras[BNP, ])) %>% 
    sf::st_collection_extract("POLYGON") %>% 
    sf::st_cast("POLYGON") %>%
    sf::st_join(
      obras %>% dplyr::select(dplyr::contains("obra")), 
      join = sf::st_nearest_feature
    )
  
  alter <- alter_buffer %>% 
    sf::st_intersection(
      BNP %>% dplyr::select(-dplyr::any_of(names(sf::st_drop_geometry(alter_buffer))))
    ) %>% 
    clean_geom() %>% 
    st_add_sup_ha() %>% 
    dplyr::mutate(Afectacion = "Alteracion buffer ECC") %>% 
    dplyr::arrange(Sup_ha) %>% 
    dplyr::select(dplyr::all_of(names(sf::st_drop_geometry(BNP))), dplyr::contains("obra"), Afectacion, Sup_ha) %>%
    dplyr::relocate(geometry, .after = dplyr::last_col()) %>% 
    suppressMessages() %>% 
    suppressWarnings()
  return(alter)
}

#' @rdname Alteracion
#' @export
get_alter_buffer_fijo <- function(
  BNP, 
  obras, 
  buffer = 5, 
  campo_obras = "Obra", 
  orden_obras = c("frente", "plaza", "camino")
) {
  BNP <- tryCatch(
    preparar_uso_veg(x = BNP),
    error = function(e) stop("Error al preparar la capa de BNP", e$message, call. = FALSE)
  )
  if (!inherits(BNP, "sf")) stop("El argumento 'BNP' debe ser un objeto 'sf'.", call. = FALSE)
  if (!inherits(obras, "sf")) stop("El argumento 'obras' debe ser un objeto 'sf'.", call. = FALSE)
  if (!is.numeric(buffer)) stop("El argumento 'buffer' debe ser un valor numérico.", call. = FALSE)
  if (!is.character(campo_obras) || length(campo_obras) != 1) {
    stop("El argumento 'campo_obras' debe ser un único nombre de columna (character).", call. = FALSE)
  }
  if (!campo_obras %in% names(obras)) {
    stop("El campo '", campo_obras, "' no existe en 'obras'.", call. = FALSE)
  }
  if (!is.null(orden_obras) && !is.character(orden_obras)) {
    stop("El argumento 'orden_obras' debe ser NULL o un vector de texto (patrones regex).", call. = FALSE)
  }

  ordenar_niveles <- function(x, patrones) {
    x <- factor(x)
    if (is.null(patrones) || length(patrones) == 0) return(x)
    prioridad <- purrr::map(
      patrones,
      ~ stringi::stri_subset_regex(levels(x), .x, case_insensitive = TRUE)
    ) %>%
      unlist() %>%
      unique()
    forcats::fct_relevel(x, prioridad)
  }

  BNP %>% 
    sf::st_intersection(obras %>% dplyr::select(dplyr::contains("obra"))) %>% 
    clean_geom() %>% 
    sf::st_buffer(buffer) %>% 
    sf::st_difference(sf::st_union(obras)) %>% 
    clean_geom() %>% 
    dplyr::mutate(
      !!campo_obras := ordenar_niveles(.data[[campo_obras]], orden_obras)
    ) %>%
    dplyr::arrange(.data[[campo_obras]]) %>% 
    sf::st_difference() %>% 
    dplyr::filter(sf::st_geometry_type(geometry) %>% stringi::stri_detect_regex("POLYGON|GEOMETRYCOLLECTION")) %>%
    dplyr::mutate(GT = sf::st_geometry_type(geometry)) %>% 
    dplyr::relocate(geometry, .after = geometry) %>% 
    dplyr::filter(!sf::st_area(geometry) %>% units::drop_units() %>% janitor::round_half_up(1) == 0) %>% 
    split(.$GT) %>%
    purrr::map_dfr(clean_geom) %>% 
    dplyr::select(-GT) %>% 
    dplyr::count(dplyr::across(c(Nom_ssubc, dplyr::contains("obra")))) %>% dplyr::select(-n) %>% 
    sf::st_intersection(BNP %>% dplyr::select(-c(Nom_ssubc, dplyr::contains("obra")))) %>% 
    clean_geom() %>% 
    st_add_sup_ha() %>% 
    dplyr::mutate(Afectacion = "Alteracion buffer ECC") %>% 
    dplyr::arrange(Sup_ha) %>% 
    dplyr::select(dplyr::all_of(names(sf::st_drop_geometry(BNP))), dplyr::contains("obra"), Afectacion, Sup_ha) %>%
    dplyr::relocate(geometry, .after = dplyr::last_col()) %>% 
    suppressMessages() %>% 
    suppressWarnings()
} 

#' @rdname Alteracion
#' @export
get_alter_buffer_variable <- function(
  BNP_inter, 
  BNP, 
  obras, 
  radio,
  clima, 
  dem,
  stat_pend = c("median", "mean"),
  decimales_buffer = 1L,
  campo_obras = "Obra", 
  orden_obras = c("frente", "plaza", "camino")
) {
  BNP_inter <- tryCatch(
    preparar_uso_veg(x = BNP_inter) %>% sf::st_zm(),
    error = function(e) stop("Error al preparar la capa de BNP", e$message, call. = FALSE)
  )
  valid_input(BNP_inter, inherit = "sf", names = c("Nom_ssubc"), geometry = "POLYGON")
  BNP <- tryCatch(
    preparar_uso_veg(x = BNP) %>% sf::st_zm(),
    error = function(e) stop("Error al preparar la capa de BNP", e$message, call. = FALSE)
  )
  valid_input(BNP, inherit = "sf", geometry = "POLYGON")
  valid_dem(dem = dem)
  stat_pend <- match.arg(stat_pend)
  if (!inherits(obras, "sf")) stop("El argumento 'obras' debe ser un objeto 'sf'.", call. = FALSE)
  if (!is.numeric(radio) || radio < 0) stop("El argumento 'radio' debe ser un valor numérico mayor a 0", call. = FALSE)
  if (!is.numeric(clima) || clima < 2 || clima > 5) stop("El argumento 'clima' debe ser un valor numérico entre 2 y 5.", call. = FALSE)
  if (!is.character(campo_obras) || length(campo_obras) != 1) {
    stop("El argumento 'campo_obras' debe ser un único nombre de columna (character).", call. = FALSE)
  }
  if (!campo_obras %in% names(obras)) {
    stop("El campo '", campo_obras, "' no existe en 'obras'.", call. = FALSE)
  }
  if (!is.null(orden_obras) && !is.character(orden_obras)) {
    stop("El argumento 'orden_obras' debe ser NULL o un vector de texto (patrones regex).", call. = FALSE)
  }

  ordenar_niveles <- function(x, patrones) {
    x <- factor(x)
    if (is.null(patrones) || length(patrones) == 0) return(x)
    prioridad <- purrr::map(
      patrones,
      ~ stringi::stri_subset_regex(levels(x), .x, case_insensitive = TRUE)
    ) %>%
      unlist() %>%
      unique()
    forcats::fct_relevel(x, prioridad)
  }

  inter_param <- BNP_inter %>% 
    dplyr::mutate(
      Radio = radio, 
      Clima = clima,
      Pendiente = get_slope(dem = dem, x = sf::st_buffer(BNP_inter, 15), stat = stat_pend),
      N_pend = dplyr::case_when(
        Pendiente >= 0 & Pendiente < 5 ~ 0,
        Pendiente >= 5 & Pendiente < 15 ~ 0.5,
        Pendiente >= 15 & Pendiente < 30 ~ 1,
        Pendiente >= 30 ~ 2
      ),
      Distancia = janitor::round_half_up((Radio * Clima) + N_pend, decimales_buffer)
    ) %>% 
    sf::st_collection_extract("POLYGON") %>% 
    dplyr::relocate(geometry, .after = dplyr::last_col()) 

  inter_param %>% 
    st_buffer_byrow(Distancia) %>% 
    sf::st_difference(sf::st_union(BNP_inter)) %>% 
    dplyr::mutate(
      !!campo_obras := ordenar_niveles(.data[[campo_obras]], orden_obras)
    ) %>%
    dplyr::arrange(.data[[campo_obras]]) %>% 
    dplyr::arrange(campo_obras) %>% 
    sf::st_difference() %>% 
    dplyr::filter(sf::st_geometry_type(geometry) %>% stringi::stri_detect_regex("POLYGON|GEOMETRYCOLLECTION")) %>%
    dplyr::mutate(GT = sf::st_geometry_type(geometry)) %>% 
    dplyr::relocate(geometry, .after = geometry) %>% 
    dplyr::filter(!sf::st_area(geometry) %>% units::drop_units() %>% janitor::round_half_up(1) == 0) %>% 
    split(.$GT) %>%
    purrr::map_dfr(clean_geom) %>% 
    dplyr::count(dplyr::across(c(Nom_ssubc, dplyr::contains("obra")))) %>% dplyr::select(-n) %>% 
    sf::st_intersection(BNP %>% dplyr::select(-c(dplyr::contains("Nom_ssubc"), dplyr::contains("obra")))) %>% 
    add_BNP_ECC() %>% 
    clean_geom() %>% 
    st_add_sup_ha() %>% 
    dplyr::mutate(Afectacion = "Alteracion buffer ECC") %>% 
    dplyr::arrange(Sup_ha) %>% 
    dplyr::select(dplyr::all_of(names(sf::st_drop_geometry(BNP))), dplyr::contains("obra"), Afectacion, Sup_ha) %>%
    dplyr::relocate(geometry, .after = dplyr::last_col()) %>% 
    suppressMessages() %>% 
    suppressWarnings()
} 