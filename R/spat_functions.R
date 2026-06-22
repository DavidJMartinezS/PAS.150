#' get_utm_epsg
#'
#' @param sf objeto sf
#'
#' @returns codigo EPSG del UTM correspondiente 
#'
#' @export
get_utm_epsg <- function(sf){
  if (!inherits(sf, "sf")) {
    stop("El input debe ser un objeto 'sf'.")
  }
  
  coords <- sf %>% 
    sf::st_combine() %>% 
    sf::st_centroid() %>% 
    sf::st_transform(4326) %>% 
    sf::st_coordinates()
  
  lon <- coords[1, "X"]
  lat <- coords[1, "Y"]
  
  zone_number <- floor((lon + 180) / 6) + 1
  base_code <- if (lat < 0) 32700 else 32600
  epsg_code <- unname(base_code + zone_number)
  
  return(epsg_code)
}

#' Agregar superficie en hectáreas
#'
#' Calcula el área de las geometrías y agrega una columna 'Sup_ha' con el valor en hectáreas.
#'
#' @param sf Objeto de clase `sf`.
#' @param field_name Nombre para asignarle al campo a crear.
#' @param digits Entero. Número de decimales para redondear. Por defecto 2.
#'
#' @return El objeto `sf` con la columna 'Sup_ha'.
#' @export
#' @examples
#' \dontrun{
#'  sf_poligonos <- sf::read_sf("ruta/al/shapefile.shp")
#'  st_add_sup_ha(sf_poligonos)
#'  st_add_sup_ha(sf_poligonos, digit = 0)
#'  st_add_sup_ha(sf_poligonos, field_name = "Superficie")
#' }
st_add_sup_ha <- function(sf, field_name = "Sup_ha", digits = 2) {
  if (!inherits(sf, "sf")) stop("El argumento 'sf' debe ser un objeto de clase 'sf'.")
  if (!is.numeric(digits) || length(digits) != 1) stop("El argumento 'digits' debe ser un valor numérico único.")
  if (!is.character(field_name) || length(field_name) != 1) stop("El argumento 'field_name' debe ser un string único.")

  sf %>% 
    dplyr::mutate(
      !!field_name := sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(digits)
    )
}

#' Agrupar geometrías por proximidad espacial
#'
#' Esta función agrupa las geometrías de un objeto `sf` que se encuentran dentro de una
#' distancia umbral especificada. Utiliza componentes conectados en un grafo de adyacencia.
#'
#' @param sf Objeto de clase `sf` o `sfc`.
#' @param distance Valor numérico. Distancia máxima en metros para considerar dos elementos como conectados.
#'
#' @return Un vector de enteros indicando el ID del grupo (membership) para cada geometría.
#' @export
#' @examples
#' \dontrun{
#'   sf_obj <- sf::read_sf("ruta/al/archivo.shp")
#'   # Agrupar geometrías que distan menos de 50 metros entre sí
#'   sf_obj$group <- group_by_distance(sf_obj, distance = 50)
#'   # Agrupar geometrias en base a una agrupación de atributos
#'   variables <- c("Grupo_1", "Grupo_2")
#'   sf_obj %>% 
#'    dplyr::group_by(!!dplyr::syms(variables)) %>% 
#'    dplyr::mutate(group = group_by_distance(geometry, distance = 50))
#' }
group_by_distance <- function(sf, distance){
  if (!inherits(sf, c("sf", "sfc"))) stop("El argumento 'sf' debe ser un objeto de clase 'sf' o 'sfc'.")
  if (!is.numeric(distance) || length(distance) != 1) stop("El argumento 'distance' debe ser un valor numérico único.")

  dist_matrix = sf::st_distance(sf, by_element = FALSE)
  class(dist_matrix) = NULL
  connected = dist_matrix <= distance
  g = igraph::graph_from_adjacency_matrix(connected)
  return(igraph::components(g)$membership)
}

#' @noRd
clean_geom <- function(sf) {
  valid_input(sf, inherit = "sf")
  sf %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON") %>%
    sf::st_cast("POLYGON") %>%
    dplyr::filter(
      janitor::round_half_up(as.numeric(sf::st_area(geometry)), 1) > 0
    ) %>% 
    suppressMessages() %>% 
    suppressWarnings()
}

#' Unión espacial completa sin superposiciones (Planarize)
#'
#' @description
#' Crea una cobertura completa de las geometrías de `x` e `y`, resolviendo superposiciones.
#' El resultado incluye:
#' 1. La intersección de `x` e `y`.
#' 2. La diferencia de `y` menos `x`.
#' 3. La diferencia de `x` menos `y`.
#'
#' Es útil para generar un mosaico de polígonos sin traslapes a partir de dos capas.
#'
#' @param x Objeto `sf` de polígonos.
#' @param y Objeto `sf` de polígonos.
#' @param overlap_rule Regla para determinar la prioridad en caso de superposición. Determina qué objeto conserva su área original: "first" (el primero), "last" (el último), "largest" (el de mayor área) o "smallest" (el de menor área).
#'
#' @return Un objeto `sf` de polígonos.
#'
#' @export
#' @examples
#' \dontrun{
#'   sf1 <- sf::read_sf("ruta/al/shapefile1.shp")
#'   sf2 <- sf::read_sf("ruta/al/shapefile2.shp")
#'   st_union_no_overlaps(sf1)
#'   st_union_no_overlaps(sf1, sf2)
#'   st_union_no_overlaps(sf1, sf2, overlap_rule = "largest")
#' }
st_union_no_overlaps <- function(
  x,
  y = NULL,
  overlap_rule = c("first", "last", "largest", "smallest")
) {
  valid_input(x, inherit = "sf")
  if (!is.null(y)) {
    valid_input(y, inherit = "sf")
  }
  overlap_rule <- match.arg(overlap_rule)

  sf::st_agr(x) <- "constant"

  # --- Caso: solo una capa ---
  if (is.null(y)) {
    if (nrow(x) <= 1) {
      return(x)
    }

    # Reordenar según la regla antes de cortar
    x <- switch(
      overlap_rule,
      first = x,
      last = x[rev(seq_len(nrow(x))), ],
      largest = x[order(sf::st_area(x), decreasing = TRUE), ],
      smallest = x[order(sf::st_area(x)), ]
    )

    # Cada feature se recorta quitando lo que ya fue "reclamado" por features anteriores
    result <- sf::st_as_sfc(sf::st_bbox(x))[0] # geometría vacía del mismo CRS
    for (i in seq_len(nrow(x))) {
      feat <- x[i, ]
      if (length(result) == 0 || !any(sf::st_intersects(feat, result, sparse = FALSE))) {
        result <- feat
      } else {
        result <- tryCatch({
          union_x <- sf::st_union(feat)
          union_y <- sf::st_union(result)

          part_intersect <- sf::st_intersection(feat, union_y) %>% clean_geom()
          part_x_only <- sf::st_difference(feat, union_y) %>% clean_geom()
          part_y_only <- sf::st_difference(result, union_x) %>% clean_geom()

          dplyr::bind_rows(part_intersect, part_x_only, part_y_only) %>% clean_geom()
        }, error = function(e) {
          warning("Conflictos en la union. ", e)
          return(dplyr::bind_rows(result, feat))
        })
      }
    }

    return(result)
  }
  # --- Caso: dos capas ---
  sf::st_agr(y) <- "constant"

  union_x <- sf::st_union(x)
  union_y <- sf::st_union(y)

  part_intersect <- sf::st_intersection(x, union_y) %>% mutate() %>% clean_geom() %>% 
    dplyr::mutate(overlap = TRUE)
  part_x_only <- sf::st_difference(x, union_y) %>% clean_geom()
  part_y_only <- sf::st_difference(y, union_x) %>% clean_geom()

  dplyr::bind_rows(part_intersect, part_x_only, part_y_only) %>%
    clean_geom()
}