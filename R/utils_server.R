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

#' @noRd
check_input <- function(x, rv = NULL, names_req, id_reset = NULL, geometry = "POLYGON"){
  ok <- T
  x <- if(is.null(rv)) x else rv[[x]]
  reset <- F
  if (inherits(x, "sf")) {
    if (!all(any(grepl(geometry, sf::st_geometry_type(x))))) {
      shinybusy::report_failure(
        title = "Ups!",
        text = sprintf("Ingresar shapefile de tipo %s", geometry)
      )
      if (!is.null(id_reset)) {
        reset <- T
      }
      ok <- F
    }
  }
  if(!all(names_req %in% names(sf::st_drop_geometry(x)))) {
    shinybusy::report_failure(
      title = "Ups!",
      text = tags$p(
        "Shapefile sin los campos requeridos", rep_br(2),
        tags$b("Requeridos: "), paste(names_req %>% shQuote(), collapse = ", "), rep_br(2),
        tags$b("Faltan: "), paste(setdiff(names_req, names(sf::st_drop_geometry(x))) %>% shQuote(), collapse = ", ")
      )
    )
    if (!is.null(id_reset)) {
      reset <- T
    }
    ok <- F
  }

  if (ok) {
    shinybusy::notify_success("Perfecto! Todos los campos necesarios :)", timeout = 3000, position = "right-bottom")
  } else {
    x <- NULL
    if (reset) shinyjs::reset(id = id_reset)
  }
  return(ok)
}

#' @noRd
valid_input <- function(..., inherit = NULL, names = NULL, geometry = NULL){
  x <- list(...)
  arg_x <- as.character(substitute(list(...)))[-1]
  class_x <- lapply(x, class)
  if (!is.null(inherit)) {
    if (!all(sapply(x, inherits, inherit))) {
      args_e <- arg_x[grep(paste(inherit, collapse = "|"), class_x, invert = T)]
      stop(
        paste(
          ngettext(length(args_e), "El argumento", "Los argumentos"),
          paste(sQuote(args_e), collapse = ", "),
          ngettext(length(inherit), "debe ser de clase", "deben ser de una de las clases"),
          paste(sQuote(inherit), collapse = ", ")
        ),
        call. = F
      )
    }
  }
  if (!is.null(geometry) & all(sapply(x, inherits, "sf"))) {
    geometry <- match.arg(geometry, choices = c("POLYGON", "POINT", "LINESTRING"))
    if (!all(purrr::map_lgl(x, ~ any(grepl(geometry, sf::st_geometry_type(.)))))) {
      args_e <- arg_x[which(!purrr::map_lgl(x, ~ any(grepl(geometry, sf::st_geometry_type(.)))))]
      stop(
        paste(
          ngettext(length(args_e), "El objeto", "Los objetos"),
          paste(sQuote(args_e), collapse = ", "),
          ngettext(length(inherit), "debe", "deben"),
          sprintf("ser de tipo %s", geometry)
        ),
        call. = F
      )
    }
  }
  if (!is.null(names) & length(x) == 1 & inherits(x[[1]], "data.frame")) {
    if (!all(names %in% names(x[[1]]))) {
      stop(
        paste(
          "El argumento", sQuote(arg_x), "debe contener al menos",
          ngettext(length(names), "el campo:", "los campos:"),
          paste(sQuote(names), collapse = ", ")
        ),
        call. = F
      )
    }
  }
  invisible()
}

#' @noRd
valid_dem <- function(dem){
  arg_dem <- substitute(dem)
  if (!inherits(dem, c("NULL", "character", "stars"))) {
    stop("DEM debe ser un objeto 'stars' o bien la ruta del archivo raster", call. = F)
  }
  if (inherits(dem, "character")) {
    if (length(dem) > 1) {
      stop(paste("Argumento", sQuote(arg_dem), "debe ser solo una ruta"), call. = F)
    }
    if (!file.exists(dem)) {
      stop(paste("La ruta del archivon ingresada para el argumento", sQuote(arg_dem), "no existe."), call. = F)
    }
    if(!tools::file_ext(dem) %in% c("tif", "tiff")) {
      stop(sprintf(
        "Extensión no válida para el argumento %s. Se permite: %s",
        shQuote(arg_dem),
        paste(c(".tif", ".tiff"), collapse = ", ")
      ), call. = F)
    }
  }
  invisible()
}

#' @noRd
valid_df <- function(df, names = NULL){
  arg_df <- substitute(df)
  if (!inherits(df, c("character", "data.frame")) | inherits(df, "sf")) {
    stop(paste("Argumento", sQuote(arg_df), "debe ser un objeto 'data.frame' o bien la ruta del archivo xlsx"), call. = F)
  }
  if (inherits(df, "data.frame")) {
    if (nrow(df) == 0) {
      stop(paste("Argumento", sQuote(arg_df), "no contiene datos."))
    }
    if (!is.null(names) & !all(names %in% names(df))) {
      diff <- setdiff(names, names(df))
      stop(
        paste(
          "El argumento", sQuote(arg_df), "debe contener al menos",
          ngettext(length(names), "el campo:", "los campos:"),
          paste0(paste(sQuote(names), collapse = ", "),"\n",
                 ngettext(length(diff), "Falta:", "Faltan:")),
          paste(sQuote(diff), collapse = ", ") %>% split_tail(",") %>% paste(collapse = " y ")
        ),
        call. = F
      )
    }
  }
  if (inherits(df, "character")) {
    if (length(df) > 1) {
      stop(paste("Argumento", sQuote(arg_df), "debe ser solo una ruta"), call. = F)
    }
    if (!file.exists(df)) {
      stop(paste("La ruta del archivon ingresada para el argumento", sQuote(arg_df), "no existe."), call. = F)
    }
    if(!tools::file_ext(df) %in% c("xlsx")) {
      stop(sprintf(
        "Extensión no válida para el argumento %s. ingresar un archivo '.xlsx'",
        shQuote(arg_df)
      ))
    }
  }
  invisible()
}

#' @noRd
get_densidad <- function(BD_fore, sp){
  BD_fore %>%
    dplyr::group_by(Parcela, Sup_parcela, UTM_E, UTM_N, Especie) %>%
    dplyr::summarise(n = sum(N_ind, na.rm = T), .groups = "drop") %>%
    dplyr::mutate(n = (n * 10000 / Sup_parcela)) %>%
    tidyr::pivot_wider(names_from = Especie, values_from = n) %>%
    dplyr::mutate(Nha_Total = rowSums(dplyr::select(., -c(1:4)), na.rm = TRUE)) %>%
    dplyr::mutate_at(-c(1:4), ~ ifelse(is.na(.), 0, .)) %>%
    dplyr::pull(sp) %>% mean() %>% janitor::round_half_up()
}

#' @noRd
bind_events <- function(
  ids, 
  rv, 
  parent_input
){
  lapply(
    ids, function(x){
      observeEvent(parent_input[[x]], {
        rv[[x]] <- parent_input[[x]]
      })
    }
  )
}
