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

#' @noRd
prepare_uso_veg <- function(x) {
  x %>%
    dplyr::rename_all(~ ifelse(
      . == "geometry", ., 
      stringi::stri_trans_totitle(stringi::stri_trans_general(., "Latin-ASCII"), type = "sentence")
    )) %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains("ecc")), stringi::stri_trans_toupper) %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^uso.*tierra", case_insensitive = T), ~ "Uso") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^sub.*uso", case_insensitive = T), ~ "Subuso") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^tipo.*for", case_insensitive = T), ~ "Tipo_for") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^sub.*tipo.*fo", case_insensitive = T), ~ "Subtipo_fo") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("ley.*20283", case_insensitive = T), ~ "F_ley20283") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("est.*censo|censado", case_insensitive = T), ~ "Censado") %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("ECC")), tidyr::replace_na, "-") %>%
    dplyr::mutate_if(is.character, stringi::stri_trans_general, "Latin-ASCII") %>% 
    {if(names(.) %>% stringi::stri_detect_regex("^ECC") %>% any()) {
      .[] %>%
        dplyr::mutate(
          BNP_ECC = purrr::pmap_chr(
            dplyr::select(sf::st_drop_geometry(.), dplyr::starts_with("ECC")),
            function(...) {
              ecc <- list(...)
              if (all(ecc == "-")) {
                "-"
              } else if (any(ecc != "-")) {
                ecc %>% subset(. != "-") %>% unique() %>% paste(collapse = " - ")
              }
            }
          )
      )
    } else .} %>%
    dplyr::mutate(Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2))
}

#' download_files
#'
#' @param x objecto o lista de objetos. admite objetos 'data.frame', 'sf' y 'wbWorkbook'.
#' @param name_save vector de caracteres con los nombres de los objetos.
#' @param dir_save directorio donde guardar el o los objetos.  
#'
#' @returns archivos 'xlsx', 'shp' o 'zip'.
#' @name download_files
#' @export
download_files <- function(x, name_save, dir_save) {
  stopifnot(dir.exists(dir_save))
  
  filetype <- x %>%
    {if(any(class(.) == "list")) . else list(.)} %>%
    purrr::map( ~ ifelse(
      inherits(., "wbWorkbook"),
      "wb",
      ifelse(
        inherits(., "sf"),
        "sf",
        ifelse(inherits(., "data.frame") & !inherits(., "sf"), "xlsx", "")
      )
    )) %>%
    {if(length(.) == 1) unlist(.) else .}
  
  stopifnot("Solo se admiten objetos de tipo 'data.frame', 'sf' y 'wbWorkbook'" = all(filetype %in% c("sf", "wb", "xlsx")))
  
  file <- file.path(dir_save, ifelse(
    length(filetype) > 1, 
    ifelse(is.null(names(name_save)), "Archivos_comprimidos.zip", paste0(names(name_save), ".zip")),
    paste0(as.character(name_save), ifelse(filetype == "sf", ".zip", ".xlsx"))
  ))
  
  wd <- getwd()
  temp_dir <- tempdir()
  setwd(temp_dir)
  file.remove(list.files(pattern = "\\."))
  purrr::pwalk(
    if(length(filetype) == 1) {
      list(list(x), list(filetype), ifelse(inherits(name_save, "list"), name_save, list(name_save)))
    } else {
      list(x, unname(unlist(filetype)), unname(unlist(name_save)))
    },
    .f = function(x, y, z) {
      switch(
        y,
        sf = sf::write_sf(x, paste0(tools::file_path_sans_ext(z), ".shp")),
        wb = openxlsx2::wb_save(x, paste0(tools::file_path_sans_ext(z), ".xlsx"), overwrite = T),
        xlsx = openxlsx2::write_xlsx(x, paste0(tools::file_path_sans_ext(z), ".xlsx"), overwrite = T)
      )
    }
  )
  list_files <- unname(unlist(map(unlist(name_save), function(x) {list.files(pattern = x)})))
  if(tools::file_ext(file) == "zip") {
    zip::zip(zipfile = file, files = list_files)
  } else {
    file.copy(from = list_files, to = file, overwrite = T)
  }
  setwd(wd)
}
