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
check_input <- function(x, rv = NULL, names_req, id_reset = NULL, geometry = "POLYGON"){
  ok <- TRUE
  x <- if(is.null(rv)) x else rv[[x]]
  reset <- FALSE
  if (inherits(x, "sf")) {
    if (!all(any(grepl(geometry, sf::st_geometry_type(x))))) {
      shinybusy::report_failure(
        title = "Ups!",
        text = sprintf("Ingresar shapefile de tipo %s", geometry)
      )
      if (!is.null(id_reset)) {
        reset <- TRUE
      }
      ok <- FALSE
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
      reset <- TRUE
    }
    ok <- FALSE
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

#' Obtener densidad media de una especie
#'
#' @param BD_fore Dataframe con datos de inventario forestal.
#' @param sp Nombre de la especie objetivo.
#'
#' @return Densidad media (N/ha) redondeada.
get_densidad <- function(BD_fore, sp){
  valid_input(BD_fore, inherit = "data.frame", names = req_names$BD_fore)
  
  if (nrow(BD_fore) == 0) {
    stop("Error: La base de datos de inventario forestal no contiene registros para procesar.", call. = FALSE)
  }

  if (!sp %in% unique(BD_fore$Especie)) {
    stop(sprintf("Error: La especie '%s' no se encuentra en los registros de la base de datos de inventarios.", sp), call. = FALSE)
  }

  tryCatch({
    BD_fore %>%
      dplyr::group_by(Parcela, Sup_parcela, UTM_E, UTM_N, Especie) %>%
      dplyr::summarise(n = sum(N_ind, na.rm = T), .groups = "drop") %>%
      dplyr::mutate(n = (n * 10000 / Sup_parcela)) %>%
      tidyr::pivot_wider(names_from = Especie, values_from = n) %>%
      dplyr::mutate(Nha_Total = rowSums(dplyr::select(., -c(1:4)), na.rm = TRUE)) %>%
      dplyr::mutate_at(-c(1:4), ~ ifelse(is.na(.), 0, .)) %>%
      dplyr::pull(sp) %>% mean() %>% janitor::round_half_up()
  }, error = function(e) {
    stop(paste("Fallo interno al calcular la densidad:", e$message), call. = FALSE)
  })
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

#' Preparar y estandarizar datos de Uso y Vegetación
#'
#' @param x Objeto sf con la cartografía de uso y vegetacion de la cuenca. Tambien util para darle formato a las capas de BNP en la cuenca y de intervenir y alterar.
#'
#' @return Objeto sf con columnas renombradas y superficie calculada.
prepare_uso_veg <- function(x) {
  valid_input(x, inherit = "sf", geometry = "POLYGON")
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
#' @param create_kmz valor lógico. \code{TRUE} para crear un kmz cuando \code{x} contenga un objeto sf.  
#' @param csv valor lógico. \code{TRUE} para descargar csv cuando \code{x} sea un data.frame. 
#'
#' @returns descarga tablas, workbooks y objetos espaciales archivos como 'xlsx', 'csv' o 'zip'.
#' @name download_files
#' @export
download_files <- function(x, name_save, dir_save, create_kmz = FALSE, csv = FALSE) {
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
    paste0(as.character(name_save), ifelse(filetype == "sf", ".zip", ifelse(csv, ".csv", ".xlsx")))
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
        sf = {
          sf::write_sf(x, paste0(tools::file_path_sans_ext(z), ".shp"))
          format_sup_ha(paste0(tools::file_path_sans_ext(z), ".shp"))
          if(create_kmz) {
            asign_name_folder_kmz(paste0(tools::file_path_sans_ext(z), ".shp")) %>% do.call("shp2kmz", .)
          }
        },
        wb = openxlsx2::wb_save(x, paste0(tools::file_path_sans_ext(z), ".xlsx"), overwrite = T),
        xlsx = {
          if(csv) {
            readr::write_excel_csv2(x, paste0(tools::file_path_sans_ext(z), ".csv"))
          } else {
            openxlsx2::write_xlsx(x, paste0(tools::file_path_sans_ext(z), ".xlsx"))
          }
        }
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

#' Crear tabla HTML con los atributos de un data.frame
#'
#' Genera un vector de strings HTML, donde cada elemento corresponde a una
#' tabla con los atributos de cada fila del data.frame de entrada. Es útil
#' para crear popups en mapas de \code{leaflet} a partir de objetos \code{sf}.
#'
#' @param fila Un \code{data.frame} o \code{tibble} con los atributos a mostrar.
#'   Las columnas llamadas \code{"geometry"}, \code{"geom"} y \code{"Name"} son
#'   excluidas automáticamente. Se recomienda usar \code{sf::st_drop_geometry()}
#'   antes de llamar a esta función si el objeto es de clase \code{sf}.
#'
#' @return Un vector de caracteres de largo \code{nrow(fila)}, donde cada
#'   elemento es una tabla HTML con los atributos de la fila correspondiente.
#'
#' @examples
#' \dontrun{
#' # Uso dentro de mutate para crear popups en leaflet
#' objeto_sf <- sf::read_sf("ruta/al/archivo.shp")
#' # Mostrar todos los atributos
#' objeto_sf %>% 
#'   sf::st_transform(4326) %>% 
#'   dplyr::mutate(popup = tabla_popup(.))
#' # Seleccionar los atributos que mostrar en la tabla html
#' objeto_sf %>% 
#'   sf::st_transform(4326) %>% 
#'   dplyr::mutate(
#'     # Ejemplos de como poder seleccionar columnas
#'     popup = tabla_popup(., cols = c(1:4)), # ej 1. Seleccionar primeras 4 columnas  
#'     popup = tabla_popup(., cols = c("Obra", "Nom_obra", "Sup_ha")), # ej 2. Declarar nombres de los campos
#'     popup = tabla_popup(., cols = dplyr::contains("Obra")), # ej 3. Seleccionar campos que contienen la palabra 'Obra'
#'     popup = tabla_popup(., cols = c(1:2, dplyr::starts_with("Obra"), "Sup_ha")), # ej 4. Combinar formas
#'   )
#' }
#'
#' @export
tabla_popup <- function(fila, cols = dplyr::everything()) {
  if (nrow(fila) == 0) {
    stop(
      "El argumento 'fila' no contiene filas.",
      call. = FALSE
    )
  }
  if (ncol(fila) == 0) {
    stop(
      "El argumento 'fila' no contiene columnas.",
      call. = FALSE
    )
  }
  columnas <- names(sf::st_drop_geometry(fila) %>% dplyr::select(cols))
  
  purrr::pmap_chr(sf::st_drop_geometry(fila[columnas]), function(...) {
    vals <- list(...)
    filas_html <- mapply(function(col, val) {
      paste0("<tr><td bgcolor='#EEEEEE'><b>", col, "</b></td><td>", val, "</td></tr>")
    }, columnas, vals, SIMPLIFY = FALSE)
    
    paste0(
      "<table border='1' style='border-collapse: collapse; width: 100%; font-family: Arial;'>",
      paste(filas_html, collapse = ""),
      "</table>"
    )
  })
}

#' @noRd
get_volumes <- function(exclude = NULL) {
  osSystem <- Sys.info()["sysname"]
  if (osSystem == "Darwin") {
    volumes <- list.dirs("/Volumes")
    names(volumes) <- basename(volumes)
  } else if (osSystem == "Linux") {
    volumes <- c(Computer = "/")
    if (isTRUE(dir_exists("/media"))) {
      media <- list.dirs("/media")
      names(media) <- basename(media)
      volumes <- c(volumes, media)
    }
  } else if (osSystem == "Windows") {
    wmic <- paste0(Sys.getenv("SystemRoot"), "\\System32\\Wbem\\WMIC.exe")
    if (!file.exists(wmic)) {
      volumes_info <- system2(
        "powershell",
        "$dvr=[System.IO.DriveInfo]::GetDrives();Write-Output $dvr.length $dvr.name $dvr.VolumeLabel;",
        stdout = TRUE
      )
      num = as.integer(volumes_info[1])
      if (num == 0) {
        return(NULL)
      }
      mat <- matrix(volumes_info[-1], nrow = num, ncol = 2)
      mat[, 1] <- gsub(":\\\\$", ":/", mat[, 1])
      sel <- mat[, 2] == ""
      mat[sel, 2] <- mat[sel, 1]
      volumes <- mat[, 1]
      volNames <- mat[, 2]
      volNames <- paste0(volNames, " (", gsub(":/$", ":", volumes), ")")
    } else {
      volumes <- system(
        paste(wmic, "logicaldisk get Caption"),
        intern = TRUE,
        ignore.stderr = TRUE
      )
      volumes <- sub(" *\\r$", "", volumes)
      keep <- !tolower(volumes) %in% c("caption", "")
      volumes <- volumes[keep]
      volNames <- system(
        paste(wmic, "/FAILFAST:1000 logicaldisk get VolumeName"),
        intern = TRUE,
        ignore.stderr = TRUE
      )
      volNames <- sub(" *\\r$", "", volNames)
      volNames <- volNames[keep]
      volNames <- paste0(volNames, ifelse(volNames == "", "", " "))
      volNames <- paste0(volNames, "(", volumes, ")")
    }
    names(volumes) <- volNames
    volumes <- gsub(":$", ":/", volumes)
  } else {
    stop("unsupported OS")
  }
  if (!is.null(exclude)) {
    volumes <- volumes[!names(volumes) %in% exclude]
  }
  return(volumes)
}

#' @noRd
path_ext <- function(x) {
  pos <- regexpr("(\\.shp)?\\.[[:alnum:]]+$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

#' @noRd
path_sans_ext <- function(x) {
  sub("(\\.shp)?\\.[[:alnum:]]+$", "", x)
}

#' Agregar un prefijo al nombre de un archivo
#'
#' @param path Caracter. Ruta completa o relativa al archivo que se desea renombrar.
#' @param prefijo Caracter. Texto que se antepondrá al nombre base del archivo.
#'
#' @return invisible(logical) TRUE si el archivo fue renombrado con éxito, FALSE si falló o no existe.
#' @noRd
agregar_prefijo <- function(path, prefijo, sep = "_") {
  if (!file.exists(path)) {
    warning("La ruta ingresada no existe: ", path)
    return(invisible(FALSE))
  }

  if (!is.character(prefijo) || length(prefijo) != 1 || is.na(prefijo)) {
    stop("El prefijo debe ser un caracter (string) válido de longitud 1.")
  }

  if (!is.character(sep) || length(sep) != 1 || is.na(sep)) {
    stop("El separador 'sep' debe ser un valor numérico de longitud 1.")
  }

  dir_part  <- dirname(path)
  filename  <- basename(path)  
  
  ext  <- path_ext(filename)
  base <- path_sans_ext(filename)
  
  if (nzchar(ext)) {
    new_name <- sprintf("%s%s%s.%s", refijo, sep, base, ext)
  } else {
    new_name <- sprintf("%s%s%s%s", refijo, sep, base, ext)
  }
  
  res <- file.rename(from = path, to = file.path(dir_part, new_name))
  return(invisible(res))
}

#' Agregar un sufijo al nombre de un archivo
#'
#' @param path Caracter. Ruta completa o relativa al archivo que se desea renombrar.
#' @param sufijo Caracter. Texto que se añadirá al final del nombre base del archivo.
#'
#' @return invisible(logical) TRUE si el archivo fue renombrado con éxito, FALSE si falló o no existe.
#' @noRd
agregar_sufijo <- function(path, sufijo, sep = "_") {
  if (!file.exists(path)) {
    warning("La ruta ingresada no existe: ", path)
    return(invisible(FALSE))
  }

  if (!is.character(sufijo) || length(sufijo) != 1 || is.na(sufijo)) {
    stop("El sufijo debe ser un caracter (string) válido de longitud 1.")
  }

  if (!is.character(sep) || length(sep) != 1 || is.na(sep)) {
    stop("El separador 'sep' debe ser un valor numérico de longitud 1.")
  }

  dir_part  <- dirname(path)
  filename  <- basename(path)  
  
  ext  <- path_ext(filename)
  base <- path_sans_ext(filename)
  
  if (nzchar(ext)) {
    new_name <- sprintf("%s%s%s.%s", base, sep, sufijo, ext)
  } else {
    new_name <- sprintf("%s%s%s%s", base, sep, sufijo, ext)
  }
  
  res <- file.rename(from = path, to = file.path(dir_part, new_name))
  return(invisible(res))
}
