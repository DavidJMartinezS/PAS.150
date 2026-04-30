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
#' @param x Objeto sf con la cartografía de uso.
#'
#' @return Objeto sf con columnas renombradas y superficie calculada.
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
          if(create_kmz) {
            sf::write_sf(prepare_kml(x = x, basename = z), paste0(tools::file_path_sans_ext(z), ".kml"), delete_dsn = TRUE)
            zip::zip(zipfile = paste0(tools::file_path_sans_ext(z), ".kmz"), files = paste0(tools::file_path_sans_ext(z), ".kml"))
            file.remove(paste0(tools::file_path_sans_ext(z), ".kml"))
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

#' @noRd
prepare_kml <- function(x, basename = NULL) {
  name_str <- if (is.null(basename)) {
    names(x)[1]
  } else {
    dplyr::case_when(
      stringi::stri_detect_regex(basename, "Cuenca_de_estudio") ~ "NOM_SSUBC",
      stringi::stri_detect_regex(basename, "Area_de_proyecto_Ubicación") ~ "NOM_SSUBC",
      stringi::stri_detect_regex(basename, "Area_de_proyecto_Obras") ~ "Obra",
      stringi::stri_detect_regex(basename, "BNP_.*_Cuenca") ~ "BNP_ECC",
      stringi::stri_detect_regex(basename, "BNP_.*_a_") ~ "Obra",
      stringi::stri_detect_regex(basename, "Censo_.*_a_") ~ "Especie",
      stringi::stri_detect_regex(basename, "Uso_actual_de_la_tierra") ~ "Subuso",
      stringi::stri_detect_regex(basename, "Vegetación_en_la_cuenca") ~ "Formacion",
      stringi::stri_detect_regex(basename, "Estimación") ~ "Obra",
      stringi::stri_detect_regex(basename, "Caminos_cuenca") ~ "ROL_LABEL",
      stringi::stri_detect_regex(basename, "Hidrografía_cuenca") ~ "Tipo",
      stringi::stri_detect_regex(basename, "Curvas_de_Nivel_cuenca") ~ "Elevacion",
      stringi::stri_detect_regex(basename, "UTM_Inventarios|COT") ~ "Parcela",
      stringi::stri_detect_regex(basename, "UTM_Registros") ~ "Especie",
      .default = names(x)[1]
    )
  }

  name <- dplyr::sym(name_str)

  shp_kml <- x %>% 
    dplyr::mutate(
      Name = !!name,
      Description = tabla_kml(sf::st_drop_geometry(.)) 
    ) %>% 
    dplyr::select(Name, Description)
  return(shp_kml)
}

#' @noRd
tabla_kml <- function(fila) {
  columnas <- setdiff(names(fila), c("geometry", "geom", "Name"))
  filas_html <- lapply(columnas, function(col) {
    paste0("<tr><td bgcolor='#EEEEEE'><b>", col, "</b></td><td>", fila[[col]], "</td></tr>")
  })
  paste0(
    "<table border='1' style='border-collapse: collapse; width: 100%; font-family: Arial;'>",
    paste(filas_html, collapse = ""),
    "</table>"
  )
}

#' @noRd
create_kmz <- function(x) {
  read_sf(x) %>% prepare_kml() %>% 
    sf::write_sf(file.path(directorio(), paste0(tools::file_path_sans_ext(x), ".kml")))
  zip::zip(
    zipfile = file.path(directorio, paste0(tools::file_path_sans_ext(x), ".kmz")), 
    files = file.path(directorio, paste0(tools::file_path_sans_ext(x), ".kml"))
  )
  file.remove(file.path(directorio, paste0(tools::file_path_sans_ext(x), ".kml")))

}
# kmz_usoveg <- function(shp, dirsave = NULL) {
#   if(!file.exists(shp) && tools::file_ext(shp) == "shp") stop("")
#   if(!is.null(dirsave) && !dir.exists(dirsave)) {
#     stop("")
#   }
#   tabla_kml <- function(fila) {
#     # Extraer nombres de columnas (excepto geometría y la propia descripción)
#     columnas <- setdiff(names(fila), c("geometry", "geom", "Name"))
    
#     # Crear filas de la tabla
#     filas_html <- lapply(columnas, function(col) {
#       paste0("<tr><td bgcolor='#EEEEEE'><b>", col, "</b></td><td>", fila[[col]], "</td></tr>")
#     })
    
#     paste0("<table border='1' style='border-collapse: collapse; width: 100%; font-family: Arial;'>",
#           paste(filas_html, collapse = ""),
#           "</table>")
#   }
#   name <- dplyr::case_when(
#     stringi::stri_detect_regex(basename(shp), "Uso_actual_de_la_tierra") ~ "Subuso",
#     stringi::stri_detect_regex(basename(shp), "Vegetación_en_la_cuenca") ~ "Formacion"
#   ) %>% dplyr::sym()
#   file <- dplyr::case_when(
#     stringi::stri_detect_regex(basename(shp), "Uso_actual_de_la_tierra") ~ "Uso",
#     stringi::stri_detect_regex(basename(shp), "Vegetación_en_la_cuenca") ~ "Formacion"
#   ) %>% dplyr::sym()

#   df_sf <- sf::read_sf(shp) %>% sf::st_transform(4326)
#   skml <- reticulate::import("simplekml")
#   kml <- skml$Kml()
#   files <- unique(df_sf %>% dplyr::pull(!!file))

#   for (u in files) {
#     folder <- kml$newfolder(name = as.character(u))
#     sub_df <- df_sf %>% dplyr::filter(!!file == u)
    
#     for (i in 1:nrow(sub_df)) {
#       fila_i <- sub_df[i,]
#       # Extraer coordenadas del anillo exterior
#       coords <- sf::st_coordinates(fila_i)
#       # Nos aseguramos de extraer solo X e Y de la primera parte del polígono
#       coords_mat <- coords[coords[,"L1"] == 1, 1:2]
      
#       # CONVERSIÓN CRUCIAL: 
#       # Convertimos la matriz en una lista de listas. 
#       # Esto es lo que reticulate traduce a Python como "List of Lists",
#       # que es exactamente lo que espera simplekml.
#       coords_list <- split(coords_mat, seq(nrow(coords_mat)))
#       names(coords_list) <- NULL # Quitamos los nombres de la lista para que sea anónima
      
#       # Crear el polígono
#       pol <- folder$newpolygon(name = as.character(sf::st_drop_geometry(sub_df[i, as.character(name)])))

#       # Asignar coordenadas 
#       pol$outerboundaryis <- coords_list

#       # Descripción HTML
#       pol$description <- tabla_kml(fila_i)
      
#       # Estilos (Borde rojo, relleno transparente)
#       pol$style$linestyle$color <- "ff0000ff" 
#       pol$style$linestyle$width <- 1
#       pol$style$polystyle$color <- "00ffffff" 
#       pol$style$labelstyle$scale <- 0
#     }
#   }

#   if (is.null(dirsave)) {
#     kml$savekmz(paste0(tools::file_path_sans_ext(shp), ".kmz"))
#     print(paste0("poligono guardado en ", sQuote(dirname(tools::file_path_as_absolute(shp)))))
#   } else {
#     kml$savekmz(file.path(normalizePath(dirsave), paste0(tools::file_path_sans_ext(basename(shp)), ".kmz")))
#     print(paste0("poligono guardado en ", sQuote(tools::file_path_as_absolute(dirsave))))
#   }
# }

