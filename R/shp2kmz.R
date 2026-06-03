#' Convertir Shapefile a KMZ con atributos extendidos
#'
#' @description
#' Lee un archivo shapefile, lo proyecta a WGS84 y genera un archivo KMZ.
#' Mantiene los atributos originales en etiquetas `ExtendedData` permitiendo
#' su visualización en Google Earth. Permite además agrupar los elementos en carpetas.
#'
#' @param shp Ruta al archivo shapefile (.shp).
#' @param name Nombre de la columna para usar como etiqueta (Name). Si es NULL, usa la primera.
#' @param folder Columna opcional para agrupar en carpetas (Folder).
#' @param dirsave Directorio de salida. Por defecto, el mismo que el input.
#'
#' @return Genera un archivo .kmz y muestra un mensaje con la ruta.
#' @export
shp2kmz <- function(shp, name = NULL, folder = NULL, dirsave = NULL) {
  if (!file.exists(shp)) stop("El archivo shapefile no existe: ", shp)
  if (tolower(tools::file_ext(shp)) != "shp") stop("El archivo debe tener extensión .shp")
  if (!is.null(dirsave) && !dir.exists(dirsave)) {
    stop("El directorio de guardado no existe: ", dirsave)
  }

  # --- Normalizar argumentos: acepta con o sin comillas ---------------------
  name <- if (is.null(name)) {
    NULL  # se asigna después de leer el sf
  } else {
    as.character(dplyr::ensym(name))
  }

  folder <- if (is.null(folder)) {
    NULL
  } else {
    as.character(dplyr::ensym(folder))
  }

  # --- Leer shapefile -------------------------------------------------------
  df_sf <- sf::read_sf(shp) |> sf::st_transform(4326)

  # Si name es NULL, usar la primera columna (excluyendo geometría)
  if (is.null(name)) {
    name <- sf::st_drop_geometry(names(df_sf))[1]
    message("Argumento 'name' no especificado. Usando la primera columna: '", name, "'")
  }

  # --- Validar campos -------------------------------------------------------
  campos_faltantes <- setdiff(c(name, folder), names(df_sf))
  if (length(campos_faltantes) > 0) {
    stop("Campo(s) no encontrado(s) en el shapefile: ",
         paste(campos_faltantes, collapse = ", "))
  }

  # --- Construir bloque ExtendedData para una fila --------------------------
  make_extended_data <- function(fila) {
    columnas <- setdiff(names(fila), c("geometry", "geom"))
    campos_xml <- sapply(columnas, function(col) {
      paste0('<Data name="', col, '"><value>',
             htmltools::htmlEscape(as.character(sf::st_drop_geometry(fila)[[col]])),
             '</value></Data>')
    })
    paste0('<ExtendedData>', paste(campos_xml, collapse = ""), '</ExtendedData>')
  }

  # --- Agregar features a una carpeta KML -----------------------------------
  agregar_features <- function(folder_kml, sub_df) {
    tipo_geom <- unique(sf::st_geometry_type(sub_df))

    for (i in seq_len(nrow(sub_df))) {
      fila_i      <- sub_df[i, ]
      nombre_i    <- as.character(sf::st_drop_geometry(fila_i)[[name]])
      coords      <- sf::st_coordinates(fila_i)

      if (any(tipo_geom %in% c("POINT", "MULTIPOINT"))) {
        feat <- folder_kml$newpoint(name = nombre_i)
        # simplekml espera una tupla (lon, lat, alt)
        feat$coords <- list(c(coords[1, "X"], coords[1, "Y"], 0))

      } else if (any(tipo_geom %in% c("LINESTRING", "MULTILINESTRING"))) {
        feat <- folder_kml$newlinestring(name = nombre_i)
        coords_list <- split(cbind(coords[, "X"], coords[, "Y"]), seq(nrow(coords)))
        names(coords_list) <- NULL
        feat$coords <- coords_list

      } else {
        # POLYGON / MULTIPOLYGON
        feat <- folder_kml$newpolygon(name = nombre_i)
        coords_mat  <- coords[coords[, "L1"] == 1, 1:2]
        coords_list <- split(coords_mat, seq(nrow(coords_mat)))
        names(coords_list) <- NULL
        feat$outerboundaryis        <- coords_list
        feat$style$polystyle$color  <- "00ffffff"
      }

      feat$description            <- ""
      feat$style$linestyle$color  <- "ff0000ff"
      feat$style$linestyle$width  <- 1
      feat$style$labelstyle$scale <- 0
    }
  }

  # --- Recursiva para carpetas anidadas -------------------------------------
  crear_carpetas <- function(parent_kml, df, campos) {
    if (length(campos) == 0) {
      agregar_features(parent_kml, df)
      return(invisible(NULL))
    }
    for (val in unique(df[[campos[1]]])) {
      sub_df  <- df[df[[campos[1]]] == val, ]
      carpeta <- parent_kml$newfolder(name = as.character(val))
      crear_carpetas(carpeta, sub_df, campos[-1])
    }
  }

  # --- Crear KML ------------------------------------------------------------
  skml <- reticulate::import("simplekml")
  kml  <- skml$Kml()

  if (is.null(folder)) {
    agregar_features(kml, df_sf)
  } else {
    crear_carpetas(kml, df_sf, folder)
  }

  # --- Guardar KML temporal y parchear con ExtendedData --------------------
  tmp_dir <- normalizePath(tempdir(), winslash = "/")
  tmp_kml <- file.path(tmp_dir, paste0("doc_", as.integer(Sys.time()), ".kml"))
  # tmp_kml <- tempfile(fileext = ".kml")
  kml$save(tmp_kml)

  kml_texto <- paste(readLines(tmp_kml, encoding = "UTF-8"), collapse = "\n")

  # Inyectar ExtendedData en cada Placemark buscando por nombre del feature
  for (i in seq_len(nrow(df_sf))) {
    fila <- df_sf[i, ]
    nombre_feat <- htmltools::htmlEscape(
      as.character(sf::st_drop_geometry(fila)[[name]])
    )
    extended_block <- make_extended_data(fila)

    patron <- paste0('(<name>', nombre_feat, '</name>\\s*)<description/>')
    reemplazo <- paste0('\\1', extended_block, '<description/>')
    kml_texto <- gsub(patron, reemplazo, kml_texto, perl = TRUE)
  }

  writeLines(kml_texto, tmp_kml, useBytes = TRUE)

  # --- Reempaquetar como KMZ ------------------------------------------------
  nombre_kmz  <- paste0(tools::file_path_sans_ext(basename(shp)), ".kmz")
  dir_destino <- if (is.null(dirsave)) dirname(shp) else dirsave
  ruta_kmz    <- file.path(dir_destino, nombre_kmz)

  tmp_kmz <- file.path(tmp_dir, paste0("doc_", as.integer(Sys.time()), ".kmz"))

  zip::zipr(zipfile = tmp_kmz, files = tmp_kml)

  file.copy(from = tmp_kmz, to = ruta_kmz, overwrite = TRUE)

  unlink(c(tmp_kml, tmp_kmz))

  message("KMZ guardado en: ", ruta_kmz)
}

asign_name_folder_kmz <- function(shp) {
  if (!file.exists(shp)) {
    message("El archivo shapefile no existe: ", shp)
    return(invisible(NULL))
  }
  if (tolower(tools::file_ext(shp)) != "shp") {
    message("El archivo debe tener extensión .shp")
    return(invisible(NULL))
  }

  shp_sf <- sf::read_sf(shp)
  fields_shp <- names(sf::st_drop_geometry(shp_sf))
  basename_shp <- basename(shp)
  field_detect <- function(x, include_na = FALSE) {
    field <- fields_shp[stringi::stri_detect_regex(fields_shp, x, case_insensitive = TRUE)][1]
    if(include_na) {
      if(is.na(field)) return(NA_character_)
    } 
    if(is.na(field)) {
      return(fields_shp[1])
    } 
    return(field)
  }
  name <- dplyr::case_when(
    stringi::stri_detect_regex(basename_shp, case_insensitive = TRUE, "Cuenca.*estudio") ~ field_detect("NOM_SSUBC"),
    stringi::stri_detect_regex(basename_shp, case_insensitive = TRUE, "Area.*proyecto.*Ubicación") ~ field_detect("NOM_SSUBC"),
    stringi::stri_detect_regex(basename_shp, case_insensitive = TRUE, "Area.*proyecto.*Obras") ~ field_detect("Obra"),
    stringi::stri_detect_regex(basename_shp, case_insensitive = TRUE, "BNP_.*_Cuenca") ~ field_detect("BNP_ECC"),
    stringi::stri_detect_regex(basename_shp, case_insensitive = TRUE, "BNP_.*_a_") ~ field_detect("Obra"),
    stringi::stri_detect_regex(basename_shp, case_insensitive = TRUE, "Censo_.*_a_") ~ field_detect("Especie"),
    stringi::stri_detect_regex(basename_shp, case_insensitive = TRUE, "Uso_actual_de_la_tierra") ~ field_detect("Subuso"),
    stringi::stri_detect_regex(basename_shp, case_insensitive = TRUE, "Vegetación_en_la_cuenca") ~ field_detect("Formacion"),
    stringi::stri_detect_regex(basename_shp, case_insensitive = TRUE, "Estimación") ~ field_detect("Obra"),
    stringi::stri_detect_regex(basename_shp, case_insensitive = TRUE, "Caminos_cuenca") ~ field_detect("ROL_LABEL"),
    stringi::stri_detect_regex(basename_shp, case_insensitive = TRUE, "Hidrografía_cuenca") ~ field_detect("Tipo"),
    stringi::stri_detect_regex(basename_shp, case_insensitive = TRUE, "Curvas_de_Nivel_cuenca") ~ field_detect("Elevacion"),
    stringi::stri_detect_regex(basename_shp, case_insensitive = TRUE, "UTM_Inventarios|COT") ~ field_detect("Parcela"),
    stringi::stri_detect_regex(basename_shp, case_insensitive = TRUE, "UTM_Registros") ~ field_detect("Especie"),
    stringi::stri_detect_regex(basename_shp, case_insensitive = TRUE, "Sitio_Medida") ~ field_detect("MEDIDA"),
    .default = fields_shp[1]
  )
  name <- if(is.na(name)) NULL else name

  folder <- dplyr::case_when(
    stringi::stri_detect_regex(basename_shp, "Uso.*actual.*tierra") ~ field_detect("Uso", include_na = TRUE),
    stringi::stri_detect_regex(basename_shp, "Vegetación.*cuenca") ~ field_detect("Formacion", include_na = TRUE),
    .default = NA_character_
  )
  folder <- if(is.na(folder)) NULL else folder

  return(list(shp = shp, name = name, folder = folder))
}

