#' Formatear precisión de la superficie en un Shapefile
#'
#' @description
#' Esta función calcula el área en hectáreas para una capa de polígonos, actualiza
#' el campo especificado (por defecto "Sup_ha") y modifica binariamente los metadatos
#' del archivo .dbf asociado para asegurar que la precisión decimal se mantenga
#' y sea visualizada correctamente en herramientas externas como Google Earth, QGIS o ArcGIS.
#'
#' @param shp Carácter. Ruta completa al archivo shapefile (.shp).
#' @param field_sup Carácter. Nombre del campo que contiene la superficie. Por defecto "Sup_ha".
#' @param precision Entero. Número de decimales deseados para el redondeo y metadatos. Por defecto 2.
#'
#' @return Invisibilidad de un valor lógico (TRUE si el proceso fue exitoso, FALSE en caso contrario).
#'
#' @importFrom sf read_sf st_zm st_geometry_type st_area st_write
#' @importFrom janitor round_half_up
#' @importFrom tools file_ext
#'
#' @export
format_sup_ha <- function(shp, field_sup = "Sup_ha", precision = 2L) {
  if (!file.exists(shp)) {
    message("Error: El archivo no existe en la ruta especificada: ", shp)
    return(invisible(FALSE))
  }
  if (tolower(tools::file_ext(shp)) != "shp") {
    message("Error: El archivo proporcionado no tiene extensión .shp")
    return(invisible(FALSE))
  }
  if (!is.character(field_sup) || length(field_sup) != 1) {
    message("Error: 'field_sup' debe ser un string de longitud 1.")
    return(invisible(FALSE))
  }
  if (!is.numeric(precision) || precision < 0) {
    message("Error: 'precision' debe ser un entero no negativo.")
    return(invisible(FALSE))
  }

  capa <- tryCatch(
    sf::read_sf(shp, quiet = TRUE) %>% sf::st_zm(),
    error = function(e) {
      message("Error: No se pudo leer el shapefile.\n  Error: ", e$message)
      return(invisible(FALSE))
    }
  )
  tipos_geom <- unique(sf::st_geometry_type(capa))
  if (!any(tipos_geom %in% c("POLYGON", "MULTIPOLYGON"))) {
    message(
      "El shapefile no es de polígonos. Tipo(s) detectado(s): ",
      paste(tipos_geom, collapse = ", ")
    )
    return(invisible(FALSE))  
  }
  if (!field_sup %in% names(capa)) {
    message(sprintf("El shapefile no contiene el campo '%s'", field_sup))
    return(invisible(FALSE))
  }

  capa[[field_sup]] <- janitor::round_half_up(as.numeric(sf::st_area(capa)) / 10000, precision)
  sf::write_sf(capa, shp, delete_dsn = TRUE, quiet = TRUE)

  dbf <- sub("\\.shp$", ".dbf", shp)
  if (!file.exists(dbf)) {
    message("Error: No se encontró el archivo .dbf asociado al shapefile.")
    return(invisible(FALSE))
  }

  raw_data <- readBin(dbf, "raw", n = file.info(dbf)$size)
  patron <- charToRaw(field_sup)
  n       <- length(raw_data)
  np      <- length(patron)

  pos <- NA
  for (i in seq_len(n - np + 1)) {
    if (all(raw_data[i:(i + np - 1)] == patron)) {
      pos <- i
      break
    }
  }
  if (is.na(pos)) stop(sprintf("Campo '%s' no encontrado en el .dbf", field_sup))

  raw_data[pos + 16] <- as.raw(24)
  raw_data[pos + 17] <- as.raw(precision)

  con <- file(dbf, "wb")
  writeBin(raw_data, con)
  close(con)
  return(invisible(TRUE))
}