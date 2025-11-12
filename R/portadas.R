#' Portadas de apendices
#'
#' @param wb Workbook.
#' @param apendice apendices correspondiente. `inventarios`, `biodiversidad` o `fragmentacion`.
#' @param nom_ssubc nombre de la subsubcuenca.
#' @param opts Opciones para personalizar una portada. usar [portada_opts()].
#'
#' @name portada 
#' @returns Workbook con portada
#' @export 
wb_portada_PAS150 <- function(
    wb,
    apendice = c("inventarios", "biodiversidad", "fragmentacion"),
    nom_ssubc = NULL,
    opts = portada_opts()
  ){
  valid_input(wb, inherit = "wbWorkbook")
  apendice <- match.arg(apendice)
  if(!is.null(nom_ssubc)) valid_input(nom_ssubc, inherit = "character")
  stopifnot(
    inherits(opts, "list"),
    sort(names(opts)) == sort(c("tipo_proj", "nom_proj", "logo")),
    lengths(opts) == 1,
    opts$tipo_proj %in% c("DIA", "EIA")
  )
  
  mes <- Sys.Date() %>% format('%B') %>% stringi::stri_trans_totitle()
  yr <- Sys.Date() %>% format('%Y')

  logo_w <- 6
  logo_img <- if(tools::file_ext(opts$logo) == "svg") {
    magick::image_read_svg(opts$logo) %>% magick::image_info()
  } else {
    magick::image_read(opts$logo) %>% magick::image_info()
  }
  logo_h <- logo_img$height * logo_w / logo_img$width
  if (logo_h > 5) {
    logo_h <- 5
    logo_w <- logo_img$width * logo_h / logo_img$height
  }

  nom_apendice <- switch(
    apendice,
    "inventarios" = "APÉNDICE 5. Base datos inventario forestal BNP",
    "biodiversidad" = "APÉNDICE 6. Biodiversidad en área de proyecto",
    "fragmentacion" = "APÉNDICE 5. Análisis fragmentación BNP"
  )

  wb <- wb %>%
    openxlsx2::wb_add_worksheet("Portada", grid_lines = F) %>%
    openxlsx2::wb_page_setup(paper_size = 1) %>%
    # img
    openxlsx2::wb_add_image(
      dims = "C7",
      file = app_sys("app/www/logo_geobiota.svg"),
      width = 7,
      height = 1.75,
      units = 'cm'
    ) %>%
    # Tipo proyecto
    openxlsx2::wb_add_data(
      x = ifelse(opts$tipo_proj == "EIA", "ESTUDIO DE IMPACTO AMBIENTAL", "DECLARACIÓN DE IMPACTO AMBIENTAL"),
      start_col = 2,
      start_row = 16
    ) %>%
    openxlsx2::wb_add_font(dims = "B16", bold = T, size = 14) %>%
    openxlsx2::wb_add_cell_style(dims = "B16", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 16, cols = 2:5), solve = T) %>%
    # Nombre proyecto
    openxlsx2::wb_add_data(x = stringi::stri_trans_toupper(opts$nom_proj), dims = "A18") %>%
    openxlsx2::wb_add_font(dims = "A18",bold = T, size = 16) %>%
    openxlsx2::wb_add_cell_style(dims = "A18", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 18:19, cols = 1:6), solve = T) %>%
    # N° apéndice
    openxlsx2::wb_add_data(x = nom_apendice, dims = "B22") %>%
    openxlsx2::wb_add_cell_style(dims = "B22", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 22, cols = 2:5), solve = T) %>%
    openxlsx2::wb_add_data(
      x = sprintf("Subsubcuenca %s", if (is.null(nom_ssubc)) "NOMBRE SUBSUBCUENCA" else nom_ssubc),
      dims = "A24"
    ) %>%
    openxlsx2::wb_add_cell_style(dims = "A24", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 24, cols = 1:6), solve = T) %>%
    # Elaborado por
    openxlsx2::wb_add_data(x = "Elaborado por Geobiota para:", start_col = 2, start_row = 31) %>%
    openxlsx2::wb_add_font(dims = "B31", bold = T, size = 14) %>%
    openxlsx2::wb_add_cell_style(dims = "B31", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 31, cols = 2:5), solve = T) %>%
    openxlsx2::wb_add_image(
      dims = "C33",
      file = opts$logo,
      width = logo_w,
      height = logo_h,
      units = 'cm'
    ) %>%
    # Fecha
    openxlsx2::wb_add_data(x = paste(mes,yr, sep = ", "),dims = "C44") %>%
    openxlsx2::wb_add_cell_style(dims = "C44", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 44, cols = 3:4), solve = T) %>%
    openxlsx2::wb_set_col_widths(cols = 1:6, widths = 13)

  return(wb)
}

#' Ajuste de portada
#'
#' @param tipo_proj Tipo de proyecto. `DIA` o `EIA`. default `EIA`.
#' @param nom_proj Nombre del proyecto.
#' @param logo Ruta del archivo de tipo imagen con el logo del cliente.
#' @param plantilla Plantilla realizada para proyectos en particular, disponible `KIM753`, `MLP612` y `default`.
#'
#' @rdname portada 
#' @return lista con ajustes de portada
#' @export
portada_opts <- function(
    tipo_proj = "EIA",
    nom_proj = NULL,
    logo = NULL,
    plantilla = "default"
  ) {
  tipo_proj <- match.arg(tipo_proj, choices = c("DIA", "EIA"))
  plantilla <- match.arg(plantilla, choices = c("default", "KIM753", "MLP612"))

  nom_proj <- switch (
    plantilla,
    "KIM753" = "LÍNEA DE TRANSMISIÓN ELÉCTRICA HVDC KIMAL - LO AGUIRRE",
    "MLP612" = "PROYECTO EXTENSIÓN VIDA ÚTIL DE MINERA LOS PELAMBRES",
    "default" = if (!is.null(nom_proj)) nom_proj else "INGRESE NOMBRE DEL PROYECTO"
  )

  logo <- switch (
    plantilla,
    "KIM753" = app_sys("app/www/logo_conexion.png"),
    "MLP612" = app_sys("app/www/logo_mlp.png"),
    "default" = if (!is.null(logo)) logo else app_sys("app/www/logo_default.png")
  )

  opts <- list(
    tipo_proj = tipo_proj,
    nom_proj = nom_proj,
    logo = logo
  )
  return(opts)
}
