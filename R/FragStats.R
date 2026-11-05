#' FragStats
#'
#' @param dir directorio
#' @param path_antes ruta del archivo '.shp' del BNP antes del proyecto
#' @param path_despues ruta del archivo '.shp' del BNP despues del proyecto
#' @param path_tif_antes ruta del archivo '.tif' del BNP antes del proyecto. Archivo creado con la función `FragStats_pre()`
#' @param path_tif_despues ruta del archivo '.tif' del BNP despues del proyecto. Archivo creado con la función `FragStats_pre()`
#' @param path_exe ruta del archivo ejecutable del software FragStats. Obtener ruta con `find_frg_exe()`
#' @param path_fca ruta del modelo .fca
#' @param edge valor del parámetro de borde
#' @param prox valor del parámetro radio de busqueda usando para le índice de proximidad 
#' 
#' @name FragStats
#' @returns crea carpetas y archivos del apendice de fragmentación
#' 
#' @export
FragStats_pre <- function(dir, path_antes, path_despues) {
  stopifnot("El directorio no existe" = dir.exists(dir))
  stopifnot("La ruta del archivo 'path_antes' no es valida" = file.exists(path_antes))
  stopifnot("La ruta del archivo 'path_despues' no es valida" = file.exists(path_despues))
  
  frag.dir <- function(dir){
    file.path(
      dir,
      c(
        "ENTREGA/Vectorial/SHP",
        "ENTREGA/Vectorial/KMZ",
        "ENTREGA/Raster/SHP",
        "ENTREGA/Raster/KMZ",
        "IMG",
        "RESULTADOS/ANTES",
        "RESULTADOS/DESPUES",
        "TABLA"
      )
    ) %>% 
      purrr::map(dir.create, recursive = T)
  }
  frag.dir(dir)
  tryCatch(
    file.copy(
      from = app_sys("app/www/FragStats.fca"), 
      to = file.path(dir, "FragStats.fca") %>% normalizePath(winslash = "/", mustWork = FALSE) 
    ),
    error = invisible()
  )
  
  sf_antes <- sf::read_sf(path_antes) %>% 
    sf::st_zm() %>% 
    dplyr::mutate(ID = 1) %>% 
    dplyr::group_by(ID) %>% 
    dplyr::tally() %>% 
    sf::st_cast("POLYGON") %>% 
    dplyr::mutate(TIPO = "BNP_ANTES") %>% 
    st_add_sup_ha() %>% 
    dplyr::select(TIPO, ID, Sup_ha)
  sf::write_sf(sf_antes, file.path(dir,"ENTREGA/Vectorial/SHP", basename(path_antes)))
  shp2kmz(
    shp = file.path(dir, "ENTREGA/Vectorial/SHP", basename(path_antes)), 
    name = "ID",  
    dirsave = file.path(dir, "ENTREGA/Vectorial/KMZ")
  )
  
  sf_despues <- sf::read_sf(path_despues) %>% 
    sf::st_zm() %>% 
    dplyr::mutate(ID = 1) %>% 
    dplyr::group_by(ID) %>% 
    dplyr::tally() %>% 
    sf::st_cast("POLYGON") %>% 
    dplyr::mutate(TIPO = "BNP_DESPUES") %>% 
    st_add_sup_ha() %>% 
    dplyr::select(TIPO, ID, Sup_ha)
  sf::write_sf(sf_despues, file.path(dir, "ENTREGA/Vectorial/SHP", basename(path_despues)))
  shp2kmz(
    shp = file.path(dir, "ENTREGA/Vectorial/SHP", basename(path_despues)), 
    name = "ID",  
    dirsave = file.path(dir, "ENTREGA/Vectorial/KMZ")
  )

  resolucion = 3
  dim_to_rast <- function(sf) {
    bbox <- sf::st_bbox(sf)
    ancho <- ceiling((bbox[3] - bbox[1]) / resolucion)
    alto <- ceiling((bbox[4] - bbox[2]) / resolucion)
    return(list(ancho = ancho, alto = alto))
  }
  
  antes_raster <- sf_antes %>% dplyr::select(ID) %>% stars::st_rasterize(nx = dim_to_rast(sf_antes)$ancho, ny = dim_to_rast(sf_antes)$alto) 
  stars::write_stars(antes_raster, file.path(dir, "IMG", gsub(".shp$", ".tif", basename(path_antes))))
  
  despues_raster <- sf_despues %>% dplyr::select(ID) %>% stars::st_rasterize(nx = dim_to_rast(sf_despues)$ancho, ny = dim_to_rast(sf_despues)$alto) 
  stars::write_stars(despues_raster, file.path(dir, "IMG", gsub(".shp$", ".tif", basename(path_despues))))
}

#' @rdname FragStats
#' @export
FragStats_run <- function(dir, path_tif_antes, path_tif_despues, path_fca, path_exe = find_frg_exe(), edge = 10, prox = 1000) {
  stopifnot("El directorio no existe" = dir.exists(dir))
  stopifnot("La ruta del archivo 'path_tif_antes' no es valida" = file.exists(path_tif_antes))
  stopifnot("La ruta del archivo 'path_tif_despues' no es valida" = file.exists(path_tif_despues))
  stopifnot("La ruta del archivo 'path_fca' no es valida" = file.exists(path_fca))
  stopifnot("La extensión del archivo no es válida. Ingresar archivo '.fca'" = tolower(tools::file_ext(path_fca)) == "fca")

  list(antes = path_tif_antes, despues = path_tif_despues) %>% 
    purrr::iwalk(function(x, id) {
      dir_out <- if(id == "antes") file.path(dir, "RESULTADOS/ANTES/") else file.path(dir, "RESULTADOS/DESPUES/")
      if(!dir.exists(dir_out)) dir.create(dir_out)
      update_param_fca(path_fca = path_fca, path_tif = x, dir_out = dir_out, edge = edge, prox = prox)
      run_fca(path_fca = path_fca, path_exe = path_exe)
    })
}

#' @rdname FragStats
#' @export
FragStats_post <- function(dir, path_antes, path_despues, edge, prox) {
  stopifnot(
    "Falta archivo .patch en RESULTADOS/ANTES" = isTruthy(list.files(
      file.path(dir, "RESULTADOS", "ANTES"),
      pattern = ".patch",
      recursive = T
    ))
  )
  stopifnot(
    "Falta archivo .patch en RESULTADOS/DESPUES" = isTruthy(list.files(
      file.path(dir, "RESULTADOS", "DESPUES"),
      pattern = ".patch",
      recursive = T
    ))
  )
  stopifnot(
    "Falta archivo id8.tif en IMG" = isTruthy(
      list.files(file.path(dir, "IMG"), pattern = "id8.tif") %>% length() == 2
    )
  )

  antes_raster_id <- list.files(
    file.path(dir, "IMG"),
    pattern = paste0(tools::file_path_sans_ext(basename(path_antes)), ".*id8"),
    ignore.case = TRUE,
    full.names = TRUE
  ) %>% 
    stars::read_stars() %>% 
    sf::st_set_crs(get_utm_epsg(sf::read_sf(path_antes))) %>% 
    `names<-`("PID")
  antes_raster_sf <- antes_raster_id %>% 
    sf::st_as_sf(merge = T) %>% 
    dplyr::filter(PID != -999) %>% 
    dplyr::group_by(PID) %>% 
    dplyr::tally() %>% 
    dplyr::select(-n) %>% 
    sf::st_collection_extract("POLYGON") %>% 
    dplyr::left_join(
      read.table(list.files(file.path(dir, "RESULTADOS/ANTES/"), pattern = ".patch$", full.names = T), header = TRUE, sep = ",") %>% dplyr::select(-1) 
    ) %>% 
    dplyr::relocate(geometry, .after = dplyr::last_col()) %>% 
    suppressMessages()
  sf::write_sf(antes_raster_sf, file.path(dir, "ENTREGA/Raster/SHP", basename(path_antes)))
  shp2kmz(
    shp = file.path(dir, "ENTREGA/Raster/SHP", basename(path_antes)), 
    name = "PID",  
    dirsave = file.path(dir, "ENTREGA/Raster/KMZ")
  )
  
  despues_raster_id <- list.files(
    file.path(dir, "IMG"),
    pattern = paste0(tools::file_path_sans_ext(basename(path_despues)), ".*id8"),
    ignore.case = TRUE,
    full.names = T
  ) %>% 
    stars::read_stars() %>% 
    sf::st_set_crs(get_utm_epsg(sf::read_sf(path_despues))) %>% 
    `names<-`("PID")
  despues_raster_sf <- despues_raster_id %>% 
    sf::st_as_sf(merge = T) %>% 
    dplyr::filter(PID != -999) %>% 
    dplyr::group_by(PID) %>% 
    dplyr::tally() %>% 
    dplyr::select(-n) %>% 
    sf::st_collection_extract("POLYGON") %>% 
    dplyr::left_join(
      read.table(list.files(file.path(dir, "RESULTADOS/DESPUES/"), pattern = ".patch$", full.names = T), header = TRUE, sep = ",") %>% dplyr::select(-1)
    ) %>% 
    dplyr::relocate(geometry, .after = dplyr::last_col()) %>% 
    suppressMessages()
  sf::write_sf(despues_raster_sf, file.path(dir, "ENTREGA/Raster/SHP", basename(path_despues)))
  shp2kmz(
    shp = file.path(dir, "ENTREGA/Raster/SHP", basename(path_despues)), 
    name = "PID",  
    dirsave = file.path(dir, "ENTREGA/Raster/KMZ")
  )
  
  wb <- openxlsx2::wb_workbook() %>% 
    openxlsx2::wb_add_worksheet(sheet = "ANTES") %>% 
    openxlsx2::wb_add_data(x = read.table(list.files(file.path(dir, "RESULTADOS/ANTES/"), pattern = ".patch$", full.names = T), header = TRUE, sep = ",") %>% select(-1)) %>% 
    openxlsx2::wb_add_worksheet(sheet = "DESPUES") %>% 
    openxlsx2::wb_add_data(x = read.table(list.files(file.path(dir, "RESULTADOS/DESPUES/"), pattern = ".patch$", full.names = T), header = TRUE, sep = ",") %>% select(-1)) 
  openxlsx2::wb_save(wb = wb, file = file.path(dir, sprintf("TABLA/JOIN_PATCH_EDGE%s_PROX%s.xlsx", edge, prox)))
  
  names_from <- list.files(file.path(dir, "RESULTADOS/ANTES"), recursive = TRUE, full.names = T)
  names_to <- file.path(dir, "RESULTADOS/ANTES", paste(
    basename(path_antes) %>% tools::file_path_sans_ext() %>% paste(sprintf("Edge%s_Prox%s", edge, prox), sep = "_"),
    tools::file_ext(names_from), sep = "."
  ))
  file.rename(names_from, names_to)
  names_from <- list.files(file.path(dir, "RESULTADOS/DESPUES"), recursive = TRUE, full.names = T)
  names_to <- file.path(dir, "RESULTADOS/DESPUES", paste(
    basename(path_despues) %>% tools::file_path_sans_ext() %>% paste(sprintf("Edge%s_Prox%s", edge, prox), sep = "_"),
    tools::file_ext(names_from), sep = "."
  ))
  file.rename(names_from, names_to)
}

#' @rdname FragStats
#' @export
FragStats <- function(dir, path_antes, path_despues, path_exe = find_frg_exe(), edge = 10, prox = 1000) {
  stopifnot("El directorio no existe" = dir.exists(dir))
  stopifnot("La ruta del archivo 'path_antes' no es valida" = file.exists(path_antes))
  stopifnot("La ruta del archivo 'path_despues' no es valida" = file.exists(path_despues))
  stopifnot("La ruta del archivo 'path_exe' no es valida. Es posible que no tenga instalado el software FragStats" = file.exists(path_exe))

  tryCatch(
    FragStats_pre(dir = dir, path_antes = path_antes, path_despues = path_despues),
    error = function(e) {
      stop("Error en 'FragStats_pre()': ", e$message, call. = FALSE)
    }
  )
  tryCatch(
    FragStats_run(
      dir = dir,
      path_tif_antes = list.files(file.path(dir, "IMG"), pattern = "(?i)antes.*\\.tif$", full.names = TRUE) %>% .[1],
      path_tif_despues = list.files(file.path(dir, "IMG"), pattern = "(?i)desp.*\\.tif$", full.names = TRUE) %>% .[1],
      path_fca = file.path(dir, "FragStats.fca") %>% normalizePath(winslash = "/", mustWork = FALSE),
      path_exe = path_exe,
      edge = edge,
      prox = prox
    ),
    error = function(e) {
      stop("Error en 'FragStats_run()': ", e$message, call. = FALSE)
    }
  )
  tryCatch(
    FragStats_post(dir = dir, path_antes = path_antes, path_despues = path_despues, edge = edge, prox = prox), 
    error = function(e) {
      stop("Error en 'FragStats_post()': ", e$message, call. = FALSE)
    }
  )
}

#' @noRd
update_param_fca <- function(
  path_fca,
  path_tif = NULL,
  dir_out = NULL,
  edge = 10,
  prox = 1000
) {
  stopifnot("La ruta del archivo 'path_fca' no es valida" = file.exists(path_fca))
  stopifnot("La extensión del archivo no es válida. Ingresar archivo '.fca'" = tolower(tools::file_ext(path_fca)) == "fca")
  if(!is.null(path_tif)) {
    stopifnot("La ruta del archivo 'path_tif' no es valida" = file.exists(path_tif))
    stopifnot("La extensión del archivo no es válida. Ingresar archivo '.tif'" = tolower(tools::file_ext(path_tif)) == "tif")
  }
  if(!is.null(dir_out)) {
    stopifnot("La ruta del directorio no existe" = dir.exists(dir_out))
  }
  stopifnot(is.numeric(edge), is.numeric(prox)) 

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path_fca)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if(!is.null(path_tif) & !is.null(dir_out)) {
    path_out <- file.path(
      dir_out,
      basename(path_tif) %>%
        tools::file_path_sans_ext() %>%
        paste(sprintf("Edge%s_Prox%s", edge, prox), sep = "_")
    ) %>%
      normalizePath(winslash = "/", mustWork = FALSE)

    r <- terra::rast(path_tif)
    e <- terra::ext(r)
    nd_val <- terra::NAflag(r)
    if (length(nd_val) == 0 || is.na(nd_val) || is.nan(nd_val)) {
      nd_val <- 9999
    } 

    DBI::dbExecute(
      con,
      "
      INSERT OR REPLACE INTO frg_landscape_layers
      (id, model_id, name_external, name_internal, name_cpf, io_info, description,
      driver_lib, driver_name, driver_id,
      xll, yll, xur, yur, cell_size, row_count, col_count,
      allow_class_zero, no_data_value, background_value)
      VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
      params = list(
        1,
        1,
        normalizePath(path_tif, winslash = "\\", mustWork = FALSE),
        "",
        "",
        "[BAND:1]",
        "",
        "GDAL",
        "GeoTIFF grid (.tif)",
        "63B45E15-C8E5-44f6-A9AB-60E1852CDB5D",
        e$xmin,
        e$ymin,
        e$xmax,
        e$ymax,
        terra::xres(r),
        terra::nrow(r),
        terra::ncol(r),
        1L,
        nd_val,
        999
      )
    )
  }

  actualizar <- function(sql, valor) {
    n <- DBI::dbExecute(con, sql, params = list(valor))
    if (n == 0) {
      warning("Ninguna fila actualizada con: ", sql, call. = FALSE)
    }
    n
  }

  DBI::dbWithTransaction(con, {
    actualizar("UPDATE frg_table_numerics SET value = ? WHERE numeric_name = 'pclEDGE_DEPTH_FIXED_VAL'", edge)
    actualizar("UPDATE frg_table_numerics SET value = ? WHERE numeric_name = 'pclSEARCH_RADIUS'", prox)
    if(!is.null(path_out)) actualizar("UPDATE frg_table_strings SET value = ? WHERE string_name = 'OUTPUT_FOLDER_PATH'", path_out)
  })

  message("¡Parámetros actualizados con éxito en: ", basename(path_fca), "!")
  invisible(path_fca)
}

#' @noRd
run_fca <- function(
  path_fca,
  path_out = NULL,
  path_exe = NULL,
  args = NULL,
  timeout = Inf
) {
  stopifnot(file.exists(path_fca), tolower(tools::file_ext(path_fca)) == "fca")

  if (is.null(path_exe)) {
    path_exe <- find_frg_exe()
  }
  if (is.null(path_exe) || !file.exists(path_exe)) {
    stop(
      "No se encontró frg.exe. Indicar su ubicación en 'path_exe'.",
      call. = FALSE
    )
  }

  path_fca <- normalizePath(path_fca, winslash = "\\", mustWork = TRUE)

  # processx no usa shell: los argumentos van sin shQuote()
  if (is.null(args)) {
    args <- c("-m", path_fca)
    if (!is.null(path_out)) {
      dir.create(path_out, showWarnings = FALSE, recursive = TRUE)
      args <- c(args, "-o", normalizePath(path_out, winslash = "\\", mustWork = TRUE))
    }
  }

  inicio <- Sys.time()
  p <- processx::process$new(
    path_exe,
    args,
    wd = dirname(path_exe), 
    stdout = "|",
    stderr = "2>&1"
  )
  on.exit(if (p$is_alive()) p$kill(), add = TRUE)

  cli::cli_progress_bar(
    format = "{cli::pb_spin} FRAGSTATS en ejecución | {cli::pb_elapsed}",
    total = NA
  )
  log <- character()
  while (p$is_alive()) {
    log <- c(log, p$read_output_lines())
    if (as.numeric(difftime(Sys.time(), inicio, units = "secs")) > timeout) {
      p$kill()
      cli::cli_progress_done(result = "failed")
      stop("Tiempo de espera excedido (", timeout, " s)", call. = FALSE)
    }
    cli::cli_progress_update()
    Sys.sleep(0.2)
  }
  log <- c(log, p$read_output_lines())

  estado <- p$get_exit_status()
  if (estado != 0) {
    cli::cli_progress_done(result = "failed")
    if (length(log)) {
      cat(log, sep = "\n")
    }
    stop("FRAGSTATS terminó con código de error ", estado, call. = FALSE)
  }
  cli::cli_progress_done()

  nuevos <- character()
  if (!is.null(path_out)) {
    todos <- list.files(path_out, full.names = TRUE, recursive = TRUE)
    nuevos <- todos[file.mtime(todos) >= inicio]
    if (length(nuevos) == 0) {
      warning(
        "No se encontraron resultados nuevos en: ",
        path_out,
        call. = FALSE
      )
    }
  }

  cli::cli_alert_success(
    "Ejecución finalizada en {round(as.numeric(difftime(Sys.time(), inicio, units = 'mins')), 1)} min"
  )
  invisible(list(log = log, resultados = nuevos))
}

#' @rdname FragStats
#' @export
find_frg_exe <- function() {
  directorios <- c("C:/Program Files (x86)", "C:/Program Files")
  
  for (dir in directorios) {
    if (dir.exists(dir)) {
      # Buscar de forma recursiva el archivo frg.exe
      archivos <- list.files(path = dir, pattern = "^frg\\.exe$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(archivos) > 0) {
        return(archivos[1]) # Retorna la primera coincidencia encontrada
      }
    }
  }
  
  # Si no está en Program Files, realizar una búsqueda general en C:/
  cat("Buscando en todo el disco C, por favor espera...\n")
  archivos_c <- list.files(path = "C:/", pattern = "^frg\\.exe$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(archivos_c) > 0) {
    return(archivos_c[1])
  }
  
  return(NULL)
}
