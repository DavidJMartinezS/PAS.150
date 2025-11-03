#' FragStats
#'
#' @param dir directorio
#' @param path_antes ruta del archivo '.shp' del BNP antes del proyecto
#' @param path_despues ruta del archivo '.shp' del BNP despues del proyecto
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
        "ENTREGA/Vectorial/KML",
        "ENTREGA/Raster/SHP",
        "ENTREGA/Raster/KML",
        "IMG",
        "RESULTADOS/ANTES",
        "RESULTADOS/DESPUES",
        "TABLA"
      )
    ) %>% 
      purrr::map(dir.create, recursive = T)
  }
  frag.dir(dir)
  
  sf_antes <- sf::read_sf(path_antes) %>% 
    sf::st_zm() %>% 
    dplyr::mutate(ID = 1) %>% 
    dplyr::group_by(ID) %>% 
    dplyr::tally() %>% 
    sf::st_cast("POLYGON") %>% 
    dplyr::mutate(
      TIPO = "BNP_ANTES",
      Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2)
    ) %>% 
    dplyr::select(TIPO, ID, Sup_ha)
  sf::write_sf(sf_antes, file.path(dir,"ENTREGA/Vectorial/SHP", basename(path_antes)))
  sf_antes %>% 
    dplyr::mutate(Description = leafpop::popupTable(sf::st_drop_geometry(.))) %>% 
    dplyr::select(TIPO, Description) %>% 
    sf::write_sf(file.path(dir, "ENTREGA/Vectorial/KML", gsub(".shp", ".kml", basename(path_antes))))
  
  sf_despues <- sf::read_sf(path_despues) %>% 
    sf::st_zm() %>% 
    dplyr::mutate(ID = 1) %>% 
    dplyr::group_by(ID) %>% 
    dplyr::tally() %>% 
    sf::st_cast("POLYGON") %>% 
    dplyr::mutate(
      TIPO = "BNP_DESPUES",
      Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2) 
    ) %>% 
    dplyr::select(TIPO, ID, Sup_ha)
  sf::write_sf(sf_despues, file.path(dir, "ENTREGA/Vectorial/SHP", basename(path_despues)))
  sf_despues %>% 
    dplyr::mutate(Description = leafpop::popupTable(sf::st_drop_geometry(.))) %>% 
    dplyr::select(TIPO, Description) %>% 
    sf::write_sf(file.path(dir, "ENTREGA/Vectorial/KML", gsub(".shp", ".kml", basename(path_despues))))
  
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
FragStats_post <- function(dir, path_antes, path_despues) {
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
    full.names = T
  ) %>% 
    stars::read_stars() %>% 
    `names<-`("PID")
  antes_raster_sf <- antes_raster_id %>% 
    sf::st_as_sf(merge = T) %>% 
    dplyr::filter(PID != -999) %>% 
    dplyr::group_by(PID) %>% 
    dplyr::tally() %>% 
    dplyr::select(-n) %>% 
    sf::st_collection_extract("POLYGON") %>% 
    dplyr::left_join(
      read.table(list.files(file.path(dir, "RESULTADOS/ANTES/"), pattern = ".patch$", full.names = T), header = T, sep = ",") %>% dplyr::select(-1) 
    ) %>% 
    dplyr::relocate(geometry, .after = dplyr::last_col())
  sf::write_sf(antes_raster_sf, file.path(dir, "ENTREGA/Raster/SHP", basename(path_antes)))
  antes_raster_sf %>% 
    dplyr::mutate(Description = leafpop::popupTable(sf::st_drop_geometry(.))) %>% 
    dplyr::select(PID, Description) %>% 
    sf::write_sf(file.path(dir, "ENTREGA/Raster/KML", gsub(".shp", ".kml", basename(path_antes))))
  
  despues_raster_id <- list.files(
    file.path(dir, "IMG"),
    pattern = paste0(tools::file_path_sans_ext(basename(path_despues)), ".*id8"),
    ignore.case = TRUE,
    full.names = T
  ) %>% 
    stars::read_stars() %>% 
    `names<-`("PID")
  despues_raster_sf <- despues_raster_id %>% 
    sf::st_as_sf(merge = T) %>% 
    dplyr::filter(PID != -999) %>% 
    dplyr::group_by(PID) %>% 
    dplyr::tally() %>% 
    dplyr::select(-n) %>% 
    sf::st_collection_extract("POLYGON") %>% 
    dplyr::left_join(
      read.table(list.files(file.path(dir, "RESULTADOS/DESPUES/"), pattern = ".patch$", full.names = T), header = T, sep = ",") %>% dplyr::select(-1)
    ) %>% 
    dplyr::relocate(geometry, .after = dplyr::last_col())
  sf::write_sf(despues_raster_sf, file.path(dir, "ENTREGA/Raster/SHP", basename(path_despues)))
  despues_raster_sf %>% 
    dplyr::mutate(Description = leafpop::popupTable(sf::st_drop_geometry(.))) %>% 
    dplyr::select(PID, Description) %>% 
    sf::write_sf(file.path(dir, "ENTREGA/Raster/KML", gsub(".shp", ".kml", basename(path_despues))))
  
  writexl::write_xlsx(
    list(
      ANTES = read.table(list.files(file.path(dir, "RESULTADOS/ANTES/"), pattern = ".patch$", full.names = T), header = T, sep = ",") %>% dplyr::select(-1),
      DESPUES = read.table(list.files(file.path(dir, "RESULTADOS/DESPUES/"), pattern = ".patch$", full.names = T), header = T, sep = ",") %>% dplyr::select(-1)
    ),
    file.path(dir, "TABLA/JOIN_PATCH25_PROX1000.xlsx")
  )
  
  names_from <- list.files(file.path(dir, "RESULTADOS/ANTES"), recursive = T, full.names = T)
  names_to <- file.path(dir, "RESULTADOS/ANTES", paste(
    basename(path_antes) %>% tools::file_path_sans_ext() %>% paste("Patch25_Prox1000", sep = "_"),
    tools::file_ext(names_from), sep = "."
  ))
  file.rename(names_from, names_to)
  names_from <- list.files(file.path(dir, "RESULTADOS/DESPUES"), recursive = T, full.names = T)
  names_to <- file.path(dir, "RESULTADOS/DESPUES", paste(
    basename(path_despues) %>% tools::file_path_sans_ext() %>% paste("Patch25_Prox1000", sep = "_"),
    tools::file_ext(names_from), sep = "."
  ))
  file.rename(names_from, names_to)
}