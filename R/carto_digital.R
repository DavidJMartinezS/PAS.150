#' Title
#'
#' @param uso_veg capa vectorial de la cartografía de uso y vegetación de la cuenca.
#' @param cuenca capa vectorial de la cuenca. obtener con `get_cuenca()`.
#' @param dem modelo de elevación digital. Ingresar ruta del archivo tif u objeto stars. Si es \code{NULL}, el dem se obtendra con `elevatr::get_elev_raster()`.
#' @param fuente_hidro fuente de la capa hidrografica. `MOP` o `BCN`.
#' @param obras capa vectorial con las obras del proyecto.
#' @param sp especie en categoría de conservación objeto del informe.
#' @param BNP_cuenca capa vectorial con los BNP en la cuenca. Obtener con `get_BNP_cuenca()`.
#' @param BNP_inter capa vectorial con la superficie de intervención de BNP. default \code{NULL}.
#' @param BNP_alter capa vectorial con la superficie de alteración de BNP. default \code{NULL}.
#' @param alt_ok valor logico. \code{TRUE} para indicar que el BNP_alter ingresado esta ok. \code{FALSE} si solo corresponden a las obras que alteran.
#' @param censo capa vectorial de tipo POINT con el censo de la especie objetivo.
#' @param upto5m valor logico. \code{TRUE} para considerar de individuos con un radio de hasta 5 metros de las obras.   
#' @param BD_flora dataframe con la base de datos de las parcelas de inventarios florísticos.
#' @param BD_fore dataframe con la base de datos de las parcelas de inventarios forestales.
#' @param densidad número de individuos por hectárea de la especie objetivo (sp).
#' @param bd_lista,bd_fore_lista,bd_flora_lista valor logico. \code{TRUE} si los datos ingresados estan listos.
#' @param in_bnp_obra valor logico. \code{TRUE} para filtrar los puntos dentro de BNP con obras.
#' @param add_cam,add_hidro,add_CN valores logicos para indicar si agregar caminos, hidrografia y/o curvas de nivel respectivamente
#'
#' @returns capas vectoriales de la cartografía digital
#' @name carto_digital
#' @export
get_cuenca <- function(uso_veg) {
  valid_input(uso_veg, inherit = "sf", geometry = "POLYGON")

  cuencas <- sf::read_sf(system.file("Subsubcuencas.gdb", package = "dataPAS")) %>% 
    sf::st_set_geometry("geometry") %>% 
    sf::st_zm() %>% 
    sf::st_transform(sf::st_crs(uso_veg))

  nom_ssubc <- if ("Nom_ssubc" %in% names(uso_veg)) {
    if (uso_veg$Nom_ssubc %>% unique() %>% length() == 1 & uso_veg$Nom_ssubc %>% unique() %in% cuencas$NOM_SSUBC) {
      unique(uso_veg$Nom_ssubc)
    } else {
      uso_veg %>% sf::st_join(cuencas %>% dplyr::select(NOM_SSUBC), largest = T) %>% dplyr::pull(NOM_SSUBC) %>% unique()
    } 
  } else {
    uso_veg %>% sf::st_join(cuencas %>% dplyr::select(NOM_SSUBC), largest = T) %>% dplyr::pull(NOM_SSUBC) %>% unique()
  }

  cuenca <- cuencas %>% dplyr::filter(NOM_SSUBC == nom_ssubc)

  return(cuenca)
}

#' @rdname carto_digital
#' @export
get_caminos <- function(cuenca) {
  sf::read_sf(system.file("Red_vial.gdb", package = "dataPAS")) %>% 
    sf::st_zm() %>% 
    sf::st_transform(sf::st_crs(cuenca)) %>%
    sf::st_intersection(cuenca) %>% 
    sf::st_collection_extract("LINESTRING") %>% 
    dplyr::mutate(
      TIPO_CAM = ifelse(stringi::stri_detect_regex(CLASIFICACION, 'Internacional|Nacional|Regional Principal'), 1,
        ifelse(stringi::stri_detect_regex(CLASIFICACION, 'Regional Provincial|Regional Comunal'), 2,
        ifelse(stringi::stri_detect_regex(CLASIFICACION, 'Acceso'), 3, 4))),
      FUENTE = 'MOP 1:5.000 - 1:25.000'
    ) %>% 
    dplyr::select(COD_SSUBC, NOM_SSUBC, ROL_ID, CALZADA, ORIENTACION, ROL_LABEL, CODIGO_CAMINO, NOMBRE_CAMINO, CLASIFICACION, CARPETA, ENROLADO, CONCESIONADO, REGION, TIPO_CAM, FUENTE)
}

#' @rdname carto_digital
#' @export
get_CN <- function(cuenca, dem = NULL) {
  valid_input(cuenca, inherit = "sf", names = "NOM_SSUBC", geometry = "POLYGON")
  valid_dem(dem)

  if (is.null(dem)) {
    dem_stars <- elevatr::get_elev_raster(
      sf::st_buffer(cuenca, 100), 
      z = 12, 
      prj = sf::st_crs(cuenca), 
      source = "alos"
    ) %>%
      stars::st_as_stars() %>%
      `names<-`("Elevacion")
  } else {
    dem_stars <- if (inherits(dem, "character")) stars::read_stars(dem) else dem 
  }
  
  step <- 50
  curv_niv <- stars::st_contour(
    dem_stars,
    contour_lines = T,
    breaks = seq(
      plyr::round_any(min(dem_stars$Elevacion, na.rm = T), step, ceiling),
      plyr::round_any(max(dem_stars$Elevacion, na.rm = T), step, floor),
      step
    )
  ) %>%
    sf::st_intersection(sf::st_union(cuenca)) %>%
    sf::st_collection_extract("LINESTRING") %>% 
    dplyr::mutate(
      NOM_SSUBC = cuenca$NOM_SSUBC,
      Elevacion = paste(Elevacion, 'm')
    ) %>% 
    dplyr::select(NOM_SSUBC, Elevacion)
  return(curv_niv)
}

#' @rdname carto_digital
#' @export
get_hidro <- function(cuenca, fuente_hidro = c("MOP", "BCN")) {
  valid_input(cuenca, inherit = "sf", names = "NOM_SSUBC", geometry = "POLYGON")
  fuente_hidro <- match.arg(fuente_hidro)

  cod_ssbuc <- cuenca$COD_SSUBC
  dplyr::bind_rows(
    sf::read_sf(
      system.file("Red_hidrografica_XV_XIII.gdb", package = "dataPAS"),
      query = paste0("SELECT * FROM \"Red_hidrografica_XV_XIII\" WHERE cod_ssubc = '", cod_ssbuc, "'")
    ),
    sf::read_sf(
      system.file("Red_hidrografica_VI_X.gdb", package = "dataPAS"),
      query = paste0("SELECT * FROM \"Red_hidrografica_VI_X\" WHERE cod_ssubc = '", cod_ssbuc, "'")
    )
  ) %>% 
    sf::st_set_geometry('geometry') %>% 
    sf::st_transform(sf::st_crs(cuenca)) %>% 
    sf::st_intersection(sf::st_union(cuenca)) %>% 
    {if(nrow(.) > 0) {
      .[] %>% 
        dplyr::select(region, dplyr::contains("ssubc"), strahler_n, dplyr::contains(fuente_hidro)) %>% 
        dplyr::rename_at(dplyr::vars(dplyr::contains(fuente_hidro)), ~stringi::stri_extract(., regex = ".*(?=_)", mode = "first")) %>%
        dplyr::rename_all(~ ifelse(. == "geometry", .,
          stringi::stri_trans_totitle(stringi::stri_trans_general(., "Latin-ASCII"), type = "sentence")
        )) %>% 
        dplyr::mutate_at("Tipo", stringi::stri_trans_general, "Latin-ASCII") %>% 
        dplyr::mutate(Tipo_dren = dplyr::case_when(
          stringi::stri_detect_regex(Tipo, "rio", case_insensitive = T) ~ 1,
          stringi::stri_detect_regex(Tipo, "estero", case_insensitive = T) ~ 2,
          stringi::stri_detect_regex(Tipo, "arroyo", case_insensitive = T) ~ 3,
          stringi::stri_detect_regex(Tipo, "quebrada", case_insensitive = T) ~ 4,
          .default = 5
        ) %>% as.integer()) %>% 
        sf::st_collection_extract("LINESTRING") %>% 
        dplyr::select(Region, Cod_ssubc, Nom_ssubc, Strahler_n, Nombre, Tipo, Tipo_dren)
    } else {
      .[]
    }}
}

#' @rdname carto_digital
#' @export
get_ubicacion <- function(obras, cuenca) {
  valid_input(obras, inherit = "sf", geometry = "POLYGON")
  valid_input(cuenca, inherit = "sf", names = "NOM_SSUBC", geometry = "POLYGON")

  obras %>%
    sf::st_zm() %>%
    sf::st_transform(sf::st_crs(cuenca)) %>% 
    sf::st_intersection(sf::st_union(cuenca)) %>%
    dplyr::summarise(geometry = sf::st_union(geometry)) %>%
    dplyr::mutate(
      NOM_SSUBC = cuenca$NOM_SSUBC,
      Centro_X = sf::st_coordinates(sf::st_centroid(geometry))[, 1] %>% janitor::round_half_up(),
      Centro_Y = sf::st_coordinates(sf::st_centroid(geometry))[, 2] %>% janitor::round_half_up(),
      Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2),
    ) %>%
    dplyr::select(NOM_SSUBC, Centro_X, Centro_Y, Sup_ha)
}

#' @rdname carto_digital
#' @export
get_obras <- function(obras, cuenca) {
  valid_input(obras, inherit = "sf", geometry = "POLYGON")
  valid_input(cuenca, inherit = "sf", names = "NOM_SSUBC", geometry = "POLYGON")

  obras %>%
    sf::st_zm() %>%
    sf::st_transform(sf::st_crs(cuenca)) %>% 
    sf::st_intersection(sf::st_union(cuenca)) %>%
    sf::st_collection_extract('POLYGON') %>% 
    dplyr::select(-starts_with("Centro")) %>% 
    dplyr::mutate(
      Centro_X = sf::st_coordinates(sf::st_centroid(geometry))[, 1] %>% janitor::round_half_up() %>% as.integer(),
      Centro_Y = sf::st_coordinates(sf::st_centroid(geometry))[, 2] %>% janitor::round_half_up() %>% as.integer(),
      Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2),
      Sup_m2 = sf::st_area(geometry) %>% units::set_units(m2) %>% units::drop_units() %>% janitor::round_half_up(2)
    ) %>% 
    dplyr::relocate(geometry, .after = last_col())
}

#' @rdname carto_digital
#' @export
get_BNP_cuenca <- function(uso_veg, sp) {
  valid_input(uso_veg, inherit = "sf", names = req_names$BNP_afect, geometry = "POLYGON")
  valid_input(sp, inherit = "character")

  uso_veg %>% 
    dplyr::filter(stringi::stri_detect_regex(F_ley20283, "preser") & stringi::stri_detect_regex(BNP_ECC, sp)) %>% 
    dplyr::mutate(
      Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2)
    ) %>% 
    dplyr::select(Nom_ssubc, Formacion, Tipo_for, Subtipo_fo, dplyr::starts_with('ECC'), F_ley20283, BNP_ECC, Sup_ha) %>% 
    dplyr::arrange(Sup_ha)
} 

#' @rdname carto_digital
#' @export
get_BNP_intervencion <- function(BNP_cuenca, obras, BNP_inter = NULL) {
  if (!is.null(BNP_inter)) {
    valid_input(BNP_inter, inherit = "sf", names = req_names$BNP_afect, geometry = "POLYGON")
    stopifnot("El shp debe contener algun campo con el nombre de las obras que intervienen" = any(names(BNP_inter) %>% stringi::stri_detect_regex("obra", case_insensitive = T)))
    
    BNP_inter %>% 
      dplyr::mutate(Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2)) %>% 
      dplyr::select(Nom_ssubc, Formacion, Tipo_for, Subtipo_fo, dplyr::starts_with("ECC"), BNP_ECC, F_ley20283, dplyr::contains("obra"), Sup_ha) 
  } else {
    valid_input(BNP_cuenca, inherit = "sf", names = req_names$BNP_afect, geometry = "POLYGON")
    valid_input(obras, inherit = "sf", geometry = "POLYGON")

    BNP_cuenca %>%
      sf::st_intersection(obras) %>% 
      sf::st_collection_extract("POLYGON") %>% 
      dplyr::mutate(
        Afectacion = "Intervención",
        Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2),
        Sup_m2 = sf::st_area(geometry) %>% units::set_units(m2) %>% units::drop_units() %>% janitor::round_half_up(2)
      ) %>% 
      dplyr::arrange(Sup_m2) %>% 
      dplyr::select(Nom_ssubc, Formacion, Tipo_for, Subtipo_fo, dplyr::starts_with("ECC"), BNP_ECC, F_ley20283, dplyr::contains("obra"), Sup_ha) 
  }
}

#' @rdname carto_digital
#' @export
get_BNP_alterar <- function(BNP_alter = NULL, BNP_cuenca, alt_ok = F) {
  valid_input(BNP_cuenca, inherit = "sf", names = req_names$BNP_afect, geometry = "POLYGON")
  valid_input(alt_ok, inherit = "logical")

  if (is.null(BNP_alter)) {
    return(invisible())
  } else {
    valid_input(BNP_alter, inherit = "sf", geometry = "POLYGON")
    if (alt_ok) {
      BNP_alter
    }
    BNP_alter %>% 
      dplyr::select(dplyr::matches("obra")) %>% 
      sf::st_intersection(BNP_cuenca) %>% 
      sf::st_collection_extract("POLYGON") %>% 
      dplyr::mutate(Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2)) %>% 
      dplyr::select(Nom_ssubc, Formacion, Tipo_for, Subtipo_fo, dplyr::starts_with("ECC"), BNP_ECC, F_ley20283, dplyr::contains("obra"), Sup_ha)
  } 
}

#' @rdname carto_digital
#' @export
get_ECC_alt <- function(BNP_alter, censo, sp) {
  valid_input(censo, inherit = "sf", names = "Especie", geometry = "POINT")
  valid_input(sp, inherit = "character")
  if(!is.null(BNP_alter)) {
    if(nrow(BNP_alter)>0){
      valid_input(BNP_alter, inherit = "sf", geometry = "POLYGON")
    } else {
      return(invisible())
    }
  } else {
    return(invisible())
  }

  censo %>% 
    sf::st_zm() %>% 
    sf::st_transform(sf::st_crs(BNP_alter)) %>% 
    dplyr::filter(Especie %>% stringi::stri_detect_fixed(sp)) %>%
    sf::st_intersection(BNP_alter) %>% 
    dplyr::mutate(
      Afectacion = "Alteración del hábitat",
      UTM_E = sf::st_coordinates(geometry)[, 1] %>% as.integer(),
      UTM_N = sf::st_coordinates(geometry)[, 2] %>% as.integer()
    ) %>% 
    dplyr::select(Especie, Afectacion, dplyr::matches("obra"), UTM_E, UTM_N)
}

#' @rdname carto_digital
#' @export
get_ECC_int <- function(censo, sp, BNP_inter, BNP_alter = NULL, upto5m = T) {
  valid_input(censo, inherit = "sf", names = "Especie", geometry = "POINT")
  valid_input(sp, inherit = "character")
  valid_input(BNP_inter, inherit = c("sf"), names = req_names$BNP_afect, geometry = "POLYGON")
  valid_input(upto5m, inherit = "logical")

  codes <- censo %>% 
    dplyr::select(Especie) %>% 
    tibble::rowid_to_column("ID") %>% 
    dplyr::filter(Especie %>% stringi::stri_detect_fixed(sp)) %>%
    {if (is.null(BNP_alter)) {
      .[] %>% sf::st_intersection(sf::st_union(BNP_inter %>% dplyr::slice(0)))
    } else {
      .[] %>% sf::st_intersection(sf::st_union(BNP_alter)) 
    }} %>% 
    dplyr::pull(ID)
    
  ecc_int <- censo %>% 
    dplyr::select(Especie) %>% 
    tibble::rowid_to_column("ID") %>% 
    dplyr::filter(Especie %>% stringi::stri_detect_fixed(sp)) %>%
    dplyr::filter(!ID %in% codes) %>% 
    {if (upto5m) {
      .[] %>% sf::st_filter(BNP_inter, .pred = sf::st_is_within_distance, dist = 5) %>% 
        sf::st_join(BNP_inter, join = sf::st_nearest_feature)
    } else {
      .[] %>% sf::st_filter(BNP_inter, .pred = sf::st_intersects) %>% 
        sf::st_join(BNP_inter, join = sf::st_intersects)
    }} %>% 
    dplyr::mutate(Afectacion = 'Eliminación') %>% 
    dplyr::select(Especie, Afectacion, dplyr::matches("Obra"))

  return(ecc_int)
}

#' @rdname carto_digital
#' @export
get_uso <- function(uso_veg) {
  valid_input(uso_veg, inherit = "sf", names = req_names$uso_veg[1:3], geometry = "POLYGON")

  uso_veg %>%
    dplyr::mutate(
      Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2)
    ) %>%
    dplyr::select(Nom_ssubc, Uso, Subuso, Sup_ha) %>% 
    dplyr::arrange(Sup_ha)
}

#' @rdname carto_digital
#' @export
get_veg <- function(uso_veg) {
  valid_input(uso_veg, inherit = "sf", names = req_names$BNP_afect, geometry = "POLYGON")

  uso_veg %>%
    dplyr::mutate(
      Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2)
    ) %>%
    dplyr::select(Nom_ssubc, Formacion, Tipo_for, Subtipo_fo, dplyr::starts_with("ECC"), F_ley20283, BNP_ECC, Sup_ha) %>% 
    dplyr::arrange(Sup_ha)
}

#' @rdname carto_digital
#' @export
get_inv_flora <- function(BD_flora, BNP_cuenca, bd_lista, in_bnp_obra = T, obras = NULL) {
  valid_df(BD_flora, names = c('Parcela', 'UTM_E', 'UTM_N'))
  valid_input(BNP_cuenca, inherit = "sf", geometry = "POLYGON")
  valid_input(bd_lista, in_bnp_obra, inherit = "logical")
  valid_input(obras, inherit = "sf", geometry = "POLYGON")

  BD_flora %>%
    dplyr::count(Parcela, UTM_E, UTM_N) %>%
    dplyr::select(-n) %>% 
    na.omit() %>%
    sf::st_as_sf(coords = c('UTM_E', 'UTM_N'), crs = sf::st_crs(BNP_cuenca), remove = F) %>%
    {if (!bd_lista) {
      if (in_bnp_obra) {
      .[] %>% sf::st_intersection(sf::st_union(
        BNP_cuenca %>% 
          sf::st_filter(obras, .predicate = sf::st_relate, pattern = "T********")
      )) 
      } else {
        .[] %>% sf::st_intersection(sf::st_union(BNP_cuenca))
      }
    } else .[] } %>% 
    dplyr::arrange(Parcela)
}

#' @rdname carto_digital
#' @export
get_inv_fores <- function(BD_fore, BNP_cuenca, bd_lista = TRUE) {
  valid_df(BD_fore, names = c('Parcela', 'UTM_E', 'UTM_N'))
  valid_input(BNP_cuenca, inherit = "sf", geometry = "POLYGON")
  valid_input(bd_lista, inherit = "logical")

  BD_fore %>%
    dplyr::count(Parcela, UTM_E, UTM_N) %>%
    dplyr::select(-n) %>%
    sf::st_as_sf(coords = c('UTM_E', 'UTM_N'), crs = sf::st_crs(BNP_cuenca), remove = F) %>%
    {if (!bd_lista) {
      .[] %>% sf::st_intersection(sf::st_union(BNP_cuenca))
    } else .[] } %>% 
    dplyr::arrange(Parcela)
}

#' @rdname carto_digital
#' @export
get_prospeccion <- function(BD_flora, BD_fore, censo, sp) {
  valid_df(BD_flora)
  valid_df(BD_fore)
  valid_input(censo, inherit = "sf", names = "Especie", geometry = "POINT")
  valid_input(sp, inherit = "character")

  dplyr::bind_rows(
    BD_flora %>%
      dplyr::filter(stringi::stri_detect_regex(Especie, sp, case_insensitive = T)) %>%
      dplyr::count(Parcela, UTM_E, UTM_N) %>%
      sf::st_as_sf(coords = c('UTM_E', 'UTM_N'), crs = sf::st_crs(censo)),
    BD_fore %>%
      dplyr::filter(stringi::stri_detect_regex(Especie, sp, case_insensitive = T)) %>%
      dplyr::count(Parcela, UTM_E, UTM_N) %>%
      sf::st_as_sf(coords = c('UTM_E', 'UTM_N'), crs = sf::st_crs(censo)),
    censo %>% dplyr::filter(stringi::stri_detect_regex(Especie, sp, case_insensitive = T))
  ) %>%
    dplyr::group_by(geometry) %>%
    dplyr::tally() %>%
    dplyr::mutate(
      Especie = sp,
      UTM_E = sf::st_coordinates(geometry)[, 1] %>% janitor::round_half_up(),
      UTM_N = sf::st_coordinates(geometry)[, 2] %>% janitor::round_half_up()
    ) %>%
    dplyr::select(Especie, dplyr::starts_with("UTM"))
}

#' @rdname carto_digital
#' @export
get_BNP_alt_sin_censo <- function(BNP_alter, densidad) {
  if(!is.null(BNP_alter)) {
    valid_input(BNP_alter, inherit = "sf", names = req_names$BNP_afect, geometry = "POLYGON")
    
    if("Censado" %in% names(BNP_alter)){
      return(
        BNP_alter %>% 
          dplyr::filter(Censado %>% stringi::stri_detect_regex("si", case_insensitive = T)) %>% 
          dplyr::mutate(
            Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2),
            Densidad = densidad,
            Afectacion = "Alteración del hábitat",
            Ind_alterar = (Densidad * Sup_ha) %>% janitor::round_half_up()
          ) %>% 
          dplyr::mutate_at("Ind_alterar", ~ifelse(. == 0, 1, .)) %>% 
          dplyr::select(Nom_ssubc, Formacion, Tipo_for, Subtipo_fo, dplyr::starts_with("ECC"), BNP_ECC, F_ley20283, dplyr::contains("obra"), Afectacion, Densidad, Ind_alterar, Sup_ha) 
      )
    } else {
      return(invisible())
    }
  } else {
    return(invisible())
  }
}

#' @rdname carto_digital
#' @export
get_BNP_int_sin_censo <- function(BNP_inter, densidad) {
  if(!is.null(BNP_inter)) {
    valid_input(BNP_inter, inherit = "sf", names = req_names$BNP_afect, geometry = "POLYGON")
    
    if("Censado" %in% names(BNP_inter)){
      return(
        BNP_inter %>% 
          dplyr::filter(Censado %>% stringi::stri_detect_regex("si", case_insensitive = T)) %>% 
          dplyr::mutate(
            Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2),
            Densidad = densidad,
            Afectacion = "Eliminación",
            Ind_interv = (Densidad * Sup_ha) %>% janitor::round_half_up()
          ) %>% 
          dplyr::mutate_at("Ind_interv", ~ifelse(. == 0, 1, .)) %>% 
          dplyr::select(Nom_ssubc, Formacion, Tipo_for, Subtipo_fo, dplyr::starts_with("ECC"), BNP_ECC, F_ley20283, dplyr::contains("obra"), Afectacion, Densidad, Ind_interv, Sup_ha) 
      )
    } else {
      return(invisible())
    }
  } else {
    return(invisible())
  }
}

#' @rdname carto_digital
#' @export
get_BNP_antes <- function(BNP_cuenca) {
  valid_input(BNP_cuenca, inherit = "sf", geometry = "POLYGON")

  BNP_cuenca %>% 
    dplyr::mutate(ID = 1) %>% 
    dplyr::count(ID) %>% 
    sf::st_cast("POLYGON") %>% 
    dplyr::mutate(
      TIPO = "BNP_ANTES",
      Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2)
    ) %>% 
    dplyr::select(TIPO, ID, Sup_ha)
}

#' @rdname carto_digital
#' @export
get_BNP_despues <- function(BNP_cuenca, BNP_inter, BNP_alter) {
  valid_input(BNP_cuenca, BNP_inter, inherit = "sf", geometry = "POLYGON")

  BNP_cuenca %>% 
    dplyr::summarise(geometry = sf::st_union(geometry)) %>% 
    sf::st_difference(sf::st_union(BNP_inter)) %>%
    {if (!is.null(BNP_alter)) {
      .[] %>% sf::st_difference(sf::st_union(BNP_alter)) 
    } else .} %>% 
    sf::st_collection_extract('POLYGON') %>%
    sf::st_cast('POLYGON') %>% 
    dplyr::filter(sf::st_area(geometry) %>% units::drop_units() %>% janitor::round_half_up(1) > 0) %>% 
    dplyr::mutate(
      ID = 1,
      TIPO = 'DESPUÉS',
      Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2)
    ) %>%
    dplyr::select(TIPO, ID, Sup_ha)
}

#' @rdname carto_digital
#' @export
get_carto_digital <- function(
  uso_veg,
  sp,
  obras,
  censo,
  BNP_inter = NULL,
  BNP_alter = NULL,
  alt_ok = F,
  densidad = NULL,
  BD_flora = NULL,
  BD_fore = NULL,
  upto5m = T,
  bd_flora_lista = F,
  bd_fore_lista = F,
  in_bnp_obra = T,
  add_cam = F,
  add_hidro = F,
  fuente_hidro = c("MOP", "BCN"),
  add_CN = F
) {
  valid_input(
    uso_veg,
    inherit = "sf",
    names = req_names$uso_veg,
    geometry = "POLYGON"
  )
  stopifnot(
    "El shp debe contener un campo con las obras" =
    names(obras) %>%
      stringi::stri_detect_regex("obra", case_insensitive = T) %>%
      any()
  )
  valid_input(censo, inherit = "sf", names = "Especie", geometry = "POINT")
  valid_input(sp, inherit = "character")
  if (!is.null(BNP_inter)) {
    valid_input(
      BNP_inter,
      inherit = "sf",
      names = req_names$BNP_afect,
      geometry = "POLYGON"
    )
  }
  if (!is.null(BNP_alter)) {
    valid_input(
      BNP_alter,
      inherit = "sf",
      names = req_names$BNP_afect,
      geometry = "POLYGON"
    )
    valid_input(alt_ok, inherit = "logical")
  }
  if (!is.null(BD_flora)) {
    valid_df(BD_flora, names = c('Parcela', 'UTM_E', 'UTM_N'))
  }
  if (!is.null(BD_fore)) {
    valid_df(BD_fore, names = c('Parcela', 'UTM_E', 'UTM_N'))
  }
  valid_input(
    bd_flora_lista,
    bd_fore_lista,
    in_bnp_obra,
    upto5m,
    add_cam,
    add_hidro,
    add_CN,
    inherit = "logical"
  )
  if (add_hidro) {
    fuente_hidro <- match.arg(fuente_hidro)
  }

  cuenca <- get_cuenca(uso_veg)
  ubicacion <- get_ubicacion(obras, cuenca)
  obras_cuenca <- get_obras(obras, cuenca)
  BNP_cuenca <- get_BNP_cuenca(uso_veg, sp)
  BNP_intervenir <- get_BNP_intervencion(BNP_cuenca, obras, BNP_inter)
  BNP_alterar <- get_BNP_alterar(BNP_alter, BNP_cuenca, alt_ok)
  ECC_alt <- get_ECC_alt(BNP_alterar, censo, sp)
  ECC_int <- get_ECC_int(censo, sp, BNP_intervenir, BNP_alterar, upto5m)
  uso_cuenca <- get_uso(uso_veg)
  veg_cuenca <- get_veg(uso_veg)
  BNP_antes <- get_BNP_antes(BNP_cuenca)
  BNP_despues <- get_BNP_despues(BNP_cuenca, BNP_intervenir, BNP_alterar)
  BNP_alt_sin_censo <- get_BNP_alt_sin_censo(BNP_alter, densidad)
  BNP_int_sin_censo <- get_BNP_int_sin_censo(BNP_inter, densidad)

  if (add_cam) {
    caminos <- get_caminos(cuenca)
  }
  if (add_hidro) {
    hidro <- get_hidro(cuenca, fuente_hidro)
  }
  if (add_CN) {
    curv_niv <- get_CN(cuenca)
  }
  if (!is.null(BD_flora)) {
    inv_flora <- get_inv_flora(
      BD_flora,
      BNP_cuenca,
      bd_flora_lista,
      in_bnp_obra,
      obras
    )
  }
  if (!is.null(BD_fore)) {
    inv_forestal <- get_inv_fores(BD_fore, BNP_cuenca, bd_fore_lista)
  }
  if (!is.null(BD_flora) & !is.null(BD_fore)) {
    prospeccion <- get_prospeccion(BD_flora, BD_fore, censo, sp)
  }

  lista <- rlang::list2(
    "Cuenca_de_estudio" = cuenca,
    "Area_de_proyecto_Ubicación" = ubicacion,
    "Area_de_proyecto_Obras" = obras_cuenca,
    !!paste0('BNP_', stringi::stri_extract_first_words(sp), '_Cuenca') := BNP_cuenca,
    !!paste0('BNP_', stringi::stri_extract_first_words(sp), '_a_Intervenir') := BNP_intervenir,
    !!paste0('BNP_', stringi::stri_extract_first_words(sp), '_a_Alterar.shp') := BNP_alterar,
    !!paste0('Censo_', stringi::stri_extract_first_words(sp), '_a_Intervenir.shp') := ECC_int,
    !!paste0('Censo_', stringi::stri_extract_first_words(sp), '_a_Alterar.shp') := ECC_alt,
    "Uso_actual_de_la_tierra" = uso_cuenca,
    "Vegetación_en_la_cuenca" = veg_cuenca,
    !!paste0('BNP_fragmentacion_', stringi::stri_extract_first_words(sp), '_Antes.shp') := BNP_antes,
    !!paste0('BNP_fragmentacion_', stringi::stri_extract_first_words(sp), '_Despues.shp') := BNP_despues,
    !!paste0('Estimacion_Alteracion_', stringi::stri_extract_first_words(sp)) := BNP_alt_sin_censo,
    !!paste0('Estimacion_Intervencion_', stringi::stri_extract_first_words(sp)) := BNP_int_sin_censo,
    "Caminos_cuenca" = if (add_cam) caminos else NULL,
    "Hidrografia_cuenca" = if (add_hidro) hidro else NULL,
    "Curvas_de_Nivel_cuenca" = if (add_CN) curv_niv else NULL,
    "UTM_Inventarios_Flora" = if (!is.null(BD_flora)) inv_flora else NULL,
    "UTM_Inventarios_Forestales" = if (!is.null(BD_fore)) inv_forestal else NULL,
    "UTM_Registros_Porlieria" = if (!is.null(BD_flora) && !is.null(BD_fore)) prospeccion else NULL
  )
  lista_final <- purrr::compact(lista)
  
  return(lista_final)
}