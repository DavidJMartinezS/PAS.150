
BD <- "~/CNX955/00. Insumos/04. BDD/CNX955-BD_Flora_20260318.xlsx" 
bd_lista = FALSE 
BNP_cuenca = get_BNP_cuenca(uso_veg = sf::read_sf("~/CNX955/01. Insumos x cuenca/C16/Capa_uso_vegetacion_C16.shp"), sp = "Porlieria chilensis")
in_bnp_obra = TRUE
obras = sf::read_sf("~/CNX955/00. Insumos/10. Layout obras/KIM753_A2_Area_Intervencion_Por_Obra_Suelos_Catenarias_20250611.shp")

prepare_bd_flora_150(
  BD = "~/CNX955/00. Insumos/04. BDD/CNX955-BD_Flora_20260318.xlsx", 
  bd_lista = FALSE, 
  BNP_cuenca = get_BNP_cuenca(uso_veg = sf::read_sf("~/CNX955/01. Insumos x cuenca/C16/Capa_uso_vegetacion_C16.shp"), sp = "Porlieria chilensis"),
  in_bnp_obra = TRUE, 
  obras = sf::read_sf("~/CNX955/00. Insumos/10. Layout obras/KIM753_A2_Area_Intervencion_Por_Obra_Suelos_Catenarias_20250611.shp")
)
bd_frag <- BD_fragmentacion(
  sf_uso = sf::read_sf(
    "~/CNX955/01. Insumos x cuenca/C12/Capa_uso_vegetacion_C12.shp"
  ),
  sf_obras = sf::read_sf(
    "~/CNX955/00. Insumos/10. Layout obras/KIM753_A2_Area_Intervencion_Por_Obra_Suelos_Catenarias_20250611.shp"
  ),
  path_frag = "~/CNX955/01. Insumos x cuenca/C12/FragStats C12/TABLA/JOIN_PATCH25_PROX1000.xlsx",
  ECC = "Porlieria chilensis",
  subusos_noveg = c(),
  alteracion = T,
  spp_acomp = BD_biodiversidad(
    BD_flora = prepare_bd_flora_150(
      BD = "~/CNX955/00. Insumos/04. BDD/CNX955-BD_Flora_20260318.xlsx",
      bd_lista = FALSE,
      BNP_cuenca = get_BNP_cuenca(
        uso_veg = sf::read_sf("~/CNX955/01. Insumos x cuenca/C12/Capa_uso_vegetacion_C12.shp"),
        sp = "Porlieria chilensis"
      ),
      in_bnp_obra = TRUE,
      obras = sf::read_sf(
        "~/CNX955/00. Insumos/10. Layout obras/KIM753_A2_Area_Intervencion_Por_Obra_Suelos_Catenarias_20250611.shp"
      )
    ), 
    sp = "Porlieria chilensis", 
    nom_ssubc = get_cuenca(sf::read_sf("~/CNX955/01. Insumos x cuenca/C12/Capa_uso_vegetacion_C12.shp"))$NOM_SSUBC
  ) %>% .$spp_acomp,
  prop = BD_inventarios(
    BD_fore = prepare_bd_fore_150(
      "~/CNX955/00. Insumos/04. BDD/CNX955-BD_Inventarios.xlsx", 
      bd_lista = FALSE, 
      BNP_cuenca = get_BNP_cuenca(
        uso_veg = sf::read_sf("~/CNX955/01. Insumos x cuenca/C12/Capa_uso_vegetacion_C12.shp"),
        sp = "Porlieria chilensis"
      )
    ), 
    BNP_cuenca = get_BNP_cuenca(
      uso_veg = sf::read_sf("~/CNX955/01. Insumos x cuenca/C12/Capa_uso_vegetacion_C12.shp"),
      sp = "Porlieria chilensis"
    ), 
    sp = "Porlieria chilensis"
  ) %>% .$prop,
  estadisticos = estadisticos_PAS150(a = 0.05, A = sum(get_BNP_cuenca(
      uso_veg = sf::read_sf("~/CNX955/01. Insumos x cuenca/C12/Capa_uso_vegetacion_C12.shp"),
      sp = "Porlieria chilensis"
    ) %>% .$Sup_ha), data = BD_inventarios(
    BD_fore = prepare_bd_fore_150(
      "~/CNX955/00. Insumos/04. BDD/CNX955-BD_Inventarios.xlsx", 
      bd_lista = FALSE, 
      BNP_cuenca = get_BNP_cuenca(
        uso_veg = sf::read_sf("~/CNX955/01. Insumos x cuenca/C12/Capa_uso_vegetacion_C12.shp"),
        sp = "Porlieria chilensis"
      )
    ), 
    BNP_cuenca = get_BNP_cuenca(
      uso_veg = sf::read_sf("~/CNX955/01. Insumos x cuenca/C12/Capa_uso_vegetacion_C12.shp"),
      sp = "Porlieria chilensis"
    ), 
    sp = "Porlieria chilensis"
  ) %>% .$BD_Nha, sp = "Porlieria chilensis")
)
read_xlsx("~/CNX955/00. Insumos/04. BDD/CNX955-BD_Flora_20260318.xlsx") %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(Especie %>% stringr::str_detect("Chorizanthe.*flav") & Cob_BB == "+")

bd_bio <- BD_biodiversidad(
  BD_flora = prepare_bd_flora_150(
    BD = "~/CNX955/00. Insumos/04. BDD/CNX955-BD_Flora_20260318.xlsx",
    bd_lista = FALSE,
    BNP_cuenca = get_BNP_cuenca(
      uso_veg = sf::read_sf("~/CNX955/01. Insumos x cuenca/C03/Capa_uso_vegetacion_C03.shp"),
      sp = "Porlieria chilensis"
    ),
    in_bnp_obra = TRUE,
    obras = sf::read_sf(
      "~/CNX955/00. Insumos/10. Layout obras/KIM753_A2_Area_Intervencion_Por_Obra_Suelos_Catenarias_20250611.shp"
    )
  ), 
  sp = "Porlieria chilensis", 
  nom_ssubc = get_cuenca(sf::read_sf("~/CNX955/01. Insumos x cuenca/C03/Capa_uso_vegetacion_C03.shp"))$NOM_SSUBC
)
bd_bio$wb$open()
tabla_indices <- bd_bio$tabla_indices
gt_indices(bd_bio$tabla_indices)

bslib::card(shinyWidgets::virtualSelectInput(
  inputId = "Id103",
  label = "Add a description:",
  choices = prepare_choices(
    data.frame(
      name = state.name,
      abb = state.abb,
      region = state.region,
      division = state.division
    ),
    label = name,
    value = abb,
    description = division
  ),
  multiple = FALSE,
  hasOptionDescription = TRUE,
  width = "100%",
  dropboxWrapper = "body"
))

dplyr::case_when(
  nrow(iris) == 0 ~ 3,
  nrow(iris) == 1 ~ 2,
  .default = 1,
)
dplyr::case_match(nrow(), 0 ~ 3, 1 ~ 2,.default = 1)

shinyWidgets::prepare_choices(
  df_sp %>% 
    dplyr::mutate(desc = paste0("RCE: ", RCE, " | Rep: ", Tipo_rep)) %>%
    dplyr::bind_rows(tibble::tibble(Especie = "Agregar nueva...", desc = "Añadir una nueva especie a la lista")) %>%
    dplyr::mutate(label_styled = ifelse(
      Especie == "Agregar nueva...",
      "<span style='color: #3d93ab; font-weight: bold; border-top: 1px solid #ddd; display: block; padding-top: 5px;'>Agregar nueva...</span>",
      Especie
    )),
  label = label_styled,
  value = Especie,
  description = desc
)

carto_digital <- get_carto_digital(
  uso_veg = sf::read_sf("~/CNX955/01. Insumos x cuenca/C03/Capa_uso_vegetacion_C03.shp"), 
  sp = "Porlieria chilensis", 
  obras = sf::read_sf("~/CNX955/00. Insumos/10. Layout obras/KIM753_A2_Area_Intervencion_Por_Obra_Suelos_Catenarias_20250611.shp"), 
  censo = sf::read_sf("~/CNX955/00. Insumos/03. Compilado censo/Compilado_20260318/Compilado_censos_20260318.shp"), 
  # BNP_alter = sf::read_sf("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/C03_Alteracion.shp"),
  BD_flora = prepare_bd_flora_150(
    BD = "~/CNX955/00. Insumos/04. BDD/CNX955-BD_Flora_20260318.xlsx", 
    bd_lista = FALSE, 
    BNP_cuenca = get_BNP_cuenca(uso_veg = sf::read_sf("~/CNX955/01. Insumos x cuenca/C16/Capa_uso_vegetacion_C16.shp"), sp = "Porlieria chilensis"),
    in_bnp_obra = TRUE, 
    obras = sf::read_sf("~/CNX955/00. Insumos/10. Layout obras/KIM753_A2_Area_Intervencion_Por_Obra_Suelos_Catenarias_20250611.shp")
  ), 
  BD_fore = prepare_bd_fore_150(
    BD = "~/CNX955/00. Insumos/04. BDD/CNX955-BD_Inventarios.xlsx", 
    bd_lista = F, 
    BNP_cuenca = get_BNP_cuenca(uso_veg = sf::read_sf("~/CNX955/01. Insumos x cuenca/C16/Capa_uso_vegetacion_C16.shp"), sp = "Porlieria chilensis")
  )
)

gt_ecc_inter(
  ecc_inter = purrr::keep(carto_digital, .p = stringi::stri_detect_regex(names(carto_digital), "Censo.*Inter", case_insensitive = T)) %>% purrr::pluck(1), 
  BNP_int_sin_censo = purrr::keep(carto_digital, .p = stringi::stri_detect_regex(names(carto_digital), "Estimación_Inter", case_insensitive = T)) %>% purrr::pluck(1),
  col_obras = c("Obra", "Nom_obra", "Obra_detal")
)

gt_sup_inter(
  obras = sf::read_sf("~/CNX955/00. Insumos/10. Layout obras/KIM753_A2_Area_Intervencion_Por_Obra_Suelos_Catenarias_20250611.shp"), 
  BNP_inter = purrr::keep(carto_digital, .p = stringi::stri_detect_regex(names(carto_digital), "BNP.*a_Inter", case_insensitive = T)) %>% purrr::pluck(1), 
  BNP_alterar = NULL,
  col_obras = "Obra"
)

bd_fore <- BD_inventarios(
  BD_fore = prepare_bd_fore_150(
    "~/CNX955/00. Insumos/04. BDD/CNX955-BD_Inventarios.xlsx", 
    bd_lista = FALSE, 
    BNP_cuenca = get_BNP_cuenca(
      uso_veg = sf::read_sf("~/CNX955/01. Insumos x cuenca/C12/Capa_uso_vegetacion_C12.shp"),
      sp = "Porlieria chilensis"
    )
  ), 
  BNP_cuenca = get_BNP_cuenca(
    uso_veg = sf::read_sf("~/CNX955/01. Insumos x cuenca/C12/Capa_uso_vegetacion_C12.shp"),
    sp = "Porlieria chilensis"
  ), 
  sp = "Porlieria chilensis"
) 

bd_fore$wb$open()

stringr::str_c("C", stringr::str_pad(c(11:12, 16:20, 28), width = 2, side = "left", pad = "0")) %>%
  purrr::map(function(x, sp = "Porlieria chilensis") {
    y <- stringr::str_extract(x, "\\d+")
    BNP_cuenca <- get_BNP_cuenca(
      uso_veg = sf::read_sf(sprintf("~/CNX955/01. Insumos x cuenca/%s/Capa_uso_vegetacion_%s.shp", x, x)), 
      sp = sp
    ) %>% sf::st_transform(32719)
    prospeccion <- get_prospeccion(
      BD_flora = prepare_bd_flora_150(
        BD = "~/CNX955/00. Insumos/04. BDD/CNX955-BD_Flora_20260318.xlsx", 
        bd_lista = FALSE, 
        BNP_cuenca = BNP_cuenca,
        in_bnp_obra = TRUE, 
        obras = sf::read_sf("~/CNX955/00. Insumos/10. Layout obras/KIM753_A2_Area_Intervencion_Por_Obra_Suelos_Catenarias_20250611.shp")
      ),  
      BD_fore = prepare_bd_fore_150(
        "~/CNX955/00. Insumos/04. BDD/CNX955-BD_Inventarios.xlsx", 
        bd_lista = FALSE, 
        BNP_cuenca = BNP_cuenca
      ), 
      censo = sf::read_sf("~/CNX955/00. Insumos/03. Compilado censo/Compilado_20260318/Compilado_censos_20260318.shp"),  
      sp = sp, 
      BNP_cuenca = BNP_cuenca
    )
    sf::write_sf(prospeccion, sprintf("~/CNX955/01. Insumos x cuenca/%s/Cartografia_digital/UTM_Registros_Porlieria_%s.shp", x, x))
    sf::write_sf(prospeccion, sprintf("N:/CNX955 - Solicitud Resolución Fundada KILO/009_Desarrollo/00. INSUMOS/10. Cartografias x cuenca/%s/Cartografia_digital/UTM_Registros_Porlieria_%s.shp", x, x))
    sf::write_sf(prospeccion, sprintf("N:/CNX955 - Solicitud Resolución Fundada KILO/009_Desarrollo/03. Actualización Informes de experto/Apendice %s.C%s IE_Porl.chil/APENDICE %s.C%s-3 Cartografía digital/11.Prospección Pedestre/SHP/UTM_Registros_Porlieria_%s.shp", y, y, y, y, x))

    sf::write_sf(
      shp %>% 
        dplyr::mutate(Description = leafpop::popupTable(sf::st_drop_geometry(.)), Name = "Especie") %>% 
        dplyr::select(Name, Description), 
    sprintf("~/CNX955/01. Insumos x cuenca/%s/Cartografia_digital/UTM_Registros_Porlieria_%s.kml", x, x))

    zip::zip(
      zipfile = sprintf("~/CNX955/01. Insumos x cuenca/%s/Cartografia_digital/UTM_Registros_Porlieria_%s.kmz", x, x),
      files = sprintf("~/CNX955/01. Insumos x cuenca/%s/Cartografia_digital/UTM_Registros_Porlieria_%s.kml", x, x)
    )
    zip::zip(
      zipfile = sprintf("N:/CNX955 - Solicitud Resolución Fundada KILO/009_Desarrollo/00. INSUMOS/10. Cartografias x cuenca/%s/Cartografia_digital/UTM_Registros_Porlieria_%s.kmz", x, x),
      files = sprintf("~/CNX955/01. Insumos x cuenca/%s/Cartografia_digital/UTM_Registros_Porlieria_%s.kml", x, x)
    )
    zip::zip(
      zipfile = sprintf("N:/CNX955 - Solicitud Resolución Fundada KILO/009_Desarrollo/03. Actualización Informes de experto/Apendice %s.C%s IE_Porl.chil/APENDICE %s.C%s-3 Cartografía digital/11.Prospección Pedestre/KMZ/UTM_Registros_Porlieria_%s.kmz", y, y, y, y, x),
      files = sprintf("~/CNX955/01. Insumos x cuenca/%s/Cartografia_digital/UTM_Registros_Porlieria_%s.kml", x, x)
    )
    file.remove(sprintf("~/CNX955/01. Insumos x cuenca/%s/Cartografia_digital/UTM_Registros_Porlieria_%s.kml", x, x))
  })

x <- read_sf("~/CNX955/01. Insumos x cuenca/C03/Cartografia_digital/Area_de_proyecto_Obras_C03.shp")
z <- "asd"
create_kmz <- T
temp_dir <- tempdir()
wd <- getwd()
setwd("~/R/PAS.150")
switch(
  "sf",
  sf = {
    sf::write_sf(x, paste0(tools::file_path_sans_ext(z), ".shp"))
    if(create_kmz) {
      sf::write_sf(x, paste0(tools::file_path_sans_ext(z), ".kml"), delete_dsn = TRUE)
      zip::zip(zipfile = paste0(tools::file_path_sans_ext(z), ".kmz"), files = paste0(tools::file_path_sans_ext(z), ".kml"))
      file.remove(paste0(tools::file_path_sans_ext(z), ".kml"))
    }
  },
  wb = openxlsx2::wb_save(x, paste0(tools::file_path_sans_ext(z), ".xlsx"), overwrite = T),
  xlsx = openxlsx2::write_xlsx(x, paste0(tools::file_path_sans_ext(z), ".xlsx"))
)

dir(temp_dir)

sf::read_sf("~/CNX955/01. Insumos x cuenca/C03/Cartografia_digital/UTM_Inventarios_Flora_C03.shp") %>% 
  dplyr::mutate(Description = leafpop::popupTable(sf::st_drop_geometry(.))) %>% 
  select(Description) %>% st_drop_geometry()

downloadButton(
  outputId = "down_btn",
  icon = icon("file-csv"),
  label = "label"
) 

censo <- read_sf("~/CNX955/00. Insumos/03. Compilado censo/Compilado_20260420/Compilado_censos_20260420.shp")
obras <- read_sf("~/CNX955/00. Insumos/10. Layout obras/KIM753_A2_Area_Intervencion_Por_Obra_Suelos_Catenarias_20250611.shp")
sp <- "Porlieria chilensis"

stringr::str_c("C", stringr::str_pad(c(3, 11:12, 16:20, 28), width = 2, side = "left", pad = "0")) %>% 
  map(function(x) {
    BNP_intervenir <- list.files("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/", pattern = sprintf("%s_Inter.*shp$", x), full.names = T) %>% 
      read_sf() %>% prepare_uso_veg() %>% get_BNP_intervencion(BNP_inter = .)
    BNP_alterar <- list.files("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/", pattern = sprintf("%s_Alter.*shp$", x), full.names = T) %>% 
      read_sf() %>% prepare_uso_veg() %>% get_BNP_alterar(BNP_alter = ., alt_ok = T)
    
    ECC_alt <- get_ECC_alt(BNP_alter = BNP_alterar, censo = censo, sp = sp)
    ECC_int <- get_ECC_int(
      censo = censo,
      sp = sp,
      BNP_inter = BNP_intervenir,
      BNP_alter = BNP_alterar,
      upto5m = T
    )
    sp_code <- stringi::stri_extract_first_words(sp)
    BNP_intervenir %>% write_sf(file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('BNP_%s_a_Intervenir_%s.shp', sp_code, x)))
    BNP_intervenir %>% prepare_kml(basename = sprintf('BNP_%s_a_Intervenir_%s.shp', sp_code, x)) %>% 
      write_sf(file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('BNP_%s_a_Intervenir_%s.kml', sp_code, x)))
    zip(
      zipfile = file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('BNP_%s_a_Intervenir_%s.kmz', sp_code, x)), 
      files = file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('BNP_%s_a_Intervenir_%s.kml', sp_code, x))
    )
    file.remove(file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('BNP_%s_a_Intervenir_%s.kml', sp_code, x)))

    BNP_alterar %>% write_sf(file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('BNP_%s_a_Alterar_%s.shp', sp_code, x)))
    BNP_alterar %>% prepare_kml(basename = sprintf('BNP_%s_a_Alterar_%s.shp', sp_code, x)) %>% 
      write_sf(file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('BNP_%s_a_Alterar_%s.kml', sp_code, x)))
    zip(
      zipfile = file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('BNP_%s_a_Alterar_%s.kmz', sp_code, x)), 
      files = file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('BNP_%s_a_Alterar_%s.kml', sp_code, x))
    )
    file.remove(file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('BNP_%s_a_Alterar_%s.kml', sp_code, x)))

    ECC_int %>% write_sf(file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('Censo_%s_a_Intervenir_%s.shp', sp_code, x)))
    ECC_int %>% prepare_kml(basename = sprintf('Censo_%s_a_Intervenir_%s.shp', sp_code, x)) %>% 
      write_sf(file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('Censo_%s_a_Intervenir_%s.kml', sp_code, x)))
    zip(
      zipfile = file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('Censo_%s_a_Intervenir_%s.kmz', sp_code, x)), 
      files = file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('Censo_%s_a_Intervenir_%s.kml', sp_code, x))
    )
    file.remove(file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('Censo_%s_a_Intervenir_%s.kml', sp_code, x)))

    ECC_alt %>% write_sf(file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('Censo_%s_a_Alterar_%s.shp', sp_code, x)))
    ECC_alt %>% prepare_kml(basename = sprintf('Censo_%s_a_Alterar_%s.shp', sp_code, x)) %>% 
      write_sf(file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('Censo_%s_a_Alterar_%s.kml', sp_code, x)))
    zip(
      zipfile = file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('Censo_%s_a_Alterar_%s.kmz', sp_code, x)), 
      files = file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('Censo_%s_a_Alterar_%s.kml', sp_code, x))
    )
    file.remove(file.path("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA", sprintf('Censo_%s_a_Alterar_%s.kml', sp_code, x)))
  })

id_ssubc <- "C18"
gt_ecc_inter(
  ecc_inter = read_sf(sprintf("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA/Censo_Porlieria_a_Intervenir_%s.shp", id_ssubc)),
  BNP_int_sin_censo = NULL,
  col_obras = "Obra" 
)
gt_ecc_alter(
  ecc_alter = read_sf(sprintf("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA/Censo_Porlieria_a_Alterar_%s.shp", id_ssubc)), 
  BNP_alt_sin_pto = NULL, 
  col_obras = "Obra"
)
gt_sup_inter(
  obras = obras, 
  BNP_inter = read_sf(sprintf("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA/BNP_Porlieria_a_Intervenir_%s.shp", id_ssubc)), 
  BNP_alterar = read_sf(sprintf("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/ENTREGA/BNP_Porlieria_a_Alterar_%s.shp", id_ssubc)), 
  col_obras = c("Obra", "Obra_detal")
)

uso_veg <- read_sf("~/CNX955/01. Insumos x cuenca/C19/Capa_uso_vegetacion_C19.shp")
sp <- "Porlieria chilensis"
dir <- "c:/Users/dmartinez/Documents/CNX955/01. Insumos x cuenca/C29/FragStats"
path_antes <- "c:/Users/dmartinez/Documents/CNX955/01. Insumos x cuenca/C29/Cartografia_digital/BNP_fragmentación_Porlieria_Antes_C29.shp"
path_despues <- "c:/Users/dmartinez/Documents/CNX955/01. Insumos x cuenca/C29/Cartografia_digital/BNP_fragmentación_Porlieria_Después_C29.shp"


get_BNP_antes(BNP_cuenca = read_sf("N:/MLP612_A2 - Adenda 2 proyecto extensión vida útil (EVU)/009_Desarrollo/24. Actualización PAS 150/5.11.1_PAS150_RevB/Apéndice 5.11.3 IE Dri/Anexo 2. Cartografía digital_Drimys/06.BNP Drimys cuenca/SHP/MLP612_A2_BNP_Drimys_cuenca_Pupio.shp") %>% st_zm()) %>% 
  write_sf("c:/Users/dmartinez/OneDrive - SERVICIOS E INVERSIONES PRAMAR LTDA/MLP612_A2/PAS 150/FragStats Drimys/MLP612_A2-BNP_fragmentación_Drimys_Antes.shp")

get_BNP_despues(
  BNP_cuenca = read_sf("N:/MLP612_A2 - Adenda 2 proyecto extensión vida útil (EVU)/009_Desarrollo/24. Actualización PAS 150/5.11.1_PAS150_RevB/Apéndice 5.11.3 IE Dri/Anexo 2. Cartografía digital_Drimys/06.BNP Drimys cuenca/SHP/MLP612_A2_BNP_Drimys_cuenca_Pupio.shp") %>% st_zm(),
  BNP_inter = read_sf("N:/MLP612_A2 - Adenda 2 proyecto extensión vida útil (EVU)/009_Desarrollo/24. Actualización PAS 150/5.11.1_PAS150_RevB/Apéndice 5.11.3 IE Dri/Anexo 2. Cartografía digital_Drimys/06.BNP Drimys cuenca/SHP/MLP612_A2_BNP_Drimys_a_intervenir.shp") %>% st_zm(),
  BNP_alter = read_sf("N:/MLP612_A2 - Adenda 2 proyecto extensión vida útil (EVU)/009_Desarrollo/24. Actualización PAS 150/5.11.1_PAS150_RevB/Apéndice 5.11.3 IE Dri/Anexo 2. Cartografía digital_Drimys/06.BNP Drimys cuenca/SHP/MLP612_A2_BNP_Drimys_a_alterar.shp") %>% st_zm()
) %>% 
  write_sf("c:/Users/dmartinez/OneDrive - SERVICIOS E INVERSIONES PRAMAR LTDA/MLP612_A2/PAS 150/FragStats Drimys/MLP612_A2-BNP_fragmentación_Drimys_Después.shp")



utm_inv <- get_inv_fores(
  BD_fore = read_xlsx("~/CNX955/01. Insumos x cuenca/C20/BD inv C20.xlsx"), 
  BNP_cuenca = get_BNP_cuenca(read_sf("~/CNX955/01. Insumos x cuenca/C20/Capa_uso_vegetacion_C20.shp"), sp = "Porlieria chilensis"), 
  bd_lista = T
) %>% 
  mutate_at(2:3, ~as.integer(round_half_up(.))) 

utm_inv %>% write_sf("~/CNX955/01. Insumos x cuenca/C20/Cartografia_digital/UTM_Inventarios_Forestales_C20.shp")
utm_inv %>% prepare_kml(basename = "Inventarios_Forestales") %>% write_sf("~/CNX955/01. Insumos x cuenca/C20/Cartografia_digital/UTM_Inventarios_Forestales_C20.kml")
zip("~/CNX955/01. Insumos x cuenca/C20/Cartografia_digital/UTM_Inventarios_Forestales_C20.kmz", "~/CNX955/01. Insumos x cuenca/C20/Cartografia_digital/UTM_Inventarios_Forestales_C20.kml")

read_xlsx("~/CNX955/01. Insumos x cuenca/C20/BD Inventarios forestales BNP Porlieria C20.xlsx", sheet = "BD_Nha") %>% as_tibble() %>% 
  write_excel_csv2("~/CNX955/01. Insumos x cuenca/C20/BD_Nha_C20.csv")

directorio <- "C:\\Users\\dmartinez\\Documents//datos_temp/prueba kmz"
capas <- list.files(directorio, pattern = ".shp$", full.names = F, recursive = T)
x <- capas[1]
capas[2] %>% 
  purrr::map(function(x) {
    sf::read_sf(file.path(directorio, x)) %>% prepare_kml(basename = tools::file_path_sans_ext(basename(x))) %>% 
      sf::write_sf(tools::file_path_as_absolute(file.path(directorio, paste0(tools::file_path_sans_ext(x), ".kml"))))
    zip::zip(
      zipfile = tools::file_path_as_absolute(file.path(directorio, paste0(tools::file_path_sans_ext(x), ".kmz"))), 
      files = tools::file_path_as_absolute(file.path(directorio, paste0(tools::file_path_sans_ext(x), ".kml")))
    )
    file.remove(file.path(directorio, paste0(tools::file_path_sans_ext(x), ".kml")))
  })
bslib::card(
  bslib::tooltip(
    trigger = list(
      "¿Crear capa de caminos?",
      bsicons::bs_icon("info-circle", class = "text-info")
    ),
    "Caminos serán creados a partir de la red vial del MOP actualizado al 07-02-2024"
  )
)

get_cuenca(uso_veg = read_sf("c:/Users/dmartinez/OneDrive - SERVICIOS E INVERSIONES PRAMAR LTDA/MLP612_A2/Fragstats/MLP612_A2_Uso_actual_cuenca_Pupio.shp") %>% st_zm())

uso_veg = read_sf("c:/Users/dmartinez/OneDrive - SERVICIOS E INVERSIONES PRAMAR LTDA/MLP612_A2/Fragstats/MLP612_A2_Uso_actual_cuenca_Pupio.shp") %>% st_zm()
uso_veg = read_sf("~/CNX955/01. Insumos x cuenca/C12/Capa_uso_vegetacion_C12.shp") %>% st_zm()
uso_join <- uso_veg %>% sf::st_join(cuencas %>% dplyr::select(NOM_SSUBC), largest = T)

uso_join %>% dplyr::pull(NOM_SSUBC) %>% table() %>% sort(decreasing = T) %>% names() %>% .[1]
uso_join %>% group_by(NOM_SSUBC) %>% add_count(name = "count") %>% slice_max(count, with_ties = F) %>% .$NOM_SSUBC

uso_veg %>% sf::st_join(cuencas %>% dplyr::select(NOM_SSUBC), largest = T)

system.time(uso_join %>% dplyr::pull(NOM_SSUBC) %>% table() %>% sort(decreasing = T) %>% names() %>% .[1])
system.time(uso_join %>% group_by(NOM_SSUBC) %>% add_count(name = "count") %>% slice_max(count, with_ties = F) %>% .$NOM_SSUBC)
system.time(uso_veg %>% sf::st_join(cuencas %>% dplyr::select(NOM_SSUBC), largest = T))
system.time(uso_veg %>% summarise(geometry = st_union(st_geometry(.))) %>% sf::st_join(cuencas %>% dplyr::select(NOM_SSUBC), largest = T))
uso_veg %>% 
  st_set_geometry("shape") %>% 
  dplyr::summarise(geometry = sf::st_union(sf::st_geometry(.))) %>%
  dplyr::mutate(
    Centro_X = sf::st_coordinates(sf::st_centroid(geometry))[, 1] %>% janitor::round_half_up() %>% as.integer(),
    Centro_Y = sf::st_coordinates(sf::st_centroid(geometry))[, 2] %>% janitor::round_half_up() %>% as.integer(),
    Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2),
  ) 

sf_uso <- read_sf("c:/Users/dmartinez/OneDrive - SERVICIOS E INVERSIONES PRAMAR LTDA/MLP612_A2/Fragstats/Porlieria/MLP612_A2_Uso_actual_cuenca_Pupio.shp") %>% 
  st_zm() %>% 
  st_transform(32719) %>% 
  st_make_valid() %>%
  st_collection_extract("POLYGON") %>% 
  prepare_uso_veg()

wb_lsm <- BD_fragmentacion(
  sf_uso = sf_uso,
  sf_obras = read_sf("c:/Users/dmartinez/OneDrive - SERVICIOS E INVERSIONES PRAMAR LTDA/MLP612_A2/Fragstats/Porlieria/MLP612_A2_Area_de_Proyecto.shp"),
  path_frag = "c:/Users/dmartinez/OneDrive - SERVICIOS E INVERSIONES PRAMAR LTDA/MLP612_A2/Fragstats/Porlieria/FragStats/TABLA/JOIN_PATCH25_PROX1000.xlsx",
  ECC = "Porlieria chilensis", RCE = "VU", Tipo_rep = "monoica",
  subusos_noveg = sort(unique(sf_uso$Subuso)) %>% 
    subset(stri_detect_regex(stri_trans_general(., "Latin-ASCII"), "rocoso|camino|zona|rio|lago|mineria|agricola|otros|ambiental|duna|playa", case_insensitive = T)),
  portada_opts = portada_opts(plantilla = "MLP612", etapa_proj = "AdComp"),
  alteracion = T,
  spp_acomp = NULL,
  prop = NULL,
  estadisticos = NULL
)
wb_lsm$wb$open()
wb_save(wb_lsm$wb, "c:/Users/dmartinez/OneDrive - SERVICIOS E INVERSIONES PRAMAR LTDA/MLP612_A2/Fragstats/Porlieria/MLP612_A2 - Análisis de fragmentación Porlieria.xlsx")

readxl::read_xls("C:/Users/dmartinez/OneDrive - SERVICIOS E INVERSIONES PRAMAR LTDA/KIM753_1 PAS 150/ANEXO 10-21_Inf.Experto_Porl.chil_SSUBC13.RevB/Apéndice 10-21G. Respaldo métricas de fragmentación/XLS/RESULTADOS_FragStat/TABLA/JOIN_PATCH13_PROX1000.xls") %>% 
  writexl::write_xlsx("~/datos_temp/JOIN_PATCH13_PROX1000.xls")

readxl::read_xls("~/datos_temp/JOIN_PATCH13_PROX1000.xls") # SI
readxl::read_xlsx("~/datos_temp/JOIN_PATCH13_PROX1000.xlsx") # NO

openxlsx2::read_xlsx("~/datos_temp/JOIN_PATCH13_PROX1000.xls") # NO
openxlsx2::read_xlsx("~/datos_temp/JOIN_PATCH13_PROX1000.xlsx") # SI

readr::read_log("~/datos_temp/JOIN_PATCH13_PROX1000.xlsx")
readr::read_csv("~/datos_temp/JOIN_PATCH13_PROX1000.xlsx")

BD_fore <- prepare_bd_fore_150(
  BD = "~/CNX955/00. Insumos/04. BDD/CNX955-BD_Inventarios.xlsx", 
  bd_lista = F, 
  BNP_cuenca = get_BNP_cuenca(
    uso_veg = prepare_uso_veg(read_sf("~/CNX955/01. Insumos x cuenca/C06/Capa_uso_vegetacion_C06.shp")), 
    sp = "Porlieria chilensis"
  )
)
get_densidad(BD_fore, "Porlieria chilensis")

BNP_alter <- read_sf("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/Tanda 1/C03_Alteracion.shp") %>% prepare_uso_veg()

dat_4326 <- BNP_alter %>%
  sf::st_transform(4326) %>%
  dplyr::filter(!sf::st_is_empty(.))

map <- leaflet::leaflet(dat_4326) %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron,  group = "CartoDB.Positron") %>%
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
  leaflet::addLayersControl(
    baseGroups = c("CartoDB.Positron", "Esri.WorldImagery"),
    options    = leaflet::layersControlOptions(collapsed = FALSE)
  )

geom_type <- as.character(sf::st_geometry_type(dat_4326, by_geometry = FALSE))

if (any(grepl("POINT", geom_type))) {
  map <- map %>% leaflet::addCircleMarkers(
    color = "#920000", radius = 6, stroke = TRUE, fillOpacity = 0.8,
    popup = tabla_kml(sf::st_drop_geometry(dat_4326))
  )
} else if (any(grepl("LINESTRING", geom_type))) {
  map <- map %>% leaflet::addPolylines(
    color = "#00A087FF", weight = 3,
    popup = tabla_kml(sf::st_drop_geometry(dat_4326))
  )
} else {
  map <- map %>% leaflet::addPolygons(
    fillColor = "#84BD00FF", fillOpacity = 0.5, color = "black", weight = 1,
    popup = tabla_kml(sf::st_drop_geometry(dat_4326))
  )
}
map

uso_veg <- read_sf("~/CNX955/01. Insumos x cuenca/C03/Capa_uso_vegetacion_C03.shp") %>% prepare_uso_veg()
leaflet::leaflet() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
  leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap") %>%
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
  leaflet::addPolygons(
    data = get_cuenca(uso_veg) %>% sf::st_transform(4326),
    fillOpacity = 0,
    weight = 2,
    color = "#00A087FF",
    dashArray = "10,5"
  ) %>%
  leaflet::addPolygons(
    data = get_BNP_cuenca(uso_veg, "Porlieria chilensis") %>% sf::st_transform(4326),
    fillColor = "#84BD00FF",
    fillOpacity = 0.8,
    group = "BNP",
    weight = 2,
    color = "snow"
  )

list.files("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/Tanda 2/", pattern = "shp$", full.names = T) %>% 
  normalizePath(winslash = "/") %>% purrr::map_dfr(read_sf) %>% st_drop_geometry() %>% 
  group_by(ID_SSUBC, Afectacion) %>% summarise(Sup_ha = sum(Sup_ha), .groups = "drop") %>% 
  pivot_wider(names_from = Afectacion, values_from = Sup_ha) %>% 
  select(ID_SSUBC, contains("Intervencion"), contains("Alteracion"))

dir <- "~/CNX955/01. Insumos x cuenca/C20/Cartografia_digital_v3/"
list.files(path = dir, pattern = "shp$", full.names = T) %>% normalizePath("/") %>% 
  map(function(x) {
    geom_type <- as.character(sf::st_geometry_type(sf::read_sf(x), by_geometry = FALSE))
    if (any(grepl("POLYGON", geom_type))) {
      SupHA::st_trans_sup_ha(x)
    } else {
      invisible()
    }
  })
list.files(dir, pattern = "shp$", full.names = T) %>% 
  purrr::map(function(x) {
    tryCatch({
      asign_name_folder_kmz(shp = x) #%>% 
        # do.call("shp2kmz", .)
    }, error = function(e) {
      message("Error al generar KMZ para '", basename(x), "': ", e$message)
      NULL 
    })
  })
asign_name_folder_kmz(shp = "C:\\Users\\dmartinez\\Documents/CNX955/01. Insumos x cuenca/C20/Cartografia_digital_v3/Area_de_proyecto_Obras_C20.shp")

read_sf("~/CNX955/01. Insumos x cuenca/C03/Cartografia_digital/Area_de_proyecto_Obras_C03.shp") %>%
  sf::st_transform(4326) %>% 
  dplyr::filter(!sf::st_is_empty(.)) %>% 
  dplyr::mutate(Name = Obra, Description = tabla_kml(sf::st_drop_geometry(.))) %>% select(Name, Description) %>% 
  write_sf("~/datos_temp/Area_de_proyecto_Obras_C03.kml")

read_sf("~/CNX955/01. Insumos x cuenca/C03/Cartografia_digital/Area_de_proyecto_Obras_C03.shp") %>% .[1,] %>% tabla_kml()

get_BNP_alterar(
  BNP_alter = read_sf("~/CNX955/00. Insumos/02. Afectacion/Afectacion_x_cuenca/Tanda 1/C03_Alteracion.shp") %>% PAS.150:::prepare_uso_veg(), 
  BNP_cuenca = read_sf("~/CNX955/01. Insumos x cuenca/C03/Cartografia_digital/BNP_Porlieria_Cuenca_C03.shp"), 
  alt_ok = T
) %>% write_sf("~/CNX955/01. Insumos x cuenca/C03/Cartografia_digital/BNP_Porlieria_a_Alterar_C03.shp")

read_sf("~/CNX955/01. Insumos x cuenca/C03/Cartografia_digital/BNP_Porlieria_a_Alterar_C03.shp")

name_fx <- function(x){
  as.character(rlang::ensym(x))
}
name_fx_rlang <- function(x){
  rlang::as_name(rlang::ensym(x))
}
name_fx("asdiuk uie (m)")
name_fx_rlang("í (m")

objeto_sf %>% 
  sf::st_transform(4326) %>% 
  dplyr::filter(!sf::st_is_empty(.)) %>% 
  dplyr::mutate(popup = tabla_popup(., cols = )) %>% 
  pull(popup) %>% .[1] %>% 
  htmltools::HTML() %>% 
  htmltools::browsable()

x <- c("filename.shp", "filename.shp.xml")
x <- c("filename.mlp.shp", "filename.mlp.shp.xml", "filename.mlp.shx", 
"UTM_Registros_Porlieria.shp.GEO-PF33KEXS.11392.31768.sr.lock","UTM_Registros_Porlieria.shp.xml.GEO-PF33KEXS.11392.31768.sr.lock")
stri_extract_all_regex(x, "(\\.shp)?\\.[[:alnum:]]+$")
sub("(\\.shp)?\\.[[:alnum:]]+$", "", x)

stri_extract_all_regex(x, "(\\.shp).*$|\\.[[:alnum:]]+$")
sub("(\\.shp).*$|\\.[[:alnum:]]+$", "", x)

censos <- read_sf("~/CNX955/00. Insumos/03. Compilado censo/Compilado_20260603/Compilado_censos_20260603.shp")
obras <- read_sf("~/CNX955/00. Insumos/10. Layout obras/KIM753_A2_Area_Intervencion_Por_Obra_Suelos_Catenarias_20250611.shp")
ssubc <- read_sf("~/CNX955/00. Insumos/01. Subsubcuencas tramitadas/Subsubcuencas_kimal.shp")
BNP <- read_sf("~/CNX955/00. Insumos/05. Segmentación LdB/KIM753_A3_Segmentacion_10k_Rev46_20260415.shp") %>% 
  filter(F_Ley20283 %>% stri_detect_regex("preserv")) %>% 
  st_intersection(ssubc %>% select(NOM_SSUBC, ID_SSUBC))
sp <- "Porlieria chilensis"

bd_fore <- BD_inventarios(
  BD_fore = prepare_bd_fore_150(
    BD = "~/CNX955/01. Insumos x cuenca/C26/BD_inv_C26.xlsx", 
    bd_lista = TRUE, 
    BNP_cuenca = get_BNP_cuenca(
      uso_veg = sf::read_sf("~/CNX955/01. Insumos x cuenca/C26/Capa_uso_vegetacion_C26.shp"),
      sp = "Prosopis chilensis"
    )
  ), 
  BNP_cuenca = get_BNP_cuenca(
    uso_veg = sf::read_sf("~/CNX955/01. Insumos x cuenca/C12/Capa_uso_vegetacion_C12.shp"),
    sp = "Porlieria chilensis"
  ), 
  sp = "Porlieria chilensis"
) 


gt_ecc_inter(ecc_inter = NULL, BNP_int_sin_censo = NULL, sp = "Prosospis chilensis", col_obras = NULL)   
gt_ecc_alter(NULL, NULL, sp = "Prosospis chilensis", col_obras = c("Obra"))   
  
c("Estimación_a_intervenir", "Censo a intervenir") %>% 
  stringi::stri_detect_regex("[censo|estim].*inter", case_insensitive = T)

carto <- list("Censo a intervenir" = iris, "Estimación_a_intervenir" = mtcars)
purrr::keep(
  carto,
  .p = stringi::stri_detect_regex(
    names(carto),
    "[censo|estim].*alter",
    case_insensitive = T
  )
) %>%
  purrr::pluck() %>% 
  purrr::map(names) %>% 
  purrr::flatten_chr() %>% 
  unique() %>% 
  subset(stringi::stri_detect_regex(., "obra", case_insensitive = TRUE))


read_sf("~/CNX955/01. Insumos x cuenca/C26/Capa_uso_vegetacion_C26.shp") %>% mutate_at(2, ~"C02") %>% 
  write_sf("~/CNX955/01. Insumos x cuenca/C02/Capa_uso_vegetacion_C02.shp") 
format_sup_ha("~/CNX955/01. Insumos x cuenca/C02/Capa_uso_vegetacion_C02.shp") 

x <- tibble::tibble(
  Formacion = "Bosque",
  ECC1 = "Porlieria chilensis",
  ECC2 = "",
  ECC3 = NA_character_,
  ECC4 = "- "
) 
add_BNP_ECC(x)

tibble::tibble(
  Formacion = rep("Bosque", 3),
  ECC1 = c("Porlieria chilensis", "-", "Carica chilensis"),
  ECC2 = c("", "Porlieria chilensis", "Porlieria chilensis"),
  ECC3 = c(NA_character_, "Carica chilensis", ""),
  ECC4 = c(" - ", "-", "")
) %>% add_BNP_ECC()
ecc <- c(" - ", "-", "Porlieria chilensis", NA_character_, "Carica chilensis")
real_ecc <- ecc %>% subset(stringi::stri_count_regex(.,"[:alpha:]") > 0) %>% unique() %>% sort() %>% paste(collapse = " - ")
real_ecc


library(shiny)
library(shinyWidgets)
if(interactive()){
  ui <- fluidPage(

    # use the Modern design
    shinyWidgets::chooseSliderSkin("Flat", color = "LightSeaGreen"),
    sliderInput("obs", "Customized slider1:",
                min = 0, max = 100, value = 50
    ),
    sliderInput("obs2", "Customized slider1:",
                min = 0, max = 100, value = 50
    ),
    sliderInput("obs3", "Customized slider1:",
                min = 0, max = 100, value = 50
    ),
    plotOutput("distPlot")
  )

  server <- function(input, output) {

    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })
  }

  shinyApp(ui, server)
}

inter <- get_BNP_intervencion(
  BNP_cuenca = read_sf("~/CNX955/01. Insumos x cuenca/C22/Cartografia_digital/BNP_Porlieria_Cuenca_C22.shp"), 
  obras = read_sf("~/CNX955/00. Insumos/10. Layout obras/KIM753_A2_Area_Intervencion_Por_Obra_Suelos_Catenarias_20250611.shp"),
) %>% 
  dplyr::mutate(Afectacion = "Intervencion")

rem <- get_alter_bnp_remanente(
  BNP = read_sf("~/CNX955/01. Insumos x cuenca/C22/Cartografia_digital/BNP_Porlieria_Cuenca_C22.shp"),
  obras = read_sf("~/CNX955/00. Insumos/10. Layout obras/KIM753_A2_Area_Intervencion_Por_Obra_Suelos_Catenarias_20250611.shp"),
  sp = "Porlieria chilensis",
  sup_ha_max = 0.5
)
buf <- get_alter_censo_buffer(
  BNP = read_sf("~/CNX955/01. Insumos x cuenca/C22/Cartografia_digital/BNP_Porlieria_Cuenca_C22.shp"),
  obras = read_sf("~/CNX955/00. Insumos/10. Layout obras/KIM753_A2_Area_Intervencion_Por_Obra_Suelos_Catenarias_20250611.shp"),
  censos = read_sf("~/CNX955/00. Insumos/03. Compilado censo/Compilado_20260609/Compilado_censos_20260609.shp"),
  sp = "Porlieria chilensis",
  distancia = 5,
  buffer = 5,
  dTolerance = 2,
  radio = 1
)

dplyr::bind_rows(inter, st_union_no_overlaps(buf, rem)) %>% st_geometry() %>% plot()

shiny::runApp(system.file('examples', 'DT-rows', package = 'DT'))

### Libraries
library(shiny)
library(dplyr)
library(DT)

### Data
input_data <- data.frame(Brand = c("Brand1", "Brand2","Brand3"),
                         ratio = c (.5, .5, .5),
                         cost = c(2000, 3000, 4000),
                         stringsAsFactors = FALSE) %>% 
  mutate(updated_price = cost * ratio)




### Module
modFunction <- function(input, output, session, data,reset) {
  
  v <- reactiveValues(data = data)
  
  proxy = dataTableProxy("mod_table")
  
  observeEvent(input$mod_table_cell_edit, {
    print(names(v$data))
    info = input$mod_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    k = info$value
    str(info)
    
    isolate(
      if (j %in% match(c("ratio","cost","updated_price"), names(v$data))) {
        print(match(c("ratio","cost", "updated_price"), names(v$data)))
        v$data[i, j] <<- DT::coerceValue(k, v$data[i, j])
        print(v$data)
        
        if (j %in% match("cost", names(v$data))) {
          v$data$updated_price <<- v$data$cost * v$data$ratio
        }
        if (j %in% match("ratio", names(v$data))) {
          v$data$updated_price <<- v$data$cost * v$data$ratio
        }
      } else {
        stop("You are not supposed to change this column.") # check to stop the user from editing only few columns
      }
    )
    replaceData(proxy, v$data, resetPaging = FALSE)  # replaces data displayed by the updated table
  })
  
  ### Reset Table
  observeEvent(reset(), {
    v$data <- data # your default data
  })
  
  print(isolate(colnames(v$data)))
  output$mod_table <- DT::renderDataTable({
    DT::datatable(v$data, editable = TRUE)
    
  })
  
  return(v)
}

modFunctionUI <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("mod_table"))
  
}

### Shiny App
shinyApp(
  ui = basicPage(
    mainPanel(
      
      actionButton("reset", "Reset"),
      tags$hr(),
      modFunctionUI("editable")
    )
  ),
  server = function(input, output) {
    demodata<-input_data
    edited <- callModule(modFunction,"editable", demodata,
                         reset = reactive(input$reset))
    data_df_final <- reactiveValues()
    observe(
      {data_df_final$data <- edited$data}
    )
    observe(print(data_df_final$data))
  }
)


