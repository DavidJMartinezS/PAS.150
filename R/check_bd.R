#' Chequeo Bases de Datos
#'
#' @param x dataframe con la base de datos. Generarla con `prepare_bd_fore_150()`
#'
#' @returns Devuelve alertas impresas en la consola o bien en modulos de shiny.
#' @rdname check_bd
#' @export
#' 
check_bd_flora <- function(x) {
  valid_df(x)
  shiny <- shiny::isRunning()
  ok <- T

  # Verificar que estén los campos mínimos ----
  if (!all(req_names$BD_flora %in% names(x))) {
    if (shiny) {
      check_input(
        x = x,
        names_req = req_names$BD_flora
      )
    } else {
      cat(
        "\033[31mProblemas!","\U0001FAE0","Shapefile sin los campos requeridos", "\n",
        "\033[34mRequeridos: ", paste(req_names$BD_flora %>% shQuote(), collapse = ", "), "\n",
        "Faltan: ", paste(setdiff(req_names$BD_flora, names(x)) %>% shQuote(), collapse = ", "), "\n"
      )
    }
    ok <- F
  }

  # Verificar coberturas Braun Blanquet ----
  if ("Cob_BB" %in% names(x)) {
    if (!x$Cob_BB %in% c("fp", "r", "+", "1", "2", "3", "4", "5", "6", "---") %>% all()) {
      if (shiny) {
        shinyalert::shinyalert(
          title = "Coberturas de Braun-Blanquet que no corresponden",
          text = tags$p(
            "Las coberturas de Braun-Blanquet deben limitarse a las siguientes nomenclaturas:",
            rep_br(1),
            paste(
              c("fp", "r", "+", "1", "2", "3", "4", "5", "6", "---") %>% shQuote(),
              collapse = ", "
            ),
            rep_br(2),
            "Las siguientes no se reconocen:",
            paste(
              setdiff(unique(x$Cob_BB), c("fp", "r", "+", "1", "2", "3", "4", "5", "6", "---")) %>% shQuote(),
              collapse = ", "
            )
          ),
          html = TRUE,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
      } else {
        cat(
          "\033[31mProblemas!","\U0001FAE0","Coberturas de Braun-Blanquet que no corresponden","\n",
          "\033[34mLas coberturas de Braun-Blanquet deben limitarse a las siguientes nomenclaturas:","\n",
          paste(
            c("fp", "r", "+", "1", "2", "3", "4", "5", "6", "---") %>% shQuote(),
            collapse = ", "
          ),"\n", "\033[37m",
          "Las siguientes no se reconocen:",
          paste(
            setdiff(unique(x$Cob_BB), c("fp", "r", "+", "1", "2", "3", "4", "5", "6", "---")) %>% shQuote(),
            collapse = ", "
          ), "\n"
        )
      }
      ok <- F
    }
  }

  # Verificar especies mal escritas ----
  if ("Especie" %in% names(x)) {
    sp <- unique(x$Especie) %>% stringi::stri_trans_totitle(type = "sentence") %>% stringi::stri_trim_both()
    dist_mat <- stringdist::stringdistmatrix(sp, sp, method = "lv")
    mat <- (dist_mat <= 3 & dist_mat != 0)
    g = igraph::graph_from_adjacency_matrix(mat)
    df_similar <- data.frame(similar = igraph::components(g)$membership, Especie = sp) %>%
      dplyr::group_by(similar) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::summarise("Especies similares" = paste(Especie, collapse = ", ")) %>%
      dplyr::select(2)
    if (nrow(df_similar) >= 1) {
      if (shiny) {
        shinyalert::shinyalert(
          title = "Posibles especies mal escritas",
          text = tags$p(
            "Las siguientes especies podrian estar repetidas por errores de tipeo:",
            tags$br(),
            df_similar %>%
              kableExtra::kbl() %>%
              kableExtra::kable_styling() %>%
              shiny::HTML()
          ),
          html = TRUE,
          type = "warning",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
      } else {
        cat(
          cat("\033[33mOJO!", "\U0001f440", "Posibles especies mal escritas!"),
          "\033[34mLas siguientes especies podrian estar repetidas por errores de tipeo:","\033[37m",
          df_similar %>%
            kableExtra::kbl(format = "simple") %>%
            suppressMessages() %>% suppressWarnings(),
          sep = "\n"
        )
      }
      ok <- F
    }
  }

  # Verificar coordenadas ----
  if (all(c("Parcela", "UTM_E", "UTM_N") %in% names(x))) {
    ## UTM_E y UTM_N ----
    if (
      !x$UTM_E %>%
      stringi::stri_count_regex("\\w") %>%
      all(. == 6) ||
      !x$UTM_N %>%
      stringi::stri_count_regex("\\w") %>%
      all(. == 7)
    ) {
      if (shiny) {
        shinyalert::shinyalert(
          title = "Error en coordenadas",
          text = tags$p(
            "Revisar las coordenadas de las siguientes parcelas:", rep_br(2),
            paste(
              x$Parcela[which(!x$UTM_E %>% stringi::stri_count_regex("\\w") %>% .[] == 6)] %>%
                c(x$Parcela[which(!x$UTM_N %>% stringi::stri_count_regex("\\w") %>% .[] == 7)]) %>%
                unique() %>%
                shQuote(),
              collapse = ", "
            )
          ),
          html = TRUE,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
      } else {
        cat(
          "\033[31mProblemas!","\U0001FAE0", "Error en coordenadas", "\n",
          "\033[34mRevisar las coordenadas de las siguientes parcelas:","\033[37m", "\n",
          paste(
            x$Parcela[which(!x$UTM_E %>% stringi::stri_count_regex("\\w") %>% .[] == 6)] %>%
              c(x$Parcela[which(!x$UTM_N %>% stringi::stri_count_regex("\\w") %>% .[] == 7)]) %>%
              unique() %>%
              shQuote(),
            collapse = ", "
          ),"\n"
        )
      }
      ok <- F
    }

    ## Verificar Parcelas repetidas ----
    if (
      !x %>%
      dplyr::count(Parcela, UTM_E, UTM_N) %>%
      dplyr::count(Parcela, sort = T) %>%
      dplyr::pull(n) %>%
      all(. == 1)
    ) {
      if (shiny) {
        shinyalert::shinyalert(
          title = "Mismas parcela, diferentes coordenadas!",
          text = tags$p(
            "Las siguientes parcelas presentan más de una coordenada:",
            rep_br(2),
            paste(
              x %>%
                dplyr::count(Parcela, UTM_E, UTM_N) %>%
                dplyr::count(Parcela) %>%
                dplyr::filter(n > 1) %>%
                dplyr::pull(Parcela) %>%
                shQuote(),
              collapse = ", "
            )
          ),
          html = TRUE,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
      } else {
        cat(
          "\033[31mProblemas!","\U0001FAE0", "Mismas parcela, diferentes coordenadas!", "\n",
          "\033[34mLas siguientes parcelas presentan más de una coordenada:", "\033[37m", "\n",
          paste(
            x %>%
              dplyr::count(Parcela, UTM_E, UTM_N) %>%
              dplyr::count(Parcela) %>%
              dplyr::filter(n > 1) %>%
              dplyr::pull(Parcela) %>%
              shQuote(),
            collapse = ", "
          ),"\n"
        )
      }
      ok <- F
    }

    ## Verificar coordenadas repetidas ----
    if (
      !x %>%
      dplyr::count(Parcela, UTM_E, UTM_N) %>%
      dplyr::count(UTM_E, UTM_N, sort = T) %>%
      dplyr::pull(n) %>%
      all(. == 1)
    ) {
      if (shiny) {
        shinyalert::shinyalert(
          title = "Mismas coordenadas, diferentes parcelas!",
          text = tags$p(
            "Las siguientes parcelas presentan mismas coordenadas:",
            rep_br(2),
            x %>%
              dplyr::count(Parcela, UTM_E, UTM_N) %>%
              dplyr::group_by(UTM_E, UTM_N) %>%
              dplyr::add_tally() %>%
              dplyr::filter(nn > 1) %>%
              dplyr::select(1:3) %>%
              kableExtra::kbl() %>%
              kableExtra::kable_styling() %>%
              shiny::HTML()
          ),
          html = TRUE,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
      } else {
        cat(
          cat("\033[31mProblemas!", "\U0001FAE0", "Mismas coordenadas, diferentes parcelas!"),
          "\033[34mLas siguientes parcelas presentan mismas coordenadas:","\033[37m",
          x %>%
            dplyr::count(Parcela, UTM_E, UTM_N) %>%
            dplyr::group_by(UTM_E, UTM_N) %>%
            dplyr::add_tally() %>%
            dplyr::filter(nn > 1) %>%
            dplyr::select(1:3) %>%
            dplyr::arrange(UTM_E, UTM_N, Parcela) %>%
            kableExtra::kbl(format = "simple") %>%
            suppressMessages() %>% suppressWarnings(),
          sep = "\n"
        )
      }
      ok <- F
    }

    ## Especies repetidas por parcela ----
    if ("Especie" %in% names(x)) {
      if (
        !x %>%
        dplyr::count(Parcela, UTM_E, UTM_N, Especie) %>%
        dplyr::pull(n) %>%
        all(. == 1)
      ) {
        if (shiny) {
          shinyalert::shinyalert(
            title = "Registro duplicado de especies en una parcela!",
            text = tags$p(
              "Las siguientes parcelas presentan el registro de más de una especie en una parcela:",
              rep_br(2),
              paste(
                x %>%
                  dplyr::count(Parcela, UTM_E, UTM_N, Especie) %>%
                  dplyr::filter(n > 1) %>%
                  dplyr::pull(Parcela) %>%
                  unique() %>%
                  shQuote(),
                collapse = ", "
              )
            ),
            html = TRUE,
            type = "error",
            closeOnEsc = T,
            showConfirmButton = T,
            confirmButtonCol = "#6FB58F",
            animation = T
          )
        } else {
          cat(
            "\033[31mProblemas!","\U0001FAE0", "Registro duplicado de especies en una parcela!", "\n",
            "\033[34mLas siguientes parcelas presentan el registro de más de una especie en una parcela:", "\033[37m", "\n",
            paste(
              x %>%
                dplyr::count(Parcela, UTM_E, UTM_N, Especie) %>%
                dplyr::filter(n > 1) %>%
                dplyr::pull(Parcela) %>%
                unique() %>%
                shQuote(),
              collapse = ", "
            ),"\n"
          )
        }
        ok <- F
      }
    }
  } %>% suppressWarnings() %>% suppressMessages()

  # Chequear DS_58, RCE, Decreto ----
  if ("Especie" %in% names(x) & any(c("DS_68", "RCE", "Decreto") %in% names(x))) {
    vars <- dplyr::syms(names(x) %>% subset(. %in% c("DS_68", "RCE", "Decreto")))
    if (!x %>% dplyr::count(Especie, !!!vars) %>% dplyr::count(Especie) %>% dplyr::pull(n) %>% all(. == 1)) {
      if (shiny) {
        shinyalert::shinyalert(
          title = sprintf("Especies con más de una variable (%s)", paste(as.character(vars), collapse = ", ")),
          text = tags$p(
            "Unificar variables de las siguientes especies:",
            rep_br(2),
            x %>%
              dplyr::count(Especie, !!!vars) %>%
              dplyr::add_count(Especie) %>%
              dplyr::filter(nn > 1) %>%
              dplyr::select(Especie, !!!vars) %>%
              kableExtra::kbl() %>%
              kableExtra::kable_styling() %>%
              shiny::HTML()
          ),
          html = TRUE,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
      } else {
        cat(
          cat("\033[31mProblemas!", "\U0001FAE0", sprintf("Especies con más de una variable (%s)", paste(as.character(vars), collapse = ", "))),
          "\033[34mUnificar variables de las siguientes especies:","\033[37m",
          x %>%
            dplyr::count(Especie, !!!vars) %>%
            dplyr::add_count(Especie) %>%
            dplyr::filter(nn > 1) %>%
            dplyr::select(Especie, !!!vars) %>%
            kableExtra::kbl(format = "simple") %>%
            suppressMessages() %>% suppressWarnings(),
          sep = "\n"
        )
      }
      ok <- F
    }
  }

  # OK ----
  if (ok) {
    if (shiny) {
      shinybusy::notify_success("Perfecto!", timeout = 3000, position = "right-bottom")
    } else {
      cat("\033[32mPerfecto!","\U0001F601", "No se han encontrado observaciones", "\n")
    }
  }
}

#' @rdname check_bd
#' @export
#' 
check_bd_fore <- function(x) {
  valid_df(x)
  shiny <- shiny::isRunning()
  ok <- T

  # Verificar que estén los campos mínimos ----
  if (!all(req_names$BD_fore %in% names(x))) {
    if (shiny) {
      check_input(
        x = x,
        names_req = req_names$BD_fore
      )
    } else {
      cat(
        "\033[31mProblemas!","\U0001FAE0","Shapefile sin los campos requeridos", "\n",
        "\033[34mRequeridos: ", paste(req_names$BD_fore %>% shQuote(), collapse = ", "), "\n",
        "Faltan: ", paste(setdiff(req_names$BD_fore, names(x)) %>% shQuote(), collapse = ", "), "\n"
      )
    }
    ok <- F
  }

  # Verificar especies mal escritas ----
  if ("Especie" %in% names(x)) {
    sp <- unique(x$Especie) %>% stringi::stri_trans_totitle(type = "sentence") %>% stringi::stri_trim_both()
    dist_mat <- stringdist::stringdistmatrix(sp, sp, method = "lv")
    mat <- (dist_mat <= 3 & dist_mat != 0)
    g = igraph::graph_from_adjacency_matrix(mat)
    df_similar <- data.frame(similar = igraph::components(g)$membership, Especie = sp) %>%
      dplyr::group_by(similar) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::summarise("Especies similares" = paste(Especie, collapse = ", ")) %>%
      dplyr::select(2)
    if (nrow(df_similar) >= 1) {
      if (shiny) {
        shinyalert::shinyalert(
          title = "Posibles especies mal escritas",
          text = tags$p(
            "Las siguientes especies podrian estar repetidas por errores de tipeo:",
            tags$br(),
            df_similar %>%
              kableExtra::kbl() %>%
              kableExtra::kable_styling() %>%
              shiny::HTML()
          ),
          html = TRUE,
          type = "warning",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
      } else {
        cat(
          cat("\033[33mOJO!", "\U0001f440", "Posibles especies mal escritas!"),
          "\033[34mLas siguientes especies podrian estar repetidas por errores de tipeo:","\033[37m",
          df_similar %>%
            kableExtra::kbl(format = "simple") %>%
            suppressMessages() %>% suppressWarnings(),
          sep = "\n"
        )
      }
      ok <- F
    }
  }

  # Verificar coordenadas ----
  if (all(c("Parcela", "UTM_E", "UTM_N") %in% names(x))) {
    ## UTM_E y UTM_N ----
    if (
      !x$UTM_E %>%
      stringi::stri_count_regex("\\w") %>%
      all(. == 6) ||
      !x$UTM_N %>%
      stringi::stri_count_regex("\\w") %>%
      all(. == 7)
    ) {
      if (shiny) {
        shinyalert::shinyalert(
          title = "Error en coordenadas",
          text = tags$p(
            "Revisar las coordenadas de las siguientes parcelas:", rep_br(2),
            paste(
              x$Parcela[which(!x$UTM_E %>% stringi::stri_count_regex("\\w") %>% .[] == 6)] %>%
                c(x$Parcela[which(!x$UTM_N %>% stringi::stri_count_regex("\\w") %>% .[] == 7)]) %>%
                unique() %>%
                shQuote(),
              collapse = ", "
            )
          ),
          html = TRUE,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
      } else {
        cat(
          "\033[31mProblemas!","\U0001FAE0", "Error en coordenadas", "\n",
          "\033[34mRevisar las coordenadas de las siguientes parcelas:","\033[37m", "\n",
          paste(
            x$Parcela[which(!x$UTM_E %>% stringi::stri_count_regex("\\w") %>% .[] == 6)] %>%
              c(x$Parcela[which(!x$UTM_N %>% stringi::stri_count_regex("\\w") %>% .[] == 7)]) %>%
              unique() %>%
              shQuote(),
            collapse = ", "
          ),"\n"
        )
      }
      ok <- F
    }

    ## Verificar Parcelas repetidas ----
    if (
      !x %>%
      dplyr::count(Parcela, UTM_E, UTM_N) %>%
      dplyr::count(Parcela, sort = T) %>%
      dplyr::pull(n) %>%
      all(. == 1)
    ) {
      if (shiny) {
        shinyalert::shinyalert(
          title = "Mismas parcela, diferentes coordenadas!",
          text = tags$p(
            "Las siguientes parcelas presentan más de una coordenada:",
            rep_br(2),
            paste(
              x %>%
                dplyr::count(Parcela, UTM_E, UTM_N) %>%
                dplyr::count(Parcela) %>%
                dplyr::filter(n > 1) %>%
                dplyr::pull(Parcela) %>%
                shQuote(),
              collapse = ", "
            )
          ),
          html = TRUE,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
      } else {
        cat(
          "\033[31mProblemas!","\U0001FAE0", "Mismas parcela, diferentes coordenadas!", "\n",
          "\033[34mLas siguientes parcelas presentan más de una coordenada:", "\033[37m", "\n",
          paste(
            x %>%
              dplyr::count(Parcela, UTM_E, UTM_N) %>%
              dplyr::count(Parcela) %>%
              dplyr::filter(n > 1) %>%
              dplyr::pull(Parcela) %>%
              shQuote(),
            collapse = ", "
          ),"\n"
        )
      }
      ok <- F
    }

    ## Verificar coordenadas repetidas ----
    if (
      !x %>%
      dplyr::count(Parcela, UTM_E, UTM_N) %>%
      dplyr::count(UTM_E, UTM_N, sort = T) %>%
      dplyr::pull(n) %>%
      all(. == 1)
    ) {
      if (shiny) {
        shinyalert::shinyalert(
          title = "Mismas coordenadas, diferentes parcelas!",
          text = tags$p(
            "Las siguientes parcelas presentan mismas coordenadas:",
            rep_br(2),
            x %>%
              dplyr::count(Parcela, UTM_E, UTM_N) %>%
              dplyr::group_by(UTM_E, UTM_N) %>%
              dplyr::add_tally() %>%
              dplyr::filter(nn > 1) %>%
              dplyr::select(1:3) %>%
              kableExtra::kbl() %>%
              kableExtra::kable_styling() %>%
              shiny::HTML()
          ),
          html = TRUE,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
      } else {
        cat(
          cat("\033[31mProblemas!", "\U0001FAE0", "Mismas coordenadas, diferentes parcelas!"),
          "\033[34mLas siguientes parcelas presentan mismas coordenadas:","\033[37m",
          x %>%
            dplyr::count(Parcela, UTM_E, UTM_N) %>%
            dplyr::group_by(UTM_E, UTM_N) %>%
            dplyr::add_tally() %>%
            dplyr::filter(nn > 1) %>%
            dplyr::select(1:3) %>%
            dplyr::arrange(UTM_E, UTM_N, Parcela) %>%
            kableExtra::kbl(format = "simple") %>%
            suppressMessages() %>% suppressWarnings(),
          sep = "\n"
        )
      }
      ok <- F
    }
  }

  # Chequear estado de desarrollo ----
  if ("Estado" %in% names(x)) {
    if (!all(na.omit(unique(x$Estado)) %in% c("Regeneracion", "Brinzal", "Adulto"))) {
      if (shiny) {
        shinyalert::shinyalert(
          title = "Estado de desarrollo fuera del standar.",
          text = tags$p(
            "Estos pueden ser 'Regeneracion', 'Brinzal' o 'Adulto'. Renombrar los siguientes:",
            rep_br(2),
            na.omit(unique(x$Estado)) %>% setdiff(c("Regeneracion", "Brinzal", "Adulto"))
          ),
          html = TRUE,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
      } else {
        cat(
          cat("\033[31mProblemas!", "\U0001FAE0", "Estado de desarrollo fuera del standar!"),
          "\033[34mRenombrar los siguientes:","\033[37m",
          paste(
            na.omit(unique(x$Estado)) %>% setdiff(c("Regeneracion", "Brinzal", "Adulto")) %>% shQuote(), 
            collapse = ", "
          ), "\n"
        )
      }
      ok <- F
    }
  }

  # OK ----
  if (ok) {
    if (shiny) {
      shinybusy::notify_success("Perfecto!", timeout = 3000, position = "right-bottom")
    } else {
      cat("\033[32mPerfecto!","\U0001F601", "No se han encontrado observaciones", "\n")
    }
  }
}