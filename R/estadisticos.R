#' estadisticos_PAS150
#'
#' @param a superficie parcela en hectareas.
#' @param A superficie unidad a inventariar. ej: BNP de la cuenca.
#' @param data data.frame con los datos del NHA. Obter con `BD_inventarios()`.
#' @inheritParams carto_digital
#'
#' @returns data.frame con estadisticos
#'
#' @export
estadisticos_PAS150 <- function(a, A, data, sp) {
  # sp <- sp %>% str_replace(" ",".")
  data %>%
    dplyr::select(sp, Nha_Total) %>%
    dplyr::mutate_all(janitor::round_half_up) %>%
    tidyr::pivot_longer(cols = 1:2, names_to = "Variable", values_to = "Nha") %>%
    dplyr::group_by(Variable) %>%
    dplyr::summarise(
      Rango = paste(min(Nha), max(Nha), sep = " - "),
      n = n(),
      Promedio = mean(Nha, na.rm = T) %>% janitor::round_half_up(),
      s2 = ((1 - (n * a / A)) * (sd(Nha)^2 / n)) %>% janitor::round_half_up(2),
      CV = ((sqrt(s2) / Promedio) * 100) %>% janitor::round_half_up(1),
      T_est = qt(0.975, n - 1),
      E_abs = (T_est * sqrt(s2)) %>% janitor::round_half_up(),
      E_rel = ((E_abs / Promedio) * 100) %>% janitor::round_half_up(1),
      Int_conf = paste(janitor::round_half_up(Promedio - E_abs), janitor::round_half_up(Promedio + E_abs), sep = " - ")
    ) %>%
    dplyr::select(-T_est)
}