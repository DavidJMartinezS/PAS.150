#' medidas 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
grafico_medidas_superficie <- function(
  sup_inter,
  sup_alter,
  sup_medida_inter,
  sup_medida_alter
) {

  # --- Parámetros fijos ---
  unidad    <- "ha"
  col_afect <- "black"    # color de la trama de afectación
  col_med   <- "gray80"   # color de las medidas
  densidad  <- 12         # líneas de la trama (más = más denso)

  # Si algún valor no es válido, dibuja un lienzo vacío en vez de fallar
  vals <- c(sup_inter, sup_alter, sup_medida_inter, sup_medida_alter
  )
  if (length(vals) != 4 || any(!is.finite(vals)) || all(vals == 0)) {
    plot.new()
    return(invisible(NULL))
  }

  total_med  <- sup_medida_inter + sup_medida_alter
  total_afec <- sup_inter + sup_alter

  fmt <- function(v) paste(format(v, decimal.mark = ",", big.mark = "."), unidad)

  y_max <-  max(sup_medida_inter, sup_medida_alter
    , total_med) * 1.25
  y_min <- -max(sup_inter, sup_alter, total_afec) * 1.25

  # Rectángulo con trama entrecruzada (líneas a 45° y -45°)
  rect_trama <- function(x0, y0, x1, y1, dens = densidad) {
    rect(x0, y0, x1, y1, col = "white", border = NA)
    rect(x0, y0, x1, y1, density = dens, angle =  45, col = col_afect, border = NA)
    rect(x0, y0, x1, y1, density = dens, angle = -45, col = col_afect, border = NA)
    rect(x0, y0, x1, y1, col = NA, border = col_afect, lwd = 1.2)
  }

  # Texto con recuadro blanco detrás, para que se lea sobre la trama
  texto_fondo <- function(x, y, label) {
    w <- strwidth(label,  font = 2) * 0.6
    h <- strheight(label, font = 2) * 0.75
    rect(x - w, y - h, x + w, y + h, col = "white", border = NA)
    text(x, y, label, font = 2)
  }

  # --- Lienzo ---
  op <- par(mar = c(2, 4, 2, 2), xpd = NA)
  on.exit(par(op), add = TRUE)
  plot.new()
  plot.window(xlim = c(0, 4.4), ylim = c(y_min, y_max))

  # --- Eje Y ---
  arrows(0.3, y_min, 0.3, y_max, code = 3, length = 0.08, lwd = 1.5)
  text(0.15, y_max, "+", font = 2, cex = 1.4)
  text(0.15, y_min, "\u2013", font = 2, cex = 1.4)
  text(0, 0, paste0("Superficie (", unidad, ")"), srt = 90, font = 2, cex = 1.5)

  # Posición de las columnas
  c1 <- c(0.6, 1.8); c2 <- c(2.4, 3.6)
  m1 <- mean(c1);    m2 <- mean(c2)

  # --- Barras columna 1: intervención + medida ---
  rect(c1[1], 0, c1[2], sup_medida_inter, col = col_med, border = "gray30")
  rect_trama(c1[1], -sup_inter, c1[2], 0)
  text(m1, sup_medida_inter / 2, "Superficie\nmedidas", font = 2)
  texto_fondo(m1, -sup_inter / 2, "Intervención")

  # --- Barras columna 2: alteración + medida ---
  rect(c2[1], 0, c2[2], sup_medida_alter
    , col = col_med, border = "gray30")
  rect_trama(c2[1], -sup_alter, c2[2], 0)
  text(m2, sup_medida_alter
     / 2, "Superficie\nmedidas", font = 2)
  texto_fondo(m2, -sup_alter / 2, "Alteración\nde hábitat")

  # --- Cifras sobre / bajo cada barra ---
  text(m1,  sup_medida_inter,      fmt(sup_medida_inter),      pos = 3, font = 2, cex = 1.3)
  text(m1, -sup_inter, fmt(sup_inter), pos = 1, font = 2, cex = 1.3)
  text(m2,  sup_medida_alter
    ,      fmt(sup_medida_alter
  ),      pos = 3, font = 2, cex = 1.3)
  text(m2, -sup_alter,   fmt(sup_alter),   pos = 1, font = 2, cex = 1.3)

  # --- Línea del cero y líneas punteadas de totales ---
  segments(0.3, 0, 4.2, 0, lwd = 1.5)

  # Tamaño del símbolo (un cuadrado de 0.15 pulgadas) en unidades del gráfico
  lado <- 0.15
  bw <- lado * diff(par("usr")[1:2]) / par("pin")[1]
  bh <- lado * diff(par("usr")[3:4]) / par("pin")[2]
  h  <- strheight("Mg", cex = 0.9)
  sep <- bw * 0.5

  # Línea de arriba: total medidas + cuadrado gris
  segments(0.3, total_med, 4.2, total_med, lty = 2, lwd = 1.2)
  y_sup <- total_med + h * 1.2
  text(4.2 - bw - sep, y_sup, paste("Total medidas:", fmt(total_med)),
       adj = c(1, 0.5), cex = 1.4)
  rect(4.2 - bw, y_sup - bh / 2, 4.2, y_sup + bh / 2,
       col = col_med, border = "gray30")

  # Línea de abajo: total afectada + cuadrado con trama
  segments(0.3, -total_afec, 4.2, -total_afec, lty = 2, lwd = 1.2)
  y_inf <- -total_afec - h * 1.2
  text(4.2 - bw - sep, y_inf, paste("Total superficie afectada:", fmt(total_afec)),
       adj = c(1, 0.5), cex = 1.4)
  rect_trama(4.2 - bw, y_inf - bh / 2, 4.2, y_inf + bh / 2, dens = densidad * 3)

}

#' @noRd
#' @importFrom graphics plot.new plot.window rect text arrows segments polygon par strwidth strheight
#' @importFrom grDevices png dev.off
grafico_medidas_individuos <- function(afectados, comprometidos) {

  # --- Parámetros fijos ---
  desfase      <- 0          # 0 = alineadas; p. ej. 0.4 = desfasadas
  col_afect    <- "black"    # color de la trama de afectados
  col_med      <- "gray80"   # color de comprometidos
  densidad     <- 12         # líneas de la trama (más = más denso)
  max_relacion <- 4          # alto máximo de la barra mayor, en veces la menor
  ancho        <- 0.6        # ancho de las barras

  # Si algún valor no es válido, dibuja un lienzo vacío en vez de fallar
  if (length(afectados) != 1 || length(comprometidos) != 1 ||
      !is.finite(afectados) || !is.finite(comprometidos) ||
      afectados <= 0 || comprometidos <= 0) {
    plot.new()
    return(invisible(NULL))
  }

  fmt <- function(v) format(v, big.mark = ".", decimal.mark = ",")

  # Rectángulo con trama entrecruzada
  rect_trama <- function(x0, y0, x1, y1) {
    rect(x0, y0, x1, y1, col = "white", border = NA)
    rect(x0, y0, x1, y1, density = densidad, angle =  45, col = col_afect, border = NA)
    rect(x0, y0, x1, y1, density = densidad, angle = -45, col = col_afect, border = NA)
    rect(x0, y0, x1, y1, col = NA, border = col_afect, lwd = 1.2)
  }

  # Texto con recuadro blanco detrás, para que se lea sobre la trama
  texto_fondo <- function(x, y, label) {
    w <- strwidth(label,  font = 2) * 0.6
    h <- strheight(label, font = 2) * 0.75
    rect(x - w, y - h, x + w, y + h, col = "white", border = NA)
    text(x, y, label, font = 2)
  }

  # --- Alturas dibujadas ---
  # Si una barra es mucho mayor que la otra, se dibuja "cortada"
  # (con un corte en zigzag) y su cifra real se escribe encima.
  h_com <- comprometidos
  h_afe <- afectados
  corte_com <- comprometidos > max_relacion * afectados
  corte_afe <- afectados     > max_relacion * comprometidos
  if (corte_com) h_com <- max_relacion * afectados
  if (corte_afe) h_afe <- max_relacion * comprometidos

  y_max <-  h_com * 1.25
  y_min <- -h_afe * 1.25

  # Dibuja el corte: una franja blanca inclinada con dos líneas paralelas
  dibujar_corte <- function(x0, x1, yc) {
    g  <- (y_max - y_min) * 0.018      # mitad del ancho de la franja
    s  <- (y_max - y_min) * 0.025      # inclinación
    ex <- 0.06                          # sobresale un poco de la barra
    polygon(c(x0 - ex, x1 + ex, x1 + ex, x0 - ex),
            c(yc - g - s, yc - g + s, yc + g + s, yc + g - s),
            col = "white", border = NA)
    segments(x0 - ex, yc - g - s, x1 + ex, yc - g + s, lwd = 1.2)
    segments(x0 - ex, yc + g - s, x1 + ex, yc + g + s, lwd = 1.2)
  }

  # --- Lienzo ---
  op <- par(mar = c(2, 4, 2, 2), xpd = NA)
  on.exit(par(op), add = TRUE)
  plot.new()
  plot.window(xlim = c(0, 3), ylim = c(y_min, y_max))

  # --- Eje Y ---
  arrows(0.3, y_min, 0.3, y_max, code = 3, length = 0.08, lwd = 1.5)
  text(0.15, y_max, "+", font = 2, cex = 1.4)
  text(0.15, y_min, "\u2013", font = 2, cex = 1.4)
  text(0.18, (y_max + y_min) / 2, "N\u00famero de individuos", srt = 90, font = 2, cex = 1.4)

  # Posición de las barras (el desfase las separa hacia los lados)
  centro <- 1.6
  x_afe  <- centro - desfase / 2
  x_com  <- centro + desfase / 2

  # --- Barra de comprometidos (arriba) ---
  rect(x_com - ancho / 2, 0, x_com + ancho / 2, h_com,
       col = col_med, border = "gray30")
  if (corte_com) dibujar_corte(x_com - ancho / 2, x_com + ancho / 2, h_com * 0.75)
  text(x_com, h_com * ifelse(corte_com, 0.38, 0.5), "Individuos\ncomprometidos", font = 2)
  text(x_com, h_com, fmt(comprometidos), pos = 3, font = 2, cex = 1.4)

  # --- Barra de afectados (abajo, con trama) ---
  rect_trama(x_afe - ancho / 2, -h_afe, x_afe + ancho / 2, 0)
  if (corte_afe) dibujar_corte(x_afe - ancho / 2, x_afe + ancho / 2, -h_afe * 0.75)
  texto_fondo(x_afe, -h_afe * ifelse(corte_afe, 0.38, 0.5), "Individuos\nafectados")
  text(x_afe, -h_afe, fmt(afectados), pos = 1, font = 2, cex = 1.4)

  # --- L\u00ednea del cero ---
  segments(0.3, 0, 2.8, 0, lwd = 1.5)

  invisible(NULL)
}

