#' @noRd
.onLoad <- function(libname, pkgname) {
  # Se ejecuta de forma silenciosa; si falla, no bloquea la carga del paquete
  try(
    if (!reticulate::py_module_available("simplekml")) {
      message("Instalando simplekml de Python...")
      reticulate::py_install("simplekml")
    }, silent = TRUE
  )
}
