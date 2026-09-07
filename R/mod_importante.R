#' importante UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_importante_ui <- function(id){
  tags$div(
    class = "p-5",
    style = "background-color: #f8f9fa; min-height: 100vh;",
      
    # Flujo de trabajo ----
    tags$div(
      tags$div(
        class = "text-center mb-5",
        tags$h2(
          "Flujo de Trabajo",
          class = "fw-bold mb-2",
          style = "color: #65a498ff;"
        ),
        tags$p(
          "Dashboard elaborado con el objetivo de apoyar la generación de insumos para los informes de experto del PAS 150. Por favor considerar todas las intrucciones y sugerencias mencionados en esta pestaña",
          class = "text-muted lead mb-0"
        )
      ),
      
      # Grid de 4 pasos (2 columnas, responsive)
      tags$div(
        class = "row g-4",
        
        # Paso 1
        tags$div(
          class = "col-md-6 col-lg-6",
          tags$div(
            class = "h-100 p-4 bg-white rounded shadow-sm",
            style = "border-left: 5px solid #65a498ff;",
            tags$div(
              class = "d-flex align-items-center mb-3",
              tags$div(
                class = "d-flex align-items-center justify-content-center rounded-circle flex-shrink-0 me-3",
                style = "width: 50px; height: 50px; background-color: #65a498ff; color: white; font-size: 1.5rem; font-weight: bold;",
                "1"
              ),
              tags$h5(
                "Preparar inputs",
                class = "fw-bold mb-0",
                style = "color: #65a498ff;"
              )
            ),
            tags$div(
              tags$p("Preparar los siguientes insumos basicos de la aplicación.", class = "text-muted mb-2"),
              tags$ul(
                class = "mb-0 text-muted",
                tags$li("Capa uso y vegetación de la cuenca."),
                tags$li("Layout de obras."),
                tags$li("Compilado de censos."),
                tags$li("Base de datos de inventario florístico."),
                tags$li("Base de datos de inventario forestal.")
              )
            )
          )
        ),
        
        # Paso 2
        tags$div(
          class = "col-md-6 col-lg-6",
          tags$div(
            class = "h-100 p-4 bg-white rounded shadow-sm",
            style = "border-left: 5px solid #65a498ff;",
            tags$div(
              class = "d-flex align-items-center mb-3",
              tags$div(
                class = "d-flex align-items-center justify-content-center rounded-circle flex-shrink-0 me-3",
                style = "width: 50px; height: 50px; background-color: #65a498ff; color: white; font-size: 1.5rem; font-weight: bold;",
                "2"
              ),
              tags$h5(
                "Revisar Capas Preliminares",
                class = "fw-bold mb-0",
                style = "color: #65a498ff;"
              )
            ),
            tags$p(
              "Revisar y validar las capa de uso y vegetación junto con las bases de datos de terreno desde la pestaña 'Exploración de datos'. Para el caso de las bases de datos, realizar el chequeo con el respectivo boton 'Check'. Realizar los cambios que requiera y cargue nuevamente los inputs en la aplicación.",
              class = "text-muted mb-0"
            )
          )
        ),
        
        # Paso 3
        tags$div(
          class = "col-md-6 col-lg-6",
          tags$div(
            class = "h-100 p-4 bg-white rounded shadow-sm",
            style = "border-left: 5px solid #65a498ff;",
            tags$div(
              class = "d-flex align-items-center mb-3",
              tags$div(
                class = "d-flex align-items-center justify-content-center rounded-circle flex-shrink-0 me-3",
                style = "width: 50px; height: 50px; background-color: #65a498ff; color: white; font-size: 1.5rem; font-weight: bold;",
                "3"
              ),
              tags$h5(
                "Generar Cartografía Digital",
                class = "fw-bold mb-0",
                style = "color: #65a498ff;"
              )
            ),
            tags$p(
              "Una vez ingresados los inputs correctamente, podrá generar la cartografía digital desde la misma barra lateral.",
              class = "text-muted mb-0"
            )
          )
        ),
        
        # Paso 4
        tags$div(
          class = "col-md-6 col-lg-6",
          tags$div(
            class = "h-100 p-4 bg-white rounded shadow-sm",
            style = "border-left: 5px solid #65a498ff;",
            tags$div(
              class = "d-flex align-items-center mb-3",
              tags$div(
                class = "d-flex align-items-center justify-content-center rounded-circle flex-shrink-0 me-3",
                style = "width: 50px; height: 50px; background-color: #65a498ff; color: white; font-size: 1.5rem; font-weight: bold;",
                "4"
              ),
              tags$h5(
                "Generar Apéndices",
                class = "fw-bold mb-0",
                style = "color: #65a498ff;"
              )
            ),
            tags$div(
              tags$p("Genera los apéndices y documentos complementarios", class = "text-muted mb-2"),
              tags$ul(
                class = "mb-0 text-muted",
                tags$li("Análisis de biodiversidad."),
                tags$li("Análisis de inventarios forestales."),
                tags$li("Análisis de fragmentación.")
              )
            )
          )
        )
      )
    ),

    # Consideraciones ----
    tags$div(
      class= "mt-5",
      tags$div(
        class = "text-center mb-5",
        tags$h2(
          "Consideraciones para el uso",
          class = "fw-bold mb-2",
          style = "color: #65a498ff;"
        )
      ),
      
      # Grid 2 columnas
      tags$div(
        class = "row g-4",
        
        # Card 1: Buenas prácticas
        tags$div(
          class = "col-md-6 col-lg-6",
          tags$div(
            class = "d-flex gap-3 p-4 bg-white rounded",
            style = "border-left: 4px solid #65a498ff; height: 100%;",
            tags$div(
              class = "flex-shrink-0 text-center",
              style = "min-width: 60px;",
              bsicons::bs_icon("pencil-square", size = "2.5em", class = "text-primary")
            ),
            tags$div(
              class = "flex-grow-1",
              tags$h5(
                "Uso responsable",
                class = "fw-bold mb-2",
                style = "color: #65a498ff;"
              ),
              tags$p(
                "Seguir las indicaciones, cuestionar el proceso y verificar siempre los resultados.",
                class = "mb-0 text-muted"
              )
            )
          )
        ),
        
        # Card 2: Carga de Shapefiles
        tags$div(
          class = "col-md-6 col-lg-6",
          tags$div(
            class = "d-flex gap-3 p-4 bg-white rounded",
            style = "border-left: 4px solid #65a498ff; height: 100%;",
            tags$div(
              class = "flex-shrink-0 text-center",
              style = "min-width: 60px;",
              bsicons::bs_icon("file-earmark-arrow-up-fill", size = "2.5em", class = "text-primary")
            ),
            tags$div(
              class = "flex-grow-1",
              tags$h5(
                "Carga de Shapefiles",
                class = "fw-bold mb-2",
                style = "color: #65a498ff;"
              ),
              tags$p(
                "Cargar los 4 archivos:", tags$code(".shp"), ", ", tags$code(".dbf"), ", ", tags$code(".prj"), " y ", tags$code(".shx"),". De otro modo no se reconocerán.",
                class = "mb-0 text-muted"
              )
            )
          )
        ),
        
        # Card 3: Especie objetivo
        tags$div(
          class = "col-md-6 col-lg-6",
          tags$div(
            class = "d-flex gap-3 p-4 bg-white rounded",
            style = "border-left: 4px solid #65a498ff; height: 100%;",
            tags$div(
              class = "flex-shrink-0 text-center",
              style = "min-width: 60px;",
              bsicons::bs_icon("flower3", size = "2.5em", class = "text-primary")
            ),
            tags$div(
              class = "flex-grow-1",
              tags$h5(
                "Especie objetivo",
                class = "fw-bold mb-2",
                style = "color: #65a498ff;"
              ),
              tags$div(
                tags$p("El nombre de la especie objetivo debe ser homologado en todos los inputs. Por ejemplo, si se selecciona la especie 'Neltuma chilensis'", class = "text-muted mb-2"),
                tags$ul(
                  class = "mb-0 text-muted",
                  tags$li(
                    icon("check", class = "text-success me-2"), 
                    "En las capas de uso y vegetación, censos, y bases de datos debe estar la especie mencionada como tal"
                  ),
                  tags$li(
                    icon("xmark", class = "text-danger me-2"), 
                    "No se reconocera con alguna sinonímia como 'Prosopis chilensis' o un nombre mal escrito como 'Neltuma chilense'."
                  )
                )
              )
            )
          )
        ),
        
        # Card 4: Campos Requeridos
        tags$div(
          class = "col-md-6 col-lg-6",
          tags$div(
            class = "d-flex gap-3 p-4 bg-white rounded",
            style = "border-left: 4px solid #65a498ff; height: 100%;",
            tags$div(
              class = "flex-shrink-0 text-center",
              style = "min-width: 60px;",
              bsicons::bs_icon("check-circle-fill", size = "2.5em", class = "text-primary")
            ),
            tags$div(
              class = "flex-grow-1",
              tags$h5(
                "Campos Requeridos",
                class = "fw-bold mb-2",
                style = "color: #65a498ff;"
              ),
              tags$p(
                "Ingresar shapefiles y Excel con los campos requeridos. Deben contener los mismos caracteres que se indican, pero pueden diferir en tildes y mayúsculas. Cada input debe tener los siguientes campos como mínimos para que generar los resultados deseados:",
                class = "mb-0 text-muted"
              )
            )
          )
        )
      ),

      tags$table(
        class = "styled-table text-muted",
        tags$thead(
          table_row(c("Input", "Campos mínimos"), header = T)
        ),
        tags$tbody(
          table_row(c(
            "Uso-vegetación",
            req_names$uso_veg %>%
              paste(collapse = ", ") %>%
              paste(
                ", más los campos con las ECC (ej: ECC1, ECC_1, ECC1_Art19, ECC...)"
              )
          )),
          table_row(c(
            "Obras",
            "Campos que contengan el caracter 'obra'. Ej: 'Obra', 'Nom_obra', 'Obra_detal'"
          )),
          table_row(c("Censo", "Especie")),
          table_row(c(
            "BNP a intervenir",
            req_names$BNP_afect %>%
              paste(collapse = ", ") %>%
              paste(
                ", campos que contengan el caracter 'obra', más los campos con las ECC (ej: ECC1, ECC_1, ECC1_Art19, ECC...). 
                Si desea estimar el censo de algunos poligonos incluir un campo 'Censado' señalando con un 'Si' o 'No' si el poligono se encuentra censado"
              )
          )),
          table_row(c(
            "BNP a alterar",
            sprintf(
              "Campos que contengan el caracter 'obra' si la capa no esta lista. \nDe lo contrario añadir a las obras: %s",
              req_names$BNP_afect %>%
                paste(collapse = ", ") %>%
                paste(
                  ", más los campos con las ECC (ej: ECC1, ECC_1, ECC1_Art19, ECC...).
                  Si desea estimar el censo de algunos poligonos incluir un campo 'Censado' señalando con un 'Si' o 'No' si el poligono se encuentra censado"
                )
            )
          )),
          table_row(c(
            "BD flora",
            req_names$BD_flora %>% paste(collapse = ", ")
          )),
          table_row(c(
            "BD inventarios",
            req_names$BD_fore %>% paste(collapse = ", ")
          ))
        )
      ),
      
      # Consideraciones cartorgaficas y BDD
      tags$div(
        class = "row g-4 mt-3 text-muted",
        tags$div(
          class = "col-md-6 col-lg-6",
          tags$h4(
            bsicons::bs_icon("map", class = "me-2 text-primary"),
            "Consideraciones para la cartorgafía de uso y vegetación",
            class = "fw-bold"
          ),
          tags$ul(
            class = "mb-1",
            list_to_li(c(
              "Si un poligono de bosque contiene una especie en categoría debe figurar como 'Bosque nativo de preservación' en el campo de 'F_ley20283'.",
              "Si un poligono es BNP, debe indicar en los campos de 'ECC...' la o las especies en categoría de conservación.",
              "Todos los bosques nativos deben tener indicado un tipo y subtipo forestal."
            ))
          )
        ),
        tags$div(
          class = "col-md-6 col-lg-6",
          tags$h4(
            bsicons::bs_icon("table", class = "me-2 text-primary"),
            "Consideraciones para las bases de datos",
            class = "fw-bold"
          ),
          tags$ul(
            class = "mb-1",
            tags$li(
              "Fijarse en la consistencia de las parcelas y sus coordenadas, cada parcela debe tener coordenadas unicas."
            ),
            tags$li(
              "Al ingresar las bases de datos estas son automaticamente filtradas a las parcelas que se encuentran dentro de los BNP de la cuenca. Seleccionar 'BD lista' para usar la base tal como la ingresó."
            ),
            tags$li(
              "Se recomienda realizar un chequeo de las bases de datos antes de utilizarlas dando click al boton 'Check'."
            ),
            tags$li(
              "Los campos 'Sup_parcela', 'UTM_E', 'UTM_N', 'N_ind', 'DAP' y 'Altura' deben ser numéricos. Fijarse bien en el separador de decimales, que no se utilicen puntos y comas en la misma base de datos."
            ),
            tags$li(
              "En la base de datos de inventarios forestales, fijarse que solo estén las especies que sean de su interés presentar. Generalmente se dejan fuera del analisis las especies exóticas como ",
              tags$i("'Olea europaea', 'Salix babylonica', 'Populus sp.'"),
              ", etc."
            ),
            tags$li(
              "El campo 'Estado' debe estar idealmente completo al menos para la especie objetivo del infome, con una de las siguientes clases: 'Adulto', 'Brinzal', 'Regeneración'."
            )
          )
        )
      )
    )
  )
}
    
#' importante Server Functions
#'
#' @noRd 
mod_importante_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_importante_ui("importante_1")
    
## To be copied in the server
# mod_importante_server("importante_1")
