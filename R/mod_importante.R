#' importante UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_importante_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "importante",
    tags$h3("Consideraciones para el uso correcto de la aplicación"),
    tags$span(
      "Este Dashboard esta elaborado con el objetivo de apoyar la elaboración del informe de experto referido al PAS150. Por favor considerar todas las intrucciones y consejos mencionados en esta página para evitar errores"
      # style = "margin-bottom: 20px; color: rgba(128, 142, 139, 1);"
    ),
    tags$br(),
    tags$br(),
    tags$ul(
      class = "ul_main",
      # style = "color: rgba(117, 126, 124, 1);",
      tags$li(
        "Para ingresar los shapefiles, ",
        tags$b(
          "debe cargar los 4 archivos necesarios para su lectura; 'dbf', 'prj' 'shp' y 'shx'."
        ),
        " De otro modo no se reconocerá el shapefile."
      ),
      tags$li(
        "Cada input debe tener los siguientes campos como mínimos para que generar los resultados deseados.",
        tags$br(),
        tags$table(
          class = "styled-table",
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
        tags$b(
          "Los nombres de estos no deben coincidir necesariamente en mayúsculas o tildes, pero si en los caractéres alfa-numéricos."
        ),
        "Ej: Tipo_For, tipo_for, TIPO_FOR, pero ",
        tags$u("NO"),
        "; TipoFor, Tipo.For, Tipo_forestal, etc"
      ),
      tags$li(
        "Para generar la cartografía digital debe haberse ingresado al menos 4 inputs basicos, que son:",
        tags$ul(
          class = "ul_sec",
          list_to_li(c(
            "Cartografía de uso y vegetación de la cuenca",
            "Especie objeto del informe",
            "Layout de obras",
            "Compilado de censos"
          ))
        ), 
        "Si in gresó"
      ),
      tags$li(
        "Para generar los apéndices de biodiversidad e inventarios forestales se requiere haber cargado previamente la cartorgafía de uso y vegetación de la cuenca además de la base de datos correspondiente"
      ),
      tags$li(
        "Uno de los inputs más importante son la capa de cartorgafía de uso y vegetación junto con las bases de datos de parcelas. Por lo cual es importante que los datos que se ingresen no contenga errores. En la ventana de 'Exploración de datos' se presentan algunos resumenes de datos que facilitan la detección de errores."
      ),
      tags$li(
        "Respecto de la ",
        tags$b(
          "cartografía de uso y vegetación",
          ", tener en consideración lo siguiente:"
        ),
        tags$ul(
          class = "ul_sec",
          list_to_li(c(
            "Si un poligono de bosque contiene una especie en categoría debe figurar como 'Bosque nativo de preservación' en el campo de 'F_ley20283'.",
            "Si un poligono es BNP, debe indicar en el campo 'BNP_ECC' la o las especies en categoría de conservación. De no ser BNP, dejar el campo vacío.",
            "Todos los bosques nativos deben tener indicado un tipo y subtipo forestal."
          ))
        )
      ),
      tags$li(
        "En cuanto a las bases de datos de flora e inventarios, tener en consideración lo siguiente:",
        tags$ul(
          class = "ul_sec",
          tags$li(
            "Fijarse en la consistencia de las parcelas y sus coordenadas, cada parcela debe tener coordenadas unicas"
          ),
          tags$li(
            "Al ingresar las bases de datos estas son automaticamente filtradas a las parcelas que se encuentran dentro de los BNP de la cuenca. ",
            tags$b(
              "Seleccionar 'BD lista' para usar la base tal como la ingresó"
            )
          ),
          tags$li(
            "Se recomienda realizar un chequeo de las bases de datos antes de utilizarlas dando click al boton 'Check'"
          ),
          tags$li(
            "Los campos 'Sup_parcela', 'UTM_E', 'UTM_N', 'N_ind', 'DAP' y 'Altura' deben ser numéricos. Fijarse bien en el separador de decimales, que no se utilicen puntos y comas en la misma base de datos"
          ),
          tags$li(
            "En la base de datos de inventarios forestales, fijarse que solo estén las especies que sean de su interés presentar. Generalmente se dejan fuera del analisis las especies exóticas como ",
            tags$i("'Olea europaea', 'Salix babylonica', 'Populus sp.'"),
            ", etc"
          ),
          tags$li(
            "El campo 'Estado' debe estar idealmente completo al menos para la especie objetivo del infome, con una de las siguientes clases: 'Adulto', 'Brinzal', 'Regeneración'"
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
