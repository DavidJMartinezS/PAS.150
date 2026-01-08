#' downfiles UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_downfiles_ui <- function(
    id,
    icon = shiny::icon("download"),
    label = "",
    class = "btn-success",
    ...
) {
  ns <- NS(id)
  tags$div(
    shinyjs::disabled(downloadButton(
      outputId = ns("down_btn"),
      icon = icon,
      label = label,
      class = class,
      ...
    )),
    style = "margin-left: 15px;"
  )
}
    
#' downfiles Server Functions
#'
#' @noRd 
mod_downfiles_server <- function(id, x, name_save){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observe({
      if(isTruthy(x())) shinyjs::enable("down_btn") else shinyjs::disable("down_btn")
    })

    filetype <- reactive({
      x() %>%
        {if(any(class(.) == "list")) . else list(.)} %>%
        purrr::map( ~ ifelse(
          inherits(., "wbWorkbook"),
          "wb",
          ifelse(
            inherits(., "sf"),
            "sf",
            ifelse(inherits(., "data.frame"), "xlsx", "")
          )
        )) %>%
        {if(length(.) == 1) unlist(.) else .}
    })

    output$down_btn <- downloadHandler(
      filename = function() {
        if (length(filetype()) > 1) {
          ifelse(is.null(names(name_save)), "Archivos_comprimidos.zip", paste0(names(name_save), ".zip"))
        } else {
          paste0(as.character(name_save), ifelse(filetype() == "sf", ".zip", ".xlsx"))
        }
      },
      content = function(file) {
        temp_dir <- tempdir()
        wd <- getwd()
        setwd(temp_dir)
        file.remove(list.files(pattern = "\\."))
        purrr::pwalk(
          if(length(filetype()) == 1) {
            list(list(x()), list(filetype()), ifelse(inherits(name_save, "list"), name_save, list(name_save)))
          } else {
            list(x(), filetype(), unlist(name_save))
          },
          .f = function(x, y, z) {
            switch(
              y,
              sf = sf::write_sf(x, paste0(tools::file_path_sans_ext(z), ".shp")),
              wb = openxlsx2::wb_save(x, paste0(tools::file_path_sans_ext(z), ".xlsx"), overwrite = T),
              xlsx = openxlsx2::write_xlsx(x, paste0(tools::file_path_sans_ext(z), ".xlsx"))
            )
          }
        )
        list_files <- unname(unlist(purrr::map(unlist(name_save), function(x){list.files(pattern = x)})))
        if(tools::file_ext(file) == "zip") {
          zip::zip(zipfile = file, files = list_files)
        } else {
          file.copy(from = list_files, to = file, overwrite = T)
        }
        setwd(wd)
      }
    )
  })
}
    
## To be copied in the UI
# mod_downfiles_ui("downfiles_1")
    
## To be copied in the server
# mod_downfiles_server("downfiles_1")
