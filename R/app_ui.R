#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = tags$a(
        tags$img(height = "35px", src = "www/logo_geobiota_mini.png", style = "margin-right: 10px"),
        "Dashboard Informe de Experto, PAS 150"
      ),
      theme = bslib::bs_theme(
        version = 5,
        bootswatch = "minty",
        secondary = "#dadddaff"
      ),
      sidebar = mod_input_ui("input_carto"),
      bslib::nav_spacer(),
      bslib::nav_panel(
        title = "Importante",
        icon = bsicons::bs_icon("info-circle"),
        mod_check_ui("importante")
      ),
      bslib::nav_panel(
        title = "Check. cartografía",
        icon = bsicons::bs_icon("check-circle"),
        mod_check_ui("check_carto")
      ),
      bslib::nav_menu(
        title = "Info cuenca",
        icon = icon("info"),
        bslib::nav_panel(
          title = "Info. subsubcuenca",
          icon = icon("info"),
          mod_info_cuenca_ui("info_cuenca")
        ),
        bslib::nav_panel(
          title = "Uso suelo",
          icon = icon("map"),
          mod_uso_suelo_ui("uso_suelo")
        ),
        bslib::nav_panel(
          title = "Vegetación",
          icon = icon("leaf"),
          mod_vegetacion_ui("vegetacion")
        )
      ),
      bslib::nav_menu(
        title = "BNP",
        icon = icon("tree-deciduous", lib = "glyphicon"),
        bslib::nav_panel(
          title = "Inv. Forestales",
          icon = icon("tree"),
          mod_inv_for_ui("inventarios")
        ),
        bslib::nav_panel(
          title = "Intervención",
          icon = icon("person-digging"),
          mod_afectacion_ui("afectacion")
        ),
        bslib::nav_panel(
          title = "Biodiversidad",
          icon = bsicons::bs_icon("flower2"),
          mod_biodiversidad_ui("biodiversidad")
        )
      ),
      bslib::nav_panel(
        title = "Fragmentación",
        icon = icon("layer-group"),
        mod_fragmentacion_ui("fragmentacion")
      ),
      bslib::nav_menu(
        title = "Autor",
        align = "right",
        icon = icon("user"),
        bslib::nav_item(
          tags$a(
            style = "display: block; margin-left: auto; margin-right: auto; text-align: center;",
            tags$img(
              src = "https://avatars.githubusercontent.com/u/74486319?s=400&u=c277213b232af5e7710bebdc7a50bb9426ab9a62&v=4",
              width = "75%",
              style = "border-radius: 50%;"
            ),
            tags$p("David Martínez")
          )
        ),
        bslib::nav_item(
          tags$a(
            id = "link_github",
            shiny::icon("github"),
            "Github",
            href = "https://github.com/DavidJMartinezS",
            target = "_blank",
            class = "action-button"
          )
        ),
        bslib::nav_item(
          tags$a(
            id = "link_geobiota",
            shiny::icon("globe"),
            "Geobiota",
            href = "https://geobiota.com/",
            target = "_blank",
            class = "action-button"
          )
        )
      ),
      bslib::nav_item(
        bslib::input_dark_mode(id = "dark_mode", mode = "light")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "PAS.150"
    ),
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),
    # Add here other external resources
    shinyjs::useShinyjs(),
    tags$script(
      HTML(
        'setTimeout(() => $(".shiny-bound-input[type=\'file\']").css("all","unset").css("display", "none"), 750);'
      )
    )
  )
}
