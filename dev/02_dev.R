# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering
usethis::use_package('bslib')
usethis::use_package('bsicons')
usethis::use_package('dplyr')
usethis::use_package('DT')
usethis::use_package('tidyr')
usethis::use_package('janitor')
usethis::use_package('units')
usethis::use_package('stringi')
usethis::use_package('gt')
usethis::use_package('leaflet')
usethis::use_package('leaflet.extras')
usethis::use_package('sf')
usethis::use_package('shinyjs')
usethis::use_package('shinyWidgets')
usethis::use_package('shinybusy')
usethis::use_package('shinyalert')
usethis::use_package('R6')
usethis::use_package('tools')
usethis::use_package('purrr')
usethis::use_package('dataPAS')
usethis::use_package('elevatr')
usethis::use_package('stars')
usethis::use_package('elevatr')
usethis::use_package('plyr')
usethis::use_package('openxlsx2')
usethis::use_package('tibble')
usethis::use_package('kableExtra')
usethis::use_package('igraph')
usethis::use_package('stringdist')
usethis::use_package('ggthemes')
usethis::use_package('forcats')
usethis::use_package('magick')
usethis::use_package('rlang')
usethis::use_package('plotly')
usethis::use_package('readxl', )
usethis::use_package('shinyFiles')
usethis::use_package('leafpop')
usethis::use_package('writexl')

tabla_guia_fragmentacion <- read_xlsx("inst/app/www/tabla_guia_fragmentacion.xlsx") %>% tibble::as_tibble()
usethis::use_data(tabla_guia_fragmentacion)

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "read_sf") # Name of the module
golem::add_module(name = "downfiles") # Name of the module
golem::add_module(name = "input") # Name of the module
golem::add_module(name = "check") # Name of the module
golem::add_module(name = "info_cuenca") # Name of the module
golem::add_module(name = "basic_input") # Name of the module
golem::add_module(name = "parcelas") # Name of the module
golem::add_module(name = "bases_carto") # Name of the module
golem::add_module(name = "get_down_carto") # Name of the module
golem::add_module(name = "apendices") # Name of the module
golem::add_module(name = "uso_suelo") # Name of the module
golem::add_module(name = "vegetacion") # Name of the module
golem::add_module(name = "fragstats") # Name of the module
golem::add_module(name = "importante") # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers", with_test = TRUE)
golem::add_utils("ui", with_test = F)
golem::add_utils("server", with_test = F)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")
golem::add_any_file("file.json")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("PAS.150")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
# covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
