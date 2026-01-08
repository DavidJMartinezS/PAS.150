#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @import dataPAS
#' @import ggthemes 
#' @import shiny
#' @importFrom golem with_golem_options
#' @importFrom bsicons bs_icon 
#' @importFrom bslib accordion accordion_panel bs_theme card card_body card_header input_dark_mode input_switch layout_column_wrap layout_columns nav_item nav_menu nav_panel nav_spacer navset_card_underline page_fillable page_navbar sidebar tooltip value_box 
#' @importFrom config get 
#' @importFrom dplyr across add_count add_row add_tally arrange bind_cols bind_rows case_when contains count ends_with filter full_join group_by if_all if_else inner_join last_col left_join matches mutate mutate_all mutate_at mutate_if n pull relocate rename rename_all rename_at rename_if row_number select slice slice_head slice_max slice_tail starts_with summarise summarise_all summarise_at syms tally ungroup vars 
#' @importFrom DT DTOutput datatable formatStyle renderDT
#' @importFrom elevatr get_elev_raster 
#' @importFrom forcats as_factor fct_na_value_to_level 
#' @importFrom fs dir_tree 
#' @importFrom gt cell_fill cell_text cells_body cells_column_labels cells_column_spanners cells_footnotes cells_stub cells_stubhead cells_title cols_align cols_label cols_width everything fmt_number fmt_percent gt gt_output matches md px render_gt starts_with tab_footnote tab_header tab_spanner tab_stubhead tab_style 
#' @importFrom igraph components graph_from_adjacency_matrix 
#' @importFrom janitor adorn_pct_formatting adorn_totals round_half_up 
#' @importFrom kableExtra kable_styling kbl 
#' @importFrom leaflet addAwesomeMarkers addCircles addLayersControl addLegend addMeasure addMiniMap addPolygons addProviderTiles addScaleBar colorFactor highlightOptions layersControlOptions leaflet leafletOutput makeAwesomeIcon providers renderLeaflet scaleBarOptions 
#' @importFrom leaflet.extras addFullscreenControl addResetMapButton 
#' @importFrom leafpop popupTable 
#' @importFrom magick image_info image_read image_read_svg 
#' @importFrom openxlsx2 fmt_txt int2col wb_add_border wb_add_cell_style wb_add_data wb_add_data_table wb_add_fill wb_add_font wb_add_formula wb_add_image wb_add_numfmt wb_add_pivot_table wb_add_worksheet wb_color wb_data wb_dims wb_merge_cells wb_page_setup wb_save wb_set_col_widths wb_workbook write_xlsx
#' @importFrom plotly layout plot_ly plotlyOutput renderPlotly 
#' @importFrom plyr round_any 
#' @importFrom purrr compact map map_dbl map_lgl map2_dbl pmap_chr pmap_dbl pwalk keep
#' @importFrom R6 R6Class 
#' @importFrom readxl read_xls read_xlsx 
#' @importFrom rlang list2 
#' @importFrom sf read_sf st_area st_as_sf st_as_text st_bbox st_buffer st_cast st_centroid st_collection_extract st_combine st_coordinates st_crs st_difference st_drop_geometry st_filter st_geometry st_geometry_type st_intersection st_intersects st_is_within_distance st_join st_make_valid st_nearest_feature st_relate st_set_geometry st_transform st_union st_zm write_sf 
#' @importFrom shinyFiles shinyDirButton shinyDirChoose 
#' @importFrom shinyWidgets pickerInput pickerOptions updatePickerInput 
#' @importFrom shinyalert shinyalert 
#' @importFrom shinybusy notify_success remove_modal_spinner report_failure report_success show_modal_spinner 
#' @importFrom shinyjs disable disabled enable reset useShinyjs 
#' @importFrom stars read_stars st_as_stars st_contour st_rasterize write_stars 
#' @importFrom stats quantile 
#' @importFrom stringdist stringdistmatrix 
#' @importFrom stringi stri_cmp_equiv stri_count_regex stri_detect_fixed stri_detect_regex stri_extract stri_extract_all_regex stri_extract_first_words stri_extract_last_words stri_replace_all_fixed stri_replace_all_regex stri_replace_first_fixed stri_replace_first_regex stri_split_regex stri_sub stri_trans_general stri_trans_tolower stri_trans_totitle stri_trans_toupper stri_trim stri_trim_both 
#' @importFrom tibble add_column add_row as_tibble as_tibble_col rowid_to_column rownames_to_column tibble 
#' @importFrom tidyr pivot_longer pivot_wider replace_na separate separate_wider_delim spread 
#' @importFrom tools file_ext file_path_as_absolute file_path_sans_ext 
#' @importFrom units drop_units set_units 
#' @importFrom zip zip 
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
