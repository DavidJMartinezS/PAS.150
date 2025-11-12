#' @noRd
sf <- R6::R6Class(
  classname = "read_sf",
  public = list(
    sf_file = NULL,
    fx = NULL,
    path = F,
    geometry_type = NULL,
    validate_sf = function() {
      (self$sf_file$datapath %>% tools::file_ext() %in% c("dbf", "prj", "shp", "shx") %>% all() &
         self$sf_file$name %>% tools::file_path_sans_ext() %>% unique() %>% length() == 1 &
         length(self$sf_file$datapath) >= 4)
    },
    invalidate_sf = function() {
      unlink(self$sf_file$datapath)
      shinyjs::reset(id = "sf_file")
    },
    leer_sf = function(...) {
      if (self$path) {
        return(self$sf_file$name[1] %>% tools::file_path_sans_ext())
      } else {
        shpdf <- self$sf_file
        tempdirname <- dirname(shpdf$datapath[1])

        for (i in 1:nrow(shpdf)) {
          file.rename(
            shpdf$datapath[i],
            file.path(tempdirname, shpdf$name[i])
          )
        }

        shp <- sf::read_sf(paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep="/"), ...) 
        geom_type <- if(sf::st_geometry_type(shp, by_geometry = F) %in% c("POLYGON", "MULTIPOLYGON")) {
          "POLYGON"
        } else if(sf::st_geometry_type(shp, by_geometry = F) %in% c("LINESTRING", "MULTILINESTRING")) {
          "LINESTRING"
        } else if(sf::st_geometry_type(shp, by_geometry = F) %in% c("POINT", "MULTIPOINT")) {
          "POINT"
        }
        shp <- shp %>%
          sf::st_zm() %>%
          sf::st_make_valid() %>% 
          sf::st_collection_extract(geom_type) %>% 
          sf::st_transform(get_utm_epsg(.)) %>% 
          {if (!is.null(self$fx)) .[] %>% self$fx() else .} 

        return(shp)
      }
    },
    initialize = function(sf_file, fx = NULL, path = F, geometry_type = "POLYGON") {
      self$sf_file <- sf_file
      self$fx <- fx
      self$path <- path
      self$geometry_type <- geometry_type
    }
  )
)

#' @noRd
sf_path <- R6::R6Class(
  public = list(
    sf_file = NULL,
    path_shp = NULL,
    validate_sf = function() {
      (self$sf_file$datapath %>% tools::file_ext() %in% c("dbf", "prj", "shp", "shx") %>% all() &
         self$sf_file$name %>% tools::file_path_sans_ext() %>% unique() %>% length() == 1 &
         length(self$sf_file$datapath) >= 4)
    },
    invalidate_sf = function() {
      unlink(self$sf_file$datapath)
      shinyjs::reset(id = "sf_file")
    },
    rename_sf = function() {
      shpdf <- self$sf_file
      tempdirname <- dirname(shpdf$datapath[1])
      file.rename(
        shpdf$datapath,
        file.path(tempdirname, shpdf$name)
      )
      self$path_shp <- file.path(tempdirname, shpdf$name) %>% subset(grepl(".shp$", .))
    },
    initialize = function(sf_file) {
      self$sf_file <- sf_file
      self$rename_sf()
    }
  )
)